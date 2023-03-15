# --------------------------------------------------------------------------
# --------- Script for calculating and plotting the transboundary ----------
# --------------------------- rate of spread -------------------------------
# --------------------------------------------------------------------------
# Set working directory ---------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Load the required libraries ---------------------------------------------
{ library(dplyr)
  library(sdmpredictors)
  library(gdistance)
  library(rSDM)
  library(SDraw)
  library(ggplot2)
  library(ccplot)
}

# Load the data sets -------------------------------------------------------

# Load the species distribution data
mg <- read.csv("./Data/Data - 2022/Final spreadsheet/Total_data_Atlantic_April_2022_Final.csv")

# Load the data for the initial introductions
mg_1 <- read.csv("./Data/First records/Initial_introductions.csv")

# Separate the database locations and the literature-based locations
database <- filter(mg_1, Type=="Database")
historic <- filter(mg_1, Type=="Introduction")

# Remove duplicates and keep earliest record ------------------------------

# Order the distribution data from the earliest record to most recent
mg <- mg[order(mg$Year, mg$Month, mg$Day, decreasing=FALSE),]
# Remove duplicates based on the longitude and latitude
mg <- mg[!duplicated(mg[c("Longitude", "Latitude")]), ]

# Load and crop the raster layer -------------------------------------------

# Load a bathymetry raster layer
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)
bathy <- bathy$MS_bathy_5m

# Crop raster to desired boundaries
min_lon <- -20
max_lon <- 20
min_lat <- 35
max_lat <- 65
geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)
Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))
coordinates(Sites.grid) <- ~ lon_bound + lat_bound
bathy <- crop(bathy, extent(Sites.grid))

# Clean the environment
rm(Sites.grid, geo_bounds, max_lat, max_lon, min_lat, min_lon)

# Convert projection of raster and make land impassable -------------------

# Convert projection of raster to UTM (which measures in metres instead of
# decimal degree)
bathy2 <- projectRaster(bathy, crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs" )

# Make land impassable
# Make all ocean cells equal to -999
bathy2[!is.na(bathy2)] <- -999
# Turn all land cells to NA
bathy2[bathy2>-999] <- NA
# Assign all ocean cells a value of 1
bathy2[bathy2==-999] <- 1

# Create a transition layer matrix using the raster -----------------------

# Checks whether cells are not NA and then creates a matrix listing all feasible connections between cells
trb <- transition(bathy2, mean, directions = 16) 
# Corrects for fact that diagonal movement is longer than straight lines
trb <- geoCorrection(trb, "c") 

rm(bathy2)

# Subset the data by year -------------------------------------------------

#table(mg$Year)

#1960s
{ mg_1965 <- filter(mg, Year==1965)
  mg_1967 <- filter(mg, Year==1967)
}
#1970s
{ mg_1974 <- filter(mg, Year==1974)
  mg_1976 <- filter(mg, Year==1976)
  mg_1978 <- filter(mg, Year==1978)
  mg_1979 <- filter(mg, Year==1979)
}
#1980s
{ mg_1980 <- filter(mg, Year==1980)
  mg_1982 <- filter(mg, Year==1982)
  mg_1983 <- filter(mg, Year==1983)
  mg_1984 <- filter(mg, Year==1984)
  mg_1985 <- filter(mg, Year==1985)
  mg_1986 <- filter(mg, Year==1986)
  mg_1987 <- filter(mg, Year==1987)
  mg_1988 <- filter(mg, Year==1988)
  mg_1989 <- filter(mg, Year==1989)
}
#1990s
{ mg_1990 <- filter(mg, Year==1990)
  mg_1991 <- filter(mg, Year==1991)
  mg_1992 <- filter(mg, Year==1992)
  mg_1993 <- filter(mg, Year==1993)
  mg_1994 <- filter(mg, Year==1994)
  mg_1995 <- filter(mg, Year==1995)
  mg_1996 <- filter(mg, Year==1996)
  mg_1997 <- filter(mg, Year==1997)
  mg_1998 <- filter(mg, Year==1998)
  mg_1999 <- filter(mg, Year==1999)
}
#2000s
{ mg_2000 <- filter(mg, Year==2000)
  mg_2001 <- filter(mg, Year==2001)
  mg_2002 <- filter(mg, Year==2002)
  mg_2003 <- filter(mg, Year==2003)
  mg_2004 <- filter(mg, Year==2004)
  mg_2005 <- filter(mg, Year==2005)
  mg_2006 <- filter(mg, Year==2006)
  mg_2007 <- filter(mg, Year==2007)
  mg_2008 <- filter(mg, Year==2008)
  mg_2009 <- filter(mg, Year==2009)
}
#2010s
{ mg_2010 <- filter(mg, Year==2010)
  mg_2011 <- filter(mg, Year==2011)
  mg_2012 <- filter(mg, Year==2012)
  mg_2013 <- filter(mg, Year==2013)
  mg_2014 <- filter(mg, Year==2014)
  mg_2015 <- filter(mg, Year==2015)
  mg_2016 <- filter(mg, Year==2016)
  mg_2017 <- filter(mg, Year==2017)
  mg_2018 <- filter(mg, Year==2018)
  mg_2019 <- filter(mg, Year==2019)
}
#2020s
{ mg_2020 <- filter(mg, Year==2020)
  mg_2021 <- filter(mg, Year==2021)
}

# Separate the first record (rows 3 and 6)
{ mg_database <- database[3, ]
  mg_historic <- historic[6, ]
}

# Convert points to spdf ---------------------------------------------------

#1960s
{ mg_1965 <- SpatialPointsDataFrame(coords=mg_1965[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1965)
mg_1967 <- SpatialPointsDataFrame(coords=mg_1967[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1967)
}
#1970s
{ mg_1974 <- SpatialPointsDataFrame(coords=mg_1974[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1974)
  mg_1976 <- SpatialPointsDataFrame(coords=mg_1976[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1976)
  mg_1978 <- SpatialPointsDataFrame(coords=mg_1978[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1978)
  mg_1979 <- SpatialPointsDataFrame(coords=mg_1979[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1979)
}
#1980s
{ mg_1980 <- SpatialPointsDataFrame(coords=mg_1980[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1980)
  mg_1982 <- SpatialPointsDataFrame(coords=mg_1982[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1982)
  mg_1983 <- SpatialPointsDataFrame(coords=mg_1983[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1983)
  mg_1984 <- SpatialPointsDataFrame(coords=mg_1984[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1984)
  mg_1985 <- SpatialPointsDataFrame(coords=mg_1985[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1985)
  mg_1986 <- SpatialPointsDataFrame(coords=mg_1986[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1986)
  mg_1987 <- SpatialPointsDataFrame(coords=mg_1987[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1987)
  mg_1988 <- SpatialPointsDataFrame(coords=mg_1988[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1988)
  mg_1989 <- SpatialPointsDataFrame(coords=mg_1989[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1989)
}
#1990s
{ mg_1990 <- SpatialPointsDataFrame(coords=mg_1990[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1990)
  mg_1991 <- SpatialPointsDataFrame(coords=mg_1991[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1991)
  mg_1992 <- SpatialPointsDataFrame(coords=mg_1992[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1992)
  mg_1993 <- SpatialPointsDataFrame(coords=mg_1993[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1993)
  mg_1994 <- SpatialPointsDataFrame(coords=mg_1994[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1994)
  mg_1995 <- SpatialPointsDataFrame(coords=mg_1995[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1995)
  mg_1996 <- SpatialPointsDataFrame(coords=mg_1996[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1996)
  mg_1997 <- SpatialPointsDataFrame(coords=mg_1997[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1997)
  mg_1998 <- SpatialPointsDataFrame(coords=mg_1998[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1998)
  mg_1999 <- SpatialPointsDataFrame(coords=mg_1999[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1999)
}
#2000s
{ mg_2000 <- SpatialPointsDataFrame(coords=mg_2000[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2000)
  mg_2001 <- SpatialPointsDataFrame(coords=mg_2001[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2001)
  mg_2002 <- SpatialPointsDataFrame(coords=mg_2002[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2002)
  mg_2003 <- SpatialPointsDataFrame(coords=mg_2003[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2003)
  mg_2004 <- SpatialPointsDataFrame(coords=mg_2004[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2004)
  mg_2005 <- SpatialPointsDataFrame(coords=mg_2005[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2005)
  mg_2006 <- SpatialPointsDataFrame(coords=mg_2006[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2006)
  mg_2007 <- SpatialPointsDataFrame(coords=mg_2007[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2007)
  mg_2008 <- SpatialPointsDataFrame(coords=mg_2008[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2008)
  mg_2009 <- SpatialPointsDataFrame(coords=mg_2009[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2009)
}
#2010s
{ mg_2010 <- SpatialPointsDataFrame(coords=mg_2010[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2010)
  mg_2011 <- SpatialPointsDataFrame(coords=mg_2011[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2011)
  mg_2012 <- SpatialPointsDataFrame(coords=mg_2012[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2012)
  mg_2013 <- SpatialPointsDataFrame(coords=mg_2013[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2013)
  mg_2014 <- SpatialPointsDataFrame(coords=mg_2014[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2014)
  mg_2015 <- SpatialPointsDataFrame(coords=mg_2015[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2015)
  mg_2016 <- SpatialPointsDataFrame(coords=mg_2016[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2016)
  mg_2017 <- SpatialPointsDataFrame(coords=mg_2017[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2017)
  mg_2018 <- SpatialPointsDataFrame(coords=mg_2018[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2018)
  mg_2019 <- SpatialPointsDataFrame(coords=mg_2019[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2019)
}
#2020s
{ mg_2020 <- SpatialPointsDataFrame(coords=mg_2020[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2020)
  mg_2021 <- SpatialPointsDataFrame(coords=mg_2021[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2021)
}

#initial
{ mg_database <- SpatialPointsDataFrame(coords=mg_database[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_database)
  mg_historic <- SpatialPointsDataFrame(coords=mg_historic[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_historic)
}

# Move points off of land -------------------------------------------------

# 1960s, 1970s, 1980s and initial introduction points are all within raster already

#1990s
{ mg_1995 <- points2nearestcell(mg_1995, bathy)
  mg_1998 <- points2nearestcell(mg_1998, bathy)
  mg_1999 <- points2nearestcell(mg_1999, bathy)
}
#2000s
{ mg_2004 <- points2nearestcell(mg_2004, bathy)
  mg_2005 <- points2nearestcell(mg_2005, bathy)
  mg_2006 <- points2nearestcell(mg_2006, bathy)
  mg_2007 <- points2nearestcell(mg_2007, bathy)
  mg_2008 <- points2nearestcell(mg_2008, bathy)
  mg_2009 <- points2nearestcell(mg_2009, bathy)
}
#2010s
{ mg_2010 <- points2nearestcell(mg_2010, bathy)
  mg_2011 <- points2nearestcell(mg_2011, bathy)
  mg_2012 <- points2nearestcell(mg_2012, bathy)
  mg_2013 <- points2nearestcell(mg_2013, bathy)
  mg_2014 <- points2nearestcell(mg_2014, bathy)
  mg_2015 <- points2nearestcell(mg_2015, bathy)
  mg_2016 <- points2nearestcell(mg_2016, bathy)
  mg_2017 <- points2nearestcell(mg_2017, bathy)
  mg_2018 <- points2nearestcell(mg_2018, bathy)
  mg_2019 <- points2nearestcell(mg_2019, bathy)
}
#2020s
{ mg_2020 <- points2nearestcell(mg_2020, bathy)
  mg_2021 <- points2nearestcell(mg_2021, bathy)
}

# Convert projection of points to UTM -------------------------------------

#1960s
{ mg_1965 <- spTransform(mg_1965, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1967 <- spTransform(mg_1967, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1970s
{ mg_1974 <- spTransform(mg_1974, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1976 <- spTransform(mg_1976, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1978 <- spTransform(mg_1978, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1979 <- spTransform(mg_1979, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1980s
{ mg_1980 <- spTransform(mg_1980, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1982 <- spTransform(mg_1982, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1983 <- spTransform(mg_1983, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1984 <- spTransform(mg_1984, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1985 <- spTransform(mg_1985, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1986 <- spTransform(mg_1986, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1987 <- spTransform(mg_1987, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1988 <- spTransform(mg_1988, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1989 <- spTransform(mg_1989, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1990s
{ mg_1990 <- spTransform(mg_1990, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1991 <- spTransform(mg_1991, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1992 <- spTransform(mg_1992, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1993 <- spTransform(mg_1993, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1994 <- spTransform(mg_1994, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1995 <- spTransform(mg_1995, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1996 <- spTransform(mg_1996, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1997 <- spTransform(mg_1997, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1998 <- spTransform(mg_1998, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1999 <- spTransform(mg_1999, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#2000s
{ mg_2000 <- spTransform(mg_2000, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2001 <- spTransform(mg_2001, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2002 <- spTransform(mg_2002, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2003 <- spTransform(mg_2003, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2004 <- spTransform(mg_2004, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2005 <- spTransform(mg_2005, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2006 <- spTransform(mg_2006, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2007 <- spTransform(mg_2007, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2008 <- spTransform(mg_2008, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2009 <- spTransform(mg_2009, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#2010s
{ mg_2010 <- spTransform(mg_2010, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2011 <- spTransform(mg_2011, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2012 <- spTransform(mg_2012, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2013 <- spTransform(mg_2013, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2014 <- spTransform(mg_2014, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2015 <- spTransform(mg_2015, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2016 <- spTransform(mg_2016, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2017 <- spTransform(mg_2017, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2018 <- spTransform(mg_2018, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2019 <- spTransform(mg_2019, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#2020s
{ mg_2020 <- spTransform(mg_2020, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2021 <- spTransform(mg_2021, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

#initial
{ mg_database <- spTransform(mg_database, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_historic <- spTransform(mg_historic, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

# Create a function to calculate the distances ----------------------------

# This function returns the seaway distance of all points in a spatial points
# data frame from one reference point. It takes three arguments: the spatial points
# data frame of multiple coordinates, the spatial points data frame of the 
# reference point to calculate to and a transition layer over which to calculate the
# distances

seaway_dist <- function(x, y, trb) { 
  
  z <- data.frame(matrix(ncol=length(x), nrow=1)) #Create an output for the distances matching the number of rows of the spdf
  
  for (i in 1:length(x)) {
    AtoB <- shortestPath(trb, c(x$Longitude[i], x$Latitude[i]), y, output="SpatialLines") 
    z[i] <- max(lineLength(AtoB, byid=TRUE)/1000)
  }
  z <- as.data.frame(t(z)) #Transform the output from row to column
}

# Calculate the shortest path to the database and historic introductions ----

#1960s
{ mg_1965@data$database_dist <- seaway_dist(mg_1965, mg_database, trb) 
mg_1965@data$historic_dist <- seaway_dist(mg_1965, mg_historic, trb)
mg_1967@data$database_dist <- seaway_dist(mg_1967, mg_database, trb) 
mg_1967@data$historic_dist <- seaway_dist(mg_1967, mg_historic, trb)
}
#1970s  
{ mg_1974@data$database_dist <- seaway_dist(mg_1974, mg_database, trb) 
  mg_1974@data$historic_dist <- seaway_dist(mg_1974, mg_historic, trb)
  mg_1976@data$database_dist <- seaway_dist(mg_1976, mg_database, trb) 
  mg_1976@data$historic_dist <- seaway_dist(mg_1976, mg_historic, trb)
  mg_1978@data$database_dist <- seaway_dist(mg_1978, mg_database, trb) 
  mg_1978@data$historic_dist <- seaway_dist(mg_1978, mg_historic, trb)
  mg_1979@data$database_dist <- seaway_dist(mg_1979, mg_database, trb) 
  mg_1979@data$historic_dist <- seaway_dist(mg_1979, mg_historic, trb)
}
#1980s
{ mg_1980@data$database_dist <- seaway_dist(mg_1980, mg_database, trb) 
  mg_1980@data$historic_dist <- seaway_dist(mg_1980, mg_historic, trb)
  mg_1982@data$database_dist <- seaway_dist(mg_1982, mg_database, trb) 
  mg_1982@data$historic_dist <- seaway_dist(mg_1982, mg_historic, trb)
  mg_1983@data$database_dist <- seaway_dist(mg_1983, mg_database, trb) 
  mg_1983@data$historic_dist <- seaway_dist(mg_1983, mg_historic, trb)
  mg_1984@data$database_dist <- seaway_dist(mg_1984, mg_database, trb) 
  mg_1984@data$historic_dist <- seaway_dist(mg_1984, mg_historic, trb)
  mg_1985@data$database_dist <- seaway_dist(mg_1985, mg_database, trb) 
  mg_1985@data$historic_dist <- seaway_dist(mg_1985, mg_historic, trb)
  mg_1986@data$database_dist <- seaway_dist(mg_1986, mg_database, trb) 
  mg_1986@data$historic_dist <- seaway_dist(mg_1986, mg_historic, trb)
  mg_1987@data$database_dist <- seaway_dist(mg_1987, mg_database, trb) 
  mg_1987@data$historic_dist <- seaway_dist(mg_1987, mg_historic, trb)
  mg_1988@data$database_dist <- seaway_dist(mg_1988, mg_database, trb) 
  mg_1988@data$historic_dist <- seaway_dist(mg_1988, mg_historic, trb)
  mg_1989@data$database_dist <- seaway_dist(mg_1989, mg_database, trb) 
  mg_1989@data$historic_dist <- seaway_dist(mg_1989, mg_historic, trb)
}
#1990s
{ mg_1990@data$database_dist <- seaway_dist(mg_1990, mg_database, trb) 
  mg_1990@data$historic_dist <- seaway_dist(mg_1990, mg_historic, trb)
  mg_1991@data$database_dist <- seaway_dist(mg_1991, mg_database, trb) 
  mg_1991@data$historic_dist <- seaway_dist(mg_1991, mg_historic, trb)
  mg_1992@data$database_dist <- seaway_dist(mg_1992, mg_database, trb) 
  mg_1992@data$historic_dist <- seaway_dist(mg_1992, mg_historic, trb)
  mg_1993@data$database_dist <- seaway_dist(mg_1993, mg_database, trb) 
  mg_1993@data$historic_dist <- seaway_dist(mg_1993, mg_historic, trb)
  mg_1994@data$database_dist <- seaway_dist(mg_1994, mg_database, trb) 
  mg_1994@data$historic_dist <- seaway_dist(mg_1994, mg_historic, trb)
  mg_1995@data$database_dist <- seaway_dist(mg_1995, mg_database, trb) 
  mg_1995@data$historic_dist <- seaway_dist(mg_1995, mg_historic, trb)
  mg_1996@data$database_dist <- seaway_dist(mg_1996, mg_database, trb) 
  mg_1996@data$historic_dist <- seaway_dist(mg_1996, mg_historic, trb)
  mg_1997@data$database_dist <- seaway_dist(mg_1997, mg_database, trb) 
  mg_1997@data$historic_dist <- seaway_dist(mg_1997, mg_historic, trb)
  mg_1998@data$database_dist <- seaway_dist(mg_1998, mg_database, trb) 
  mg_1998@data$historic_dist <- seaway_dist(mg_1998, mg_historic, trb)
  mg_1999@data$database_dist <- seaway_dist(mg_1999, mg_database, trb) 
  mg_1999@data$historic_dist <- seaway_dist(mg_1999, mg_historic, trb)
}
#2000s
{ mg_2000@data$database_dist <- seaway_dist(mg_2000, mg_database, trb) 
  mg_2000@data$historic_dist <- seaway_dist(mg_2000, mg_historic, trb)
  mg_2001@data$database_dist <- seaway_dist(mg_2001, mg_database, trb) 
  mg_2001@data$historic_dist <- seaway_dist(mg_2001, mg_historic, trb)
  mg_2002@data$database_dist <- seaway_dist(mg_2002, mg_database, trb) 
  mg_2002@data$historic_dist <- seaway_dist(mg_2002, mg_historic, trb)
  mg_2003@data$database_dist <- seaway_dist(mg_2003, mg_database, trb) 
  mg_2003@data$historic_dist <- seaway_dist(mg_2003, mg_historic, trb)
  mg_2004@data$database_dist <- seaway_dist(mg_2004, mg_database, trb) 
  mg_2004@data$historic_dist <- seaway_dist(mg_2004, mg_historic, trb)
  mg_2005@data$database_dist <- seaway_dist(mg_2005, mg_database, trb) 
  mg_2005@data$historic_dist <- seaway_dist(mg_2005, mg_historic, trb)
  mg_2006@data$database_dist <- seaway_dist(mg_2006, mg_database, trb) 
  mg_2006@data$historic_dist <- seaway_dist(mg_2006, mg_historic, trb)
  mg_2007@data$database_dist <- seaway_dist(mg_2007, mg_database, trb) 
  mg_2007@data$historic_dist <- seaway_dist(mg_2007, mg_historic, trb)
  mg_2008@data$database_dist <- seaway_dist(mg_2008, mg_database, trb) 
  mg_2008@data$historic_dist <- seaway_dist(mg_2008, mg_historic, trb)
  mg_2009@data$database_dist <- seaway_dist(mg_2009, mg_database, trb) 
  mg_2009@data$historic_dist <- seaway_dist(mg_2009, mg_historic, trb)
}
#2010s
{ mg_2010@data$database_dist <- seaway_dist(mg_2010, mg_database, trb) 
  mg_2010@data$historic_dist <- seaway_dist(mg_2010, mg_historic, trb)
  mg_2011@data$database_dist <- seaway_dist(mg_2011, mg_database, trb) 
  mg_2011@data$historic_dist <- seaway_dist(mg_2011, mg_historic, trb)
  mg_2012@data$database_dist <- seaway_dist(mg_2012, mg_database, trb) 
  mg_2012@data$historic_dist <- seaway_dist(mg_2012, mg_historic, trb)
  mg_2013@data$database_dist <- seaway_dist(mg_2013, mg_database, trb) 
  mg_2013@data$historic_dist <- seaway_dist(mg_2013, mg_historic, trb)
  mg_2014@data$database_dist <- seaway_dist(mg_2014, mg_database, trb) 
  mg_2014@data$historic_dist <- seaway_dist(mg_2014, mg_historic, trb)
  mg_2015@data$database_dist <- seaway_dist(mg_2015, mg_database, trb) 
  mg_2015@data$historic_dist <- seaway_dist(mg_2015, mg_historic, trb)
  mg_2016@data$database_dist <- seaway_dist(mg_2016, mg_database, trb) 
  mg_2016@data$historic_dist <- seaway_dist(mg_2016, mg_historic, trb)
  mg_2017@data$database_dist <- seaway_dist(mg_2017, mg_database, trb) 
  mg_2017@data$historic_dist <- seaway_dist(mg_2017, mg_historic, trb)
  mg_2018@data$database_dist <- seaway_dist(mg_2018, mg_database, trb) 
  mg_2018@data$historic_dist <- seaway_dist(mg_2018, mg_historic, trb)
  mg_2019@data$database_dist <- seaway_dist(mg_2019, mg_database, trb) 
  mg_2019@data$historic_dist <- seaway_dist(mg_2019, mg_historic, trb)
}
#2020s
{ mg_2020@data$database_dist <- seaway_dist(mg_2020, mg_database, trb) 
  mg_2020@data$historic_dist <- seaway_dist(mg_2020, mg_historic, trb)
  mg_2021@data$database_dist <- seaway_dist(mg_2021, mg_database, trb) 
  mg_2021@data$historic_dist <- seaway_dist(mg_2021, mg_historic, trb)
}

# Convert distances to data frame -----------------------------------------

#1960s
{ ddist <- mg_1965$database_dist
hdist <- mg_1965$historic_dist
lat <- as.data.frame(mg_1965$Latitude)
lon <- as.data.frame(mg_1965$Longitude)
year <- as.data.frame(mg_1965$Year)
mg_1965 <- cbind(year, lon, lat, ddist, hdist)
rownames(mg_1965) <- NULL
colnames(mg_1965) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

ddist <- mg_1967$database_dist
hdist <- mg_1967$historic_dist
lat <- as.data.frame(mg_1967$Latitude)
lon <- as.data.frame(mg_1967$Longitude)
year <- as.data.frame(mg_1967$Year)
mg_1967 <- cbind(year, lon, lat, ddist, hdist)
rownames(mg_1967) <- NULL
colnames(mg_1967) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
}
#1970s
{ ddist <- mg_1974$database_dist
  hdist <- mg_1974$historic_dist
  lat <- as.data.frame(mg_1974$Latitude)
  lon <- as.data.frame(mg_1974$Longitude)
  year <- as.data.frame(mg_1974$Year)
  mg_1974 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1974) <- NULL
  colnames(mg_1974) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1976$database_dist
  hdist <- mg_1976$historic_dist
  lat <- as.data.frame(mg_1976$Latitude)
  lon <- as.data.frame(mg_1976$Longitude)
  year <- as.data.frame(mg_1976$Year)
  mg_1976 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1976) <- NULL
  colnames(mg_1976) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1978$database_dist
  hdist <- mg_1978$historic_dist
  lat <- as.data.frame(mg_1978$Latitude)
  lon <- as.data.frame(mg_1978$Longitude)
  year <- as.data.frame(mg_1978$Year)
  mg_1978 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1978) <- NULL
  colnames(mg_1978) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1979$database_dist
  hdist <- mg_1979$historic_dist
  lat <- as.data.frame(mg_1979$Latitude)
  lon <- as.data.frame(mg_1979$Longitude)
  year <- as.data.frame(mg_1979$Year)
  mg_1979 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1979) <- NULL
  colnames(mg_1979) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
}
#1980s
{ ddist <- mg_1980$database_dist
  hdist <- mg_1980$historic_dist
  lat <- as.data.frame(mg_1980$Latitude)
  lon <- as.data.frame(mg_1980$Longitude)
  year <- as.data.frame(mg_1980$Year)
  mg_1980 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1980) <- NULL
  colnames(mg_1980) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1982$database_dist
  hdist <- mg_1982$historic_dist
  lat <- as.data.frame(mg_1982$Latitude)
  lon <- as.data.frame(mg_1982$Longitude)
  year <- as.data.frame(mg_1982$Year)
  mg_1982 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1982) <- NULL
  colnames(mg_1982) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1983$database_dist
  hdist <- mg_1983$historic_dist
  lat <- as.data.frame(mg_1983$Latitude)
  lon <- as.data.frame(mg_1983$Longitude)
  year <- as.data.frame(mg_1983$Year)
  mg_1983 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1983) <- NULL
  colnames(mg_1983) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1984$database_dist
  hdist <- mg_1984$historic_dist
  lat <- as.data.frame(mg_1984$Latitude)
  lon <- as.data.frame(mg_1984$Longitude)
  year <- as.data.frame(mg_1984$Year)
  mg_1984 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1984) <- NULL
  colnames(mg_1984) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1985$database_dist
  hdist <- mg_1985$historic_dist
  lat <- as.data.frame(mg_1985$Latitude)
  lon <- as.data.frame(mg_1985$Longitude)
  year <- as.data.frame(mg_1985$Year)
  mg_1985 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1985) <- NULL
  colnames(mg_1985) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1986$database_dist
  hdist <- mg_1986$historic_dist
  lat <- as.data.frame(mg_1986$Latitude)
  lon <- as.data.frame(mg_1986$Longitude)
  year <- as.data.frame(mg_1986$Year)
  mg_1986 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1986) <- NULL
  colnames(mg_1986) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1987$database_dist
  hdist <- mg_1987$historic_dist
  lat <- as.data.frame(mg_1987$Latitude)
  lon <- as.data.frame(mg_1987$Longitude)
  year <- as.data.frame(mg_1987$Year)
  mg_1987 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1987) <- NULL
  colnames(mg_1987) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1988$database_dist
  hdist <- mg_1988$historic_dist
  lat <- as.data.frame(mg_1988$Latitude)
  lon <- as.data.frame(mg_1988$Longitude)
  year <- as.data.frame(mg_1988$Year)
  mg_1988 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1988) <- NULL
  colnames(mg_1988) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1989$database_dist
  hdist <- mg_1989$historic_dist
  lat <- as.data.frame(mg_1989$Latitude)
  lon <- as.data.frame(mg_1989$Longitude)
  year <- as.data.frame(mg_1989$Year)
  mg_1989 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1989) <- NULL
  colnames(mg_1989) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
}
#1990s
{ ddist <- mg_1990$database_dist
  hdist <- mg_1990$historic_dist
  lat <- as.data.frame(mg_1990$Latitude)
  lon <- as.data.frame(mg_1990$Longitude)
  year <- as.data.frame(mg_1990$Year)
  mg_1990 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1990) <- NULL
  colnames(mg_1990) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1991$database_dist
  hdist <- mg_1991$historic_dist
  lat <- as.data.frame(mg_1991$Latitude)
  lon <- as.data.frame(mg_1991$Longitude)
  year <- as.data.frame(mg_1991$Year)
  mg_1991 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1991) <- NULL
  colnames(mg_1991) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1992$database_dist
  hdist <- mg_1992$historic_dist
  lat <- as.data.frame(mg_1992$Latitude)
  lon <- as.data.frame(mg_1992$Longitude)
  year <- as.data.frame(mg_1992$Year)
  mg_1992 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1992) <- NULL
  colnames(mg_1992) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1993$database_dist
  hdist <- mg_1993$historic_dist
  lat <- as.data.frame(mg_1993$Latitude)
  lon <- as.data.frame(mg_1993$Longitude)
  year <- as.data.frame(mg_1993$Year)
  mg_1993 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1993) <- NULL
  colnames(mg_1993) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1994$database_dist
  hdist <- mg_1994$historic_dist
  lat <- as.data.frame(mg_1994$Latitude)
  lon <- as.data.frame(mg_1994$Longitude)
  year <- as.data.frame(mg_1994$Year)
  mg_1994 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1994) <- NULL
  colnames(mg_1994) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1995$database_dist
  hdist <- mg_1995$historic_dist
  lat <- as.data.frame(mg_1995$Latitude)
  lon <- as.data.frame(mg_1995$Longitude)
  year <- as.data.frame(mg_1995$Year)
  mg_1995 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1995) <- NULL
  colnames(mg_1995) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1996$database_dist
  hdist <- mg_1996$historic_dist
  lat <- as.data.frame(mg_1996$Latitude)
  lon <- as.data.frame(mg_1996$Longitude)
  year <- as.data.frame(mg_1996$Year)
  mg_1996 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1996) <- NULL
  colnames(mg_1996) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1997$database_dist
  hdist <- mg_1997$historic_dist
  lat <- as.data.frame(mg_1997$Latitude)
  lon <- as.data.frame(mg_1997$Longitude)
  year <- as.data.frame(mg_1997$Year)
  mg_1997 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1997) <- NULL
  colnames(mg_1997) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1998$database_dist
  hdist <- mg_1998$historic_dist
  lat <- as.data.frame(mg_1998$Latitude)
  lon <- as.data.frame(mg_1998$Longitude)
  year <- as.data.frame(mg_1998$Year)
  mg_1998 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1998) <- NULL
  colnames(mg_1998) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1999$database_dist
  hdist <- mg_1999$historic_dist
  lat <- as.data.frame(mg_1999$Latitude)
  lon <- as.data.frame(mg_1999$Longitude)
  year <- as.data.frame(mg_1999$Year)
  mg_1999 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1999) <- NULL
  colnames(mg_1999) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
}
#2000s
{ ddist <- mg_2000$database_dist
  hdist <- mg_2000$historic_dist
  lat <- as.data.frame(mg_2000$Latitude)
  lon <- as.data.frame(mg_2000$Longitude)
  year <- as.data.frame(mg_2000$Year)
  mg_2000 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2000) <- NULL
  colnames(mg_2000) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2001$database_dist
  hdist <- mg_2001$historic_dist
  lat <- as.data.frame(mg_2001$Latitude)
  lon <- as.data.frame(mg_2001$Longitude)
  year <- as.data.frame(mg_2001$Year)
  mg_2001 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2001) <- NULL
  colnames(mg_2001) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2002$database_dist
  hdist <- mg_2002$historic_dist
  lat <- as.data.frame(mg_2002$Latitude)
  lon <- as.data.frame(mg_2002$Longitude)
  year <- as.data.frame(mg_2002$Year)
  mg_2002 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2002) <- NULL
  colnames(mg_2002) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2003$database_dist
  hdist <- mg_2003$historic_dist
  lat <- as.data.frame(mg_2003$Latitude)
  lon <- as.data.frame(mg_2003$Longitude)
  year <- as.data.frame(mg_2003$Year)
  mg_2003 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2003) <- NULL
  colnames(mg_2003) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2004$database_dist
  hdist <- mg_2004$historic_dist
  lat <- as.data.frame(mg_2004$Latitude)
  lon <- as.data.frame(mg_2004$Longitude)
  year <- as.data.frame(mg_2004$Year)
  mg_2004 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2004) <- NULL
  colnames(mg_2004) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2005$database_dist
  hdist <- mg_2005$historic_dist
  lat <- as.data.frame(mg_2005$Latitude)
  lon <- as.data.frame(mg_2005$Longitude)
  year <- as.data.frame(mg_2005$Year)
  mg_2005 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2005) <- NULL
  colnames(mg_2005) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2006$database_dist
  hdist <- mg_2006$historic_dist
  lat <- as.data.frame(mg_2006$Latitude)
  lon <- as.data.frame(mg_2006$Longitude)
  year <- as.data.frame(mg_2006$Year)
  mg_2006 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2006) <- NULL
  colnames(mg_2006) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2007$database_dist
  hdist <- mg_2007$historic_dist
  lat <- as.data.frame(mg_2007$Latitude)
  lon <- as.data.frame(mg_2007$Longitude)
  year <- as.data.frame(mg_2007$Year)
  mg_2007 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2007) <- NULL
  colnames(mg_2007) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2008$database_dist
  hdist <- mg_2008$historic_dist
  lat <- as.data.frame(mg_2008$Latitude)
  lon <- as.data.frame(mg_2008$Longitude)
  year <- as.data.frame(mg_2008$Year)
  mg_2008 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2008) <- NULL
  colnames(mg_2008) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2009$database_dist
  hdist <- mg_2009$historic_dist
  lat <- as.data.frame(mg_2009$Latitude)
  lon <- as.data.frame(mg_2009$Longitude)
  year <- as.data.frame(mg_2009$Year)
  mg_2009 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2009) <- NULL
  colnames(mg_2009) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
}
#2010s
{ ddist <- mg_2010$database_dist
  hdist <- mg_2010$historic_dist
  lat <- as.data.frame(mg_2010$Latitude)
  lon <- as.data.frame(mg_2010$Longitude)
  year <- as.data.frame(mg_2010$Year)
  mg_2010 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2010) <- NULL
  colnames(mg_2010) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2011$database_dist
  hdist <- mg_2011$historic_dist
  lat <- as.data.frame(mg_2011$Latitude)
  lon <- as.data.frame(mg_2011$Longitude)
  year <- as.data.frame(mg_2011$Year)
  mg_2011 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2011) <- NULL
  colnames(mg_2011) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2012$database_dist
  hdist <- mg_2012$historic_dist
  lat <- as.data.frame(mg_2012$Latitude)
  lon <- as.data.frame(mg_2012$Longitude)
  year <- as.data.frame(mg_2012$Year)
  mg_2012 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2012) <- NULL
  colnames(mg_2012) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2013$database_dist
  hdist <- mg_2013$historic_dist
  lat <- as.data.frame(mg_2013$Latitude)
  lon <- as.data.frame(mg_2013$Longitude)
  year <- as.data.frame(mg_2013$Year)
  mg_2013 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2013) <- NULL
  colnames(mg_2013) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2014$database_dist
  hdist <- mg_2014$historic_dist
  lat <- as.data.frame(mg_2014$Latitude)
  lon <- as.data.frame(mg_2014$Longitude)
  year <- as.data.frame(mg_2014$Year)
  mg_2014 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2014) <- NULL
  colnames(mg_2014) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2015$database_dist
  hdist <- mg_2015$historic_dist
  lat <- as.data.frame(mg_2015$Latitude)
  lon <- as.data.frame(mg_2015$Longitude)
  year <- as.data.frame(mg_2015$Year)
  mg_2015 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2015) <- NULL
  colnames(mg_2015) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2016$database_dist
  hdist <- mg_2016$historic_dist
  lat <- as.data.frame(mg_2016$Latitude)
  lon <- as.data.frame(mg_2016$Longitude)
  year <- as.data.frame(mg_2016$Year)
  mg_2016 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2016) <- NULL
  colnames(mg_2016) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2017$database_dist
  hdist <- mg_2017$historic_dist
  lat <- as.data.frame(mg_2017$Latitude)
  lon <- as.data.frame(mg_2017$Longitude)
  year <- as.data.frame(mg_2017$Year)
  mg_2017 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2017) <- NULL
  colnames(mg_2017) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2018$database_dist
  hdist <- mg_2018$historic_dist
  lat <- as.data.frame(mg_2018$Latitude)
  lon <- as.data.frame(mg_2018$Longitude)
  year <- as.data.frame(mg_2018$Year)
  mg_2018 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2018) <- NULL
  colnames(mg_2018) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2019$database_dist
  hdist <- mg_2019$historic_dist
  lat <- as.data.frame(mg_2019$Latitude)
  lon <- as.data.frame(mg_2019$Longitude)
  year <- as.data.frame(mg_2019$Year)
  mg_2019 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2019) <- NULL
  colnames(mg_2019) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
}  
#2020s
{ ddist <- mg_2020$database_dist
  hdist <- mg_2020$historic_dist
  lat <- as.data.frame(mg_2020$Latitude)
  lon <- as.data.frame(mg_2020$Longitude)
  year <- as.data.frame(mg_2020$Year)
  mg_2020 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2020) <- NULL
  colnames(mg_2020) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_2021$database_dist
  hdist <- mg_2021$historic_dist
  lat <- as.data.frame(mg_2021$Latitude)
  lon <- as.data.frame(mg_2021$Longitude)
  year <- as.data.frame(mg_2021$Year)
  mg_2021 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2021) <- NULL
  colnames(mg_2021) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
}  

mg_2 <- rbind(mg_1965, mg_1967, mg_1974, mg_1976, mg_1978, mg_1979, mg_1980, 
              mg_1982, mg_1983, mg_1984, mg_1985, mg_1986, mg_1987, mg_1988, 
              mg_1989, mg_1990, mg_1991, mg_1992, mg_1993, mg_1994, mg_1995, 
              mg_1996, mg_1997, mg_1998, mg_1999, mg_2000, mg_2001, mg_2002, 
              mg_2003, mg_2004, mg_2005, mg_2006, mg_2007, mg_2008, mg_2009, 
              mg_2010, mg_2011, mg_2012, mg_2013, mg_2014, mg_2015, mg_2016, 
              mg_2017, mg_2018, mg_2019, mg_2020, mg_2021)

colnames(mg_2) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

write.csv(mg_2, "./Data/Distance regression/All_atlantic.csv", row.names=FALSE)

# Plot --------------------------------------------------------------------

# Read back in the saved data
mg_2 <- read.csv("./Data/Distance regression/All_atlantic.csv")

#Filter the distance and year columns for the database distances
mg_d <- mg_2[, c(1, 4)]
colnames(mg_d) <- c("Year", "Distance") # Change the column names
mg_d$type <- "database" # Add a column for the distance type
mg_d$Year <- as.integer(mg_d$Year) # Chang year to an integer

# Do the same for the distances based on published introductions
mg_h <- mg_2[, c(1, 5)]
colnames(mg_h) <- c("Year", "Distance")
mg_h$type <- "historical" 
mg_h$Year <- as.integer(mg_h$Year)

# Bind the two new datasets together
mg_2 <- rbind(mg_d, mg_h)

# Keep only the maximum record for each year
mg_d_l <- mg_d %>% group_by(Year) %>% top_n(1, Distance)
# Remove years with duplicates
mg_d_l <- mg_d_l[!duplicated(mg_d_l), ]
# Ungroup
mg_d_l <- ungroup(mg_d_l)
# Remove rows smaller than the previous row (keep the first row)
mg_d_l <- mg_d_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 5 times
plot(mg_d_l$Distance~mg_d_l$Year, type="b")

# Do the same for the historical
mg_h_l <- mg_h %>% group_by(Year) %>% top_n(1, Distance)
mg_h_l <- mg_h_l[!duplicated(mg_h_l), ]
mg_h_l <- ungroup(mg_h_l)
mg_h_l <- mg_h_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 9 times
plot(mg_h_l$Distance~mg_h_l$Year, type="b")

# Create the plot
all <- ggplot(data=mg_2, aes(x=Year, y=Distance, group=1, colour=type, fill=type)) +
  geom_point(size=1.5, alpha=0.5)+
  scale_x_continuous(breaks=c(1965, 1970, 1975, 1980, 1985, 1990, 1995,
                              2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(limits=c(NA, 2000))+
  scale_colour_manual(values = c("#f1a340", "#998ec3"))+
  scale_fill_manual(values = c("#f1a340", "#998ec3"))+
  geom_smooth(data=mg_h_l, method="lm", se=FALSE, linetype="dashed")+
  geom_point(data=mg_h_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#998ec3")+
  geom_smooth(data=mg_d_l, method="lm", se=FALSE)+
  geom_point(data=mg_d_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#f1a340")+
  ylab("Distance from initial observation (km)")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))+
  geom_rug(data=database, aes(x=Year), inherit.aes=F, colour="#f1a340", size=1)+
  geom_rug(data=historic, aes(x=Year), inherit.aes=F, colour="#998ec3", size=1)
all

# Linear models -----------------------------------------------------------

model_d <- lm(Distance~Year, mg_d_l)
summary(model_d)
# RSE = 194.3 on 13 DoF 
# Multiple R-squared = 0.8465
# Adjusted R-squared = 0.8347
# F-stat = 71.71
# P-value = 0.00000119
# Rate of spread = 23.123 km/y +- 2.731

# Plot model to check
par(mfrow=c(2,2))
plot(model_d) 
# Residuals vs Fitted: should be straight red line - not too far off. No homoscedasticity of variance but that's due to the sample size being so small
# Normal Q-Q shows an S-shape which isn't unheard of
# Residuals vs Leverage shows that 1 is outside of Cook's distance, but when you look at the plot it is within proximity of other values that are considered fine so this probably shouldn't cause concern
par(mfrow=c(1,1))

# Check normality
plot(density(mg_d_l$Distance)) # looks skewed

# Check the residuals for normality
shapiro.test(residuals(model_d))
# P-value = 0.1707
# residuals are fine

## Do the same for historical ---
model_h <- lm(Distance~Year, mg_h_l)
summary(model_h)
# RSE = 346.6 on 3 DoF - Much smaller
# Multiple R-squared = 0.5701
# Adjusted R-squared = 0.4268
# F-stat = 3.979
# P-value = 0.140
# Rate of spread = 17.384 km/y +- 8.715

# Plot model to check
par(mfrow=c(2,2))
plot(model_h)
# The model doesn't perform well on any of the diagnostics but that is to be expected given the fact it is not significant and has a low sample size
par(mfrow=c(1,1))

# Check normality
plot(density(mg_h_l$Distance)) # Not too bad

# Check the residuals for normality
shapiro.test(residuals(model_h))
# P-value = 0.1113
# residuals are fine

# Comparison of slopes ----------------------------------------------------

# Bind the data
mod_data <- rbind(mg_d_l, mg_h_l)

# Construct a new model with both introduction types
comp_mod <- lm(Distance~Year*type, data=mod_data)
summary(comp_mod)

# Construct a second model without the interaction term to test for a significant difference in the slope
comp_mod2 <- lm(Distance~Year+type, data=mod_data)
summary(comp_mod2)

# Compare the two models with an anova to assess if removing the interaction term significantly affects the fit of the model
anova(comp_mod, comp_mod2)
# P-value = 0.4004
# No significant effect

# Therefore, there is no significant difference between the slope for the database and the slope for the literature

