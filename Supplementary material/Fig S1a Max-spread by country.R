# --------------------------------------------------------------------------
# --------- Script for calculating and plotting the rate of spread ---------
# --------------------------- within each country --------------------------
# --------------------------------------------------------------------------
# Set working directory ---------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Load the required libraries ---------------------------------------------
{ library(dplyr)
  library(geosphere)
  library(ccplot)
  library(ggplot2)
  library(performance)
  library(randomForest)
  library(mgcv)
  library(raster)
  library(sf)
  library(fasterize)
  library(sp)
  library(rSDM)
  library(gdistance)
  library(SDraw)
  library(sdmpredictors)
  library(lsmeans)
  library(rockchalk)
}

library(dplyr)
library(sdmpredictors)
library(raster)
library(gdistance)

# Load the datasets -------------------------------------------------------

# Load the species distribution data
mg <- read.csv("./Data/Data - 2022/Final spreadsheet/Total_data_Atlantic_April_2022_Final.csv")

# Load the data for the initial introductions
mg_1 <- read.csv("./Data/First records/Initial_introductions.csv")

database <- filter(mg_1, Type=="Database")
historic <- filter(mg_1, Type=="Introduction")

## Remove duplicates and keep earliest record ------------------------------

# Order the distribution data from the earliest record to latest
mg <- mg[order(mg$Year, mg$Month, mg$Day, decreasing=FALSE),]
# Remove duplicates based on the longitude and latitude
mg <- mg[!duplicated(mg[c("Longitude", "Latitude")]), ]

## Load and crop the raster layer -------------------------------------------

# Load bathymetry raster
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

rm(Sites.grid, geo_bounds, max_lat, max_lon, min_lat, min_lon)

## Convert projection of raster and make land impassable -------------------

# Convert projection of raster to UTM
bathy2 <- projectRaster(bathy, crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs" )

# Make land impassable
# Make all ocean cells equal to -999
bathy2[!is.na(bathy2)] <- -999
# Turn all land cells to NA
bathy2[bathy2>-999] <- NA
# Assign all ocean cells a value of 1
bathy2[bathy2==-999] <- 1

# Check this has worked
#plot(bathy2)
# Add some points to check the projections are the same
#points(mg_2021)

## Create a transition layer matrix using the raster -----------------------

# Checks whether cells are not NA and then creates a matrix listing all feasible connections between cells
trb <- transition(bathy2, mean, directions = 16) 
# Corrects for fact that diagonal movement is longer than straight lines
trb <- geoCorrection(trb, "c") 

rm(bathy2)

# -------------------------------------------------------------------------
# -------------------------- Subset the countries -------------------------
# -------------------------------------------------------------------------
## Subset by country -------------------------------------------------------

#table(mg$Country)

BEL <- filter(mg, Country=="Belgium")
DEN <- filter(mg, Country=="Denmark")
FRA <- filter(mg, Country=="France")
GER <- filter(mg, Country=="Germany")
IRE <- filter(mg, Country=="Ireland")
NET <- filter(mg, Country=="Netherlands")
NOR <- filter(mg, Country=="Norway")
SWE <- filter(mg, Country=="Sweden")
UK <- filter(mg, Country=="United Kingdom")

# -------------------------------------------------------------------------
# --------------------------------- Belgium -------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

#table(BEL$Year)

#1990s
{ mg_1995 <- filter(BEL, Year==1995)
mg_1997 <- filter(BEL, Year==1997)
mg_1998 <- filter(BEL, Year==1998)
mg_1999 <- filter(BEL, Year==1999)
}
#2000s
{ mg_2001 <- filter(BEL, Year==2001)
  mg_2004 <- filter(BEL, Year==2004)
  mg_2005 <- filter(BEL, Year==2005)
  mg_2006 <- filter(BEL, Year==2006)
  mg_2007 <- filter(BEL, Year==2007)
  mg_2008 <- filter(BEL, Year==2008)
  mg_2009 <- filter(BEL, Year==2009)
}
#2010s
{ mg_2010 <- filter(BEL, Year==2010)
  mg_2011 <- filter(BEL, Year==2011)
  mg_2012 <- filter(BEL, Year==2012)
  mg_2013 <- filter(BEL, Year==2013)
  mg_2014 <- filter(BEL, Year==2014)
  mg_2015 <- filter(BEL, Year==2015)
  mg_2016 <- filter(BEL, Year==2016)
  mg_2017 <- filter(BEL, Year==2017)
  mg_2018 <- filter(BEL, Year==2018)
  mg_2019 <- filter(BEL, Year==2019)
}
#2020s
{ mg_2020 <- filter(BEL, Year==2020)
  mg_2021 <- filter(BEL, Year==2021)
}

# First record from Belgium
BEL_first <- filter(mg_1, Country=="Belgium")

## Separate the long and lat values ----------------------------------------

#1990s
{ mg_1995 <- mg_1995[, 8:7]
mg_1997 <- mg_1997[, 8:7]
mg_1998 <- mg_1998[, 8:7]
mg_1999 <- mg_1999[, 8:7]
}
#2000s
{ mg_2001 <- mg_2001[, 8:7]
  mg_2004 <- mg_2004[, 8:7]
  mg_2005 <- mg_2005[, 8:7]
  mg_2006 <- mg_2006[, 8:7]
  mg_2007 <- mg_2007[, 8:7]
  mg_2008 <- mg_2008[, 8:7]
  mg_2009 <- mg_2009[, 8:7]
}
#2010s
{ mg_2010 <- mg_2010[, 8:7]
  mg_2011 <- mg_2011[, 8:7]
  mg_2012 <- mg_2012[, 8:7]
  mg_2013 <- mg_2013[, 8:7]
  mg_2014 <- mg_2014[, 8:7]
  mg_2015 <- mg_2015[, 8:7]
  mg_2016 <- mg_2016[, 8:7]
  mg_2017 <- mg_2017[, 8:7]
  mg_2018 <- mg_2018[, 8:7]
  mg_2019 <- mg_2019[, 8:7]
}
#2020s
{ mg_2020 <- mg_2020[, 8:7]
  mg_2021 <- mg_2021[, 8:7]
}

BEL_database <- BEL_first[2, 3:4]
BEL_historic <- BEL_first[1, 3:4]

## Convert points to spdf ---------------------------------------------------

#1990s
{ mg_1995 <- SpatialPointsDataFrame(coords=mg_1995[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1995)
mg_1997 <- SpatialPointsDataFrame(coords=mg_1997[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1997)
mg_1998 <- SpatialPointsDataFrame(coords=mg_1998[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1998)
mg_1999 <- SpatialPointsDataFrame(coords=mg_1999[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1999)
}
#2000s
{ mg_2001 <- SpatialPointsDataFrame(coords=mg_2001[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2001)
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
{ BEL_database <- SpatialPointsDataFrame(coords=BEL_database[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=BEL_database)
  BEL_historic <- SpatialPointsDataFrame(coords=BEL_historic[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=BEL_historic)
}

## Move points off of land -------------------------------------------------

#1990s
{ mg_1995 <- points2nearestcell(mg_1995, bathy)
mg_1999 <- points2nearestcell(mg_1999, bathy)
}
#2000s
{ mg_2004 <- points2nearestcell(mg_2004, bathy)
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
  mg_2017 <- points2nearestcell(mg_2017, bathy)
  mg_2018 <- points2nearestcell(mg_2018, bathy)
  mg_2019 <- points2nearestcell(mg_2019, bathy)
}
#2020s
{ mg_2020 <- points2nearestcell(mg_2020, bathy)
  mg_2021 <- points2nearestcell(mg_2021, bathy)
}

## Convert projection of points to UTM -------------------------------------

#1990s
{ mg_1995 <- spTransform(mg_1995, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_1997 <- spTransform(mg_1997, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_1998 <- spTransform(mg_1998, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_1999 <- spTransform(mg_1999, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#2000s
{ mg_2001 <- spTransform(mg_2001, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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
{ BEL_database <- spTransform(BEL_database, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  BEL_historic <- spTransform(BEL_historic, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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

## Calculate the shortest path to the database and historic introductions ----

#1990s
{ mg_1995@data$database_dist <- seaway_dist(mg_1995, BEL_database, trb) 
mg_1995@data$historic_dist <- seaway_dist(mg_1995, BEL_historic, trb)
mg_1997@data$database_dist <- seaway_dist(mg_1997, BEL_database, trb) 
mg_1997@data$historic_dist <- seaway_dist(mg_1997, BEL_historic, trb)
mg_1998@data$database_dist <- seaway_dist(mg_1998, BEL_database, trb) 
mg_1998@data$historic_dist <- seaway_dist(mg_1998, BEL_historic, trb)
mg_1999@data$database_dist <- seaway_dist(mg_1999, BEL_database, trb) 
mg_1999@data$historic_dist <- seaway_dist(mg_1999, BEL_historic, trb)
}
#2000s
{ mg_2001@data$database_dist <- seaway_dist(mg_2001, BEL_database, trb) 
  mg_2001@data$historic_dist <- seaway_dist(mg_2001, BEL_historic, trb)
  mg_2004@data$database_dist <- seaway_dist(mg_2004, BEL_database, trb) 
  mg_2004@data$historic_dist <- seaway_dist(mg_2004, BEL_historic, trb)
  mg_2005@data$database_dist <- seaway_dist(mg_2005, BEL_database, trb) 
  mg_2005@data$historic_dist <- seaway_dist(mg_2005, BEL_historic, trb)
  mg_2006@data$database_dist <- seaway_dist(mg_2006, BEL_database, trb) 
  mg_2006@data$historic_dist <- seaway_dist(mg_2006, BEL_historic, trb)
  mg_2007@data$database_dist <- seaway_dist(mg_2007, BEL_database, trb) 
  mg_2007@data$historic_dist <- seaway_dist(mg_2007, BEL_historic, trb)
  mg_2008@data$database_dist <- seaway_dist(mg_2008, BEL_database, trb) 
  mg_2008@data$historic_dist <- seaway_dist(mg_2008, BEL_historic, trb)
  mg_2009@data$database_dist <- seaway_dist(mg_2009, BEL_database, trb) 
  mg_2009@data$historic_dist <- seaway_dist(mg_2009, BEL_historic, trb)
}
#2010s
{ mg_2010@data$database_dist <- seaway_dist(mg_2010, BEL_database, trb) 
  mg_2010@data$historic_dist <- seaway_dist(mg_2010, BEL_historic, trb)
  mg_2011@data$database_dist <- seaway_dist(mg_2011, BEL_database, trb) 
  mg_2011@data$historic_dist <- seaway_dist(mg_2011, BEL_historic, trb)
  mg_2012@data$database_dist <- seaway_dist(mg_2012, BEL_database, trb) 
  mg_2012@data$historic_dist <- seaway_dist(mg_2012, BEL_historic, trb)
  mg_2013@data$database_dist <- seaway_dist(mg_2013, BEL_database, trb) 
  mg_2013@data$historic_dist <- seaway_dist(mg_2013, BEL_historic, trb)
  mg_2014@data$database_dist <- seaway_dist(mg_2014, BEL_database, trb) 
  mg_2014@data$historic_dist <- seaway_dist(mg_2014, BEL_historic, trb)
  mg_2015@data$database_dist <- seaway_dist(mg_2015, BEL_database, trb) 
  mg_2015@data$historic_dist <- seaway_dist(mg_2015, BEL_historic, trb)
  mg_2016@data$database_dist <- seaway_dist(mg_2016, BEL_database, trb) 
  mg_2016@data$historic_dist <- seaway_dist(mg_2016, BEL_historic, trb)
  mg_2017@data$database_dist <- seaway_dist(mg_2017, BEL_database, trb) 
  mg_2017@data$historic_dist <- seaway_dist(mg_2017, BEL_historic, trb)
  mg_2018@data$database_dist <- seaway_dist(mg_2018, BEL_database, trb) 
  mg_2018@data$historic_dist <- seaway_dist(mg_2018, BEL_historic, trb)
  mg_2019@data$database_dist <- seaway_dist(mg_2019, BEL_database, trb) 
  mg_2019@data$historic_dist <- seaway_dist(mg_2019, BEL_historic, trb)
}
#2020s
{ mg_2010@data$database_dist <- seaway_dist(mg_2020, BEL_database, trb) 
  mg_2010@data$historic_dist <- seaway_dist(mg_2020, BEL_historic, trb)
  mg_2011@data$database_dist <- seaway_dist(mg_2021, BEL_database, trb) 
  mg_2011@data$historic_dist <- seaway_dist(mg_2021, BEL_historic, trb)
}

## Convert distances to data frame -----------------------------------------

#1990s
{ ddist <- mg_1995$database_dist
hdist <- mg_1995$historic_dist
lat <- as.data.frame(mg_1995$Latitude)
lon <- as.data.frame(mg_1995$Longitude)
year <- as.data.frame(mg_1995$Year)
mg_1995 <- cbind(year, lon, lat, ddist, hdist)
rownames(mg_1995) <- NULL
colnames(mg_1995) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

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
{ ddist <- mg_2001$database_dist
  hdist <- mg_2001$historic_dist
  lat <- as.data.frame(mg_2001$Latitude)
  lon <- as.data.frame(mg_2001$Longitude)
  year <- as.data.frame(mg_2001$Year)
  mg_2001 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_2001) <- NULL
  colnames(mg_2001) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
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
#2010s
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

BEL <- rbind(mg_1995, mg_1997, mg_1998, mg_1999, mg_2001, mg_2004, mg_2005, 
             mg_2006, mg_2007, mg_2008, mg_2009, mg_2010, mg_2011, mg_2012, 
             mg_2013, mg_2014, mg_2015, mg_2016, mg_2017, mg_2018, mg_2019, 
             mg_2020, mg_2021)

colnames(BEL) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

write.csv(BEL, "./Data/Distance regression/Distance_regression_Belgium.csv", row.names=FALSE)

# Plot --------------------------------------------------------------------

BEL <- read.csv("./Data/Distance regression/Distance_regression_Belgium.csv")

BEL$Year <- as.integer(BEL$Year)

BEL_d <- BEL[, c(1, 4)]
colnames(BEL_d) <- c("Year", "Distance")
BEL_d$type <- "database" 

BEL_h <- BEL[, c(1, 5)]
colnames(BEL_h) <- c("Year", "Distance")
BEL_h$type <- "historical" 

BEL <- rbind(BEL_d, BEL_h)

BEL_d_l <- BEL_d %>% group_by(Year) %>% top_n(1, Distance)
BEL_d_l <- BEL_d_l[!duplicated(BEL_d_l), ]
BEL_d_l <- ungroup(BEL_d_l)
BEL_d_l <- BEL_d_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat twice
plot(BEL_d_l$Distance~BEL_d_l$Year, type="b")

BEL_h_l <- BEL_h %>% group_by(Year) %>% top_n(1, Distance)
BEL_h_l <- BEL_h_l[!duplicated(BEL_h_l), ]
BEL_h_l <- ungroup(BEL_h_l)
BEL_h_l <- BEL_h_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat twice
plot(BEL_h_l$Distance~BEL_h_l$Year, type="b")

BEL_plot <- ggplot(data=BEL, aes(x=Year, y=Distance, group=1, colour=type, fill=type)) +
  geom_point(size=1.5, alpha=0.3)+
  scale_x_continuous(breaks=c(1965, 1970, 1975, 1980, 1985, 1990, 1995,
                              2000, 2005, 2010, 2015, 2020))+
  scale_y_continuous(limits=c(NA, 150))+
  scale_colour_manual(values = c("#f1a340", "#998ec3"))+
  scale_fill_manual(values = c("#f1a340", "#998ec3"))+
  geom_smooth(data=BEL_h_l, method="lm", se=FALSE, linetype="dashed")+
  geom_point(data=BEL_h_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#998ec3")+
  geom_smooth(data=BEL_d_l, method="lm", se=FALSE)+
  geom_point(data=BEL_d_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#f1a340")+
  ylab("Distance from initial observation (km)")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))
BEL_plot

# Linear models -----------------------------------------------------------

model_BEL_d <- lm(Distance~Year, BEL_d_l)
summary(model_BEL_d)
# RSE = 15.2 on 2 DoF
# Multiple R-squared = 0.923
# Adjusted R-squared = 0.8845
# F-stat = 23.98
# P-value = 0.03926
# Rate of spread = 3.996 km/y +- 0.816

# Plot model to check
par(mfrow=c(2,2))
plot(model_BEL_d) 
# Plots are awful but it's due to low sample numbers
par(mfrow=c(1,1))

# Check normality
plot(density(BEL_d_l$Distance)) # Not too bad
# Check skewness
skewness(BEL_d_l$Distance) # 0.2814561

# Check the residuals for normality
shapiro.test(residuals(model_BEL_d))
# P-value = 0.8705
# residuals are fine

# Do the same for historical
model_BEL_h <- lm(Distance~Year, BEL_h_l)
summary(model_BEL_h)
# RSE = 25.29 on 1 DoF - Okay
# Multiple R-squared = 0.7689
# Adjusted R-squared = 0.5377
# F-stat = 3.326
# P-value = 0.3193
# Rate of spread = 2.706 km/y +- 1.483

# Plot model to check
par(mfrow=c(2,2))
plot(model_BEL_h) 
# Plots are awful but it's due to low sample numbers
par(mfrow=c(1,1))

# Check normality
plot(density(BEL_h_l$Distance)) # Not too bad
# Check skewness
skewness(BEL_h_l$Distance) # 0.3610341

# Check the residuals for normality
shapiro.test(residuals(model_BEL_h))
# P-value = 0.1832
# residuals are fine

# Comparison of slopes ----------------------------------------------------

# Bind the data
mod_data <- rbind(BEL_d_l, BEL_h_l)

# Construct a new model with both introduction types
comp_mod <- lm(Distance~Year*type, data=mod_data)
summary(comp_mod)

# Construct a second model without the interaction term to test for a significant difference in the slope
comp_mod2 <- lm(Distance~Year+type, data=mod_data)
summary(comp_mod2)

# Compare the two models with an anova to assess if removing the interaction term significantly affects the fit of the model
anova(comp_mod, comp_mod2)
# P-value = 0.4593
# No significant effect 

# Therefore, there is no significant difference between the slope for the database and the slope for the literature

# -------------------------------------------------------------------------
# --------------------------------- Denmark -------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

#table(DEN$Year)

#2010s
{ mg_2014 <- filter(DEN, Year==2014)
mg_2015 <- filter(DEN, Year==2015)
mg_2016 <- filter(DEN, Year==2016)
mg_2017 <- filter(DEN, Year==2017)
mg_2018 <- filter(DEN, Year==2018)
mg_2019 <- filter(DEN, Year==2019)
}
#2020s
{ mg_2020 <- filter(DEN, Year==2020)
  mg_2021 <- filter(DEN, Year==2021)
}

# First record from Denmark
DEN_first <- filter(mg_1, Country=="Denmark")

## Separate the long and lat values ----------------------------------------

#2010s
{ mg_2014 <- mg_2014[, 8:7]
mg_2015 <- mg_2015[, 8:7]
mg_2016 <- mg_2016[, 8:7]
mg_2017 <- mg_2017[, 8:7]
mg_2018 <- mg_2018[, 8:7]
mg_2019 <- mg_2019[, 8:7]
}
#2020s
{ mg_2020 <- mg_2020[, 8:7]
  mg_2021 <- mg_2021[, 8:7]
}

DEN_database <- DEN_first[2, 3:4]
DEN_historic <- DEN_first[1, 3:4]

## Convert points to spdf ---------------------------------------------------

#2010s
{ mg_2014 <- SpatialPointsDataFrame(coords=mg_2014[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2014)
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
{ DEN_database <- SpatialPointsDataFrame(coords=DEN_database[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=DEN_database)
  DEN_historic <- SpatialPointsDataFrame(coords=DEN_historic[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=DEN_historic)
}

## Move points off of land -------------------------------------------------

#2010s
{ mg_2017 <- points2nearestcell(mg_2017, bathy)
mg_2019 <- points2nearestcell(mg_2019, bathy)
}
#2020s
{ mg_2020 <- points2nearestcell(mg_2020, bathy)
  mg_2021 <- points2nearestcell(mg_2021, bathy)
}

#initial
{ DEN_historic <- points2nearestcell(DEN_historic, bathy)
}

## Convert projection of points to UTM -------------------------------------

#2010s
{ mg_2014 <- spTransform(mg_2014, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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
{ DEN_database <- spTransform(DEN_database, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  DEN_historic <- spTransform(DEN_historic, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

## Calculate the shortest path to the database and historic introductions ----

#2010s
{ mg_2014@data$database_dist <- seaway_dist(mg_2014, DEN_database, trb) 
mg_2014@data$historic_dist <- seaway_dist(mg_2014, DEN_historic, trb)
mg_2015@data$database_dist <- seaway_dist(mg_2015, DEN_database, trb) 
mg_2015@data$historic_dist <- seaway_dist(mg_2015, DEN_historic, trb)
mg_2016@data$database_dist <- seaway_dist(mg_2016, DEN_database, trb) 
mg_2016@data$historic_dist <- seaway_dist(mg_2016, DEN_historic, trb)
mg_2017@data$database_dist <- seaway_dist(mg_2017, DEN_database, trb) 
mg_2017@data$historic_dist <- seaway_dist(mg_2017, DEN_historic, trb)
mg_2018@data$database_dist <- seaway_dist(mg_2018, DEN_database, trb) 
mg_2018@data$historic_dist <- seaway_dist(mg_2018, DEN_historic, trb)
mg_2019@data$database_dist <- seaway_dist(mg_2019, DEN_database, trb) 
mg_2019@data$historic_dist <- seaway_dist(mg_2019, DEN_historic, trb)
}
#2020s
{ mg_2020@data$database_dist <- seaway_dist(mg_2020, DEN_database, trb) 
  mg_2020@data$historic_dist <- seaway_dist(mg_2020, DEN_historic, trb)
  mg_2021@data$database_dist <- seaway_dist(mg_2021, DEN_database, trb) 
  mg_2021@data$historic_dist <- seaway_dist(mg_2021, DEN_historic, trb)
}

## Convert distances to data frame -----------------------------------------

#2010s
{ddist <- mg_2014$database_dist
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
#2010s
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

DEN <- rbind(mg_2014, mg_2015, mg_2016, mg_2017, mg_2018, mg_2019, mg_2020, 
             mg_2021)

colnames(DEN) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

write.csv(DEN, "./Data/Distance regression/Distance_regression_Denmark.csv", row.names=FALSE)

# Plot --------------------------------------------------------------------

DEN <- read.csv("./Data/Distance regression/Distance_regression_Denmark.csv")

DEN$Year <- as.integer(DEN$Year)

DEN_d <- DEN[, c(1, 4)]
colnames(DEN_d) <- c("Year", "Distance")
DEN_d$type <- "database" 

DEN_h <- DEN[, c(1, 5)]
colnames(DEN_h) <- c("Year", "Distance")
DEN_h$type <- "historical" 

DEN <- rbind(DEN_d, DEN_h)

DEN_d_l <- DEN_d %>% group_by(Year) %>% top_n(1, Distance)
DEN_d_l <- DEN_d_l[!duplicated(DEN_d_l), ]
DEN_d_l <- ungroup(DEN_d_l)
DEN_d_l <- DEN_d_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat twice
plot(DEN_d_l$Distance~DEN_d_l$Year, type="b")

DEN_h_l <- DEN_h %>% group_by(Year) %>% top_n(1, Distance)
DEN_h_l <- DEN_h_l[!duplicated(DEN_h_l), ]
DEN_h_l <- ungroup(DEN_h_l)
DEN_h_l <- DEN_h_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat twice
plot(DEN_h_l$Distance~DEN_h_l$Year, type="b")

DEN_plot <- ggplot(data=DEN, aes(x=Year, y=Distance, group=1, colour=type, fill=type)) +
  geom_point(size=1.5, alpha=0.3)+
  scale_y_continuous(limits=c(NA, 750))+
  scale_colour_manual(values = c("#f1a340", "#998ec3"))+
  scale_fill_manual(values = c("#f1a340", "#998ec3"))+
  geom_smooth(data=DEN_h_l, method="lm", se=FALSE, linetype="dashed")+
  geom_point(data=DEN_h_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#998ec3")+
  geom_smooth(data=DEN_d_l, method="lm", se=FALSE, linetype="dashed")+
  geom_point(data=DEN_d_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#f1a340")+
  ylab("Distance from initial observation (km)")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))
DEN_plot

# Linear models -----------------------------------------------------------

model_DEN_d <- lm(Distance~Year, DEN_d_l)
summary(model_DEN_d)
# RSE = 150.4 on 2 DoF - Okay
# Multiple R-squared = 0.8779
# Adjusted R-squared = 0.8168
# F-stat = 14.38
# P-value = 0.06305
# Rate of spread = 255.06 km/y +- 67.27

# Plot model to check
par(mfrow=c(2,2))
plot(model_DEN_d) 
# Not great but likely due to low sample size
par(mfrow=c(1,1))

# Check normality
plot(density(DEN_d_l$Distance)) # pretty good
# Check skewness
skewness(DEN_d_l$Distance) # 0.0191086

# Check the residuals for normality
shapiro.test(residuals(model_DEN_d))
# P-value = 0.8797
# residuals are fine

## Do the same for historical ---
model_DEN_h <- lm(Distance~Year, DEN_h_l)
summary(model_DEN_h)
# RSE = 32.45 on 1 DoF - Okay
# Multiple R-squared = 0.919
# Adjusted R-squared = 0.8379
# F-stat = 11.34
# P-value = 0.184
# Rate of spread = 50.59 km/y +- 15.02

# Plot model to check
par(mfrow=c(2,2))
plot(model_DEN_h) 
# Not great but likely due to low sample size
par(mfrow=c(1,1))

# Check normality
plot(density(DEN_h_l$Distance)) # pretty good
# Check skewness
skewness(DEN_h_l$Distance) # 0.112125

# Check the residuals for normality
shapiro.test(residuals(model_DEN_h))
# P-value = 0.3631
# residuals are fine

# Comparison of slopes ----------------------------------------------------

# Bind the data
mod_data <- rbind(DEN_d_l, DEN_h_l)

# Construct a new model with both introduction types
comp_mod <- lm(Distance~Year*type, data=mod_data)
summary(comp_mod)

# Construct a second model without the interaction term to test for a significant difference in the slope
comp_mod2 <- lm(Distance~Year+type, data=mod_data)
summary(comp_mod2)

# Compare the two models with an anova to assess if removing the interaction term significantly affects the fit of the model
anova(comp_mod, comp_mod2)
# P-value = 0.08343
# No significant effect

# Therefore, there is no significant difference between the slope for the database and the slope for the literature

# -------------------------------------------------------------------------
# -------------------------------- France --------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

table(FRA$Year)

#1960s
{ mg_1965 <- filter(FRA, Year==1965)
  mg_1967 <- filter(FRA, Year==1967)
}
#1980s
{ mg_1980 <- filter(FRA, Year==1980)
  mg_1983 <- filter(FRA, Year==1983)
  mg_1984 <- filter(FRA, Year==1984)
  mg_1985 <- filter(FRA, Year==1985)
  mg_1987 <- filter(FRA, Year==1987)
}
#1990s
{ mg_1990 <- filter(FRA, Year==1990)
  mg_1994 <- filter(FRA, Year==1994)
  mg_1995 <- filter(FRA, Year==1995)
  mg_1996 <- filter(FRA, Year==1996)
  mg_1997 <- filter(FRA, Year==1997)
  mg_1999 <- filter(FRA, Year==1999)
}
#2000s
{ mg_2000 <- filter(FRA, Year==2000)
  mg_2001 <- filter(FRA, Year==2001)
  mg_2003 <- filter(FRA, Year==2003)
  mg_2004 <- filter(FRA, Year==2004)
  mg_2005 <- filter(FRA, Year==2005)
  mg_2006 <- filter(FRA, Year==2006)
  mg_2007 <- filter(FRA, Year==2007)
  mg_2008 <- filter(FRA, Year==2008)
  mg_2009 <- filter(FRA, Year==2009)
}
#2010s
{ mg_2010 <- filter(FRA, Year==2010)
  mg_2011 <- filter(FRA, Year==2011)
  mg_2012 <- filter(FRA, Year==2012)
  mg_2013 <- filter(FRA, Year==2013)
  mg_2014 <- filter(FRA, Year==2014)
  mg_2015 <- filter(FRA, Year==2015)
  mg_2016 <- filter(FRA, Year==2016)
  mg_2017 <- filter(FRA, Year==2017)
  mg_2018 <- filter(FRA, Year==2018)
  mg_2019 <- filter(FRA, Year==2019)
}
#2020s
{ mg_2020 <- filter(FRA, Year==2020)
  mg_2021 <- filter(FRA, Year==2021)
}

# First record from Germany
FRA_first <- filter(mg_1, Country=="France")

## Separate the long and lat values ----------------------------------------

#1960s
{ mg_1965 <- mg_1965[, 8:7]
mg_1967 <- mg_1967[, 8:7]
}
#1980s
{ mg_1980 <- mg_1980[, 8:7]
  mg_1983 <- mg_1983[, 8:7]
  mg_1984 <- mg_1984[, 8:7]
  mg_1985 <- mg_1985[, 8:7]
  mg_1987 <- mg_1987[, 8:7]
}
#1990s
{ mg_1990 <- mg_1990[, 8:7]
  mg_1994 <- mg_1994[, 8:7]
  mg_1995 <- mg_1995[, 8:7]
  mg_1996 <- mg_1996[, 8:7]
  mg_1997 <- mg_1997[, 8:7]
  mg_1999 <- mg_1999[, 8:7]
}
#2000s
{ mg_2000 <- mg_2000[, 8:7]
  mg_2001 <- mg_2001[, 8:7]
  mg_2003 <- mg_2003[, 8:7]
  mg_2004 <- mg_2004[, 8:7]
  mg_2005 <- mg_2005[, 8:7]
  mg_2006 <- mg_2006[, 8:7]
  mg_2007 <- mg_2007[, 8:7]
  mg_2008 <- mg_2008[, 8:7]
  mg_2009 <- mg_2009[, 8:7]
}
#2010s
{ mg_2010 <- mg_2010[, 8:7]
  mg_2011 <- mg_2011[, 8:7]
  mg_2012 <- mg_2012[, 8:7]
  mg_2013 <- mg_2013[, 8:7]
  mg_2014 <- mg_2014[, 8:7]
  mg_2015 <- mg_2015[, 8:7]
  mg_2016 <- mg_2016[, 8:7]
  mg_2017 <- mg_2017[, 8:7]
  mg_2018 <- mg_2018[, 8:7]
  mg_2019 <- mg_2019[, 8:7]
}
#2020s
{ mg_2020 <- mg_2020[, 8:7]
  mg_2021 <- mg_2021[, 8:7]
}

FRA_database <- FRA_first[2, 3:4]
FRA_historic <- FRA_first[1, 3:4]

## Convert points to spdf ---------------------------------------------------

#1960s
{ mg_1965 <- SpatialPointsDataFrame(coords=mg_1965[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1965)
mg_1967 <- SpatialPointsDataFrame(coords=mg_1967[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1967)
}
#1980s
{ mg_1980 <- SpatialPointsDataFrame(coords=mg_1980[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1980)
  mg_1983 <- SpatialPointsDataFrame(coords=mg_1983[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1983)
  mg_1984 <- SpatialPointsDataFrame(coords=mg_1984[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1984)
  mg_1985 <- SpatialPointsDataFrame(coords=mg_1985[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1985)
  mg_1987 <- SpatialPointsDataFrame(coords=mg_1987[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1987)
}
#1990s
{ mg_1990 <- SpatialPointsDataFrame(coords=mg_1990[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1990)
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
{ FRA_database <- SpatialPointsDataFrame(coords=FRA_database[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=FRA_database)
  FRA_historic <- SpatialPointsDataFrame(coords=FRA_historic[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=FRA_historic)
}

## Move points off of land -------------------------------------------------

#2000s
{ mg_2006 <- points2nearestcell(mg_2006, bathy)
mg_2007 <- points2nearestcell(mg_2007, bathy)
}
#2010s
{ mg_2012 <- points2nearestcell(mg_2012, bathy)
  mg_2017 <- points2nearestcell(mg_2017, bathy)
  mg_2018 <- points2nearestcell(mg_2018, bathy)
}
#2020s
{ mg_2020 <- points2nearestcell(mg_2020, bathy)
}

## Convert projection of points to UTM -------------------------------------

#1960s
{ mg_1965 <- spTransform(mg_1965, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_1967 <- spTransform(mg_1967, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1980s
{ mg_1980 <- spTransform(mg_1980, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1983 <- spTransform(mg_1983, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1984 <- spTransform(mg_1984, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1985 <- spTransform(mg_1985, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1987 <- spTransform(mg_1987, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1990s
{ mg_1990 <- spTransform(mg_1990, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1994 <- spTransform(mg_1994, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1995 <- spTransform(mg_1995, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1996 <- spTransform(mg_1996, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1997 <- spTransform(mg_1997, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1999 <- spTransform(mg_1999, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#2000s
{ mg_2000 <- spTransform(mg_2000, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2001 <- spTransform(mg_2001, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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
{ FRA_database <- spTransform(FRA_database, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  FRA_historic <- spTransform(FRA_historic, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

## Calculate the shortest path to the database and historic introductions ----

#1960s
{ mg_1965@data$database_dist <- seaway_dist(mg_1965, FRA_database, trb) 
mg_1965@data$historic_dist <- seaway_dist(mg_1965, FRA_historic, trb)
mg_1967@data$database_dist <- seaway_dist(mg_1967, FRA_database, trb) 
mg_1967@data$historic_dist <- seaway_dist(mg_1967, FRA_historic, trb)
}
#1980s
{ mg_1980@data$database_dist <- seaway_dist(mg_1980, FRA_database, trb) 
  mg_1980@data$historic_dist <- seaway_dist(mg_1980, FRA_historic, trb)
  mg_1983@data$database_dist <- seaway_dist(mg_1983, FRA_database, trb) 
  mg_1983@data$historic_dist <- seaway_dist(mg_1983, FRA_historic, trb)
  mg_1984@data$database_dist <- seaway_dist(mg_1984, FRA_database, trb) 
  mg_1984@data$historic_dist <- seaway_dist(mg_1984, FRA_historic, trb)
  mg_1985@data$database_dist <- seaway_dist(mg_1985, FRA_database, trb) 
  mg_1985@data$historic_dist <- seaway_dist(mg_1985, FRA_historic, trb)
  mg_1987@data$database_dist <- seaway_dist(mg_1987, FRA_database, trb) 
  mg_1987@data$historic_dist <- seaway_dist(mg_1987, FRA_historic, trb)
}
#1990s
{ mg_1990@data$database_dist <- seaway_dist(mg_1990, FRA_database, trb) 
  mg_1990@data$historic_dist <- seaway_dist(mg_1990, FRA_historic, trb)
  mg_1994@data$database_dist <- seaway_dist(mg_1994, FRA_database, trb) 
  mg_1994@data$historic_dist <- seaway_dist(mg_1994, FRA_historic, trb)
  mg_1995@data$database_dist <- seaway_dist(mg_1995, FRA_database, trb) 
  mg_1995@data$historic_dist <- seaway_dist(mg_1995, FRA_historic, trb)
  mg_1996@data$database_dist <- seaway_dist(mg_1996, FRA_database, trb) 
  mg_1996@data$historic_dist <- seaway_dist(mg_1996, FRA_historic, trb)
  mg_1997@data$database_dist <- seaway_dist(mg_1997, FRA_database, trb) 
  mg_1997@data$historic_dist <- seaway_dist(mg_1997, FRA_historic, trb)
  mg_1999@data$database_dist <- seaway_dist(mg_1999, FRA_database, trb) 
  mg_1999@data$historic_dist <- seaway_dist(mg_1999, FRA_historic, trb)
}
#2000s
{ mg_2000@data$database_dist <- seaway_dist(mg_2000, FRA_database, trb) 
  mg_2000@data$historic_dist <- seaway_dist(mg_2000, FRA_historic, trb)
  mg_2001@data$database_dist <- seaway_dist(mg_2001, FRA_database, trb) 
  mg_2001@data$historic_dist <- seaway_dist(mg_2001, FRA_historic, trb)
  mg_2003@data$database_dist <- seaway_dist(mg_2003, FRA_database, trb) 
  mg_2003@data$historic_dist <- seaway_dist(mg_2003, FRA_historic, trb)
  mg_2004@data$database_dist <- seaway_dist(mg_2004, FRA_database, trb) 
  mg_2004@data$historic_dist <- seaway_dist(mg_2004, FRA_historic, trb)
  mg_2005@data$database_dist <- seaway_dist(mg_2005, FRA_database, trb) 
  mg_2005@data$historic_dist <- seaway_dist(mg_2005, FRA_historic, trb)
  mg_2006@data$database_dist <- seaway_dist(mg_2006, FRA_database, trb) 
  mg_2006@data$historic_dist <- seaway_dist(mg_2006, FRA_historic, trb)
  mg_2007@data$database_dist <- seaway_dist(mg_2007, FRA_database, trb) 
  mg_2007@data$historic_dist <- seaway_dist(mg_2007, FRA_historic, trb)
  mg_2008@data$database_dist <- seaway_dist(mg_2008, FRA_database, trb) 
  mg_2008@data$historic_dist <- seaway_dist(mg_2008, FRA_historic, trb)
  mg_2009@data$database_dist <- seaway_dist(mg_2009, FRA_database, trb) 
  mg_2009@data$historic_dist <- seaway_dist(mg_2009, FRA_historic, trb)
}
#2010s
{ mg_2010@data$database_dist <- seaway_dist(mg_2010, FRA_database, trb) 
  mg_2010@data$historic_dist <- seaway_dist(mg_2010, FRA_historic, trb)
  mg_2011@data$database_dist <- seaway_dist(mg_2011, FRA_database, trb) 
  mg_2011@data$historic_dist <- seaway_dist(mg_2011, FRA_historic, trb)
  mg_2012@data$database_dist <- seaway_dist(mg_2012, FRA_database, trb) 
  mg_2012@data$historic_dist <- seaway_dist(mg_2012, FRA_historic, trb)
  mg_2013@data$database_dist <- seaway_dist(mg_2013, FRA_database, trb) 
  mg_2013@data$historic_dist <- seaway_dist(mg_2013, FRA_historic, trb)
  mg_2014@data$database_dist <- seaway_dist(mg_2014, FRA_database, trb) 
  mg_2014@data$historic_dist <- seaway_dist(mg_2014, FRA_historic, trb)
  mg_2015@data$database_dist <- seaway_dist(mg_2015, FRA_database, trb) 
  mg_2015@data$historic_dist <- seaway_dist(mg_2015, FRA_historic, trb)
  mg_2016@data$database_dist <- seaway_dist(mg_2016, FRA_database, trb) 
  mg_2016@data$historic_dist <- seaway_dist(mg_2016, FRA_historic, trb)
  mg_2017@data$database_dist <- seaway_dist(mg_2017, FRA_database, trb) 
  mg_2017@data$historic_dist <- seaway_dist(mg_2017, FRA_historic, trb)
  mg_2018@data$database_dist <- seaway_dist(mg_2018, FRA_database, trb) 
  mg_2018@data$historic_dist <- seaway_dist(mg_2018, FRA_historic, trb)
  mg_2019@data$database_dist <- seaway_dist(mg_2019, FRA_database, trb) 
  mg_2019@data$historic_dist <- seaway_dist(mg_2019, FRA_historic, trb)
}
#2020s
{ mg_2020@data$database_dist <- seaway_dist(mg_2020, FRA_database, trb) 
  mg_2020@data$historic_dist <- seaway_dist(mg_2020, FRA_historic, trb)
  mg_2021@data$database_dist <- seaway_dist(mg_2021, FRA_database, trb) 
  mg_2021@data$historic_dist <- seaway_dist(mg_2021, FRA_historic, trb)
}

## Convert distances to data frame -----------------------------------------

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
#1980s
{ ddist <- mg_1980$database_dist
  hdist <- mg_1980$historic_dist
  lat <- as.data.frame(mg_1980$Latitude)
  lon <- as.data.frame(mg_1980$Longitude)
  year <- as.data.frame(mg_1980$Year)
  mg_1980 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1980) <- NULL
  colnames(mg_1980) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
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
  
  ddist <- mg_1987$database_dist
  hdist <- mg_1987$historic_dist
  lat <- as.data.frame(mg_1987$Latitude)
  lon <- as.data.frame(mg_1987$Longitude)
  year <- as.data.frame(mg_1987$Year)
  mg_1987 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1987) <- NULL
  colnames(mg_1987) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
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
#2010s
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

FRA <- rbind(mg_1965, mg_1967, mg_1980, mg_1983, mg_1984, mg_1985,
             mg_1987, mg_1990, mg_1994, mg_1995, mg_1996, mg_1997,
             mg_1999, mg_2000, mg_2001, mg_2003, mg_2004, mg_2005, mg_2006, 
             mg_2007, mg_2008, mg_2009, mg_2010, mg_2011, mg_2012, mg_2013, 
             mg_2014, mg_2015, mg_2016, mg_2017, mg_2018, mg_2019, mg_2020, 
             mg_2021)

colnames(FRA) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

write.csv(FRA, "./Data/Distance regression/Distance_regression_France.csv", row.names=FALSE)

# Plot --------------------------------------------------------------------

FRA <- read.csv("./Data/Distance regression/Distance_regression_France.csv")

FRA$Year <- as.integer(FRA$Year)

FRA_d <- FRA[, c(1, 4)]
colnames(FRA_d) <- c("Year", "Distance")
FRA_d$type <- "database" 

FRA_h <- FRA[, c(1, 5)]
colnames(FRA_h) <- c("Year", "Distance")
FRA_h$type <- "historical" 

FRA <- rbind(FRA_d, FRA_h)

FRA_d_l <- FRA_d %>% group_by(Year) %>% top_n(1, Distance)
FRA_d_l <- FRA_d_l[!duplicated(FRA_d_l), ]
FRA_d_l <- ungroup(FRA_d_l)
FRA_d_l <- FRA_d_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 6 times
plot(FRA_d_l$Distance~FRA_d_l$Year, type="b")

FRA_h_l <- FRA_h %>% group_by(Year) %>% top_n(1, Distance)
FRA_h_l <- FRA_h_l[!duplicated(FRA_h_l), ]
FRA_h_l <- ungroup(FRA_h_l)
FRA_h_l <- FRA_h_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 5 times
plot(FRA_h_l$Distance~FRA_h_l$Year, type="b")

FRA_plot <- ggplot(data=FRA, aes(x=Year, y=Distance, group=1, colour=type, fill=type)) +
  geom_point(size=1.5, alpha=0.3)+
  scale_x_continuous(breaks=c(1965, 1974, 1983, 1992, 2001, 2010, 2019))+
  scale_colour_manual(values = c("#f1a340", "#998ec3"))+
  scale_fill_manual(values = c("#f1a340", "#998ec3"))+
  geom_smooth(data=FRA_h_l, method="lm", se=FALSE, linetype="dashed")+
  geom_point(data=FRA_h_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#998ec3")+
  geom_smooth(data=FRA_d_l, method="lm", se=FALSE)+
  geom_point(data=FRA_d_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#f1a340")+
  ylab("Distance from initial observation (km)")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))
FRA_plot

# Linear models -----------------------------------------------------------

model_FRA_d <- lm(Distance~Year, FRA_d_l)
summary(model_FRA_d)
# RSE = 185.8 on 4 DoF - Quite large
# Multiple R-squared = 0.8053
# Adjusted R-squared = 0.7566
# F-stat = 16.54
# P-value = 0.01526
# Rate of spread = 37.717 km/y +- 9.274

# Plot model to check
par(mfrow=c(2,2))
plot(model_FRA_d) 
# Not terrible, but not brilliant
par(mfrow=c(1,1))

# Check normality
plot(density(FRA_d_l$Distance)) # pretty good
# Check skewness
skewness(FRA_d_l$Distance) # 0.01050033

# Check the residuals for normality
shapiro.test(residuals(model_FRA_d))
# P-value = 0.1207
# residuals are fine

## Do the same for historical ---
model_FRA_h <- lm(Distance~Year, FRA_h_l)
summary(model_FRA_h)
# RSE = 134.3 on 4 DoF 
# Multiple R-squared = 0.4287
# Adjusted R-squared = 0.2858
# F-stat = 3.001
# P-value = 0.1582
# Rate of spread = 5.281 km/y +- 3.048

# Plot model to check
par(mfrow=c(2,2))
plot(model_FRA_h) 
# really bad
par(mfrow=c(1,1))

# Check normality
plot(density(FRA_h_l$Distance)) # not bad
# Check skewness
skewness(FRA_h_l$Distance) # 0.4923212

# Check the residuals for normality
shapiro.test(residuals(model_FRA_h))
# P-value = 0.1476
# residuals are fine

# Comparison of slopes ----------------------------------------------------

# Bind the data
mod_data <- rbind(FRA_d_l, FRA_h_l)

# Construct a new model with both introduction types
comp_mod <- lm(Distance~Year*type, data=mod_data)
summary(comp_mod)

# Construct a second model without the interaction term to test for a significant difference in the slope
comp_mod2 <- lm(Distance~Year+type, data=mod_data)
summary(comp_mod2)

# Compare the two models with an anova to assess if removing the interaction term significantly affects the fit of the model
anova(comp_mod, comp_mod2)
# P-value = 0.006504
# Significant

# There is a significant difference between the slope for the database and the slope for the literature

# -------------------------------------------------------------------------
# -------------------------------- Germany --------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

#table(GER$Year)

#2000s
{ mg_2006 <- filter(GER, Year==2006)
mg_2007 <- filter(GER, Year==2007)
mg_2008 <- filter(GER, Year==2008)
mg_2009 <- filter(GER, Year==2009)
}
#2010s
{ mg_2010 <- filter(GER, Year==2010)
  mg_2011 <- filter(GER, Year==2011)
  mg_2012 <- filter(GER, Year==2012)
  mg_2013 <- filter(GER, Year==2013)
  mg_2014 <- filter(GER, Year==2014)
  mg_2015 <- filter(GER, Year==2015)
  mg_2016 <- filter(GER, Year==2016)
  mg_2017 <- filter(GER, Year==2017)
  mg_2018 <- filter(GER, Year==2018)
  mg_2019 <- filter(GER, Year==2019)
}
#2020s
{ mg_2020 <- filter(GER, Year==2020)
  mg_2021 <- filter(GER, Year==2021)
}

# First record from Germany
GER_first <- filter(mg_1, Country=="Germany")

## Separate the long and lat values ----------------------------------------

#2000s
{ mg_2006 <- mg_2006[, 8:7]
mg_2007 <- mg_2007[, 8:7]
mg_2008 <- mg_2008[, 8:7]
mg_2009 <- mg_2009[, 8:7]
}
#2010s
{ mg_2010 <- mg_2010[, 8:7]
  mg_2011 <- mg_2011[, 8:7]
  mg_2012 <- mg_2012[, 8:7]
  mg_2013 <- mg_2013[, 8:7]
  mg_2014 <- mg_2014[, 8:7]
  mg_2015 <- mg_2015[, 8:7]
  mg_2016 <- mg_2016[, 8:7]
  mg_2017 <- mg_2017[, 8:7]
  mg_2018 <- mg_2018[, 8:7]
  mg_2019 <- mg_2019[, 8:7]
}
#2020s
{ mg_2020 <- mg_2020[, 8:7]
  mg_2021 <- mg_2021[, 8:7]
}

GER_database <- GER_first[2, 3:4]
GER_historic <- GER_first[1, 3:4]

## Convert points to spdf ---------------------------------------------------

#2000s
{ mg_2006 <- SpatialPointsDataFrame(coords=mg_2006[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2006)
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
{ GER_database <- SpatialPointsDataFrame(coords=GER_database[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=GER_database)
  GER_historic <- SpatialPointsDataFrame(coords=GER_historic[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=GER_historic)
}

## Move points off of land -------------------------------------------------

#2000s
{ mg_2006 <- points2nearestcell(mg_2006, bathy)
}
#2010s
{ mg_2017 <- points2nearestcell(mg_2017, bathy)
}

#initial
{ GER_database <- points2nearestcell(GER_database, bathy)
}

## Convert projection of points to UTM -------------------------------------

#2000s
{ mg_2006 <- spTransform(mg_2006, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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
{ GER_database <- spTransform(GER_database, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  GER_historic <- spTransform(GER_historic, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

## Calculate the shortest path to the database and historic introductions ----

#2000s
{ mg_2006@data$database_dist <- seaway_dist(mg_2006, GER_database, trb) 
mg_2006@data$historic_dist <- seaway_dist(mg_2006, GER_historic, trb)
mg_2007@data$database_dist <- seaway_dist(mg_2007, GER_database, trb) 
mg_2007@data$historic_dist <- seaway_dist(mg_2007, GER_historic, trb)
mg_2008@data$database_dist <- seaway_dist(mg_2008, GER_database, trb) 
mg_2008@data$historic_dist <- seaway_dist(mg_2008, GER_historic, trb)
mg_2009@data$database_dist <- seaway_dist(mg_2009, GER_database, trb) 
mg_2009@data$historic_dist <- seaway_dist(mg_2009, GER_historic, trb)
}
#2010s
{ mg_2010@data$database_dist <- seaway_dist(mg_2010, GER_database, trb) 
  mg_2010@data$historic_dist <- seaway_dist(mg_2010, GER_historic, trb)
  mg_2011@data$database_dist <- seaway_dist(mg_2011, GER_database, trb) 
  mg_2011@data$historic_dist <- seaway_dist(mg_2011, GER_historic, trb)
  mg_2012@data$database_dist <- seaway_dist(mg_2012, GER_database, trb) 
  mg_2012@data$historic_dist <- seaway_dist(mg_2012, GER_historic, trb)
  mg_2013@data$database_dist <- seaway_dist(mg_2013, GER_database, trb) 
  mg_2013@data$historic_dist <- seaway_dist(mg_2013, GER_historic, trb)
  mg_2014@data$database_dist <- seaway_dist(mg_2014, GER_database, trb) 
  mg_2014@data$historic_dist <- seaway_dist(mg_2014, GER_historic, trb)
  mg_2015@data$database_dist <- seaway_dist(mg_2015, GER_database, trb) 
  mg_2015@data$historic_dist <- seaway_dist(mg_2015, GER_historic, trb)
  mg_2016@data$database_dist <- seaway_dist(mg_2016, GER_database, trb) 
  mg_2016@data$historic_dist <- seaway_dist(mg_2016, GER_historic, trb)
  mg_2017@data$database_dist <- seaway_dist(mg_2017, GER_database, trb) 
  mg_2017@data$historic_dist <- seaway_dist(mg_2017, GER_historic, trb)
  mg_2018@data$database_dist <- seaway_dist(mg_2018, GER_database, trb) 
  mg_2018@data$historic_dist <- seaway_dist(mg_2018, GER_historic, trb)
  mg_2019@data$database_dist <- seaway_dist(mg_2019, GER_database, trb) 
  mg_2019@data$historic_dist <- seaway_dist(mg_2019, GER_historic, trb)
}
#2020s
{ mg_2020@data$database_dist <- seaway_dist(mg_2020, GER_database, trb) 
  mg_2020@data$historic_dist <- seaway_dist(mg_2020, GER_historic, trb)
  mg_2021@data$database_dist <- seaway_dist(mg_2021, GER_database, trb) 
  mg_2021@data$historic_dist <- seaway_dist(mg_2021, GER_historic, trb)
}

## Convert distances to data frame -----------------------------------------

#2000s
{ ddist <- mg_2006$database_dist
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
#2010s
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

GER <- rbind(mg_2006, mg_2007, mg_2008, mg_2009, mg_2010, mg_2011, mg_2012, 
             mg_2013, mg_2014, mg_2015, mg_2016, mg_2017, mg_2018, mg_2019, 
             mg_2020, mg_2021)

colnames(GER) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

write.csv(GER, "./Data/Distance regression/Distance_regression_Germany.csv", row.names=FALSE)

## Plot --------------------------------------------------------------------

GER <- read.csv("./Data/Distance regression/Distance_regression_Germany.csv")

GER$Year <- as.integer(GER$Year)

GER_d <- GER[, c(1, 4)]
colnames(GER_d) <- c("Year", "Distance")
GER_d$type <- "database" 

GER_h <- GER[, c(1, 5)]
colnames(GER_h) <- c("Year", "Distance")
GER_h$type <- "historical" 

GER <- rbind(GER_d, GER_h)

GER_d_l <- GER_d %>% group_by(Year) %>% top_n(1, Distance)
GER_d_l <- GER_d_l[!duplicated(GER_d_l), ]
GER_d_l <- ungroup(GER_d_l)
GER_d_l <- GER_d_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 3 times
plot(GER_d_l$Distance~GER_d_l$Year, type="b")

GER_h_l <- GER_h %>% group_by(Year) %>% top_n(1, Distance)
GER_h_l <- GER_h_l[!duplicated(GER_h_l), ]
GER_h_l <- ungroup(GER_h_l)
GER_h_l <- GER_h_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 3 times
plot(GER_h_l$Distance~GER_h_l$Year, type="b")

GER_plot <- ggplot(data=GER, aes(x=Year, y=Distance, group=1, colour=type, fill=type)) +
  geom_point(size=1.5, alpha=0.3)+
  scale_x_continuous(breaks=c(2006, 2009, 2012, 2015, 2018, 2021))+
  scale_y_continuous(limits=c(NA, 800))+
  scale_colour_manual(values = c("#f1a340", "#998ec3"))+
  scale_fill_manual(values = c("#f1a340", "#998ec3"))+
  geom_smooth(data=GER_h_l, method="lm", se=FALSE, linetype="dashed")+
  geom_point(data=GER_h_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#998ec3")+
  geom_smooth(data=GER_d_l, method="lm", se=FALSE, linetype="dashed")+
  geom_point(data=GER_d_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#f1a340")+
  ylab("Distance from initial observation (km)")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))
GER_plot

# Linear models -----------------------------------------------------------

model_GER_d <- lm(Distance~Year, GER_d_l)
summary(model_GER_d)
# RSE = 177.9 on 4 DoF 
# Multiple R-squared = 0.5043
# Adjusted R-squared = 0.3804
# F-stat = 4.069
# P-value = 0.1139
# Rate of spread = 27.78 km/y +- 13.77

# Plot model to check
par(mfrow=c(2,2))
plot(model_GER_d) 
# Plots actually look pretty good
par(mfrow=c(1,1))

# Check normality
plot(density(GER_d_l$Distance)) # really skewed
# Check skewness
skewness(GER_d_l$Distance) # 1.346166

# Check the residuals for normality
shapiro.test(residuals(model_GER_d))
# P-value = 0.4121
# residuals are fine

## Do the same for historical ---
model_GER_h <- lm(Distance~Year, GER_h_l)
summary(model_GER_h)
# RSE = 190.4 on 5 DoF
# Multiple R-squared = 0.3489
# Adjusted R-squared = 0.2187
# F-stat = 2.679
# P-value = 0.1626
# Rate of spread = 22.27 km/y +- 13.6

# Plot model to check
par(mfrow=c(2,2))
plot(model_GER_h) 
# Not brilliant
par(mfrow=c(1,1))

# Check normality
plot(density(GER_h_l$Distance)) # really skewed
# Check skewness
skewness(GER_h_l$Distance) # 1.603989

# Check the residuals for normality
shapiro.test(residuals(model_GER_h))
# P-value = 0.1561
# residuals are fine

# Comparison of slopes ----------------------------------------------------

# Bind the data
mod_data <- rbind(GER_d_l, GER_h_l)

# Construct a new model with both introduction types
comp_mod <- lm(Distance~Year*type, data=mod_data)
summary(comp_mod)

# Construct a second model without the interaction term to test for a significant difference in the slope
comp_mod2 <- lm(Distance~Year+type, data=mod_data)
summary(comp_mod2)

# Compare the two models with an anova to assess if removing the interaction term significantly affects the fit of the model
anova(comp_mod, comp_mod2)
# P-value = 0.7835
# No significant effect

# Therefore, there is no significant difference between the slope for the database and the slope for the literature

# -------------------------------------------------------------------------
# -------------------------------- Ireland --------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

#table(IRE$Year)

#1980s
{ mg_1986 <- filter(IRE, Year==1986)
}
#1990s
{ mg_1992 <- filter(IRE, Year==1992)
  mg_1994 <- filter(IRE, Year==1994)
  mg_1995 <- filter(IRE, Year==1995)
  mg_1996 <- filter(IRE, Year==1996)
  mg_1997 <- filter(IRE, Year==1997)
  mg_1999 <- filter(IRE, Year==1999)
}
#2000s
{ mg_2000 <- filter(IRE, Year==2000)
  mg_2001 <- filter(IRE, Year==2001)
  mg_2002 <- filter(IRE, Year==2002)
  mg_2003 <- filter(IRE, Year==2003)
  mg_2004 <- filter(IRE, Year==2004)
  mg_2005 <- filter(IRE, Year==2005)
  mg_2006 <- filter(IRE, Year==2006)
  mg_2007 <- filter(IRE, Year==2007)
  mg_2009 <- filter(IRE, Year==2009)
}
#2010s
{ mg_2010 <- filter(IRE, Year==2010)
  mg_2014 <- filter(IRE, Year==2014)
  mg_2015 <- filter(IRE, Year==2015)
  mg_2017 <- filter(IRE, Year==2017)
  mg_2018 <- filter(IRE, Year==2018)
}

# First record from Germany
IRE_first <- filter(mg_1, Country=="Ireland")

## Separate the long and lat values ----------------------------------------

#1980s
{ mg_1986 <- mg_1986[, 8:7]
}
#1990s
{ mg_1992 <- mg_1992[, 8:7]
  mg_1994 <- mg_1994[, 8:7]
  mg_1995 <- mg_1995[, 8:7]
  mg_1996 <- mg_1996[, 8:7]
  mg_1997 <- mg_1997[, 8:7]
  mg_1999 <- mg_1999[, 8:7]
}
#2000s
{ mg_2000 <- mg_2000[, 8:7]
  mg_2001 <- mg_2001[, 8:7]
  mg_2002 <- mg_2002[, 8:7]
  mg_2003 <- mg_2003[, 8:7]
  mg_2004 <- mg_2004[, 8:7]
  mg_2005 <- mg_2005[, 8:7]
  mg_2006 <- mg_2006[, 8:7]
  mg_2007 <- mg_2007[, 8:7]
  mg_2009 <- mg_2009[, 8:7]
}
#2010s
{ mg_2010 <- mg_2010[, 8:7]
  mg_2014 <- mg_2014[, 8:7]
  mg_2015 <- mg_2015[, 8:7]
  mg_2017 <- mg_2017[, 8:7]
  mg_2018 <- mg_2018[, 8:7]
}

IRE_database <- IRE_first[2, 3:4]
IRE_historic <- IRE_first[1, 3:4]

## Convert points to spdf ---------------------------------------------------

#1980s
{ mg_1986 <- SpatialPointsDataFrame(coords=mg_1986[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1986)
}
#1990s
{ mg_1992 <- SpatialPointsDataFrame(coords=mg_1992[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1992)
  mg_1994 <- SpatialPointsDataFrame(coords=mg_1994[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1994)
  mg_1995 <- SpatialPointsDataFrame(coords=mg_1995[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1995)
  mg_1996 <- SpatialPointsDataFrame(coords=mg_1996[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1996)
  mg_1997 <- SpatialPointsDataFrame(coords=mg_1997[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1997)
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
  mg_2009 <- SpatialPointsDataFrame(coords=mg_2009[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2009)
}
#2010s
{ mg_2010 <- SpatialPointsDataFrame(coords=mg_2010[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2010)
  mg_2014 <- SpatialPointsDataFrame(coords=mg_2014[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2014)
  mg_2015 <- SpatialPointsDataFrame(coords=mg_2015[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2015)
  mg_2017 <- SpatialPointsDataFrame(coords=mg_2017[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2017)
  mg_2018 <- SpatialPointsDataFrame(coords=mg_2018[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2018)
}

#initial
{ IRE_database <- SpatialPointsDataFrame(coords=IRE_database[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=IRE_database)
  IRE_historic <- SpatialPointsDataFrame(coords=IRE_historic[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=IRE_historic)
}

## Convert projection of points to UTM -------------------------------------

#1980s
{ mg_1986 <- spTransform(mg_1986, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1990s
{ mg_1992 <- spTransform(mg_1992, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1994 <- spTransform(mg_1994, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1995 <- spTransform(mg_1995, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1996 <- spTransform(mg_1996, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1997 <- spTransform(mg_1997, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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
  mg_2009 <- spTransform(mg_2009, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#2010s
{ mg_2010 <- spTransform(mg_2010, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2014 <- spTransform(mg_2014, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2015 <- spTransform(mg_2015, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2017 <- spTransform(mg_2017, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_2018 <- spTransform(mg_2018, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

#initial
{ IRE_database <- spTransform(IRE_database, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  IRE_historic <- spTransform(IRE_historic, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

## Calculate the shortest path to the database and historic introductions ----

#1980s
{ mg_1986@data$database_dist <- seaway_dist(mg_1986, IRE_database, trb) 
mg_1986@data$historic_dist <- seaway_dist(mg_1986, mg_historic, trb)
}
#1990s
{ mg_1992@data$database_dist <- seaway_dist(mg_1992, IRE_database, trb) 
  mg_1992@data$historic_dist <- seaway_dist(mg_1992, IRE_historic, trb)
  mg_1994@data$database_dist <- seaway_dist(mg_1994, IRE_database, trb) 
  mg_1994@data$historic_dist <- seaway_dist(mg_1994, IRE_historic, trb)
  mg_1995@data$database_dist <- seaway_dist(mg_1995, IRE_database, trb) 
  mg_1995@data$historic_dist <- seaway_dist(mg_1995, IRE_historic, trb)
  mg_1996@data$database_dist <- seaway_dist(mg_1996, IRE_database, trb) 
  mg_1996@data$historic_dist <- seaway_dist(mg_1996, IRE_historic, trb)
  mg_1997@data$database_dist <- seaway_dist(mg_1997, IRE_database, trb) 
  mg_1997@data$historic_dist <- seaway_dist(mg_1997, IRE_historic, trb)
  mg_1999@data$database_dist <- seaway_dist(mg_1999, IRE_database, trb) 
  mg_1999@data$historic_dist <- seaway_dist(mg_1999, IRE_historic, trb)
}
#2000s
{ mg_2000@data$database_dist <- seaway_dist(mg_2000, IRE_database, trb) 
  mg_2000@data$historic_dist <- seaway_dist(mg_2000, IRE_historic, trb)
  mg_2001@data$database_dist <- seaway_dist(mg_2001, IRE_database, trb) 
  mg_2001@data$historic_dist <- seaway_dist(mg_2001, IRE_historic, trb)
  mg_2002@data$database_dist <- seaway_dist(mg_2002, IRE_database, trb) 
  mg_2002@data$historic_dist <- seaway_dist(mg_2002, IRE_historic, trb)
  mg_2003@data$database_dist <- seaway_dist(mg_2003, IRE_database, trb) 
  mg_2003@data$historic_dist <- seaway_dist(mg_2003, IRE_historic, trb)
  mg_2004@data$database_dist <- seaway_dist(mg_2004, IRE_database, trb) 
  mg_2004@data$historic_dist <- seaway_dist(mg_2004, IRE_historic, trb)
  mg_2005@data$database_dist <- seaway_dist(mg_2005, IRE_database, trb) 
  mg_2005@data$historic_dist <- seaway_dist(mg_2005, IRE_historic, trb)
  mg_2006@data$database_dist <- seaway_dist(mg_2006, IRE_database, trb) 
  mg_2006@data$historic_dist <- seaway_dist(mg_2006, IRE_historic, trb)
  mg_2007@data$database_dist <- seaway_dist(mg_2007, IRE_database, trb) 
  mg_2007@data$historic_dist <- seaway_dist(mg_2007, IRE_historic, trb)
  mg_2009@data$database_dist <- seaway_dist(mg_2009, IRE_database, trb) 
  mg_2009@data$historic_dist <- seaway_dist(mg_2009, IRE_historic, trb)
}
#2010s
{ mg_2010@data$database_dist <- seaway_dist(mg_2010, IRE_database, trb) 
  mg_2010@data$historic_dist <- seaway_dist(mg_2010, IRE_historic, trb)
  mg_2014@data$database_dist <- seaway_dist(mg_2014, IRE_database, trb) 
  mg_2014@data$historic_dist <- seaway_dist(mg_2014, IRE_historic, trb)
  mg_2015@data$database_dist <- seaway_dist(mg_2015, IRE_database, trb) 
  mg_2015@data$historic_dist <- seaway_dist(mg_2015, IRE_historic, trb)
  mg_2017@data$database_dist <- seaway_dist(mg_2017, IRE_database, trb) 
  mg_2017@data$historic_dist <- seaway_dist(mg_2017, IRE_historic, trb)
  mg_2018@data$database_dist <- seaway_dist(mg_2018, IRE_database, trb) 
  mg_2018@data$historic_dist <- seaway_dist(mg_2018, IRE_historic, trb)
  }

## Convert distances to data frame -----------------------------------------

#1980s
{ ddist <- mg_1986$database_dist
hdist <- mg_1986$historic_dist
lat <- as.data.frame(mg_1986$Latitude)
lon <- as.data.frame(mg_1986$Longitude)
year <- as.data.frame(mg_1986$Year)
mg_1986 <- cbind(year, lon, lat, ddist, hdist)
rownames(mg_1986) <- NULL
colnames(mg_1986) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
}
#1990s
{ ddist <- mg_1992$database_dist
  hdist <- mg_1992$historic_dist
  lat <- as.data.frame(mg_1992$Latitude)
  lon <- as.data.frame(mg_1992$Longitude)
  year <- as.data.frame(mg_1992$Year)
  mg_1992 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1992) <- NULL
  colnames(mg_1992) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
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
}  

IRE <- rbind(mg_1986, mg_1992, mg_1994, mg_1995, mg_1996, mg_1997, mg_1999, 
             mg_2000, mg_2001, mg_2002, mg_2003, mg_2004, mg_2005, mg_2006, 
             mg_2007, mg_2009, mg_2010, mg_2014, mg_2015, mg_2017, mg_2018)

colnames(IRE) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

write.csv(IRE, "./Data/Distance regression/Distance_regression_Ireland.csv", row.names=FALSE)

# Plot --------------------------------------------------------------------

IRE <- read.csv("./Data/Distance regression/Distance_regression_Ireland.csv")

IRE$Year <- as.integer(IRE$Year)

IRE_d <- IRE[, c(1, 4)]
colnames(IRE_d) <- c("Year", "Distance")
IRE_d$type <- "database" 

IRE_h <- IRE[, c(1, 5)]
colnames(IRE_h) <- c("Year", "Distance")
IRE_h$type <- "historical" 

IRE <- rbind(IRE_d, IRE_h)

IRE_d_l <- IRE_d %>% group_by(Year) %>% top_n(1, Distance)
IRE_d_l <- IRE_d_l[!duplicated(IRE_d_l), ]
IRE_d_l <- ungroup(IRE_d_l)
IRE_d_l <- IRE_d_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 3 times
plot(IRE_d_l$Distance~IRE_d_l$Year, type="b")

IRE_h_l <- IRE_h %>% group_by(Year) %>% top_n(1, Distance)
IRE_h_l <- IRE_h_l[!duplicated(IRE_h_l), ]
IRE_h_l <- ungroup(IRE_h_l)
IRE_h_l <- IRE_h_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 3 times
plot(IRE_h_l$Distance~IRE_h_l$Year, type="b")

IRE_plot <- ggplot(data=IRE, aes(x=Year, y=Distance, group=1, colour=type, fill=type)) +
  geom_point(size=1.5, alpha=0.3)+
  scale_x_continuous(breaks=c(1986, 1992, 1998, 2004, 2010, 2016))+
  scale_colour_manual(values = c("#f1a340", "#998ec3"))+
  scale_fill_manual(values = c("#f1a340", "#998ec3"))+
  geom_smooth(data=IRE_h_l, method="lm", se=FALSE)+
  geom_point(data=IRE_h_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#998ec3")+
  geom_smooth(data=IRE_d_l, method="lm", se=FALSE)+
  geom_point(data=IRE_d_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#f1a340")+
  ylab("Distance from initial observation (km)")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))
IRE_plot

# Linear models -----------------------------------------------------------

model_IRE_d <- lm(Distance~Year, IRE_d_l)
summary(model_IRE_d)
# RSE = 114.9 on 3 DoF - Much smaller
# Multiple R-squared = 0.8189
# Adjusted R-squared = 0.7585
# F-stat = 13.56
# P-value = 0.03468
# Rate of spread = 22.334 km/y +- 6.064

# Plot model to check
par(mfrow=c(2,2))
plot(model_IRE_d) 
# Not great
par(mfrow=c(1,1))

# Check normality
plot(density(IRE_d_l$Distance)) # pretty good
# Check skewness
skewness(IRE_d_l$Distance) # -0.5514765

# Check the residuals for normality
shapiro.test(residuals(model_IRE_d))
# P-value = 0.364
# residuals are fine

## Do the same for historical ---
model_IRE_h <- lm(Distance~Year, IRE_h_l)
summary(model_IRE_h)
# RSE = 111.8 on 3 DoF - Much smaller
# Multiple R-squared = 0.8209
# Adjusted R-squared = 0.7612
# F-stat = 13.75
# P-value = 0.03409
# Rate of spread = 21.891 km/y +- 5.904

# Plot model to check
par(mfrow=c(2,2))
plot(model_IRE_h) 
# Not great
par(mfrow=c(1,1))

# Check normality
plot(density(IRE_h_l$Distance)) # pretty good
# Check skewness
skewness(IRE_h_l$Distance) # -0.5309733

# Check the residuals for normality
shapiro.test(residuals(model_IRE_h))
# P-value = 0.3981
# residuals are fine

# Comparison of slopes ----------------------------------------------------

# Bind the data
mod_data <- rbind(IRE_d_l, IRE_h_l)

# Construct a new model with both introduction types
comp_mod <- lm(Distance~Year*type, data=mod_data)
summary(comp_mod)

# Construct a second model without the interaction term to test for a significant difference in the slope
comp_mod2 <- lm(Distance~Year+type, data=mod_data)
summary(comp_mod2)

# Compare the two models with an anova to assess if removing the interaction term significantly affects the fit of the model
anova(comp_mod, comp_mod2)
# P-value = 0.96
# No significant effect

# Therefore, there is no significant difference between the slope for the database and the slope for the literature

# -------------------------------------------------------------------------
# ------------------------------ Netherlands ------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

#table(NET$Year)

#1980s
{ mg_1982 <- filter(NET, Year==1982)
}
#1990s
{ mg_1993 <- filter(NET, Year==1993)
  mg_1996 <- filter(NET, Year==1996)
}
#2000s
{ mg_2000 <- filter(NET, Year==2000)
  mg_2001 <- filter(NET, Year==2001)
  mg_2002 <- filter(NET, Year==2002)
  mg_2003 <- filter(NET, Year==2003)
  mg_2004 <- filter(NET, Year==2004)
  mg_2005 <- filter(NET, Year==2005)
  mg_2006 <- filter(NET, Year==2006)
  mg_2007 <- filter(NET, Year==2007)
  mg_2008 <- filter(NET, Year==2008)
  mg_2009 <- filter(NET, Year==2009)
}
#2010s
{ mg_2010 <- filter(NET, Year==2010)
  mg_2011 <- filter(NET, Year==2011)
  mg_2012 <- filter(NET, Year==2012)
  mg_2013 <- filter(NET, Year==2013)
  mg_2014 <- filter(NET, Year==2014)
  mg_2015 <- filter(NET, Year==2015)
  mg_2016 <- filter(NET, Year==2016)
  mg_2017 <- filter(NET, Year==2017)
  mg_2018 <- filter(NET, Year==2018)
  mg_2019 <- filter(NET, Year==2019)
}
#2020s
{ mg_2020 <- filter(NET, Year==2020)
  mg_2021 <- filter(NET, Year==2021)
}

# First record from Germany
NET_first <- filter(mg_1, Country=="Netherlands")

## Separate the long and lat values ----------------------------------------

#1980s
{ mg_1982 <- mg_1982[, 8:7]
}
#1990s
{ mg_1993 <- mg_1993[, 8:7]
  mg_1996 <- mg_1996[, 8:7]
}
#2000s
{ mg_2000 <- mg_2000[, 8:7]
  mg_2001 <- mg_2001[, 8:7]
  mg_2002 <- mg_2002[, 8:7]
  mg_2003 <- mg_2003[, 8:7]
  mg_2004 <- mg_2004[, 8:7]
  mg_2005 <- mg_2005[, 8:7]
  mg_2006 <- mg_2006[, 8:7]
  mg_2007 <- mg_2007[, 8:7]
  mg_2008 <- mg_2008[, 8:7]
  mg_2009 <- mg_2009[, 8:7]
}
#2010s
{ mg_2010 <- mg_2010[, 8:7]
  mg_2011 <- mg_2011[, 8:7]
  mg_2012 <- mg_2012[, 8:7]
  mg_2013 <- mg_2013[, 8:7]
  mg_2014 <- mg_2014[, 8:7]
  mg_2015 <- mg_2015[, 8:7]
  mg_2016 <- mg_2016[, 8:7]
  mg_2017 <- mg_2017[, 8:7]
  mg_2018 <- mg_2018[, 8:7]
  mg_2019 <- mg_2019[, 8:7]
}
#2020s
{ mg_2020 <- mg_2020[, 8:7]
  mg_2021 <- mg_2021[, 8:7]
}

NET_database <- NET_first[2, 3:4]
NET_historic <- NET_first[1, 3:4]

## Convert points to spdf ---------------------------------------------------

#1980s
{ mg_1982 <- SpatialPointsDataFrame(coords=mg_1982[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1982)
}
#1990s
{ mg_1993 <- SpatialPointsDataFrame(coords=mg_1993[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1993)
  mg_1996 <- SpatialPointsDataFrame(coords=mg_1996[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1996)
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
{  mg_2020 <- SpatialPointsDataFrame(coords=mg_2020[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2020)
  mg_2021 <- SpatialPointsDataFrame(coords=mg_2021[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2021)
}

#initial
{ NET_database <- SpatialPointsDataFrame(coords=NET_database[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=NET_database)
  NET_historic <- SpatialPointsDataFrame(coords=NET_historic[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=NET_historic)
}

## Move points off of land -------------------------------------------------

#2010s
{ mg_2014 <- points2nearestcell(mg_2014, bathy)
}
#2020s
{ mg_2020 <- points2nearestcell(mg_2020, bathy)
}

## Convert projection of points to UTM -------------------------------------

#1980s
{ mg_1982 <- spTransform(mg_1982, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1990s
{ mg_1993 <- spTransform(mg_1993, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1996 <- spTransform(mg_1996, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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
{ NET_database <- spTransform(NET_database, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  NET_historic <- spTransform(NET_historic, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

## Calculate the shortest path to the database and historic introductions ----

#1980s
{ mg_1982@data$database_dist <- seaway_dist(mg_1982, NET_database, trb) 
mg_1982@data$historic_dist <- seaway_dist(mg_1982, NET_historic, trb)
}
#1990s
{ mg_1993@data$database_dist <- seaway_dist(mg_1993, NET_database, trb) 
  mg_1993@data$historic_dist <- seaway_dist(mg_1993, NET_historic, trb)
  mg_1996@data$database_dist <- seaway_dist(mg_1996, NET_database, trb) 
  mg_1996@data$historic_dist <- seaway_dist(mg_1996, NET_historic, trb)
}
#2000s
{ mg_2000@data$database_dist <- seaway_dist(mg_2000, NET_database, trb) 
  mg_2000@data$historic_dist <- seaway_dist(mg_2000, NET_historic, trb)
  mg_2001@data$database_dist <- seaway_dist(mg_2001, NET_database, trb) 
  mg_2001@data$historic_dist <- seaway_dist(mg_2001, NET_historic, trb)
  mg_2002@data$database_dist <- seaway_dist(mg_2002, NET_database, trb) 
  mg_2002@data$historic_dist <- seaway_dist(mg_2002, NET_historic, trb)
  mg_2003@data$database_dist <- seaway_dist(mg_2003, NET_database, trb) 
  mg_2003@data$historic_dist <- seaway_dist(mg_2003, NET_historic, trb)
  mg_2004@data$database_dist <- seaway_dist(mg_2004, NET_database, trb) 
  mg_2004@data$historic_dist <- seaway_dist(mg_2004, NET_historic, trb)
  mg_2005@data$database_dist <- seaway_dist(mg_2005, NET_database, trb) 
  mg_2005@data$historic_dist <- seaway_dist(mg_2005, NET_historic, trb)
  mg_2006@data$database_dist <- seaway_dist(mg_2006, NET_database, trb) 
  mg_2006@data$historic_dist <- seaway_dist(mg_2006, NET_historic, trb)
  mg_2007@data$database_dist <- seaway_dist(mg_2007, NET_database, trb) 
  mg_2007@data$historic_dist <- seaway_dist(mg_2007, NET_historic, trb)
  mg_2008@data$database_dist <- seaway_dist(mg_2008, NET_database, trb) 
  mg_2008@data$historic_dist <- seaway_dist(mg_2008, NET_historic, trb)
  mg_2009@data$database_dist <- seaway_dist(mg_2009, NET_database, trb) 
  mg_2009@data$historic_dist <- seaway_dist(mg_2009, NET_historic, trb)
}
#2010s
{ mg_2010@data$database_dist <- seaway_dist(mg_2010, NET_database, trb) 
  mg_2010@data$historic_dist <- seaway_dist(mg_2010, NET_historic, trb)
  mg_2011@data$database_dist <- seaway_dist(mg_2011, NET_database, trb) 
  mg_2011@data$historic_dist <- seaway_dist(mg_2011, NET_historic, trb)
  mg_2012@data$database_dist <- seaway_dist(mg_2012, NET_database, trb) 
  mg_2012@data$historic_dist <- seaway_dist(mg_2012, NET_historic, trb)
  mg_2013@data$database_dist <- seaway_dist(mg_2013, NET_database, trb) 
  mg_2013@data$historic_dist <- seaway_dist(mg_2013, NET_historic, trb)
  mg_2014@data$database_dist <- seaway_dist(mg_2014, NET_database, trb) 
  mg_2014@data$historic_dist <- seaway_dist(mg_2014, NET_historic, trb)
  mg_2015@data$database_dist <- seaway_dist(mg_2015, NET_database, trb) 
  mg_2015@data$historic_dist <- seaway_dist(mg_2015, NET_historic, trb)
  mg_2016@data$database_dist <- seaway_dist(mg_2016, NET_database, trb) 
  mg_2016@data$historic_dist <- seaway_dist(mg_2016, NET_historic, trb)
  mg_2017@data$database_dist <- seaway_dist(mg_2017, NET_database, trb) 
  mg_2017@data$historic_dist <- seaway_dist(mg_2017, NET_historic, trb)
  mg_2018@data$database_dist <- seaway_dist(mg_2018, NET_database, trb) 
  mg_2018@data$historic_dist <- seaway_dist(mg_2018, NET_historic, trb)
  mg_2019@data$database_dist <- seaway_dist(mg_2019, NET_database, trb) 
  mg_2019@data$historic_dist <- seaway_dist(mg_2019, NET_historic, trb)
}
#2020s
{ mg_2020@data$database_dist <- seaway_dist(mg_2020, NET_database, trb) 
  mg_2020@data$historic_dist <- seaway_dist(mg_2020, NET_historic, trb)
  mg_2021@data$database_dist <- seaway_dist(mg_2021, NET_database, trb) 
  mg_2021@data$historic_dist <- seaway_dist(mg_2021, NET_historic, trb)
}

## Convert distances to data frame -----------------------------------------

#1980s
{ ddist <- mg_1982$database_dist
hdist <- mg_1982$historic_dist
lat <- as.data.frame(mg_1982$Latitude)
lon <- as.data.frame(mg_1982$Longitude)
year <- as.data.frame(mg_1982$Year)
mg_1982 <- cbind(year, lon, lat, ddist, hdist)
rownames(mg_1982) <- NULL
colnames(mg_1982) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
}
#1990s
{ ddist <- mg_1993$database_dist
  hdist <- mg_1993$historic_dist
  lat <- as.data.frame(mg_1993$Latitude)
  lon <- as.data.frame(mg_1993$Longitude)
  year <- as.data.frame(mg_1993$Year)
  mg_1993 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1993) <- NULL
  colnames(mg_1993) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
  
  ddist <- mg_1996$database_dist
  hdist <- mg_1996$historic_dist
  lat <- as.data.frame(mg_1996$Latitude)
  lon <- as.data.frame(mg_1996$Longitude)
  year <- as.data.frame(mg_1996$Year)
  mg_1996 <- cbind(year, lon, lat, ddist, hdist)
  rownames(mg_1996) <- NULL
  colnames(mg_1996) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")
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

NET <- rbind(mg_1982, mg_1993, mg_1996, mg_2000, mg_2001, mg_2002, mg_2003, 
             mg_2004, mg_2005, mg_2006, mg_2007, mg_2008, mg_2009, mg_2010, 
             mg_2011, mg_2012, mg_2013, mg_2014, mg_2015, mg_2016, mg_2017, 
             mg_2018, mg_2019, mg_2020, mg_2021)

colnames(NET) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

write.csv(NET, "./Data/Distance regression/Distance_regression_Netherlands.csv", row.names=FALSE)

# Plot --------------------------------------------------------------------

NET <- read.csv("./Data/Distance regression/Distance_regression_Netherlands.csv")

NET$Year <- as.integer(NET$Year)

NET_d <- NET[, c(1, 4)]
colnames(NET_d) <- c("Year", "Distance")
NET_d$type <- "database" 

NET_h <- NET[, c(1, 5)]
colnames(NET_h) <- c("Year", "Distance")
NET_h$type <- "historical" 

NET <- rbind(NET_d, NET_h)

NET_d_l <- NET_d %>% group_by(Year) %>% top_n(1, Distance)
NET_d_l <- NET_d_l[!duplicated(NET_d_l), ]
NET_d_l <- ungroup(NET_d_l)
NET_d_l <- NET_d_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 7 times
plot(NET_d_l$Distance~NET_d_l$Year, type="b")

NET_h_l <- NET_h %>% group_by(Year) %>% top_n(1, Distance)
NET_h_l <- NET_h_l[!duplicated(NET_h_l), ]
NET_h_l <- ungroup(NET_h_l) 
NET_h_l <- NET_h_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 7 times
plot(NET_h_l$Distance~NET_h_l$Year, type="b")

NET_plot <- ggplot(data=NET, aes(x=Year, y=Distance, group=1, colour=type, fill=type)) +
  geom_point(size=1.5, alpha=0.3)+
  scale_x_continuous(breaks=c(1989, 1995, 2001, 2007, 2013, 2019))+
  scale_y_continuous(limits=c(NA, 400))+
  scale_colour_manual(values = c("#f1a340", "#998ec3"))+
  scale_fill_manual(values = c("#f1a340", "#998ec3"))+
  geom_smooth(data=NET_h_l, method="lm", se=FALSE, linetype="dotted")+
  geom_point(data=NET_h_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#998ec3")+
  geom_smooth(data=NET_d_l, method="lm", se=FALSE, linetype="dotted")+
  geom_point(data=NET_d_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#f1a340")+
  ylab("Distance from initial observation (km)")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))
NET_plot

# Linear models -----------------------------------------------------------

# It's not model-able because it's only 2 data points

# -------------------------------------------------------------------------
# -------------------------------- Norway ---------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

#table(NOR$Year)

#2000s
{ mg_2001 <- filter(NOR, Year==2001)
mg_2005 <- filter(NOR, Year==2005)
mg_2007 <- filter(NOR, Year==2007)
mg_2008 <- filter(NOR, Year==2008)
mg_2009 <- filter(NOR, Year==2009)
}
#2010s
{ mg_2011 <- filter(NOR, Year==2011)
  mg_2012 <- filter(NOR, Year==2012)
  mg_2013 <- filter(NOR, Year==2013)
  mg_2014 <- filter(NOR, Year==2014)
  mg_2015 <- filter(NOR, Year==2015)
  mg_2016 <- filter(NOR, Year==2016)
  mg_2017 <- filter(NOR, Year==2017)
  mg_2018 <- filter(NOR, Year==2018)
  mg_2019 <- filter(NOR, Year==2019)
}
#2020s
{ mg_2020 <- filter(NOR, Year==2020)
  mg_2021 <- filter(NOR, Year==2021)
}

# First record from Germany
NOR_first <- filter(mg_1, Country=="Norway")

## Separate the long and lat values ----------------------------------------

#2000s
{ mg_2001 <- mg_2001[, 8:7]
mg_2005 <- mg_2005[, 8:7]
mg_2007 <- mg_2007[, 8:7]
mg_2008 <- mg_2008[, 8:7]
mg_2009 <- mg_2009[, 8:7]
}
#2010s
{ mg_2011 <- mg_2011[, 8:7]
  mg_2012 <- mg_2012[, 8:7]
  mg_2013 <- mg_2013[, 8:7]
  mg_2014 <- mg_2014[, 8:7]
  mg_2015 <- mg_2015[, 8:7]
  mg_2016 <- mg_2016[, 8:7]
  mg_2017 <- mg_2017[, 8:7]
  mg_2018 <- mg_2018[, 8:7]
  mg_2019 <- mg_2019[, 8:7]
}
#2020s
{ mg_2020 <- mg_2020[, 8:7]
  mg_2021 <- mg_2021[, 8:7]
}

NOR_database <- NOR_first[2, 3:4]
NOR_historic <- NOR_first[1, 3:4]

## Convert points to spdf ---------------------------------------------------

#2000s
{ mg_2001 <- SpatialPointsDataFrame(coords=mg_2001[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2001)
mg_2005 <- SpatialPointsDataFrame(coords=mg_2005[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2005)
mg_2007 <- SpatialPointsDataFrame(coords=mg_2007[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2007)
mg_2008 <- SpatialPointsDataFrame(coords=mg_2008[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2008)
mg_2009 <- SpatialPointsDataFrame(coords=mg_2009[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2009)
}
#2010s
{ mg_2011 <- SpatialPointsDataFrame(coords=mg_2011[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2011)
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
{ NOR_database <- SpatialPointsDataFrame(coords=NOR_database[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=NOR_database)
  NOR_historic <- SpatialPointsDataFrame(coords=NOR_historic[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=NOR_historic)
}

## Move points off of land -------------------------------------------------

#2010s
{ mg_2016 <- points2nearestcell(mg_2016, bathy)
mg_2017 <- points2nearestcell(mg_2017, bathy)
mg_2018 <- points2nearestcell(mg_2018, bathy)
mg_2019 <- points2nearestcell(mg_2019, bathy)
}
#2020s
{ mg_2020 <- points2nearestcell(mg_2020, bathy)
}

## Convert projection of points to UTM -------------------------------------

#2000s
{ mg_2001 <- spTransform(mg_2001, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_2005 <- spTransform(mg_2005, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_2007 <- spTransform(mg_2007, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_2008 <- spTransform(mg_2008, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_2009 <- spTransform(mg_2009, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#2010s
{ mg_2011 <- spTransform(mg_2011, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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
{ NOR_database <- spTransform(NOR_database, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  NOR_historic <- spTransform(NOR_historic, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

## Calculate the shortest path to the database and historic introductions ----

#2000s
{ mg_2001@data$database_dist <- seaway_dist(mg_2001, NOR_database, trb) 
mg_2001@data$historic_dist <- seaway_dist(mg_2001, NOR_historic, trb)
mg_2005@data$database_dist <- seaway_dist(mg_2005, NOR_database, trb) 
mg_2005@data$historic_dist <- seaway_dist(mg_2005, NOR_historic, trb)
mg_2007@data$database_dist <- seaway_dist(mg_2007, NOR_database, trb) 
mg_2007@data$historic_dist <- seaway_dist(mg_2007, NOR_historic, trb)
mg_2008@data$database_dist <- seaway_dist(mg_2008, NOR_database, trb) 
mg_2008@data$historic_dist <- seaway_dist(mg_2008, NOR_historic, trb)
mg_2009@data$database_dist <- seaway_dist(mg_2009, NOR_database, trb) 
mg_2009@data$historic_dist <- seaway_dist(mg_2009, NOR_historic, trb)
}
#2010s
{ mg_2011@data$database_dist <- seaway_dist(mg_2011, NOR_database, trb) 
  mg_2011@data$historic_dist <- seaway_dist(mg_2011, NOR_historic, trb)
  mg_2012@data$database_dist <- seaway_dist(mg_2012, NOR_database, trb) 
  mg_2012@data$historic_dist <- seaway_dist(mg_2012, NOR_historic, trb)
  mg_2013@data$database_dist <- seaway_dist(mg_2013, NOR_database, trb) 
  mg_2013@data$historic_dist <- seaway_dist(mg_2013, NOR_historic, trb)
  mg_2014@data$database_dist <- seaway_dist(mg_2014, NOR_database, trb) 
  mg_2014@data$historic_dist <- seaway_dist(mg_2014, NOR_historic, trb)
  mg_2015@data$database_dist <- seaway_dist(mg_2015, NOR_database, trb) 
  mg_2015@data$historic_dist <- seaway_dist(mg_2015, NOR_historic, trb)
  mg_2016@data$database_dist <- seaway_dist(mg_2016, NOR_database, trb) 
  mg_2016@data$historic_dist <- seaway_dist(mg_2016, NOR_historic, trb)
  mg_2017@data$database_dist <- seaway_dist(mg_2017, NOR_database, trb) 
  mg_2017@data$historic_dist <- seaway_dist(mg_2017, NOR_historic, trb)
  mg_2018@data$database_dist <- seaway_dist(mg_2018, NOR_database, trb) 
  mg_2018@data$historic_dist <- seaway_dist(mg_2018, NOR_historic, trb)
  mg_2019@data$database_dist <- seaway_dist(mg_2019, NOR_database, trb) 
  mg_2019@data$historic_dist <- seaway_dist(mg_2019, NOR_historic, trb)
}
#2020s
{ mg_2020@data$database_dist <- seaway_dist(mg_2020, NOR_database, trb) 
  mg_2020@data$historic_dist <- seaway_dist(mg_2020, NOR_historic, trb)
  mg_2021@data$database_dist <- seaway_dist(mg_2021, NOR_database, trb) 
  mg_2021@data$historic_dist <- seaway_dist(mg_2021, NOR_historic, trb)
}

## Convert distances to data frame -----------------------------------------

#2000s
{ ddist <- mg_2001$database_dist
hdist <- mg_2001$historic_dist
lat <- as.data.frame(mg_2001$Latitude)
lon <- as.data.frame(mg_2001$Longitude)
year <- as.data.frame(mg_2001$Year)
mg_2001 <- cbind(year, lon, lat, ddist, hdist)
rownames(mg_2001) <- NULL
colnames(mg_2001) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

ddist <- mg_2005$database_dist
hdist <- mg_2005$historic_dist
lat <- as.data.frame(mg_2005$Latitude)
lon <- as.data.frame(mg_2005$Longitude)
year <- as.data.frame(mg_2005$Year)
mg_2005 <- cbind(year, lon, lat, ddist, hdist)
rownames(mg_2005) <- NULL
colnames(mg_2005) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

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
{ ddist <- mg_2011$database_dist
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

NOR <- rbind(mg_2001, mg_2005, mg_2007, mg_2008, mg_2009, mg_2011, mg_2012, 
             mg_2013, mg_2014, mg_2015, mg_2016, mg_2017, mg_2018, mg_2019, 
             mg_2020, mg_2021)

colnames(NOR) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

write.csv(NOR, "./Data/Distance regression/Distance_regression_Norway.csv", row.names=FALSE)

# Plot --------------------------------------------------------------------

NOR <- read.csv("./Data/Distance regression/Distance_regression_Norway.csv")

NOR$Year <- as.integer(NOR$Year)

NOR_d <- NOR[, c(1, 4)]
colnames(NOR_d) <- c("Year", "Distance")
NOR_d$type <- "database" 

NOR_h <- NOR[, c(1, 5)]
colnames(NOR_h) <- c("Year", "Distance")
NOR_h$type <- "historical" 

NOR <- rbind(NOR_d, NOR_h)

NOR_d_l <- NOR_d %>% group_by(Year) %>% top_n(1, Distance)
NOR_d_l <- NOR_d_l[!duplicated(NOR_d_l), ]
NOR_d_l <- ungroup(NOR_d_l)
NOR_d_l <- NOR_d_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat twice
plot(NOR_d_l$Distance~NOR_d_l$Year, type="b")

NOR_h_l <- NOR_h %>% group_by(Year) %>% top_n(1, Distance)
NOR_h_l <- NOR_h_l[!duplicated(NOR_h_l), ]
NOR_h_l <- ungroup(NOR_h_l)
NOR_h_l <- NOR_h_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat twice
plot(NOR_h_l$Distance~NOR_h_l$Year, type="b")

NOR_plot <- ggplot(data=NOR, aes(x=Year, y=Distance, group=1, colour=type, fill=type)) +
  geom_point(size=1.5, alpha=0.3)+
  scale_x_continuous(breaks=c(2001, 2005, 2009, 2013, 2017, 2021))+
  scale_y_continuous(limits=c(NA, 900))+
  scale_colour_manual(values = c("#f1a340", "#998ec3"))+
  scale_fill_manual(values = c("#f1a340", "#998ec3"))+
  geom_smooth(data=NOR_h_l, method="lm", se=FALSE)+
  geom_point(data=NOR_h_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#998ec3")+
  geom_smooth(data=NOR_d_l, method="lm", se=FALSE)+
  geom_point(data=NOR_d_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#f1a340")+
  ylab("Distance from initial observation (km)")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))
NOR_plot

# Linear models -----------------------------------------------------------

model_NOR_d <- lm(Distance~Year, NOR_d_l)
summary(model_NOR_d)
# RSE = 80.48 on 4 DoF 
# Multiple R-squared = 0.8224
# Adjusted R-squared = 0.778
# F-stat = 18.52
# P-value = 0.01261
# Rate of spread = 30.51 km/y +- 7.09

# Plot model to check
par(mfrow=c(2,2))
plot(model_NOR_d) 
# Not great
par(mfrow=c(1,1))

# Check normality
plot(density(NOR_d_l$Distance)) # looks skewed
# Check skewness
skewness(NOR_d_l$Distance) # -1.137164

# Check the residuals for normality
shapiro.test(residuals(model_NOR_d))
# P-value = 0.8545
# residuals are fine

## Do the same for historical ---
model_NOR_h <- lm(Distance~Year, NOR_h_l)
summary(model_NOR_h)
# RSE = 89.44 on 4 DoF 
# Multiple R-squared = 0.8082
# Adjusted R-squared = 0.7602
# F-stat = 16.85
# P-value = 0.01479
# Rate of spread = 32.35 km/y +- 7.88

# Plot model to check
par(mfrow=c(2,2))
plot(model_NOR_h) 
# Not great
par(mfrow=c(1,1))

# Check normality
plot(density(NOR_h_l$Distance)) # looks skewed
# Check skewness
skewness(NOR_h_l$Distance) # -1.163928

# Check the residuals for normality
shapiro.test(residuals(model_NOR_h))
# P-value = 0.8794
# residuals are fine

# Comparison of slopes ----------------------------------------------------

# Bind the data
mod_data <- rbind(NOR_d_l, NOR_h_l)

# Construct a new model with both introduction types
comp_mod <- lm(Distance~Year*type, data=mod_data)
summary(comp_mod)

# Construct a second model without the interaction term to test for a significant difference in the slope
comp_mod2 <- lm(Distance~Year+type, data=mod_data)
summary(comp_mod2)

# Compare the two models with an anova to assess if removing the interaction term significantly affects the fit of the model
anova(comp_mod, comp_mod2)
# P-value = 0.8669
# No significant effect

# Therefore, there is no significant difference between the slope for the database and the slope for the literature

# -------------------------------------------------------------------------
# -------------------------------- Sweden ---------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

#table(SWE$Year)

#2000s
{ mg_2007 <- filter(SWE, Year==2007)
mg_2008 <- filter(SWE, Year==2008)
mg_2009 <- filter(SWE, Year==2009)
}
#2010s
{ mg_2010 <- filter(SWE, Year==2010)
  mg_2011 <- filter(SWE, Year==2011)
  mg_2012 <- filter(SWE, Year==2012)
  mg_2013 <- filter(SWE, Year==2013)
  mg_2014 <- filter(SWE, Year==2014)
  mg_2015 <- filter(SWE, Year==2015)
  mg_2016 <- filter(SWE, Year==2016)
  mg_2017 <- filter(SWE, Year==2017)
  mg_2018 <- filter(SWE, Year==2018)
  mg_2019 <- filter(SWE, Year==2019)
}
#2020s
{ mg_2020 <- filter(SWE, Year==2020)
  mg_2021 <- filter(SWE, Year==2021)
}

# First record from Germany
SWE_first <- filter(mg_1, Country=="Sweden")

## Separate the long and lat values ----------------------------------------

#2000s
{ mg_2007 <- mg_2007[, 8:7]
mg_2008 <- mg_2008[, 8:7]
mg_2009 <- mg_2009[, 8:7]
}
#2010s
{ mg_2010 <- mg_2010[, 8:7]
  mg_2011 <- mg_2011[, 8:7]
  mg_2012 <- mg_2012[, 8:7]
  mg_2013 <- mg_2013[, 8:7]
  mg_2014 <- mg_2014[, 8:7]
  mg_2015 <- mg_2015[, 8:7]
  mg_2016 <- mg_2016[, 8:7]
  mg_2017 <- mg_2017[, 8:7]
  mg_2018 <- mg_2018[, 8:7]
  mg_2019 <- mg_2019[, 8:7]
}
#2020s
{ mg_2020 <- mg_2020[, 8:7]
  mg_2021 <- mg_2021[, 8:7]
}

SWE_database <- SWE_first[2, 3:4]
SWE_historic <- SWE_first[1, 3:4]

## Convert points to spdf ---------------------------------------------------

#2000s
{ mg_2007 <- SpatialPointsDataFrame(coords=mg_2007[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2007)
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
{ SWE_database <- SpatialPointsDataFrame(coords=SWE_database[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=SWE_database)
  SWE_historic <- SpatialPointsDataFrame(coords=SWE_historic[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=SWE_historic)
}

## Move points off of land -------------------------------------------------

#2020s
{ mg_2021 <- points2nearestcell(mg_2021, bathy)
}

## Convert projection of points to UTM -------------------------------------

#2000s
{ mg_2007 <- spTransform(mg_2007, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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
{ SWE_database <- spTransform(SWE_database, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  SWE_historic <- spTransform(SWE_historic, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

## Calculate the shortest path to the database and historic introductions ----

#2000s
{ mg_2007@data$database_dist <- seaway_dist(mg_2007, SWE_database, trb) 
mg_2007@data$historic_dist <- seaway_dist(mg_2007, SWE_historic, trb)
mg_2008@data$database_dist <- seaway_dist(mg_2008, SWE_database, trb) 
mg_2008@data$historic_dist <- seaway_dist(mg_2008, SWE_historic, trb)
mg_2009@data$database_dist <- seaway_dist(mg_2009, SWE_database, trb) 
mg_2009@data$historic_dist <- seaway_dist(mg_2009, SWE_historic, trb)
}
#2010s
{ mg_2010@data$database_dist <- seaway_dist(mg_2010, SWE_database, trb) 
  mg_2010@data$historic_dist <- seaway_dist(mg_2010, SWE_historic, trb)
  mg_2011@data$database_dist <- seaway_dist(mg_2011, SWE_database, trb) 
  mg_2011@data$historic_dist <- seaway_dist(mg_2011, SWE_historic, trb)
  mg_2012@data$database_dist <- seaway_dist(mg_2012, SWE_database, trb) 
  mg_2012@data$historic_dist <- seaway_dist(mg_2012, SWE_historic, trb)
  mg_2013@data$database_dist <- seaway_dist(mg_2013, SWE_database, trb) 
  mg_2013@data$historic_dist <- seaway_dist(mg_2013, SWE_historic, trb)
  mg_2014@data$database_dist <- seaway_dist(mg_2014, SWE_database, trb) 
  mg_2014@data$historic_dist <- seaway_dist(mg_2014, SWE_historic, trb)
  mg_2015@data$database_dist <- seaway_dist(mg_2015, SWE_database, trb) 
  mg_2015@data$historic_dist <- seaway_dist(mg_2015, SWE_historic, trb)
  mg_2016@data$database_dist <- seaway_dist(mg_2016, SWE_database, trb) 
  mg_2016@data$historic_dist <- seaway_dist(mg_2016, SWE_historic, trb)
  mg_2017@data$database_dist <- seaway_dist(mg_2017, SWE_database, trb) 
  mg_2017@data$historic_dist <- seaway_dist(mg_2017, SWE_historic, trb)
  mg_2018@data$database_dist <- seaway_dist(mg_2018, SWE_database, trb) 
  mg_2018@data$historic_dist <- seaway_dist(mg_2018, SWE_historic, trb)
  mg_2019@data$database_dist <- seaway_dist(mg_2019, SWE_database, trb) 
  mg_2019@data$historic_dist <- seaway_dist(mg_2019, SWE_historic, trb)
}
#2020s
{ mg_2020@data$database_dist <- seaway_dist(mg_2020, SWE_database, trb) 
  mg_2020@data$historic_dist <- seaway_dist(mg_2020, SWE_historic, trb)
  mg_2021@data$database_dist <- seaway_dist(mg_2021, SWE_database, trb) 
  mg_2021@data$historic_dist <- seaway_dist(mg_2021, SWE_historic, trb)
}

## Convert distances to data frame -----------------------------------------

#2000s
{ ddist <- mg_2007$database_dist
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

SWE <- rbind(mg_2007, mg_2008, mg_2009, mg_2010, mg_2011, mg_2012, mg_2013, 
             mg_2014, mg_2015, mg_2016, mg_2017, mg_2018, mg_2019, mg_2020, 
             mg_2021)

colnames(SWE) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

write.csv(SWE, "./Data/Distance regression/Distance_regression_Sweden.csv", row.names=FALSE)

# Plot --------------------------------------------------------------------

SWE <- read.csv("./Data/Distance regression/Distance_regression_Sweden.csv")

SWE$Year <- as.integer(SWE$Year)

SWE_d <- SWE[, c(1, 4)]
colnames(SWE_d) <- c("Year", "Distance")
SWE_d$type <- "database" 

SWE_h <- SWE[, c(1, 5)]
colnames(SWE_h) <- c("Year", "Distance")
SWE_h$type <- "historical" 

SWE <- rbind(SWE_d, SWE_h)

SWE_d_l <- SWE_d %>% group_by(Year) %>% top_n(1, Distance)
SWE_d_l <- SWE_d_l[!duplicated(SWE_d_l), ]
SWE_d_l <- ungroup(SWE_d_l)
SWE_d_l <- SWE_d_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 3 times
plot(SWE_d_l$Distance~SWE_d_l$Year, type="b")

SWE_h_l <- SWE_h %>% group_by(Year) %>% top_n(1, Distance)
SWE_h_l <- SWE_h_l[!duplicated(SWE_h_l), ]
SWE_h_l <- ungroup(SWE_h_l)
SWE_h_l <- SWE_h_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 3 times
plot(SWE_h_l$Distance~SWE_h_l$Year, type="b")

SWE_plot <- ggplot(data=SWE, aes(x=Year, y=Distance, group=1, colour=type, fill=type)) +
  geom_point(size=1.5, alpha=0.3)+
  scale_x_continuous(breaks=c(2007, 2011, 2015, 2019))+
  scale_colour_manual(values = c("#f1a340", "#998ec3"))+
  scale_fill_manual(values = c("#f1a340", "#998ec3"))+
  geom_smooth(data=SWE_h_l, method="lm", se=FALSE)+
  geom_point(data=SWE_h_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#998ec3")+
  geom_smooth(data=SWE_d_l, method="lm", se=FALSE)+
  geom_point(data=SWE_d_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#f1a340")+
  ylab("Distance from initial observation (km)")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))
SWE_plot

# Linear models -----------------------------------------------------------

model_SWE_d <- lm(Distance~Year, SWE_d_l)
summary(model_SWE_d)
# RSE = 52.63 on 5 DoF 
# Multiple R-squared = 0.8264
# Adjusted R-squared = 0.7917
# F-stat = 23.8
# P-value = 0.004559
# Rate of spread = 19.825 km/y +- 4.064

# Plot model to check
par(mfrow=c(2,2))
plot(model_SWE_d) 
# not great
par(mfrow=c(1,1))

# Check normality
plot(density(SWE_d_l$Distance)) # not bad
# Check skewness
skewness(SWE_d_l$Distance) # -0.3775437

# Check the residuals for normality
shapiro.test(residuals(model_SWE_d))
# P-value = 0.9449
# residuals are fine

## Do the same for historical ---
model_SWE_h <- lm(Distance~Year, SWE_h_l)
summary(model_SWE_h)
# RSE = 50.87 on 5 DoF - Quite large
# Multiple R-squared = 0.833
# Adjusted R-squared = 0.7996
# F-stat = 24.93
# P-value = 0.004128
# Rate of spread = 19.612 km/y +- 3.928

# Plot model to check
par(mfrow=c(2,2))
plot(model_SWE_h) 
# not great
par(mfrow=c(1,1))

# Check normality
plot(density(SWE_h_l$Distance)) # not bad
# Check skewness
skewness(SWE_h_l$Distance) # -0.3539822

# Check the residuals for normality
shapiro.test(residuals(model_SWE_h))
# P-value = 0.9517
# residuals are fine

# Comparison of slopes ----------------------------------------------------

# Bind the data
mod_data <- rbind(SWE_d_l, SWE_h_l)

# Construct a new model with both introduction types
comp_mod <- lm(Distance~Year*type, data=mod_data)
summary(comp_mod)

# Construct a second model without the interaction term to test for a significant difference in the slope
comp_mod2 <- lm(Distance~Year+type, data=mod_data)
summary(comp_mod2)

# Compare the two models with an anova to assess if removing the interaction term significantly affects the fit of the model
anova(comp_mod, comp_mod2)
# P-value = 0.9707
# No significant effect

# Therefore, there is no significant difference between the slope for the database and the slope for the literature

# -------------------------------------------------------------------------
# ----------------------------- United Kingdom ----------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

#table(UK$Year)

#1970s
{ mg_1974 <- filter(UK, Year==1974)
mg_1976 <- filter(UK, Year==1976)
mg_1978 <- filter(UK, Year==1978)
mg_1979 <- filter(UK, Year==1979)
}
#1980s
{ mg_1984 <- filter(UK, Year==1984)
  mg_1985 <- filter(UK, Year==1985)
  mg_1988 <- filter(UK, Year==1988)
  mg_1989 <- filter(UK, Year==1989)
}
#1990s
{ mg_1991 <- filter(UK, Year==1991)
  mg_1992 <- filter(UK, Year==1992)
  mg_1993 <- filter(UK, Year==1993)
  mg_1994 <- filter(UK, Year==1994)
  mg_1996 <- filter(UK, Year==1996)
  mg_1997 <- filter(UK, Year==1997)
  mg_1998 <- filter(UK, Year==1998)
  mg_1999 <- filter(UK, Year==1999)
}
#2000s
{ mg_2000 <- filter(UK, Year==2000)
  mg_2001 <- filter(UK, Year==2001)
  mg_2002 <- filter(UK, Year==2002)
  mg_2003 <- filter(UK, Year==2003)
  mg_2004 <- filter(UK, Year==2004)
  mg_2005 <- filter(UK, Year==2005)
  mg_2006 <- filter(UK, Year==2006)
  mg_2007 <- filter(UK, Year==2007)
  mg_2008 <- filter(UK, Year==2008)
  mg_2009 <- filter(UK, Year==2009)
}
#2010s
{ mg_2010 <- filter(UK, Year==2010)
  mg_2011 <- filter(UK, Year==2011)
  mg_2012 <- filter(UK, Year==2012)
  mg_2013 <- filter(UK, Year==2013)
  mg_2014 <- filter(UK, Year==2014)
  mg_2015 <- filter(UK, Year==2015)
  mg_2016 <- filter(UK, Year==2016)
  mg_2017 <- filter(UK, Year==2017)
  mg_2018 <- filter(UK, Year==2018)
  mg_2019 <- filter(UK, Year==2019)
}
#2020s
{ mg_2020 <- filter(UK, Year==2020)
  mg_2021 <- filter(UK, Year==2021)
}

UK_first <- filter(mg_1, Country=="UK")

## Separate the lon and lat values -----------------------------------------

#1970s
{ mg_1974 <- mg_1974[, 8:7]
mg_1976 <- mg_1976[, 8:7]
mg_1978 <- mg_1978[, 8:7]
mg_1979 <- mg_1979[, 8:7]
}
#1980s
{ mg_1984 <-mg_1984[, 8:7]
  mg_1985 <-mg_1985[, 8:7]
  mg_1988 <-mg_1988[, 8:7]
  mg_1989 <-mg_1989[, 8:7]
}
#1990s
{ mg_1991 <- mg_1991[, 8:7]
  mg_1992 <- mg_1992[, 8:7]
  mg_1993 <- mg_1993[, 8:7]
  mg_1994 <- mg_1994[, 8:7]
  mg_1996 <- mg_1996[, 8:7]
  mg_1997 <- mg_1997[, 8:7]
  mg_1998 <- mg_1998[, 8:7]
  mg_1999 <- mg_1999[, 8:7]
}
#2000s
{ mg_2000 <- mg_2000[, 8:7]
  mg_2001 <- mg_2001[, 8:7]
  mg_2002 <- mg_2002[, 8:7]
  mg_2003 <- mg_2003[, 8:7]
  mg_2004 <- mg_2004[, 8:7]
  mg_2005 <- mg_2005[, 8:7]
  mg_2006 <- mg_2006[, 8:7]
  mg_2007 <- mg_2007[, 8:7]
  mg_2008 <- mg_2008[, 8:7]
  mg_2009 <- mg_2009[, 8:7]
}
#2010s
{ mg_2010 <- mg_2010[, 8:7]
  mg_2011 <- mg_2011[, 8:7]
  mg_2012 <- mg_2012[, 8:7]
  mg_2013 <- mg_2013[, 8:7]
  mg_2014 <- mg_2014[, 8:7]
  mg_2015 <- mg_2015[, 8:7]
  mg_2016 <- mg_2016[, 8:7]
  mg_2017 <- mg_2017[, 8:7]
  mg_2018 <- mg_2018[, 8:7]
  mg_2019 <- mg_2019[, 8:7]
}
#2020s
{ mg_2020 <- mg_2020[, 8:7]
  mg_2021 <- mg_2021[, 8:7]
}

UK_database <- UK_first[2, 3:4]
UK_historic <- UK_first[1, 3:4]

## Convert points to spdf ---------------------------------------------------

#1970s
{ mg_1974 <- SpatialPointsDataFrame(coords=mg_1974[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1974)
mg_1976 <- SpatialPointsDataFrame(coords=mg_1976[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1976)
mg_1978 <- SpatialPointsDataFrame(coords=mg_1978[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1978)
mg_1979 <- SpatialPointsDataFrame(coords=mg_1979[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1979)
}
#1980s
{ mg_1984 <- SpatialPointsDataFrame(coords=mg_1984[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1984)
  mg_1985 <- SpatialPointsDataFrame(coords=mg_1985[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1985)
  mg_1988 <- SpatialPointsDataFrame(coords=mg_1988[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1988)
  mg_1989 <- SpatialPointsDataFrame(coords=mg_1989[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1989)
}
#1990s
{ mg_1991 <- SpatialPointsDataFrame(coords=mg_1991[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1991)
  mg_1992 <- SpatialPointsDataFrame(coords=mg_1992[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1992)
  mg_1993 <- SpatialPointsDataFrame(coords=mg_1993[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1993)
  mg_1994 <- SpatialPointsDataFrame(coords=mg_1994[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1994)
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
{ UK_database <- SpatialPointsDataFrame(coords=UK_database[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=UK_database)
  UK_historic <- SpatialPointsDataFrame(coords=UK_historic[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=UK_historic)
}

## Move points off of land -------------------------------------------------

#1990s
{ mg_1998 <- points2nearestcell(mg_1998, bathy)
}
#2000s
{ mg_2005 <- points2nearestcell(mg_2005, bathy)
  mg_2007 <- points2nearestcell(mg_2007, bathy)
  mg_2009 <- points2nearestcell(mg_2009, bathy)
}
#2010s
{ mg_2011 <- points2nearestcell(mg_2011, bathy)
  mg_2013 <- points2nearestcell(mg_2013, bathy)
  mg_2014 <- points2nearestcell(mg_2014, bathy)
  mg_2015 <- points2nearestcell(mg_2015, bathy)
  mg_2016 <- points2nearestcell(mg_2016, bathy)
  mg_2018 <- points2nearestcell(mg_2018, bathy)
  mg_2019 <- points2nearestcell(mg_2019, bathy)
}

## Convert projection of points to UTM -------------------------------------

#1970s
{ mg_1974 <- spTransform(mg_1974, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_1976 <- spTransform(mg_1976, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_1978 <- spTransform(mg_1978, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_1979 <- spTransform(mg_1979, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1980s
{ mg_1984 <- spTransform(mg_1984, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1985 <- spTransform(mg_1985, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1988 <- spTransform(mg_1988, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1989 <- spTransform(mg_1989, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1990s
{ mg_1991 <- spTransform(mg_1991, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1992 <- spTransform(mg_1992, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1993 <- spTransform(mg_1993, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1994 <- spTransform(mg_1994, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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
{ UK_database <- spTransform(UK_database, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  UK_historic <- spTransform(UK_historic, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

## Calculate the shortest path to the database and historic introductions ----

#1970s  
{ mg_1974@data$database_dist <- seaway_dist(mg_1974, UK_database, trb) 
mg_1974@data$historic_dist <- seaway_dist(mg_1974, UK_historic, trb)
mg_1976@data$database_dist <- seaway_dist(mg_1976, UK_database, trb) 
mg_1976@data$historic_dist <- seaway_dist(mg_1976, UK_historic, trb)
mg_1978@data$database_dist <- seaway_dist(mg_1978, UK_database, trb) 
mg_1978@data$historic_dist <- seaway_dist(mg_1978, UK_historic, trb)
mg_1979@data$database_dist <- seaway_dist(mg_1979, UK_database, trb) 
mg_1979@data$historic_dist <- seaway_dist(mg_1979, UK_historic, trb)
}
#1980s
{ mg_1984@data$database_dist <- seaway_dist(mg_1984, UK_database, trb) 
  mg_1984@data$historic_dist <- seaway_dist(mg_1984, UK_historic, trb)
  mg_1985@data$database_dist <- seaway_dist(mg_1985, UK_database, trb) 
  mg_1985@data$historic_dist <- seaway_dist(mg_1985, UK_historic, trb)
  mg_1988@data$database_dist <- seaway_dist(mg_1988, UK_database, trb) 
  mg_1988@data$historic_dist <- seaway_dist(mg_1988, UK_historic, trb)
  mg_1989@data$database_dist <- seaway_dist(mg_1989, UK_database, trb) 
  mg_1989@data$historic_dist <- seaway_dist(mg_1989, UK_historic, trb)
}
#1990s
{ mg_1991@data$database_dist <- seaway_dist(mg_1991, UK_database, trb) 
  mg_1991@data$historic_dist <- seaway_dist(mg_1991, UK_historic, trb)
  mg_1992@data$database_dist <- seaway_dist(mg_1992, UK_database, trb) 
  mg_1992@data$historic_dist <- seaway_dist(mg_1992, UK_historic, trb)
  mg_1993@data$database_dist <- seaway_dist(mg_1993, UK_database, trb) 
  mg_1993@data$historic_dist <- seaway_dist(mg_1993, UK_historic, trb)
  mg_1994@data$database_dist <- seaway_dist(mg_1994, UK_database, trb) 
  mg_1994@data$historic_dist <- seaway_dist(mg_1994, UK_historic, trb)
  mg_1996@data$database_dist <- seaway_dist(mg_1996, UK_database, trb) 
  mg_1996@data$historic_dist <- seaway_dist(mg_1996, UK_historic, trb)
  mg_1997@data$database_dist <- seaway_dist(mg_1997, UK_database, trb) 
  mg_1997@data$historic_dist <- seaway_dist(mg_1997, UK_historic, trb)
  mg_1998@data$database_dist <- seaway_dist(mg_1998, UK_database, trb) 
  mg_1998@data$historic_dist <- seaway_dist(mg_1998, UK_historic, trb)
  mg_1999@data$database_dist <- seaway_dist(mg_1999, UK_database, trb) 
  mg_1999@data$historic_dist <- seaway_dist(mg_1999, UK_historic, trb)
}
#2000s
{ mg_2000@data$database_dist <- seaway_dist(mg_2000, UK_database, trb) 
  mg_2000@data$historic_dist <- seaway_dist(mg_2000, UK_historic, trb)
  mg_2001@data$database_dist <- seaway_dist(mg_2001, UK_database, trb) 
  mg_2001@data$historic_dist <- seaway_dist(mg_2001, UK_historic, trb)
  mg_2002@data$database_dist <- seaway_dist(mg_2002, UK_database, trb) 
  mg_2002@data$historic_dist <- seaway_dist(mg_2002, UK_historic, trb)
  mg_2003@data$database_dist <- seaway_dist(mg_2003, UK_database, trb) 
  mg_2003@data$historic_dist <- seaway_dist(mg_2003, UK_historic, trb)
  mg_2004@data$database_dist <- seaway_dist(mg_2004, UK_database, trb) 
  mg_2004@data$historic_dist <- seaway_dist(mg_2004, UK_historic, trb)
  mg_2005@data$database_dist <- seaway_dist(mg_2005, UK_database, trb) 
  mg_2005@data$historic_dist <- seaway_dist(mg_2005, UK_historic, trb)
  mg_2006@data$database_dist <- seaway_dist(mg_2006, UK_database, trb) 
  mg_2006@data$historic_dist <- seaway_dist(mg_2006, UK_historic, trb)
  mg_2007@data$database_dist <- seaway_dist(mg_2007, UK_database, trb) 
  mg_2007@data$historic_dist <- seaway_dist(mg_2007, UK_historic, trb)
  mg_2008@data$database_dist <- seaway_dist(mg_2008, UK_database, trb) 
  mg_2008@data$historic_dist <- seaway_dist(mg_2008, UK_historic, trb)
  mg_2009@data$database_dist <- seaway_dist(mg_2009, UK_database, trb) 
  mg_2009@data$historic_dist <- seaway_dist(mg_2009, UK_historic, trb)
}
#2010s
{ mg_2010@data$database_dist <- seaway_dist(mg_2010, UK_database, trb) 
  mg_2010@data$historic_dist <- seaway_dist(mg_2010, UK_historic, trb)
  mg_2011@data$database_dist <- seaway_dist(mg_2011, UK_database, trb) 
  mg_2011@data$historic_dist <- seaway_dist(mg_2011, UK_historic, trb)
  mg_2012@data$database_dist <- seaway_dist(mg_2012, UK_database, trb) 
  mg_2012@data$historic_dist <- seaway_dist(mg_2012, UK_historic, trb)
  mg_2013@data$database_dist <- seaway_dist(mg_2013, UK_database, trb) 
  mg_2013@data$historic_dist <- seaway_dist(mg_2013, UK_historic, trb)
  mg_2014@data$database_dist <- seaway_dist(mg_2014, UK_database, trb) 
  mg_2014@data$historic_dist <- seaway_dist(mg_2014, UK_historic, trb)
  mg_2015@data$database_dist <- seaway_dist(mg_2015, UK_database, trb) 
  mg_2015@data$historic_dist <- seaway_dist(mg_2015, UK_historic, trb)
  mg_2016@data$database_dist <- seaway_dist(mg_2016, UK_database, trb) 
  mg_2016@data$historic_dist <- seaway_dist(mg_2016, UK_historic, trb)
  mg_2017@data$database_dist <- seaway_dist(mg_2017, UK_database, trb) 
  mg_2017@data$historic_dist <- seaway_dist(mg_2017, UK_historic, trb)
  mg_2018@data$database_dist <- seaway_dist(mg_2018, UK_database, trb) 
  mg_2018@data$historic_dist <- seaway_dist(mg_2018, UK_historic, trb)
  mg_2019@data$database_dist <- seaway_dist(mg_2019, UK_database, trb) 
  mg_2019@data$historic_dist <- seaway_dist(mg_2019, UK_historic, trb)
}
#2020s
{ mg_2020@data$database_dist <- seaway_dist(mg_2020, UK_database, trb) 
  mg_2020@data$historic_dist <- seaway_dist(mg_2020, UK_historic, trb)
  mg_2021@data$database_dist <- seaway_dist(mg_2021, UK_database, trb) 
  mg_2021@data$historic_dist <- seaway_dist(mg_2021, UK_historic, trb)
}

## Convert distances to data frame -----------------------------------------

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
{ ddist <- mg_1984$database_dist
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
{ ddist <- mg_1991$database_dist
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

UK <- rbind(mg_1974, mg_1976, mg_1978, mg_1979, mg_1984, mg_1985, mg_1988, mg_1989, 
            mg_1991, mg_1992, mg_1993, mg_1994, mg_1996, mg_1997, mg_1998, 
            mg_1999, mg_2000, mg_2001, mg_2002, mg_2003, mg_2004, mg_2005, 
            mg_2006, mg_2007, mg_2008, mg_2009, mg_2010, mg_2011, mg_2012, 
            mg_2013, mg_2014, mg_2015, mg_2016, mg_2017, mg_2018, mg_2019, 
            mg_2020, mg_2021)

colnames(UK) <- c("Year", "Longitude", "Latitude", "database_dist", "historic_dist")

write.csv(UK, "./Data/Distance regression/Distance_regression_UK.csv", row.names=FALSE)

# Plot --------------------------------------------------------------------

UK <- read.csv("./Data/Distance regression/Distance_regression_UK.csv")

UK$Year <- as.integer(UK$Year)

UK_d <- UK[, c(1, 4)]
colnames(UK_d) <- c("Year", "Distance")
UK_d$type <- "database" 

UK_h <- UK[, c(1, 5)]
colnames(UK_h) <- c("Year", "Distance")
UK_h$type <- "historical" 

UK <- rbind(UK_d, UK_h)

UK_d_l <- UK_d %>% group_by(Year) %>% top_n(1, Distance)
UK_d_l <- UK_d_l[!duplicated(UK_d_l), ]
UK_d_l <- ungroup(UK_d_l)
UK_d_l <- UK_d_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 8 times
plot(UK_d_l$Distance~UK_d_l$Year, type="b")

UK_h_l <- UK_h %>% group_by(Year) %>% top_n(1, Distance)
UK_h_l <- UK_h_l[!duplicated(UK_h_l), ]
UK_h_l <- ungroup(UK_h_l)
UK_h_l <- UK_h_l %>% filter((Distance>(lag(Distance)))| row_number()==1) # repeat 6 times
plot(UK_h_l$Distance~UK_h_l$Year, type="b")

UK_plot <- ggplot(data=UK, aes(x=Year, y=Distance, group=1, colour=type, fill=type)) +
  geom_point(size=1.5, alpha=0.3)+
  scale_x_continuous(breaks=c(1974, 1983, 1992, 2001, 2010, 2019))+
  scale_y_continuous(limits=c(NA, 1400))+
  scale_colour_manual(values = c("#f1a340", "#998ec3"))+
  scale_fill_manual(values = c("#f1a340", "#998ec3"))+
  geom_smooth(data=UK_h_l, method="lm", se=FALSE, linetype="dotted")+
  geom_point(data=UK_h_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#998ec3")+
  geom_smooth(data=UK_d_l, method="lm", se=FALSE, linetype="dashed")+
  geom_point(data=UK_d_l, aes(x=Year, y=Distance), size=5, pch=21, fill=NA, colour="#f1a340")+
  ylab("Distance from initial observation (km)")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))
UK_plot

# Linear models -----------------------------------------------------------

model_UK_d <- lm(Distance~Year, UK_d_l)
summary(model_UK_d)
# RSE = 366.5 on 1 DoF 
# Multiple R-squared = 0.841
# Adjusted R-squared = 0.682
# F-stat = 5.289
# P-value = 0.261
# Rate of spread = 236.8 km/y +- 103

# Plot model to check
par(mfrow=c(2,2))
plot(model_UK_d) 
# not great
par(mfrow=c(1,1))

# Check normality
plot(density(UK_d_l$Distance)) # not bad
# Check skewness
skewness(UK_d_l$Distance) # -0.2980227

# Check the residuals for normality
shapiro.test(residuals(model_UK_d))
# P-value = 0.2196
# residuals are fine

# Doesn't work for the historical because there are only 2 points