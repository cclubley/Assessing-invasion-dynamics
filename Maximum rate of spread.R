# -----------------------------------------------------------------------------
# -------- Script for plotting the distance between the farthest points -------
# -------------------------- as a measure of range size -----------------------
# -----------------------------------------------------------------------------
# Set the working directory ------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Load the required libraries -------------------------------------------------
{ library(dplyr)
  library(sdmpredictors)
  library(rSDM)
  library(gdistance)
  library(SDraw)
  library(ggplot2)
  library(ccplot)
}

# Load the distribution data --------------------------------------------------

# Distribution data for northern and western Europe
mg <- read.csv("./Data/Data - 2022/Final spreadsheet/Total_data_Atlantic_April_2022_Final.csv")

# Separate points pole-ward (north) and equator-ward (south) from the 
# introduction point
mg_s <- filter(mg, Latitude<=49.59492) #1016
mg_n <- filter(mg, Latitude>=49.59492) #5678

# Also load the first record data for each country
database <- read.csv("./Data/First records/Initial_introductions.csv")
database$Year <- as.character(database$Year)
database <- filter(database, Type=="Database")

# Load and crop the raster layer ----------------------------------------------

# Load a bathymetry raster layer
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)
bathy <- bathy$MS_bathy_5m

# Crop the raster to the desired boundaries
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

# Create the function to calculate the distances --------------------------

# Calculates the seaway distance between the two points furthest from each other
# in any given year. Requires a spatial points data frame containing coordinates
# and a transition layer over which to calculate the distances as input
# arguments

max_seaway <- function(x, trb) {   # Requires a spdf of coordinates and a transition layer
  
  z <- data.frame(matrix(ncol=length(x), nrow=length(x))) # Creates an empty data frame the same size as the spdf
  
  y <- x # Duplictaes the spdf
  
  for (i in 1:nrow(y)) { # Repeat for the length of the duplicated data
    AtoB <- (shortestPath(trb, y, x, output="SpatialLines")) # Calculate the shortest path between the first two points, travelling only over the transition layer
    length <- lineLength(AtoB, byid=TRUE)/1000 # Calculate the distance of the shortest path in km
    z[i, ] <- length # Add the distance to the empty data frame
    y <- x[-1, ]} # Remove the first point from the duplicated data 
  
  d <- max(z) # Find the maximum distance and save the output
  d
  
}

# -----------------------------------------------------------------------------
# ----------------------------- Equator-ward spread ---------------------------
# -----------------------------------------------------------------------------
# Subset the data by year (cumulatively) --------------------------------------

#table(mg_s$Year)

#1960s
{ pts_1965 <- filter(mg_s, Year<=1965)
pts_1967 <- filter(mg_s, Year<=1967)
}
#1980s
{ pts_1980 <- filter(mg_s, Year<=1980)
  pts_1983 <- filter(mg_s, Year<=1983)
  pts_1984 <- filter(mg_s, Year<=1984)
  pts_1985 <- filter(mg_s, Year<=1985)
  pts_1987 <- filter(mg_s, Year<=1987)
  pts_1988 <- filter(mg_s, Year<=1988)
}
#1990s
{ pts_1990 <- filter(mg_s, Year<=1990)
  pts_1994 <- filter(mg_s, Year<=1994)
  pts_1995 <- filter(mg_s, Year<=1995)
  pts_1996 <- filter(mg_s, Year<=1996)
  pts_1997 <- filter(mg_s, Year<=1997)
  pts_1998 <- filter(mg_s, Year<=1998)
  pts_1999 <- filter(mg_s, Year<=1999)
}
#2000s
{ pts_2000 <- filter(mg_s, Year<=2000)
  pts_2001 <- filter(mg_s, Year<=2001)
  pts_2002 <- filter(mg_s, Year<=2002)
  pts_2003 <- filter(mg_s, Year<=2003)
  pts_2004 <- filter(mg_s, Year<=2004)
  pts_2005 <- filter(mg_s, Year<=2005)
  pts_2006 <- filter(mg_s, Year<=2006)
  pts_2007 <- filter(mg_s, Year<=2007)
  pts_2008 <- filter(mg_s, Year<=2008)
  pts_2009 <- filter(mg_s, Year<=2009)
}
#2010s
{ pts_2010 <- filter(mg_s, Year<=2010)
  pts_2011 <- filter(mg_s, Year<=2011)
  pts_2012 <- filter(mg_s, Year<=2012)
  pts_2013 <- filter(mg_s, Year<=2013)
  pts_2014 <- filter(mg_s, Year<=2014)
  pts_2015 <- filter(mg_s, Year<=2015)
  pts_2016 <- filter(mg_s, Year<=2016)
  pts_2017 <- filter(mg_s, Year<=2017)
  pts_2018 <- filter(mg_s, Year<=2018)
  pts_2019 <- filter(mg_s, Year<=2019)
}
#2020s
{ pts_2020 <- filter(mg_s, Year<=2020)
  pts_2021 <- filter(mg_s, Year<=2021)
}

# Convert points to spdf ------------------------------------------------------

#1960s
{ pts_1965 <- SpatialPointsDataFrame(coords=pts_1965[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1965)
pts_1967 <- SpatialPointsDataFrame(coords=pts_1967[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1967)
}
#1980s
{ pts_1980 <- SpatialPointsDataFrame(coords=pts_1980[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1980)
  pts_1983 <- SpatialPointsDataFrame(coords=pts_1983[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1983)
  pts_1984 <- SpatialPointsDataFrame(coords=pts_1984[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1984)
  pts_1985 <- SpatialPointsDataFrame(coords=pts_1985[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1985)
  pts_1987 <- SpatialPointsDataFrame(coords=pts_1987[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1987)
  pts_1988 <- SpatialPointsDataFrame(coords=pts_1988[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1988)
}
#1990s
{ pts_1990 <- SpatialPointsDataFrame(coords=pts_1990[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1990)
  pts_1994 <- SpatialPointsDataFrame(coords=pts_1994[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1994)
  pts_1995 <- SpatialPointsDataFrame(coords=pts_1995[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1995)
  pts_1996 <- SpatialPointsDataFrame(coords=pts_1996[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1996)
  pts_1997 <- SpatialPointsDataFrame(coords=pts_1997[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1997)
  pts_1998 <- SpatialPointsDataFrame(coords=pts_1998[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1998)
  pts_1999 <- SpatialPointsDataFrame(coords=pts_1999[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_1999)
}
#2000s
{ pts_2000 <- SpatialPointsDataFrame(coords=pts_2000[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2000)
  pts_2001 <- SpatialPointsDataFrame(coords=pts_2001[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2001)
  pts_2002 <- SpatialPointsDataFrame(coords=pts_2002[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2002)
  pts_2003 <- SpatialPointsDataFrame(coords=pts_2003[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2003)
  pts_2004 <- SpatialPointsDataFrame(coords=pts_2004[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2004)
  pts_2005 <- SpatialPointsDataFrame(coords=pts_2005[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2005)
  pts_2006 <- SpatialPointsDataFrame(coords=pts_2006[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2006)
  pts_2007 <- SpatialPointsDataFrame(coords=pts_2007[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2007)
  pts_2008 <- SpatialPointsDataFrame(coords=pts_2008[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2008)
  pts_2009 <- SpatialPointsDataFrame(coords=pts_2009[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2009)
}
#2010s
{ pts_2010 <- SpatialPointsDataFrame(coords=pts_2010[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2010)
  pts_2011 <- SpatialPointsDataFrame(coords=pts_2011[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2011)
  pts_2012 <- SpatialPointsDataFrame(coords=pts_2012[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2012)
  pts_2013 <- SpatialPointsDataFrame(coords=pts_2013[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2013)
  pts_2014 <- SpatialPointsDataFrame(coords=pts_2014[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2014)
  pts_2015 <- SpatialPointsDataFrame(coords=pts_2015[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2015)
  pts_2016 <- SpatialPointsDataFrame(coords=pts_2016[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2016)
  pts_2017 <- SpatialPointsDataFrame(coords=pts_2017[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2017)
  pts_2018 <- SpatialPointsDataFrame(coords=pts_2018[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2018)
  pts_2019 <- SpatialPointsDataFrame(coords=pts_2019[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2019)
}
#2020s
{ pts_2020 <- SpatialPointsDataFrame(coords=pts_2020[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2020)
  pts_2021 <- SpatialPointsDataFrame(coords=pts_2021[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=pts_2021)
}

# Move points off of land -----------------------------------------------------

# 1960s, 1970s, 1980s and 1990s are all within raster already
# The function 'points2nearestcell' moves points not overlapping the raster to 
# the nearest rater cell to the point
#2000s
{ pts_2006 <- points2nearestcell(pts_2006, bathy)
pts_2007 <- points2nearestcell(pts_2007, bathy)
pts_2008 <- points2nearestcell(pts_2008, bathy)
pts_2009 <- points2nearestcell(pts_2009, bathy)
}
#2010s
{ pts_2010 <- points2nearestcell(pts_2010, bathy)
  pts_2011 <- points2nearestcell(pts_2011, bathy)
  pts_2012 <- points2nearestcell(pts_2012, bathy)
  pts_2013 <- points2nearestcell(pts_2013, bathy)
  pts_2014 <- points2nearestcell(pts_2014, bathy)
  pts_2015 <- points2nearestcell(pts_2015, bathy)
  pts_2016 <- points2nearestcell(pts_2016, bathy)
  pts_2017 <- points2nearestcell(pts_2017, bathy)
  pts_2018 <- points2nearestcell(pts_2018, bathy)
  pts_2019 <- points2nearestcell(pts_2019, bathy)
}
#2020s
{ pts_2020 <- points2nearestcell(pts_2020, bathy)
  pts_2021 <- points2nearestcell(pts_2021, bathy)
}

# Convert projection of points to UTM -------------------------------------

# Converting the points to UTM means that measurements can be taken in
# metres rather than decimal degrees

#1960s
{ pts_1965 <- spTransform(pts_1965, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
pts_1967 <- spTransform(pts_1967, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1980s
{ pts_1980 <- spTransform(pts_1980, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_1983 <- spTransform(pts_1983, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_1984 <- spTransform(pts_1984, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_1985 <- spTransform(pts_1985, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_1987 <- spTransform(pts_1987, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_1988 <- spTransform(pts_1988, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1990s
{ pts_1990 <- spTransform(pts_1990, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_1994 <- spTransform(pts_1994, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_1995 <- spTransform(pts_1995, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_1996 <- spTransform(pts_1996, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_1997 <- spTransform(pts_1997, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_1998 <- spTransform(pts_1998, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_1999 <- spTransform(pts_1999, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#2000s
{ pts_2000 <- spTransform(pts_2000, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2001 <- spTransform(pts_2001, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2002 <- spTransform(pts_2002, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2003 <- spTransform(pts_2003, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2004 <- spTransform(pts_2004, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2005 <- spTransform(pts_2005, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2006 <- spTransform(pts_2006, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2007 <- spTransform(pts_2007, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2008 <- spTransform(pts_2008, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2009 <- spTransform(pts_2009, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#2010s
{ pts_2010 <- spTransform(pts_2010, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2011 <- spTransform(pts_2011, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2012 <- spTransform(pts_2012, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2013 <- spTransform(pts_2013, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2014 <- spTransform(pts_2014, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2015 <- spTransform(pts_2015, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2016 <- spTransform(pts_2016, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2017 <- spTransform(pts_2017, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2018 <- spTransform(pts_2018, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2019 <- spTransform(pts_2019, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#2020s
{ pts_2020 <- spTransform(pts_2020, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  pts_2021 <- spTransform(pts_2021, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}

# Convert projection of raster and make land impassable -----------------------

# Convert projection of raster to UTM
bathy <- projectRaster(bathy, crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs" )

# Make land impassable
# Make all ocean cells equal to -999
bathy[!is.na(bathy)] <- -999
# Turn all land cells to NA
bathy[bathy>-999] <- NA
# Assign all ocean cells a value of 1
bathy[bathy==-999] <- 1

# Create a transition layer matrix using the raster ---------------------------

# Checks whether cells are not NA (as designated in previous code) and then 
# creates a matrix listing all feasible connections between cells (here we 
# allow for 16 directions of movement from any given cell)
trb <- transition(bathy, mean, directions = 16) 
# Corrects for fact that diagonal movement is longer than straight lines - 
# important in getting accurate distance measurements
trb <- geoCorrection(trb, "c") 

# Set up the empty data frame -----------------------------------------------

Max_distances_south <- as.data.frame(1965:2021) 
colnames(Max_distances_south) <- "Year"
Max_distances_south$Distance <- NA

# Calculate the distances ---------------------------------------------------

#table(mg_s$Year)

#1960s & 1970s
{ Max_distances_south[1,2] <- max_seaway(pts_1965, trb)
Max_distances_south[2,2] <- Max_distances_south[1,2]
Max_distances_south[3,2] <- max_seaway(pts_1967, trb)
Max_distances_south[4,2] <- Max_distances_south[3,2]
Max_distances_south[5,2] <- Max_distances_south[3,2]
Max_distances_south[6,2] <- Max_distances_south[3,2]
Max_distances_south[7,2] <- Max_distances_south[3,2]
Max_distances_south[8,2] <- Max_distances_south[3,2]
Max_distances_south[9,2] <- Max_distances_south[3,2]
Max_distances_south[10,2] <- Max_distances_south[3,2]
Max_distances_south[11,2] <- Max_distances_south[3,2]
Max_distances_south[12,2] <- Max_distances_south[3,2]
Max_distances_south[13,2] <- Max_distances_south[3,2]
Max_distances_south[14,2] <- Max_distances_south[3,2]
Max_distances_south[15,2] <- Max_distances_south[3,2]
}
#1980s
{  Max_distances_south[16,2] <- max_seaway(pts_1980, trb)
  Max_distances_south[17,2] <- Max_distances_south[16,2]
  Max_distances_south[18,2] <- Max_distances_south[16,2]
  Max_distances_south[19,2] <- max_seaway(pts_1983, trb)
  Max_distances_south[20,2] <- max_seaway(pts_1984, trb)
  Max_distances_south[21,2] <- max_seaway(pts_1985, trb)
  Max_distances_south[22,2] <- Max_distances_south[21,2]
  Max_distances_south[23,2] <- max_seaway(pts_1987, trb)
  Max_distances_south[24,2] <- max_seaway(pts_1988, trb)
  Max_distances_south[25,2] <- Max_distances_south[24,2]
}
#1990s
{ Max_distances_south[26,2] <- max_seaway(pts_1990, trb)
  Max_distances_south[27,2] <- Max_distances_south[26,2]
  Max_distances_south[28,2] <- Max_distances_south[26,2]
  Max_distances_south[29,2] <- Max_distances_south[26,2]
  Max_distances_south[30,2] <- max_seaway(pts_1994, trb)
  Max_distances_south[31,2] <- max_seaway(pts_1995, trb)
  Max_distances_south[32,2] <- max_seaway(pts_1996, trb)
  Max_distances_south[33,2] <- max_seaway(pts_1997, trb)
  Max_distances_south[34,2] <- max_seaway(pts_1998, trb)
  Max_distances_south[35,2] <- max_seaway(pts_1999, trb)
}
#2000s
{ Max_distances_south[36,2] <- max_seaway(pts_2000, trb)
  Max_distances_south[37,2] <- max_seaway(pts_2001, trb)
  Max_distances_south[38,2] <- max_seaway(pts_2002, trb)
  Max_distances_south[39,2] <- max_seaway(pts_2003, trb)
  Max_distances_south[40,2] <- max_seaway(pts_2004, trb)
  Max_distances_south[41,2] <- max_seaway(pts_2005, trb)
  Max_distances_south[42,2] <- max_seaway(pts_2006, trb)
  Max_distances_south[43,2] <- max_seaway(pts_2007, trb)
  Max_distances_south[44,2] <- max_seaway(pts_2008, trb)
  Max_distances_south[45,2] <- max_seaway(pts_2009, trb)
}
#2010s
{ Max_distances_south[46,2] <- max_seaway(pts_2010, trb)
  Max_distances_south[47,2] <- max_seaway(pts_2011, trb)
  Max_distances_south[48,2] <- max_seaway(pts_2012, trb)
  Max_distances_south[49,2] <- max_seaway(pts_2013, trb)
  Max_distances_south[50,2] <- max_seaway(pts_2014, trb)
  Max_distances_south[51,2] <- max_seaway(pts_2015, trb)
  Max_distances_south[52,2] <- max_seaway(pts_2016, trb)
  Max_distances_south[53,2] <- max_seaway(pts_2017, trb)
  Max_distances_south[54,2] <- max_seaway(pts_2018, trb)
  Max_distances_south[55,2] <- max_seaway(pts_2019, trb)
}
#2020s
{ Max_distances_south[56,2] <- max_seaway(pts_2020, trb)
  Max_distances_south[57,2] <- max_seaway(pts_2021, trb)
}

# Save the .csv file ------------------------------------------------------

write.csv(Max_distances_south, "./Data/Farthest points/Max_distances_south.csv", row.names=FALSE)  

# -----------------------------------------------------------------------------
# ------------------------------ Pole-ward spread -----------------------------
# -----------------------------------------------------------------------------
# Subset the data by year (cumulatively) -----------------------------------

# REMEMBER - re-load the 'bathy' raster layer before starting
#table(mg_n$Year)

#1960s
{ mg_1965 <- filter(mg_n, Year<=1965)
}
#1970s
{ mg_1974 <- filter(mg_n, Year<=1974)
  mg_1976 <- filter(mg_n, Year<=1976)
  mg_1978 <- filter(mg_n, Year<=1978)
  mg_1979 <- filter(mg_n, Year<=1979)
}
#1980s
{ mg_1982 <- filter(mg_n, Year<=1982)
  mg_1984 <- filter(mg_n, Year<=1984)
  mg_1985 <- filter(mg_n, Year<=1985)
  mg_1986 <- filter(mg_n, Year<=1986)
  mg_1988 <- filter(mg_n, Year<=1988)
  mg_1989 <- filter(mg_n, Year<=1989)
}
#1990s
{ mg_1991 <- filter(mg_n, Year<=1991)
  mg_1992 <- filter(mg_n, Year<=1992)
  mg_1993 <- filter(mg_n, Year<=1993)
  mg_1994 <- filter(mg_n, Year<=1994)
  mg_1995 <- filter(mg_n, Year<=1995)
  mg_1996 <- filter(mg_n, Year<=1996)
  mg_1997 <- filter(mg_n, Year<=1997)
  mg_1998 <- filter(mg_n, Year<=1998)
  mg_1999 <- filter(mg_n, Year<=1999)
}
#2000s
{ mg_2000 <- filter(mg_n, Year<=2000)
  mg_2001 <- filter(mg_n, Year<=2001)
  mg_2002 <- filter(mg_n, Year<=2002)
  mg_2003 <- filter(mg_n, Year<=2003)
  mg_2004 <- filter(mg_n, Year<=2004)
  mg_2005 <- filter(mg_n, Year<=2005)
  mg_2006 <- filter(mg_n, Year<=2006)
  mg_2007 <- filter(mg_n, Year<=2007)
  mg_2008 <- filter(mg_n, Year<=2008)
  mg_2009 <- filter(mg_n, Year<=2009)
}
#2010s
{ mg_2010 <- filter(mg_n, Year<=2010)
  mg_2011 <- filter(mg_n, Year<=2011)
  mg_2012 <- filter(mg_n, Year<=2012)
  mg_2013 <- filter(mg_n, Year<=2013)
  mg_2014 <- filter(mg_n, Year<=2014)
  mg_2015 <- filter(mg_n, Year<=2015)
  mg_2016 <- filter(mg_n, Year<=2016)
  mg_2017 <- filter(mg_n, Year<=2017)
  mg_2018 <- filter(mg_n, Year<=2018)
  mg_2019 <- filter(mg_n, Year<=2019)
}
#2020s
{ mg_2020 <- filter(mg_n, Year<=2020)
  mg_2021 <- filter(mg_n, Year<=2021)
}

# Convert points to spdf ---------------------------------------------------

#1960s
{ mg_1965 <- SpatialPointsDataFrame(coords=mg_1965[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1965)
}
#1970s
{ mg_1974 <- SpatialPointsDataFrame(coords=mg_1974[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1974)
  mg_1976 <- SpatialPointsDataFrame(coords=mg_1976[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1976)
  mg_1978 <- SpatialPointsDataFrame(coords=mg_1978[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1978)
  mg_1979 <- SpatialPointsDataFrame(coords=mg_1979[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1979)
}
#1980s
{ mg_1982 <- SpatialPointsDataFrame(coords=mg_1982[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1982)
  mg_1984 <- SpatialPointsDataFrame(coords=mg_1984[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1984)
  mg_1985 <- SpatialPointsDataFrame(coords=mg_1985[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1985)
  mg_1986 <- SpatialPointsDataFrame(coords=mg_1986[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1986)
  mg_1988 <- SpatialPointsDataFrame(coords=mg_1988[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1988)
  mg_1989 <- SpatialPointsDataFrame(coords=mg_1989[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1989)
}
#1990s
{ mg_1991 <- SpatialPointsDataFrame(coords=mg_1991[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1991)
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

# Move points off of land -------------------------------------------------

# 1960s, 1970s and 1980s are all within raster already
# 1990s
{ mg_1995 <- points2nearestcell(mg_1995, bathy)
mg_1996 <- points2nearestcell(mg_1996, bathy)
mg_1997 <- points2nearestcell(mg_1997, bathy)
mg_1998 <- points2nearestcell(mg_1998, bathy)
mg_1999 <- points2nearestcell(mg_1999, bathy)
}
#2000s
{ mg_2000 <- points2nearestcell(mg_2000, bathy)
  mg_2001 <- points2nearestcell(mg_2001, bathy)
  mg_2002 <- points2nearestcell(mg_2002, bathy)
  mg_2003 <- points2nearestcell(mg_2003, bathy)
  mg_2004 <- points2nearestcell(mg_2004, bathy)
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
}
#1970s
{ mg_1974 <- spTransform(mg_1974, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1976 <- spTransform(mg_1976, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1978 <- spTransform(mg_1978, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1979 <- spTransform(mg_1979, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1980s
{ mg_1982 <- spTransform(mg_1982, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1984 <- spTransform(mg_1984, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1985 <- spTransform(mg_1985, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1986 <- spTransform(mg_1986, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1988 <- spTransform(mg_1988, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1989 <- spTransform(mg_1989, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1990s
{ mg_1991 <- spTransform(mg_1991, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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

# Convert projection of raster and make land impassable -------------------

# Convert projection of raster to UTM
bathy <- projectRaster(bathy, crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs" )

# Make land impassable
# Make all ocean cells equal to -999
bathy[!is.na(bathy)] <- -999
# Turn all land cells to NA
bathy[bathy>-999] <- NA
# Assign all ocean cells a value of 1
bathy[bathy==-999] <- 1

# Create a transition layer matrix using the raster -----------------------

# Checks whether cells are not NA and then creates a matrix listing all feasible connections between cells
trb <- transition(bathy, mean, directions = 16) 
# Corrects for fact that diagonal movement is longer than straight lines
trb <- geoCorrection(trb, "c") 

rm(bathy)

# Set up the empty data frame -----------------------------------------------

Max_distances_north <- as.data.frame(1965:2021) 
colnames(Max_distances_north) <- "Year"
Max_distances_north$Distance <- NA

# Calculate the distances ---------------------------------------------------

#table(mg_n$Year)

#1960s
{ Max_distances_north[1,2] <- max_seaway(mg_1965, trb)
Max_distances_north[2,2] <- Max_distances_north[1,2]
Max_distances_north[3,2] <- Max_distances_north[1,2]
Max_distances_north[4,2] <- Max_distances_north[1,2]
Max_distances_north[5,2] <- Max_distances_north[1,2]
Max_distances_north[6,2] <- Max_distances_north[1,2]
Max_distances_north[7,2] <- Max_distances_north[1,2]
Max_distances_north[8,2] <- Max_distances_north[1,2]
Max_distances_north[9,2] <- Max_distances_north[1,2]
}
#1970s
{ Max_distances_north[10,2] <- max_seaway(mg_1974, trb)
  Max_distances_north[11,2] <- Max_distances_north[10,2]
  Max_distances_north[12,2] <- max_seaway(mg_1976, trb)
  Max_distances_north[13,2] <- Max_distances_north[12,2]
  Max_distances_north[14,2] <- max_seaway(mg_1978, trb)
  Max_distances_north[15,2] <- max_seaway(mg_1979, trb)
  Max_distances_north[16,2] <- Max_distances_north[15,2]
  Max_distances_north[17,2] <- Max_distances_north[15,2]
}
#1980s
{ Max_distances_north[18,2] <- max_seaway(mg_1982, trb)
  Max_distances_north[19,2] <- Max_distances_north[18,2]
  Max_distances_north[20,2] <- max_seaway(mg_1984, trb)
  Max_distances_north[21,2] <- max_seaway(mg_1985, trb)
  Max_distances_north[22,2] <- max_seaway(mg_1986, trb)
  Max_distances_north[23,2] <- Max_distances_north[22,2]
  Max_distances_north[24,2] <- max_seaway(mg_1988, trb)
  Max_distances_north[25,2] <- max_seaway(mg_1989, trb)
  Max_distances_north[26,2] <- Max_distances_north[25,2]
}
#1990s
{ Max_distances_north[27,2] <- max_seaway(mg_1991, trb)  
  Max_distances_north[28,2] <- max_seaway(mg_1992, trb)  
  Max_distances_north[29,2] <- max_seaway(mg_1993, trb)  
  Max_distances_north[30,2] <- max_seaway(mg_1994, trb)  
  Max_distances_north[31,2] <- max_seaway(mg_1995, trb)  
  Max_distances_north[32,2] <- max_seaway(mg_1996, trb)  
  Max_distances_north[33,2] <- max_seaway(mg_1997, trb)  
  Max_distances_north[34,2] <- max_seaway(mg_1998, trb)  
  Max_distances_north[35,2] <- max_seaway(mg_1999, trb)  
}
#2000s
{ Max_distances_north[36,2] <- max_seaway(mg_2000, trb)
  Max_distances_north[37,2] <- max_seaway(mg_2001, trb)
  Max_distances_north[38,2] <- max_seaway(mg_2002, trb)
  Max_distances_north[39,2] <- max_seaway(mg_2003, trb)
  Max_distances_north[40,2] <- max_seaway(mg_2004, trb)
  Max_distances_north[41,2] <- max_seaway(mg_2005, trb)
  Max_distances_north[42,2] <- max_seaway(mg_2006, trb)
  Max_distances_north[43,2] <- max_seaway(mg_2007, trb)
  Max_distances_north[44,2] <- max_seaway(mg_2008, trb)
  Max_distances_north[45,2] <- max_seaway(mg_2009, trb)
}
#2010s
{ Max_distances_north[46,2] <- max_seaway(mg_2010, trb)
  Max_distances_north[47,2] <- max_seaway(mg_2011, trb)
  Max_distances_north[48,2] <- max_seaway(mg_2012, trb)
  Max_distances_north[49,2] <- max_seaway(mg_2013, trb)
  Max_distances_north[50,2] <- max_seaway(mg_2014, trb)
  Max_distances_north[51,2] <- max_seaway(mg_2015, trb)
  Max_distances_north[52,2] <- max_seaway(mg_2016, trb)
  Max_distances_north[53,2] <- max_seaway(mg_2017, trb)
  Max_distances_north[54,2] <- max_seaway(mg_2018, trb)
  Max_distances_north[55,2] <- max_seaway(mg_2019, trb)
}
#2020s
{ Max_distances_north[56,2] <- max_seaway(mg_2020, trb)
  Max_distances_north[57,2] <- max_seaway(mg_2021, trb)
}

# Save the .csv file ------------------------------------------------------

write.csv(Max_distances_north, "./Data/Farthest points/Max_distances_north.csv", row.names=FALSE)  

# -----------------------------------------------------------------------------
# --------------------------- Omni-directional spread -------------------------
# -----------------------------------------------------------------------------
# Subset the data by year (cumulatively) -----------------------------------

# REMEMBER - re-load the 'bathy' raster layer beforeyou start
#table(mg$Year)

#1960s
{ mg_1965 <- filter(mg, Year<=1965)
mg_1966 <- filter(mg, Year<=1966)
mg_1967 <- filter(mg, Year<=1967)
mg_1968 <- filter(mg, Year<=1968)
mg_1969 <- filter(mg, Year<=1969)
}
#1970s
{ mg_1970 <- filter(mg, Year<=1970)
  mg_1971 <- filter(mg, Year<=1971)
  mg_1972 <- filter(mg, Year<=1972)
  mg_1973 <- filter(mg, Year<=1973)
  mg_1974 <- filter(mg, Year<=1974)
  mg_1975 <- filter(mg, Year<=1975)
  mg_1976 <- filter(mg, Year<=1976)
  mg_1977 <- filter(mg, Year<=1977)
  mg_1978 <- filter(mg, Year<=1978)
  mg_1979 <- filter(mg, Year<=1979)
}
#1980s
{ mg_1980 <- filter(mg, Year<=1980)
  mg_1981 <- filter(mg, Year<=1981)
  mg_1982 <- filter(mg, Year<=1982)
  mg_1983 <- filter(mg, Year<=1983)
  mg_1984 <- filter(mg, Year<=1984)
  mg_1985 <- filter(mg, Year<=1985)
  mg_1986 <- filter(mg, Year<=1986)
  mg_1987 <- filter(mg, Year<=1987)
  mg_1988 <- filter(mg, Year<=1988)
  mg_1989 <- filter(mg, Year<=1989)
}
#1990s
{ mg_1990 <- filter(mg, Year<=1990)
  mg_1991 <- filter(mg, Year<=1991)
  mg_1992 <- filter(mg, Year<=1992)
  mg_1993 <- filter(mg, Year<=1993)
  mg_1994 <- filter(mg, Year<=1994)
  mg_1995 <- filter(mg, Year<=1995)
  mg_1996 <- filter(mg, Year<=1996)
  mg_1997 <- filter(mg, Year<=1997)
  mg_1998 <- filter(mg, Year<=1998)
  mg_1999 <- filter(mg, Year<=1999)
}
#2000s
{ mg_2000 <- filter(mg, Year<=2000)
  mg_2001 <- filter(mg, Year<=2001)
  mg_2002 <- filter(mg, Year<=2002)
  mg_2003 <- filter(mg, Year<=2003)
  mg_2004 <- filter(mg, Year<=2004)
  mg_2005 <- filter(mg, Year<=2005)
  mg_2006 <- filter(mg, Year<=2006)
  mg_2007 <- filter(mg, Year<=2007)
  mg_2008 <- filter(mg, Year<=2008)
  mg_2009 <- filter(mg, Year<=2009)
}
#2010s
{ mg_2010 <- filter(mg, Year<=2010)
  mg_2011 <- filter(mg, Year<=2011)
  mg_2012 <- filter(mg, Year<=2012)
  mg_2013 <- filter(mg, Year<=2013)
  mg_2014 <- filter(mg, Year<=2014)
  mg_2015 <- filter(mg, Year<=2015)
  mg_2016 <- filter(mg, Year<=2016)
  mg_2017 <- filter(mg, Year<=2017)
  mg_2018 <- filter(mg, Year<=2018)
  mg_2019 <- filter(mg, Year<=2019)
}
#2020s
{ mg_2020 <- filter(mg, Year<=2020)
  mg_2021 <- filter(mg, Year<=2021)
}

# Convert points to spdf ---------------------------------------------------

#1960s
{ mg_1965 <- SpatialPointsDataFrame(coords=mg_1965[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1965)
mg_1966 <- SpatialPointsDataFrame(coords=mg_1966[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1966)
mg_1967 <- SpatialPointsDataFrame(coords=mg_1967[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1967)
mg_1968 <- SpatialPointsDataFrame(coords=mg_1968[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1968)
mg_1969 <- SpatialPointsDataFrame(coords=mg_1969[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1969)
}
#1970s
{ mg_1970 <- SpatialPointsDataFrame(coords=mg_1970[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1970)
  mg_1971 <- SpatialPointsDataFrame(coords=mg_1971[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1971)
  mg_1972 <- SpatialPointsDataFrame(coords=mg_1972[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1972)
  mg_1973 <- SpatialPointsDataFrame(coords=mg_1973[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1973)
  mg_1974 <- SpatialPointsDataFrame(coords=mg_1974[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1974)
  mg_1975 <- SpatialPointsDataFrame(coords=mg_1975[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1975)
  mg_1976 <- SpatialPointsDataFrame(coords=mg_1976[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1976)
  mg_1977 <- SpatialPointsDataFrame(coords=mg_1977[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1977)
  mg_1978 <- SpatialPointsDataFrame(coords=mg_1978[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1978)
  mg_1979 <- SpatialPointsDataFrame(coords=mg_1979[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1979)
}
#1980s
{ mg_1980 <- SpatialPointsDataFrame(coords=mg_1980[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1980)
  mg_1981 <- SpatialPointsDataFrame(coords=mg_1981[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1981)
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

# Move points off of land -------------------------------------------------

# 1960s, 1970s and 1980s are all within raster already
#1990s
{ mg_1995 <- points2nearestcell(mg_1995, bathy)
mg_1996 <- points2nearestcell(mg_1996, bathy)
mg_1997 <- points2nearestcell(mg_1997, bathy)
mg_1998 <- points2nearestcell(mg_1998, bathy)
mg_1999 <- points2nearestcell(mg_1999, bathy)
}
#2000s
{ mg_2000 <- points2nearestcell(mg_2000, bathy)
  mg_2001 <- points2nearestcell(mg_2001, bathy)
  mg_2002 <- points2nearestcell(mg_2002, bathy)
  mg_2003 <- points2nearestcell(mg_2003, bathy)
  mg_2004 <- points2nearestcell(mg_2004, bathy)
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
mg_1966 <- spTransform(mg_1966, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_1967 <- spTransform(mg_1967, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_1968 <- spTransform(mg_1968, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
mg_1969 <- spTransform(mg_1969, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1970s
{ mg_1970 <- spTransform(mg_1970, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1971 <- spTransform(mg_1971, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1972 <- spTransform(mg_1972, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1973 <- spTransform(mg_1973, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1974 <- spTransform(mg_1974, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1975 <- spTransform(mg_1975, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1976 <- spTransform(mg_1976, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1977 <- spTransform(mg_1977, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1978 <- spTransform(mg_1978, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1979 <- spTransform(mg_1979, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
#1980s
{ mg_1980 <- spTransform(mg_1980, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
  mg_1981 <- spTransform(mg_1981, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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

# Convert projection of raster and make land impassable -------------------

# Convert projection of raster to UTM
bathy <- projectRaster(bathy, crs="+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs" )

# Make land impassable
# Make all ocean cells equal to -999
bathy[!is.na(bathy)] <- -999
# Turn all land cells to NA
bathy[bathy>-999] <- NA
# Assign all ocean cells a value of 1
bathy[bathy==-999] <- 1

# Create a transition layer matrix using the raster -----------------------

# Checks whether cells are not NA and then creates a matrix listing all feasible connections between cells
trb <- transition(bathy, mean, directions = 16) 
# Corrects for fact that diagonal movement is longer than straight lines
trb <- geoCorrection(trb, "c") 

rm(bathy)

# Set up the empty data frame -----------------------------------------------

Max_distances_omni <- as.data.frame(1965:2021) 
colnames(Max_distances_omni) <- "Year"
Max_distances_omni$Distance <- NA

# Calculate the distances ---------------------------------------------------

#table(mg$Year)

#1960s
{ Max_distances_omni[1,2] <- max_seaway(mg_1965, trb)
Max_distances_omni[2,2] <- Max_distances_north[1,2]
Max_distances_omni[3,2] <- max_seaway(mg_1967, trb)
Max_distances_omni[4,2] <- Max_distances_north[3,2]
Max_distances_omni[5,2] <- Max_distances_north[3,2]
Max_distances_omni[6,2] <- Max_distances_north[3,2]
Max_distances_omni[7,2] <- Max_distances_north[3,2]
Max_distances_omni[8,2] <- Max_distances_north[3,2]
Max_distances_omni[9,2] <- Max_distances_north[3,2]
}
#1970s
{ Max_distances_omni[10,2] <- max_seaway(mg_1974, trb)
  Max_distances_omni[11,2] <- Max_distances_north[10,2]
  Max_distances_omni[12,2] <- max_seaway(mg_1976, trb)
  Max_distances_omni[13,2] <- Max_distances_north[12,2]
  Max_distances_omni[14,2] <- max_seaway(mg_1978, trb)
  Max_distances_omni[15,2] <- max_seaway(mg_1979, trb)
}
#1980s
{ Max_distances_omni[16,2] <- max_seaway(mg_1980, trb)
  Max_distances_omni[17,2] <- Max_distances_north[16,2]
  Max_distances_omni[18,2] <- max_seaway(mg_1982, trb)
  Max_distances_omni[19,2] <- max_seaway(mg_1983, trb)
  Max_distances_omni[20,2] <- max_seaway(mg_1984, trb)
  Max_distances_omni[21,2] <- max_seaway(mg_1985, trb)
  Max_distances_omni[22,2] <- max_seaway(mg_1986, trb)
  Max_distances_omni[23,2] <- max_seaway(mg_1987, trb)
  Max_distances_omni[24,2] <- max_seaway(mg_1988, trb)
  Max_distances_omni[25,2] <- max_seaway(mg_1989, trb)
}
#1990s
{ Max_distances_omni[26,2] <- max_seaway(mg_1990, trb)
  Max_distances_omni[27,2] <- max_seaway(mg_1991, trb)
  Max_distances_omni[28,2] <- max_seaway(mg_1992, trb)
  Max_distances_omni[29,2] <- max_seaway(mg_1993, trb)
  Max_distances_omni[30,2] <- max_seaway(mg_1994, trb)
  Max_distances_omni[31,2] <- max_seaway(mg_1995, trb)
  Max_distances_omni[32,2] <- max_seaway(mg_1996, trb)
  Max_distances_omni[33,2] <- max_seaway(mg_1997, trb)
  Max_distances_omni[34,2] <- max_seaway(mg_1998, trb)
  Max_distances_omni[35,2] <- max_seaway(mg_1999, trb)
}
#2000s
{ Max_distances_omni[36,2] <- max_seaway(mg_2000, trb)
  Max_distances_omni[37,2] <- max_seaway(mg_2001, trb)
  Max_distances_omni[38,2] <- max_seaway(mg_2002, trb)
  Max_distances_omni[39,2] <- max_seaway(mg_2003, trb)
  Max_distances_omni[40,2] <- max_seaway(mg_2004, trb)
  Max_distances_omni[41,2] <- max_seaway(mg_2005, trb)
  Max_distances_omni[42,2] <- max_seaway(mg_2006, trb)
  Max_distances_omni[43,2] <- max_seaway(mg_2007, trb)
  Max_distances_omni[44,2] <- max_seaway(mg_2008, trb)
  Max_distances_omni[45,2] <- max_seaway(mg_2009, trb)
}
#2010s
{ Max_distances_omni[46,2] <- max_seaway(mg_2010, trb)
  Max_distances_omni[47,2] <- max_seaway(mg_2011, trb)
  Max_distances_omni[48,2] <- max_seaway(mg_2012, trb)
  Max_distances_omni[49,2] <- max_seaway(mg_2013, trb)
  Max_distances_omni[50,2] <- max_seaway(mg_2014, trb)
  Max_distances_omni[51,2] <- max_seaway(mg_2015, trb)
  Max_distances_omni[52,2] <- max_seaway(mg_2016, trb)
  Max_distances_omni[53,2] <- max_seaway(mg_2017, trb)
  Max_distances_omni[54,2] <- max_seaway(mg_2018, trb)
  Max_distances_omni[55,2] <- max_seaway(mg_2019, trb)
}
#2020s
{ Max_distances_omni[56,2] <- max_seaway(mg_2020, trb)
  Max_distances_omni[57,2] <- max_seaway(mg_2021, trb)
}

# Save the .csv file ------------------------------------------------------

write.csv(Max_distances_omni, "./Data/Farthest points/Max_distances_omni.csv", row.names=FALSE)  

# -----------------------------------------------------------------------------
# ----------------------------   Combined plot --------------------------------
# -----------------------------------------------------------------------------
# Combined plot -----------------------------------------------------------

# Load all of the previously calculated distances
Max_distances_omni <- read.csv("./Data/Farthest points/Max_distances_omni.csv")
Max_distances_omni$Year <- as.character(Max_distances_omni$Year)
Max_distances_north <- read.csv("./Data/Farthest points/Max_distances_north.csv")
Max_distances_north$Year <- as.character(Max_distances_north$Year)
Max_distances_south <- read.csv("./Data/Farthest points/Max_distances_south.csv")
Max_distances_south$Year <- as.character(Max_distances_south$Year)

# Assign each data frame a column with the direction of spread
Max_distances_omni$Direction <- "Omnidirectional"
Max_distances_north$Direction <- "Poleward"
Max_distances_south$Direction <- "Equatorward"

# Remove unnecessary column
#Max_distances_omni$Increase <- NULL

Max_distances <- rbind(Max_distances_omni, Max_distances_north, Max_distances_south)

ggplot(data=Max_distances, aes(x=Year, y=Distance, group=Direction, colour=Direction))+
  geom_line(size=1.5)+
  scale_colour_manual(values=c("aquamarine3", "purple", "tan1"))+
  scale_x_discrete(breaks=c("1965","1970","1975","1980","1985","1990","1995",
                            "2000","2005","2010","2015","2020"),
                   labels=c("1965", "1970","1975","1980","1985","1990","1995",
                            "2000","2005","2010","2015","2020"))+
  ylab("Distance between the most distant records (km)")+
  cc_theme()+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16), legend.position=c(0.1, 0.9))+
  geom_rug(data=database, aes(x=year), inherit.aes=F, colour="purple", size=1)
