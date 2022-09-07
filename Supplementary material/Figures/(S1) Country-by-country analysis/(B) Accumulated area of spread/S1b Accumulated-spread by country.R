# Set the working directory ------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Load the required libraries ---------------------------------------------
{library(rgdal)
library(sdmpredictors)
library(raster)
library(dplyr)
library(rSDM)
library(ggplot2)
}

## Load the distribution data ----------------------------------------------

mg <- read.csv("./Data/Data - 2022/Final spreadsheet/Total_data_Atlantic_April_2022_Final.csv")

## Load an ocean grid ------------------------------------------------------

# Load in a high resolution coastal grid for Europe
# This grid has a resolution of 0.5 degrees and already has polygon IDs
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/(4) Results/(Fig 3) Spread panel/Coastal grids/Europe")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

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

table(BEL$Year)

#1990s
{ mg_1995 <- filter(BEL, Year<=1995)
  mg_1996 <- filter(BEL, Year<=1996)
  mg_1997 <- filter(BEL, Year<=1997)
  mg_1998 <- filter(BEL, Year<=1998)
  mg_1999 <- filter(BEL, Year<=1999)
}
#2000s
{ mg_2000 <- filter(BEL, Year<=2000)
  mg_2001 <- filter(BEL, Year<=2001)
  mg_2002 <- filter(BEL, Year<=2002)
  mg_2003 <- filter(BEL, Year<=2003)
  mg_2004 <- filter(BEL, Year<=2004)
  mg_2005 <- filter(BEL, Year<=2005)
  mg_2006 <- filter(BEL, Year<=2006)
  mg_2007 <- filter(BEL, Year<=2007)
  mg_2008 <- filter(BEL, Year<=2008)
  mg_2009 <- filter(BEL, Year<=2009)
}
#2010s
{ mg_2010 <- filter(BEL, Year<=2010)
  mg_2011 <- filter(BEL, Year<=2011)
  mg_2012 <- filter(BEL, Year<=2012)
  mg_2013 <- filter(BEL, Year<=2013)
  mg_2014 <- filter(BEL, Year<=2014)
  mg_2015 <- filter(BEL, Year<=2015)
  mg_2016 <- filter(BEL, Year<=2016)
  mg_2017 <- filter(BEL, Year<=2017)
  mg_2018 <- filter(BEL, Year<=2018)
  mg_2019 <- filter(BEL, Year<=2019)
}
#2020s
{ mg_2020 <- filter(BEL, Year<=2020)
  mg_2021 <- filter(BEL, Year<=2021)
}

## Separate the lon and lat values -----------------------------------------

#1990s
{ mg_1995 <- mg_1995[, 8:7]
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

## Convert points to spdf ---------------------------------------------------

#1990s
{ mg_1995 <- SpatialPointsDataFrame(coords=mg_1995[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1995)
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

## Move points off of land -------------------------------------------------

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

# Load country-specific coast grid ----------------------------------------

setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/(4) Results/(Fig 3) Spread panel/Coastal grids/Belgium")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Calculate the number of polygons with data for each year ----------------

#1990s
{ poly_1995 <- over(mg_1995, ocean_grid)
  poly_1996 <- over(mg_1996, ocean_grid)
  poly_1997 <- over(mg_1997, ocean_grid)
  poly_1998 <- over(mg_1998, ocean_grid)
  poly_1999 <- over(mg_1999, ocean_grid)
}
#2000s
{ poly_2000 <- over(mg_2000, ocean_grid)
  poly_2001 <- over(mg_2001, ocean_grid)
  poly_2002 <- over(mg_2002, ocean_grid)
  poly_2003 <- over(mg_2003, ocean_grid)
  poly_2004 <- over(mg_2004, ocean_grid)
  poly_2005 <- over(mg_2005, ocean_grid)
  poly_2006 <- over(mg_2006, ocean_grid)
  poly_2007 <- over(mg_2007, ocean_grid)
  poly_2008 <- over(mg_2008, ocean_grid)
  poly_2009 <- over(mg_2009, ocean_grid)
}
#2010s
{ poly_2010 <- over(mg_2010, ocean_grid)
  poly_2011 <- over(mg_2011, ocean_grid)
  poly_2012 <- over(mg_2012, ocean_grid)
  poly_2013 <- over(mg_2013, ocean_grid)
  poly_2014 <- over(mg_2014, ocean_grid)
  poly_2015 <- over(mg_2015, ocean_grid)
  poly_2016 <- over(mg_2016, ocean_grid)
  poly_2017 <- over(mg_2017, ocean_grid)
  poly_2018 <- over(mg_2018, ocean_grid)
  poly_2019 <- over(mg_2019, ocean_grid)
}
#2020s
{ poly_2020 <- over(mg_2020, ocean_grid)
  poly_2021 <- over(mg_2021, ocean_grid)
}

# Remove duplicate polygons each year -------------------------------------

#1990s
{ poly_1995 <- poly_1995[!duplicated(poly_1995), ]
  poly_1996 <- poly_1996[!duplicated(poly_1996), ]
  poly_1997 <- poly_1997[!duplicated(poly_1997), ]
  poly_1998 <- poly_1998[!duplicated(poly_1998), ]
  poly_1999 <- poly_1999[!duplicated(poly_1999), ]
}
#2000s
{ poly_2000 <- poly_2000[!duplicated(poly_2000), ]
  poly_2001 <- poly_2001[!duplicated(poly_2001), ]
  poly_2002 <- poly_2002[!duplicated(poly_2002), ]
  poly_2003 <- poly_2003[!duplicated(poly_2003), ]
  poly_2004 <- poly_2004[!duplicated(poly_2004), ]
  poly_2005 <- poly_2005[!duplicated(poly_2005), ]
  poly_2006 <- poly_2006[!duplicated(poly_2006), ]
  poly_2007 <- poly_2007[!duplicated(poly_2007), ]
  poly_2008 <- poly_2008[!duplicated(poly_2008), ]
  poly_2009 <- poly_2009[!duplicated(poly_2009), ]
}
#2010s
{ poly_2010 <- poly_2010[!duplicated(poly_2010), ]
  poly_2011 <- poly_2011[!duplicated(poly_2011), ]
  poly_2012 <- poly_2012[!duplicated(poly_2012), ]
  poly_2013 <- poly_2013[!duplicated(poly_2013), ]
  poly_2014 <- poly_2014[!duplicated(poly_2014), ]
  poly_2015 <- poly_2015[!duplicated(poly_2015), ]
  poly_2016 <- poly_2016[!duplicated(poly_2016), ]
  poly_2017 <- poly_2017[!duplicated(poly_2017), ]
  poly_2018 <- poly_2018[!duplicated(poly_2018), ]
  poly_2019 <- poly_2019[!duplicated(poly_2019), ]
}
#2020s
{ poly_2020 <- poly_2020[!duplicated(poly_2020), ]
  poly_2021 <- poly_2021[!duplicated(poly_2021), ]
}

# Remove NA values --------------------------------------------------------

#1990s
{ poly_1995 <- filter(poly_1995, !is.na(ID))
  poly_1996 <- filter(poly_1996, !is.na(ID))
  poly_1997 <- filter(poly_1997, !is.na(ID))
  poly_1998 <- filter(poly_1998, !is.na(ID))
  poly_1999 <- filter(poly_1999, !is.na(ID))
}
#2000s
{ poly_2000 <- filter(poly_2000, !is.na(ID))
  poly_2001 <- filter(poly_2001, !is.na(ID))
  poly_2002 <- filter(poly_2002, !is.na(ID))
  poly_2003 <- filter(poly_2003, !is.na(ID))
  poly_2004 <- filter(poly_2004, !is.na(ID))
  poly_2005 <- filter(poly_2005, !is.na(ID))
  poly_2006 <- filter(poly_2006, !is.na(ID))
  poly_2007 <- filter(poly_2007, !is.na(ID))
  poly_2008 <- filter(poly_2008, !is.na(ID))
  poly_2009 <- filter(poly_2009, !is.na(ID))
}
#2010s
{ poly_2010 <- filter(poly_2010, !is.na(ID))
  poly_2011 <- filter(poly_2011, !is.na(ID))
  poly_2012 <- filter(poly_2012, !is.na(ID))
  poly_2013 <- filter(poly_2013, !is.na(ID))
  poly_2014 <- filter(poly_2014, !is.na(ID))
  poly_2015 <- filter(poly_2015, !is.na(ID))
  poly_2016 <- filter(poly_2016, !is.na(ID))
  poly_2017 <- filter(poly_2017, !is.na(ID))
  poly_2018 <- filter(poly_2018, !is.na(ID))
  poly_2019 <- filter(poly_2019, !is.na(ID))
}
#2020s
{ poly_2020 <- filter(poly_2020, !is.na(ID))
  poly_2021 <- filter(poly_2021, !is.na(ID))
}

# Set up blank data frame -------------------------------------------------

Area_BEL <- as.data.frame(1995:2021)
colnames(Area_BEL) <- "Year"
Area_BEL$Area <- NA

# Calculate Number of grid cells per year ----------------------------

#1990s
{ Area_BEL[1,2] <- nrow(poly_1995)
  Area_BEL[2,2] <- nrow(poly_1996)
  Area_BEL[3,2] <- nrow(poly_1997)
  Area_BEL[4,2] <- nrow(poly_1998)
  Area_BEL[5,2] <- nrow(poly_1999)
}
#2000s
{ Area_BEL[6,2] <- nrow(poly_2000)
  Area_BEL[7,2] <- nrow(poly_2001)
  Area_BEL[8,2] <- nrow(poly_2002)
  Area_BEL[9,2] <- nrow(poly_2003)
  Area_BEL[10,2] <- nrow(poly_2004)
  Area_BEL[11,2] <- nrow(poly_2005)
  Area_BEL[12,2] <- nrow(poly_2006)
  Area_BEL[13,2] <- nrow(poly_2007)
  Area_BEL[14,2] <- nrow(poly_2008)
  Area_BEL[15,2] <- nrow(poly_2009)
}
#2010s
{ Area_BEL[16,2] <- nrow(poly_2010)
  Area_BEL[17,2] <- nrow(poly_2011)
  Area_BEL[18,2] <- nrow(poly_2012)
  Area_BEL[19,2] <- nrow(poly_2013)
  Area_BEL[20,2] <- nrow(poly_2014)
  Area_BEL[21,2] <- nrow(poly_2015)
  Area_BEL[22,2] <- nrow(poly_2016)
  Area_BEL[23,2] <- nrow(poly_2017)
  Area_BEL[24,2] <- nrow(poly_2018)
  Area_BEL[25,2] <- nrow(poly_2019)
}
#2020s
{ Area_BEL[26,2] <- nrow(poly_2020)
  Area_BEL[27,2] <- nrow(poly_2021)
}

write.csv(Area_BEL, "./Data/Area of occupancy/Individual countries/AOO_BEL.csv")

# Plot --------------------------------------------------------------------

Area_BEL <- read.csv("./Data/Area of occupancy/Individual countries/AOO_BEL.csv")

# Make a quick plot
ggplot(Area_BEL, aes(x=Year, y=Area))+
  geom_point()

# -------------------------------------------------------------------------
# --------------------------------- Denmark -------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

table(DEN$Year)

#2010s
{ mg_2014 <- filter(DEN, Year<=2014)
  mg_2015 <- filter(DEN, Year<=2015)
  mg_2016 <- filter(DEN, Year<=2016)
  mg_2017 <- filter(DEN, Year<=2017)
  mg_2018 <- filter(DEN, Year<=2018)
  mg_2019 <- filter(DEN, Year<=2019)
}
#2020s
{ mg_2020 <- filter(DEN, Year<=2020)
  mg_2021 <- filter(DEN, Year<=2021)
}

## Separate the lon and lat values -----------------------------------------

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

## Move points off of land -------------------------------------------------

#2010s
{ mg_2017 <- points2nearestcell(mg_2017, bathy)
  mg_2018 <- points2nearestcell(mg_2018, bathy)
  mg_2019 <- points2nearestcell(mg_2019, bathy)
}
#2020s
{ mg_2020 <- points2nearestcell(mg_2020, bathy)
  mg_2021 <- points2nearestcell(mg_2021, bathy)
}

# Load country-specific coast grid ----------------------------------------

setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/(4) Results/(Fig 3) Spread panel/Coastal grids/Denmark")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Calculate the number of polygons with data for each year ----------------

#2010s
{ poly_2014 <- over(mg_2014, ocean_grid)
  poly_2015 <- over(mg_2015, ocean_grid)
  poly_2016 <- over(mg_2016, ocean_grid)
  poly_2017 <- over(mg_2017, ocean_grid)
  poly_2018 <- over(mg_2018, ocean_grid)
  poly_2019 <- over(mg_2019, ocean_grid)
}
#2020s
{ poly_2020 <- over(mg_2020, ocean_grid)
  poly_2021 <- over(mg_2021, ocean_grid)
}

# Remove duplicate polygons each year -------------------------------------

#2010s
{ poly_2014 <- poly_2014[!duplicated(poly_2014), ]
  poly_2015 <- poly_2015[!duplicated(poly_2015), ]
  poly_2016 <- poly_2016[!duplicated(poly_2016), ]
  poly_2017 <- poly_2017[!duplicated(poly_2017), ]
  poly_2018 <- poly_2018[!duplicated(poly_2018), ]
  poly_2019 <- poly_2019[!duplicated(poly_2019), ]
}
#2020s
{ poly_2020 <- poly_2020[!duplicated(poly_2020), ]
  poly_2021 <- poly_2021[!duplicated(poly_2021), ]
}

# Remove NA values --------------------------------------------------------

#2010s
{ poly_2014 <- filter(poly_2014, !is.na(ID))
  poly_2015 <- filter(poly_2015, !is.na(ID))
  poly_2016 <- filter(poly_2016, !is.na(ID))
  poly_2017 <- filter(poly_2017, !is.na(ID))
  poly_2018 <- filter(poly_2018, !is.na(ID))
  poly_2019 <- filter(poly_2019, !is.na(ID))
}
#2020s
{ poly_2020 <- filter(poly_2020, !is.na(ID))
  poly_2021 <- filter(poly_2021, !is.na(ID))
}

# Set up blank data frame -------------------------------------------------

Area_DEN <- as.data.frame(2014:2021)
colnames(Area_DEN) <- "Year"
Area_DEN$Area <- NA

# Calculate Number of grid cells per year ----------------------------

#2010s
{ Area_DEN[1,2] <- nrow(poly_2014)
  Area_DEN[2,2] <- nrow(poly_2015)
  Area_DEN[3,2] <- nrow(poly_2016)
  Area_DEN[4,2] <- nrow(poly_2017)
  Area_DEN[5,2] <- nrow(poly_2018)
  Area_DEN[6,2] <- nrow(poly_2019)
}
#2020s
{ Area_DEN[7,2] <- nrow(poly_2020)
  Area_DEN[8,2] <- nrow(poly_2021)
}

write.csv(Area_DEN, "./Data/Area of occupancy/Individual countries/AOO_DEN.csv")

# Plot --------------------------------------------------------------------

Area_DEN <- read.csv("./Data/Area of occupancy/Individual countries/AOO_DEN.csv")

# Make quick plot
ggplot(Area_DEN, aes(x=Year, y=Area))+
  geom_point()

# -------------------------------------------------------------------------
# ---------------------------------- France -------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

table(FRA$Year)

#1960s
{ mg_1965 <- filter(FRA, Year<=1965)
  mg_1966 <- filter(FRA, Year<=1966)
  mg_1967 <- filter(FRA, Year<=1967)
  mg_1968 <- filter(FRA, Year<=1968)
  mg_1969 <- filter(FRA, Year<=1969)
}
#1970s
{ mg_1970 <- filter(FRA, Year<=1970)
  mg_1971 <- filter(FRA, Year<=1971)
  mg_1972 <- filter(FRA, Year<=1972)
  mg_1973 <- filter(FRA, Year<=1973)
  mg_1974 <- filter(FRA, Year<=1974)
  mg_1975 <- filter(FRA, Year<=1975)
  mg_1976 <- filter(FRA, Year<=1976)
  mg_1977 <- filter(FRA, Year<=1977)
  mg_1978 <- filter(FRA, Year<=1978)
  mg_1979 <- filter(FRA, Year<=1979)
}
#1980s
{ mg_1980 <- filter(FRA, Year<=1980)
  mg_1981 <- filter(FRA, Year<=1981)
  mg_1982 <- filter(FRA, Year<=1982)
  mg_1983 <- filter(FRA, Year<=1983)
  mg_1984 <- filter(FRA, Year<=1984)
  mg_1985 <- filter(FRA, Year<=1985)
  mg_1986 <- filter(FRA, Year<=1986)
  mg_1987 <- filter(FRA, Year<=1987)
  mg_1988 <- filter(FRA, Year<=1988)
  mg_1989 <- filter(FRA, Year<=1989)
}
#1990s
{ mg_1990 <- filter(FRA, Year<=1990)
  mg_1991 <- filter(FRA, Year<=1991)
  mg_1992 <- filter(FRA, Year<=1992)
  mg_1993 <- filter(FRA, Year<=1993)
  mg_1994 <- filter(FRA, Year<=1994)
  mg_1995 <- filter(FRA, Year<=1995)
  mg_1996 <- filter(FRA, Year<=1996)
  mg_1997 <- filter(FRA, Year<=1997)
  mg_1998 <- filter(FRA, Year<=1998)
  mg_1999 <- filter(FRA, Year<=1999)
}
#2000s
{ mg_2000 <- filter(FRA, Year<=2000)
  mg_2001 <- filter(FRA, Year<=2001)
  mg_2002 <- filter(FRA, Year<=2002)
  mg_2003 <- filter(FRA, Year<=2003)
  mg_2004 <- filter(FRA, Year<=2004)
  mg_2005 <- filter(FRA, Year<=2005)
  mg_2006 <- filter(FRA, Year<=2006)
  mg_2007 <- filter(FRA, Year<=2007)
  mg_2008 <- filter(FRA, Year<=2008)
  mg_2009 <- filter(FRA, Year<=2009)
}
#2010s
{ mg_2010 <- filter(FRA, Year<=2010)
  mg_2011 <- filter(FRA, Year<=2011)
  mg_2012 <- filter(FRA, Year<=2012)
  mg_2013 <- filter(FRA, Year<=2013)
  mg_2014 <- filter(FRA, Year<=2014)
  mg_2015 <- filter(FRA, Year<=2015)
  mg_2016 <- filter(FRA, Year<=2016)
  mg_2017 <- filter(FRA, Year<=2017)
  mg_2018 <- filter(FRA, Year<=2018)
  mg_2019 <- filter(FRA, Year<=2019)
}
#2020s
{ mg_2020 <- filter(FRA, Year<=2020)
  mg_2021 <- filter(FRA, Year<=2021)
}

## Separate the lon and lat values -----------------------------------------

#1960s
{ mg_1965 <-mg_1965[, 8:7]
mg_1966 <-mg_1966[, 8:7]
mg_1967 <-mg_1967[, 8:7]
mg_1968 <-mg_1968[, 8:7]
mg_1969 <-mg_1969[, 8:7]
}
#1970s
{ mg_1970 <- mg_1970[, 8:7]
  mg_1971 <- mg_1971[, 8:7]
  mg_1972 <- mg_1972[, 8:7]
  mg_1973 <- mg_1973[, 8:7]
  mg_1974 <- mg_1974[, 8:7]
mg_1975 <- mg_1975[, 8:7]
mg_1976 <- mg_1976[, 8:7]
mg_1977 <- mg_1977[, 8:7]
mg_1978 <- mg_1978[, 8:7]
mg_1979 <- mg_1979[, 8:7]
}
#1980s
{ mg_1980 <-mg_1980[, 8:7]
  mg_1981 <-mg_1981[, 8:7]
  mg_1982 <-mg_1982[, 8:7]
  mg_1983 <-mg_1983[, 8:7]
  mg_1984 <-mg_1984[, 8:7]
  mg_1985 <-mg_1985[, 8:7]
  mg_1986 <-mg_1986[, 8:7]
  mg_1987 <-mg_1987[, 8:7]
  mg_1988 <-mg_1988[, 8:7]
  mg_1989 <-mg_1989[, 8:7]
}
#1990s
{ mg_1990 <- mg_1990[, 8:7]
  mg_1991 <- mg_1991[, 8:7]
  mg_1992 <- mg_1992[, 8:7]
  mg_1993 <- mg_1993[, 8:7]
  mg_1994 <- mg_1994[, 8:7]
  mg_1995 <- mg_1995[, 8:7]
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

## Convert points to spdf ---------------------------------------------------

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

## Move points off of land -------------------------------------------------

#2000s
{ mg_2006 <- points2nearestcell(mg_2006, bathy)
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

# Load country-specific coast grid ----------------------------------------

setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/(4) Results/(Fig 3) Spread panel/Coastal grids/France")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Calculate the number of polygons with data for each year ----------------

#1960s
{ poly_1965 <- over(mg_1965, ocean_grid)
poly_1966 <- over(mg_1966, ocean_grid)
poly_1967 <- over(mg_1967, ocean_grid)
poly_1968 <- over(mg_1968, ocean_grid)
poly_1969 <- over(mg_1969, ocean_grid)
}
#1970s
{ poly_1970 <- over(mg_1970, ocean_grid)
  poly_1971 <- over(mg_1971, ocean_grid)
  poly_1972 <- over(mg_1972, ocean_grid)
  poly_1973 <- over(mg_1973, ocean_grid)
  poly_1974 <- over(mg_1974, ocean_grid)
poly_1975 <- over(mg_1975, ocean_grid)
poly_1976 <- over(mg_1976, ocean_grid)
poly_1977 <- over(mg_1977, ocean_grid)
poly_1978 <- over(mg_1978, ocean_grid)
poly_1979 <- over(mg_1979, ocean_grid)
}
#1980s
{ poly_1980 <- over(mg_1980, ocean_grid)
  poly_1981 <- over(mg_1981, ocean_grid)
  poly_1982 <- over(mg_1982, ocean_grid)
  poly_1983 <- over(mg_1983, ocean_grid)
  poly_1984 <- over(mg_1984, ocean_grid)
  poly_1985 <- over(mg_1985, ocean_grid)
  poly_1986 <- over(mg_1986, ocean_grid)
  poly_1987 <- over(mg_1987, ocean_grid)
  poly_1988 <- over(mg_1988, ocean_grid)
  poly_1989 <- over(mg_1989, ocean_grid)
}
#1990s
{ poly_1990 <- over(mg_1990, ocean_grid)
  poly_1991 <- over(mg_1991, ocean_grid)
  poly_1992 <- over(mg_1992, ocean_grid)
  poly_1993 <- over(mg_1993, ocean_grid)
  poly_1994 <- over(mg_1994, ocean_grid)
  poly_1995 <- over(mg_1995, ocean_grid)
  poly_1996 <- over(mg_1996, ocean_grid)
  poly_1997 <- over(mg_1997, ocean_grid)
  poly_1998 <- over(mg_1998, ocean_grid)
  poly_1999 <- over(mg_1999, ocean_grid)
}
#2000s
{ poly_2000 <- over(mg_2000, ocean_grid)
  poly_2001 <- over(mg_2001, ocean_grid)
  poly_2002 <- over(mg_2002, ocean_grid)
  poly_2003 <- over(mg_2003, ocean_grid)
  poly_2004 <- over(mg_2004, ocean_grid)
  poly_2005 <- over(mg_2005, ocean_grid)
  poly_2006 <- over(mg_2006, ocean_grid)
  poly_2007 <- over(mg_2007, ocean_grid)
  poly_2008 <- over(mg_2008, ocean_grid)
  poly_2009 <- over(mg_2009, ocean_grid)
}
#2010s
{ poly_2010 <- over(mg_2010, ocean_grid)
  poly_2011 <- over(mg_2011, ocean_grid)
  poly_2012 <- over(mg_2012, ocean_grid)
  poly_2013 <- over(mg_2013, ocean_grid)
  poly_2014 <- over(mg_2014, ocean_grid)
  poly_2015 <- over(mg_2015, ocean_grid)
  poly_2016 <- over(mg_2016, ocean_grid)
  poly_2017 <- over(mg_2017, ocean_grid)
  poly_2018 <- over(mg_2018, ocean_grid)
  poly_2019 <- over(mg_2019, ocean_grid)
}
#2020s
{ poly_2020 <- over(mg_2020, ocean_grid)
  poly_2021 <- over(mg_2021, ocean_grid)
}

# Remove duplicate polygons each year -------------------------------------

#1960s
{ poly_1965 <- poly_1965[!duplicated(poly_1965), ]
poly_1966 <- poly_1966[!duplicated(poly_1966), ]
poly_1967 <- poly_1967[!duplicated(poly_1967), ]
poly_1968 <- poly_1968[!duplicated(poly_1968), ]
poly_1969 <- poly_1969[!duplicated(poly_1969), ]
}
#1970s
{ poly_1970 <- poly_1970[!duplicated(poly_1970), ]
  poly_1971 <- poly_1971[!duplicated(poly_1971), ]
  poly_1972 <- poly_1972[!duplicated(poly_1972), ]
  poly_1973 <- poly_1973[!duplicated(poly_1973), ]
  poly_1974 <- poly_1974[!duplicated(poly_1974), ]
poly_1975 <- poly_1975[!duplicated(poly_1975), ]
poly_1976 <- poly_1976[!duplicated(poly_1976), ]
poly_1977 <- poly_1977[!duplicated(poly_1977), ]
poly_1978 <- poly_1978[!duplicated(poly_1978), ]
poly_1979 <- poly_1979[!duplicated(poly_1979), ]
}
#1980s
{ poly_1980 <- poly_1980[!duplicated(poly_1980), ]
  poly_1981 <- poly_1981[!duplicated(poly_1981), ]
  poly_1982 <- poly_1982[!duplicated(poly_1982), ]
  poly_1983 <- poly_1983[!duplicated(poly_1983), ]
  poly_1984 <- poly_1984[!duplicated(poly_1984), ]
  poly_1985 <- poly_1985[!duplicated(poly_1985), ]
  poly_1986 <- poly_1986[!duplicated(poly_1986), ]
  poly_1987 <- poly_1987[!duplicated(poly_1987), ]
  poly_1988 <- poly_1988[!duplicated(poly_1988), ]
  poly_1989 <- poly_1989[!duplicated(poly_1989), ]
}
#1990s
{ poly_1990 <- poly_1990[!duplicated(poly_1990), ]
  poly_1991 <- poly_1991[!duplicated(poly_1991), ]
  poly_1992 <- poly_1992[!duplicated(poly_1992), ]
  poly_1993 <- poly_1993[!duplicated(poly_1993), ]
  poly_1994 <- poly_1994[!duplicated(poly_1994), ]
  poly_1995 <- poly_1995[!duplicated(poly_1995), ]
  poly_1996 <- poly_1996[!duplicated(poly_1996), ]
  poly_1997 <- poly_1997[!duplicated(poly_1997), ]
  poly_1998 <- poly_1998[!duplicated(poly_1998), ]
  poly_1999 <- poly_1999[!duplicated(poly_1999), ]
}
#2000s
{ poly_2000 <- poly_2000[!duplicated(poly_2000), ]
  poly_2001 <- poly_2001[!duplicated(poly_2001), ]
  poly_2002 <- poly_2002[!duplicated(poly_2002), ]
  poly_2003 <- poly_2003[!duplicated(poly_2003), ]
  poly_2004 <- poly_2004[!duplicated(poly_2004), ]
  poly_2005 <- poly_2005[!duplicated(poly_2005), ]
  poly_2006 <- poly_2006[!duplicated(poly_2006), ]
  poly_2007 <- poly_2007[!duplicated(poly_2007), ]
  poly_2008 <- poly_2008[!duplicated(poly_2008), ]
  poly_2009 <- poly_2009[!duplicated(poly_2009), ]
}
#2010s
{ poly_2010 <- poly_2010[!duplicated(poly_2010), ]
  poly_2011 <- poly_2011[!duplicated(poly_2011), ]
  poly_2012 <- poly_2012[!duplicated(poly_2012), ]
  poly_2013 <- poly_2013[!duplicated(poly_2013), ]
  poly_2014 <- poly_2014[!duplicated(poly_2014), ]
  poly_2015 <- poly_2015[!duplicated(poly_2015), ]
  poly_2016 <- poly_2016[!duplicated(poly_2016), ]
  poly_2017 <- poly_2017[!duplicated(poly_2017), ]
  poly_2018 <- poly_2018[!duplicated(poly_2018), ]
  poly_2019 <- poly_2019[!duplicated(poly_2019), ]
}
#2020s
{ poly_2020 <- poly_2020[!duplicated(poly_2020), ]
  poly_2021 <- poly_2021[!duplicated(poly_2021), ]
}

# Remove NA values --------------------------------------------------------

#1960s
{ poly_1965 <- filter(poly_1965, !is.na(ID))
poly_1966 <- filter(poly_1966, !is.na(ID))
poly_1967 <- filter(poly_1967, !is.na(ID))
poly_1968 <- filter(poly_1968, !is.na(ID))
poly_1969 <- filter(poly_1969, !is.na(ID))
}
#1970s
{ poly_1970 <- filter(poly_1970, !is.na(ID))
  poly_1971 <- filter(poly_1971, !is.na(ID))
  poly_1972 <- filter(poly_1972, !is.na(ID))
  poly_1973 <- filter(poly_1973, !is.na(ID))
  poly_1974 <- filter(poly_1974, !is.na(ID))
poly_1975 <- filter(poly_1975, !is.na(ID))
poly_1976 <- filter(poly_1976, !is.na(ID))
poly_1977 <- filter(poly_1977, !is.na(ID))
poly_1978 <- filter(poly_1978, !is.na(ID))
poly_1979 <- filter(poly_1979, !is.na(ID))
}
#1980s
{ poly_1980 <- filter(poly_1980, !is.na(ID))
  poly_1981 <- filter(poly_1981, !is.na(ID))
  poly_1982 <- filter(poly_1982, !is.na(ID))
  poly_1983 <- filter(poly_1983, !is.na(ID))
  poly_1984 <- filter(poly_1984, !is.na(ID))
  poly_1985 <- filter(poly_1985, !is.na(ID))
  poly_1986 <- filter(poly_1986, !is.na(ID))
  poly_1987 <- filter(poly_1987, !is.na(ID))
  poly_1988 <- filter(poly_1988, !is.na(ID))
  poly_1989 <- filter(poly_1989, !is.na(ID))
}
#1990s
{ poly_1990 <- filter(poly_1990, !is.na(ID))
  poly_1991 <- filter(poly_1991, !is.na(ID))
  poly_1992 <- filter(poly_1992, !is.na(ID))
  poly_1993 <- filter(poly_1993, !is.na(ID))
  poly_1994 <- filter(poly_1994, !is.na(ID))
  poly_1995 <- filter(poly_1995, !is.na(ID))
  poly_1996 <- filter(poly_1996, !is.na(ID))
  poly_1997 <- filter(poly_1997, !is.na(ID))
  poly_1998 <- filter(poly_1998, !is.na(ID))
  poly_1999 <- filter(poly_1999, !is.na(ID))
}
#2000s
{ poly_2000 <- filter(poly_2000, !is.na(ID))
  poly_2001 <- filter(poly_2001, !is.na(ID))
  poly_2002 <- filter(poly_2002, !is.na(ID))
  poly_2003 <- filter(poly_2003, !is.na(ID))
  poly_2004 <- filter(poly_2004, !is.na(ID))
  poly_2005 <- filter(poly_2005, !is.na(ID))
  poly_2006 <- filter(poly_2006, !is.na(ID))
  poly_2007 <- filter(poly_2007, !is.na(ID))
  poly_2008 <- filter(poly_2008, !is.na(ID))
  poly_2009 <- filter(poly_2009, !is.na(ID))
}
#2010s
{ poly_2010 <- filter(poly_2010, !is.na(ID))
  poly_2011 <- filter(poly_2011, !is.na(ID))
  poly_2012 <- filter(poly_2012, !is.na(ID))
  poly_2013 <- filter(poly_2013, !is.na(ID))
  poly_2014 <- filter(poly_2014, !is.na(ID))
  poly_2015 <- filter(poly_2015, !is.na(ID))
  poly_2016 <- filter(poly_2016, !is.na(ID))
  poly_2017 <- filter(poly_2017, !is.na(ID))
  poly_2018 <- filter(poly_2018, !is.na(ID))
  poly_2019 <- filter(poly_2019, !is.na(ID))
}
#2020s
{ poly_2020 <- filter(poly_2020, !is.na(ID))
  poly_2021 <- filter(poly_2021, !is.na(ID))
}

# Set up blank data frame -------------------------------------------------

Area_FRA <- as.data.frame(1965:2021)
colnames(Area_FRA) <- "Year"
Area_FRA$Area <- NA

# Calculate Number of grid cells per year ----------------------------

#1960s
{ Area_FRA[1,2] <- nrow(poly_1965)
Area_FRA[2,2] <- nrow(poly_1966)
Area_FRA[3,2] <- nrow(poly_1967)
Area_FRA[4,2] <- nrow(poly_1968)
Area_FRA[5,2] <- nrow(poly_1969)
}
#1970s
{ Area_FRA[6,2] <- nrow(poly_1970)
  Area_FRA[7,2] <- nrow(poly_1971)
  Area_FRA[8,2] <- nrow(poly_1972)
  Area_FRA[9,2] <- nrow(poly_1973)
  Area_FRA[10,2] <- nrow(poly_1974)
  Area_FRA[11,2] <- nrow(poly_1975)
  Area_FRA[12,2] <- nrow(poly_1976)
  Area_FRA[13,2] <- nrow(poly_1977)
  Area_FRA[14,2] <- nrow(poly_1978)
  Area_FRA[15,2] <- nrow(poly_1979)
}
#1980s
{ Area_FRA[16,2] <- nrow(poly_1980)
  Area_FRA[17,2] <- nrow(poly_1981)
  Area_FRA[18,2] <- nrow(poly_1982)
  Area_FRA[19,2] <- nrow(poly_1983)
  Area_FRA[20,2] <- nrow(poly_1984)
  Area_FRA[21,2] <- nrow(poly_1985)
  Area_FRA[22,2] <- nrow(poly_1986)
  Area_FRA[23,2] <- nrow(poly_1987)
  Area_FRA[24,2] <- nrow(poly_1988)
  Area_FRA[25,2] <- nrow(poly_1989)
}
#1990s
{ Area_FRA[26,2] <- nrow(poly_1990)
  Area_FRA[27,2] <- nrow(poly_1991)
  Area_FRA[28,2] <- nrow(poly_1992)
  Area_FRA[29,2] <- nrow(poly_1993)
  Area_FRA[30,2] <- nrow(poly_1994)
  Area_FRA[31,2] <- nrow(poly_1995)
  Area_FRA[32,2] <- nrow(poly_1996)
  Area_FRA[33,2] <- nrow(poly_1997)
  Area_FRA[34,2] <- nrow(poly_1998)
  Area_FRA[35,2] <- nrow(poly_1999)
}
#2000s
{ Area_FRA[36,2] <- nrow(poly_2000)
  Area_FRA[37,2] <- nrow(poly_2001)
  Area_FRA[38,2] <- nrow(poly_2002)
  Area_FRA[39,2] <- nrow(poly_2003)
  Area_FRA[40,2] <- nrow(poly_2004)
  Area_FRA[41,2] <- nrow(poly_2005)
  Area_FRA[42,2] <- nrow(poly_2006)
  Area_FRA[43,2] <- nrow(poly_2007)
  Area_FRA[44,2] <- nrow(poly_2008)
  Area_FRA[45,2] <- nrow(poly_2009)
}
#2010s
{ Area_FRA[46,2] <- nrow(poly_2010)
  Area_FRA[47,2] <- nrow(poly_2011)
  Area_FRA[48,2] <- nrow(poly_2012)
  Area_FRA[49,2] <- nrow(poly_2013)
  Area_FRA[50,2] <- nrow(poly_2014)
  Area_FRA[51,2] <- nrow(poly_2015)
  Area_FRA[52,2] <- nrow(poly_2016)
  Area_FRA[53,2] <- nrow(poly_2017)
  Area_FRA[54,2] <- nrow(poly_2018)
  Area_FRA[55,2] <- nrow(poly_2019)
}
#2020s
{ Area_FRA[56,2] <- nrow(poly_2020)
  Area_FRA[57,2] <- nrow(poly_2021)
}

write.csv(Area_FRA, "./Data/Area of occupancy/Individual countries/AOO_FRA.csv")

# Plot --------------------------------------------------------------------

Area_FRA <- read.csv("./Data/Area of occupancy/Individual countries/AOO_FRA.csv")

# Make quick plot
ggplot(Area_FRA, aes(x=Year, y=Area))+
  geom_point()

# -------------------------------------------------------------------------
# --------------------------------- Germany -------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

table(GER$Year)

#2000s
{ mg_2006 <- filter(GER, Year<=2006)
  mg_2007 <- filter(GER, Year<=2007)
  mg_2008 <- filter(GER, Year<=2008)
  mg_2009 <- filter(GER, Year<=2009)
}
#2010s
{ mg_2010 <- filter(GER, Year<=2010)
  mg_2011 <- filter(GER, Year<=2011)
  mg_2012 <- filter(GER, Year<=2012)
  mg_2013 <- filter(GER, Year<=2013)
  mg_2014 <- filter(GER, Year<=2014)
  mg_2015 <- filter(GER, Year<=2015)
  mg_2016 <- filter(GER, Year<=2016)
  mg_2017 <- filter(GER, Year<=2017)
  mg_2018 <- filter(GER, Year<=2018)
  mg_2019 <- filter(GER, Year<=2019)
}
#2020s
{ mg_2020 <- filter(GER, Year<=2020)
  mg_2021 <- filter(GER, Year<=2021)
}

## Separate the lon and lat values -----------------------------------------

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

## Move points off of land -------------------------------------------------

#2000s
{ mg_2006 <- points2nearestcell(mg_2006, bathy)
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

# Load country-specific coast grid ----------------------------------------

setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/(4) Results/(Fig 3) Spread panel/Coastal grids/Germany")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Calculate the number of polygons with data for each year ----------------

#2000s
{ poly_2006 <- over(mg_2006, ocean_grid)
  poly_2007 <- over(mg_2007, ocean_grid)
  poly_2008 <- over(mg_2008, ocean_grid)
  poly_2009 <- over(mg_2009, ocean_grid)
}
#2010s
{ poly_2010 <- over(mg_2010, ocean_grid)
  poly_2011 <- over(mg_2011, ocean_grid)
  poly_2012 <- over(mg_2012, ocean_grid)
  poly_2013 <- over(mg_2013, ocean_grid)
  poly_2014 <- over(mg_2014, ocean_grid)
  poly_2015 <- over(mg_2015, ocean_grid)
  poly_2016 <- over(mg_2016, ocean_grid)
  poly_2017 <- over(mg_2017, ocean_grid)
  poly_2018 <- over(mg_2018, ocean_grid)
  poly_2019 <- over(mg_2019, ocean_grid)
}
#2020s
{ poly_2020 <- over(mg_2020, ocean_grid)
  poly_2021 <- over(mg_2021, ocean_grid)
}

# Remove duplicate polygons each year -------------------------------------

#2000s
{ poly_2006 <- poly_2006[!duplicated(poly_2006), ]
  poly_2007 <- poly_2007[!duplicated(poly_2007), ]
  poly_2008 <- poly_2008[!duplicated(poly_2008), ]
  poly_2009 <- poly_2009[!duplicated(poly_2009), ]
}
#2010s
{ poly_2010 <- poly_2010[!duplicated(poly_2010), ]
  poly_2011 <- poly_2011[!duplicated(poly_2011), ]
  poly_2012 <- poly_2012[!duplicated(poly_2012), ]
  poly_2013 <- poly_2013[!duplicated(poly_2013), ]
  poly_2014 <- poly_2014[!duplicated(poly_2014), ]
  poly_2015 <- poly_2015[!duplicated(poly_2015), ]
  poly_2016 <- poly_2016[!duplicated(poly_2016), ]
  poly_2017 <- poly_2017[!duplicated(poly_2017), ]
  poly_2018 <- poly_2018[!duplicated(poly_2018), ]
  poly_2019 <- poly_2019[!duplicated(poly_2019), ]
}
#2020s
{ poly_2020 <- poly_2020[!duplicated(poly_2020), ]
  poly_2021 <- poly_2021[!duplicated(poly_2021), ]
}

# Remove NA values --------------------------------------------------------

#2000s
{ poly_2006 <- filter(poly_2006, !is.na(ID))
  poly_2007 <- filter(poly_2007, !is.na(ID))
  poly_2008 <- filter(poly_2008, !is.na(ID))
  poly_2009 <- filter(poly_2009, !is.na(ID))
}
#2010s
{ poly_2010 <- filter(poly_2010, !is.na(ID))
  poly_2011 <- filter(poly_2011, !is.na(ID))
  poly_2012 <- filter(poly_2012, !is.na(ID))
  poly_2013 <- filter(poly_2013, !is.na(ID))
  poly_2014 <- filter(poly_2014, !is.na(ID))
  poly_2015 <- filter(poly_2015, !is.na(ID))
  poly_2016 <- filter(poly_2016, !is.na(ID))
  poly_2017 <- filter(poly_2017, !is.na(ID))
  poly_2018 <- filter(poly_2018, !is.na(ID))
  poly_2019 <- filter(poly_2019, !is.na(ID))
}
#2020s
{ poly_2020 <- filter(poly_2020, !is.na(ID))
  poly_2021 <- filter(poly_2021, !is.na(ID))
}

# Set up blank data frame -------------------------------------------------

Area_GER <- as.data.frame(2006:2021)
colnames(Area_GER) <- "Year"
Area_GER$Area <- NA

# Calculate Number of grid cells per year ----------------------------

#2000s
{ Area_GER[1,2] <- nrow(poly_2006)
Area_GER[2,2] <- nrow(poly_2007)
Area_GER[3,2] <- nrow(poly_2008)
Area_GER[4,2] <- nrow(poly_2009)
}
#2010s
{ Area_GER[5,2] <- nrow(poly_2010)
  Area_GER[6,2] <- nrow(poly_2011)
  Area_GER[7,2] <- nrow(poly_2012)
  Area_GER[8,2] <- nrow(poly_2013)
  Area_GER[9,2] <- nrow(poly_2014)
  Area_GER[10,2] <- nrow(poly_2015)
  Area_GER[11,2] <- nrow(poly_2016)
  Area_GER[12,2] <- nrow(poly_2017)
  Area_GER[13,2] <- nrow(poly_2018)
  Area_GER[14,2] <- nrow(poly_2019)
}
#2020s
{ Area_GER[15,2] <- nrow(poly_2020)
  Area_GER[16,2] <- nrow(poly_2021)
}

write.csv(Area_GER, "./Data/Area of occupancy/Individual countries/AOO_GER.csv")

# Plot --------------------------------------------------------------------

Area_GER <- read.csv("./Data/Area of occupancy/Individual countries/AOO_GER.csv")

# Make quick plot
ggplot(Area_GER, aes(x=Year, y=Area))+
  geom_point()

# -------------------------------------------------------------------------
# --------------------------------- Ireland -------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

table(IRE$Year)

#1980s
{ mg_1986 <- filter(IRE, Year<=1986)
  mg_1987 <- filter(IRE, Year<=1987)
  mg_1988 <- filter(IRE, Year<=1988)
  mg_1989 <- filter(IRE, Year<=1989)
}
#1990s
{ mg_1990 <- filter(IRE, Year<=1990)
  mg_1991 <- filter(IRE, Year<=1991)
  mg_1992 <- filter(IRE, Year<=1992)
  mg_1993 <- filter(IRE, Year<=1993)
  mg_1994 <- filter(IRE, Year<=1994)
  mg_1995 <- filter(IRE, Year<=1995)
  mg_1996 <- filter(IRE, Year<=1996)
  mg_1997 <- filter(IRE, Year<=1997)
  mg_1998 <- filter(IRE, Year<=1998)
  mg_1999 <- filter(IRE, Year<=1999)
}
#2000s
{ mg_2000 <- filter(IRE, Year<=2000)
  mg_2001 <- filter(IRE, Year<=2001)
  mg_2002 <- filter(IRE, Year<=2002)
  mg_2003 <- filter(IRE, Year<=2003)
  mg_2004 <- filter(IRE, Year<=2004)
  mg_2005 <- filter(IRE, Year<=2005)
  mg_2006 <- filter(IRE, Year<=2006)
  mg_2007 <- filter(IRE, Year<=2007)
  mg_2008 <- filter(IRE, Year<=2008)
  mg_2009 <- filter(IRE, Year<=2009)
}
#2010s
{ mg_2010 <- filter(IRE, Year<=2010)
  mg_2011 <- filter(IRE, Year<=2011)
  mg_2012 <- filter(IRE, Year<=2012)
  mg_2013 <- filter(IRE, Year<=2013)
  mg_2014 <- filter(IRE, Year<=2014)
  mg_2015 <- filter(IRE, Year<=2015)
  mg_2016 <- filter(IRE, Year<=2016)
  mg_2017 <- filter(IRE, Year<=2017)
  mg_2018 <- filter(IRE, Year<=2018)
  mg_2019 <- filter(IRE, Year<=2019)
}
#2020s
{ mg_2020 <- filter(IRE, Year<=2020)
  mg_2021 <- filter(IRE, Year<=2021)
}

## Separate the lon and lat values -----------------------------------------

#1980s
{ mg_1986 <-mg_1986[, 8:7]
  mg_1987 <-mg_1987[, 8:7]
  mg_1988 <-mg_1988[, 8:7]
  mg_1989 <-mg_1989[, 8:7]
}
#1990s
{ mg_1990 <- mg_1990[, 8:7]
  mg_1991 <- mg_1991[, 8:7]
  mg_1992 <- mg_1992[, 8:7]
  mg_1993 <- mg_1993[, 8:7]
  mg_1994 <- mg_1994[, 8:7]
  mg_1995 <- mg_1995[, 8:7]
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

## Convert points to spdf ---------------------------------------------------

#1980s
{ mg_1986 <- SpatialPointsDataFrame(coords=mg_1986[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1986)
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

# Load country-specific coast grid ----------------------------------------

setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/(4) Results/(Fig 3) Spread panel/Coastal grids/Ireland")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Calculate the number of polygons with data for each year ----------------

#1980s
{ poly_1986 <- over(mg_1986, ocean_grid)
  poly_1987 <- over(mg_1987, ocean_grid)
  poly_1988 <- over(mg_1988, ocean_grid)
  poly_1989 <- over(mg_1989, ocean_grid)
}
#1990s
{ poly_1990 <- over(mg_1990, ocean_grid)
  poly_1991 <- over(mg_1991, ocean_grid)
  poly_1992 <- over(mg_1992, ocean_grid)
  poly_1993 <- over(mg_1993, ocean_grid)
  poly_1994 <- over(mg_1994, ocean_grid)
  poly_1995 <- over(mg_1995, ocean_grid)
  poly_1996 <- over(mg_1996, ocean_grid)
  poly_1997 <- over(mg_1997, ocean_grid)
  poly_1998 <- over(mg_1998, ocean_grid)
  poly_1999 <- over(mg_1999, ocean_grid)
}
#2000s
{ poly_2000 <- over(mg_2000, ocean_grid)
  poly_2001 <- over(mg_2001, ocean_grid)
  poly_2002 <- over(mg_2002, ocean_grid)
  poly_2003 <- over(mg_2003, ocean_grid)
  poly_2004 <- over(mg_2004, ocean_grid)
  poly_2005 <- over(mg_2005, ocean_grid)
  poly_2006 <- over(mg_2006, ocean_grid)
  poly_2007 <- over(mg_2007, ocean_grid)
  poly_2008 <- over(mg_2008, ocean_grid)
  poly_2009 <- over(mg_2009, ocean_grid)
}
#2010s
{ poly_2010 <- over(mg_2010, ocean_grid)
  poly_2011 <- over(mg_2011, ocean_grid)
  poly_2012 <- over(mg_2012, ocean_grid)
  poly_2013 <- over(mg_2013, ocean_grid)
  poly_2014 <- over(mg_2014, ocean_grid)
  poly_2015 <- over(mg_2015, ocean_grid)
  poly_2016 <- over(mg_2016, ocean_grid)
  poly_2017 <- over(mg_2017, ocean_grid)
  poly_2018 <- over(mg_2018, ocean_grid)
  poly_2019 <- over(mg_2019, ocean_grid)
}
#2020s
{ poly_2020 <- over(mg_2020, ocean_grid)
  poly_2021 <- over(mg_2021, ocean_grid)
}

# Remove duplicate polygons each year -------------------------------------

#1980s
{ poly_1986 <- poly_1986[!duplicated(poly_1986), ]
  poly_1987 <- poly_1987[!duplicated(poly_1987), ]
  poly_1988 <- poly_1988[!duplicated(poly_1988), ]
  poly_1989 <- poly_1989[!duplicated(poly_1989), ]
}
#1990s
{ poly_1990 <- poly_1990[!duplicated(poly_1990), ]
  poly_1991 <- poly_1991[!duplicated(poly_1991), ]
  poly_1992 <- poly_1992[!duplicated(poly_1992), ]
  poly_1993 <- poly_1993[!duplicated(poly_1993), ]
  poly_1994 <- poly_1994[!duplicated(poly_1994), ]
  poly_1995 <- poly_1995[!duplicated(poly_1995), ]
  poly_1996 <- poly_1996[!duplicated(poly_1996), ]
  poly_1997 <- poly_1997[!duplicated(poly_1997), ]
  poly_1998 <- poly_1998[!duplicated(poly_1998), ]
  poly_1999 <- poly_1999[!duplicated(poly_1999), ]
}
#2000s
{ poly_2000 <- poly_2000[!duplicated(poly_2000), ]
  poly_2001 <- poly_2001[!duplicated(poly_2001), ]
  poly_2002 <- poly_2002[!duplicated(poly_2002), ]
  poly_2003 <- poly_2003[!duplicated(poly_2003), ]
  poly_2004 <- poly_2004[!duplicated(poly_2004), ]
  poly_2005 <- poly_2005[!duplicated(poly_2005), ]
  poly_2006 <- poly_2006[!duplicated(poly_2006), ]
  poly_2007 <- poly_2007[!duplicated(poly_2007), ]
  poly_2008 <- poly_2008[!duplicated(poly_2008), ]
  poly_2009 <- poly_2009[!duplicated(poly_2009), ]
}
#2010s
{ poly_2010 <- poly_2010[!duplicated(poly_2010), ]
  poly_2011 <- poly_2011[!duplicated(poly_2011), ]
  poly_2012 <- poly_2012[!duplicated(poly_2012), ]
  poly_2013 <- poly_2013[!duplicated(poly_2013), ]
  poly_2014 <- poly_2014[!duplicated(poly_2014), ]
  poly_2015 <- poly_2015[!duplicated(poly_2015), ]
  poly_2016 <- poly_2016[!duplicated(poly_2016), ]
  poly_2017 <- poly_2017[!duplicated(poly_2017), ]
  poly_2018 <- poly_2018[!duplicated(poly_2018), ]
  poly_2019 <- poly_2019[!duplicated(poly_2019), ]
}
#2020s
{ poly_2020 <- poly_2020[!duplicated(poly_2020), ]
  poly_2021 <- poly_2021[!duplicated(poly_2021), ]
}

# Remove NA values --------------------------------------------------------

#1980s
{ poly_1986 <- filter(poly_1986, !is.na(ID))
  poly_1987 <- filter(poly_1987, !is.na(ID))
  poly_1988 <- filter(poly_1988, !is.na(ID))
  poly_1989 <- filter(poly_1989, !is.na(ID))
}
#1990s
{ poly_1990 <- filter(poly_1990, !is.na(ID))
  poly_1991 <- filter(poly_1991, !is.na(ID))
  poly_1992 <- filter(poly_1992, !is.na(ID))
  poly_1993 <- filter(poly_1993, !is.na(ID))
  poly_1994 <- filter(poly_1994, !is.na(ID))
  poly_1995 <- filter(poly_1995, !is.na(ID))
  poly_1996 <- filter(poly_1996, !is.na(ID))
  poly_1997 <- filter(poly_1997, !is.na(ID))
  poly_1998 <- filter(poly_1998, !is.na(ID))
  poly_1999 <- filter(poly_1999, !is.na(ID))
}
#2000s
{ poly_2000 <- filter(poly_2000, !is.na(ID))
  poly_2001 <- filter(poly_2001, !is.na(ID))
  poly_2002 <- filter(poly_2002, !is.na(ID))
  poly_2003 <- filter(poly_2003, !is.na(ID))
  poly_2004 <- filter(poly_2004, !is.na(ID))
  poly_2005 <- filter(poly_2005, !is.na(ID))
  poly_2006 <- filter(poly_2006, !is.na(ID))
  poly_2007 <- filter(poly_2007, !is.na(ID))
  poly_2008 <- filter(poly_2008, !is.na(ID))
  poly_2009 <- filter(poly_2009, !is.na(ID))
}
#2010s
{ poly_2010 <- filter(poly_2010, !is.na(ID))
  poly_2011 <- filter(poly_2011, !is.na(ID))
  poly_2012 <- filter(poly_2012, !is.na(ID))
  poly_2013 <- filter(poly_2013, !is.na(ID))
  poly_2014 <- filter(poly_2014, !is.na(ID))
  poly_2015 <- filter(poly_2015, !is.na(ID))
  poly_2016 <- filter(poly_2016, !is.na(ID))
  poly_2017 <- filter(poly_2017, !is.na(ID))
  poly_2018 <- filter(poly_2018, !is.na(ID))
  poly_2019 <- filter(poly_2019, !is.na(ID))
}
#2020s
{ poly_2020 <- filter(poly_2020, !is.na(ID))
  poly_2021 <- filter(poly_2021, !is.na(ID))
}

# Set up blank data frame -------------------------------------------------

Area_IRE <- as.data.frame(1986:2021)
colnames(Area_IRE) <- "Year"
Area_IRE$Area <- NA

# Calculate Number of grid cells per year ----------------------------

#1980s
{ Area_IRE[1,2] <- nrow(poly_1986)
Area_IRE[2,2] <- nrow(poly_1987)
Area_IRE[3,2] <- nrow(poly_1988)
Area_IRE[4,2] <- nrow(poly_1989)
}
#1990s
{ Area_IRE[5,2] <- nrow(poly_1990)
  Area_IRE[6,2] <- nrow(poly_1991)
  Area_IRE[7,2] <- nrow(poly_1992)
  Area_IRE[8,2] <- nrow(poly_1993)
  Area_IRE[9,2] <- nrow(poly_1994)
  Area_IRE[10,2] <- nrow(poly_1995)
  Area_IRE[11,2] <- nrow(poly_1996)
  Area_IRE[12,2] <- nrow(poly_1997)
  Area_IRE[13,2] <- nrow(poly_1998)
  Area_IRE[14,2] <- nrow(poly_1999)
}
#2000s
{ Area_IRE[15,2] <- nrow(poly_2000)
  Area_IRE[16,2] <- nrow(poly_2001)
  Area_IRE[17,2] <- nrow(poly_2002)
  Area_IRE[18,2] <- nrow(poly_2003)
  Area_IRE[19,2] <- nrow(poly_2004)
  Area_IRE[20,2] <- nrow(poly_2005)
  Area_IRE[21,2] <- nrow(poly_2006)
  Area_IRE[22,2] <- nrow(poly_2007)
  Area_IRE[23,2] <- nrow(poly_2008)
  Area_IRE[24,2] <- nrow(poly_2009)
}
#2010s
{ Area_IRE[25,2] <- nrow(poly_2010)
  Area_IRE[26,2] <- nrow(poly_2011)
  Area_IRE[27,2] <- nrow(poly_2012)
  Area_IRE[28,2] <- nrow(poly_2013)
  Area_IRE[29,2] <- nrow(poly_2014)
  Area_IRE[30,2] <- nrow(poly_2015)
  Area_IRE[31,2] <- nrow(poly_2016)
  Area_IRE[32,2] <- nrow(poly_2017)
  Area_IRE[33,2] <- nrow(poly_2018)
  Area_IRE[34,2] <- nrow(poly_2019)
}
#2020s
{ Area_IRE[35,2] <- nrow(poly_2020)
  Area_IRE[36,2] <- nrow(poly_2021)
}

write.csv(Area_IRE, "./Data/Area of occupancy/Individual countries/AOO_IRE.csv")

# Plot --------------------------------------------------------------------

Area_IRE <- read.csv("./Data/Area of occupancy/Individual countries/AOO_IRE.csv")

# Make quick plot
ggplot(Area_IRE, aes(x=Year, y=Area))+
  geom_point()

# -------------------------------------------------------------------------
# ------------------------------- Netherlands -----------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

table(NET$Year)

#1980s
{ mg_1982 <- filter(NET, Year<=1982)
  mg_1983 <- filter(NET, Year<=1983)
  mg_1984 <- filter(NET, Year<=1984)
  mg_1985 <- filter(NET, Year<=1985)
  mg_1986 <- filter(NET, Year<=1986)
  mg_1987 <- filter(NET, Year<=1987)
  mg_1988 <- filter(NET, Year<=1988)
  mg_1989 <- filter(NET, Year<=1989)
}
#1990s
{ mg_1990 <- filter(NET, Year<=1990)
  mg_1991 <- filter(NET, Year<=1991)
  mg_1992 <- filter(NET, Year<=1992)
  mg_1993 <- filter(NET, Year<=1993)
  mg_1994 <- filter(NET, Year<=1994)
  mg_1995 <- filter(NET, Year<=1995)
  mg_1996 <- filter(NET, Year<=1996)
  mg_1997 <- filter(NET, Year<=1997)
  mg_1998 <- filter(NET, Year<=1998)
  mg_1999 <- filter(NET, Year<=1999)
}
#2000s
{ mg_2000 <- filter(NET, Year<=2000)
  mg_2001 <- filter(NET, Year<=2001)
  mg_2002 <- filter(NET, Year<=2002)
  mg_2003 <- filter(NET, Year<=2003)
  mg_2004 <- filter(NET, Year<=2004)
  mg_2005 <- filter(NET, Year<=2005)
  mg_2006 <- filter(NET, Year<=2006)
  mg_2007 <- filter(NET, Year<=2007)
  mg_2008 <- filter(NET, Year<=2008)
  mg_2009 <- filter(NET, Year<=2009)
}
#2010s
{ mg_2010 <- filter(NET, Year<=2010)
  mg_2011 <- filter(NET, Year<=2011)
  mg_2012 <- filter(NET, Year<=2012)
  mg_2013 <- filter(NET, Year<=2013)
  mg_2014 <- filter(NET, Year<=2014)
  mg_2015 <- filter(NET, Year<=2015)
  mg_2016 <- filter(NET, Year<=2016)
  mg_2017 <- filter(NET, Year<=2017)
  mg_2018 <- filter(NET, Year<=2018)
  mg_2019 <- filter(NET, Year<=2019)
}
#2020s
{ mg_2020 <- filter(NET, Year<=2020)
  mg_2021 <- filter(NET, Year<=2021)
}

## Separate the lon and lat values -----------------------------------------

#1980s
{ mg_1982 <-mg_1982[, 8:7]
  mg_1983 <-mg_1983[, 8:7]
  mg_1984 <-mg_1984[, 8:7]
  mg_1985 <-mg_1985[, 8:7]
  mg_1986 <-mg_1986[, 8:7]
  mg_1987 <-mg_1987[, 8:7]
  mg_1988 <-mg_1988[, 8:7]
  mg_1989 <-mg_1989[, 8:7]
}
#1990s
{ mg_1990 <- mg_1990[, 8:7]
  mg_1991 <- mg_1991[, 8:7]
  mg_1992 <- mg_1992[, 8:7]
  mg_1993 <- mg_1993[, 8:7]
  mg_1994 <- mg_1994[, 8:7]
  mg_1995 <- mg_1995[, 8:7]
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

## Convert points to spdf ---------------------------------------------------

#1980s
{ mg_1982 <- SpatialPointsDataFrame(coords=mg_1982[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1982)
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

## Move points off of land -------------------------------------------------

#2010s
{ mg_2014 <- points2nearestcell(mg_2014, bathy)
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

# Load country-specific coast grid ----------------------------------------

setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/(4) Results/(Fig 3) Spread panel/Coastal grids/Netherlands")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Calculate the number of polygons with data for each year ----------------

#1980s
{ poly_1982 <- over(mg_1982, ocean_grid)
  poly_1983 <- over(mg_1983, ocean_grid)
  poly_1984 <- over(mg_1984, ocean_grid)
  poly_1985 <- over(mg_1985, ocean_grid)
  poly_1986 <- over(mg_1986, ocean_grid)
  poly_1987 <- over(mg_1987, ocean_grid)
  poly_1988 <- over(mg_1988, ocean_grid)
  poly_1989 <- over(mg_1989, ocean_grid)
}
#1990s
{ poly_1990 <- over(mg_1990, ocean_grid)
  poly_1991 <- over(mg_1991, ocean_grid)
  poly_1992 <- over(mg_1992, ocean_grid)
  poly_1993 <- over(mg_1993, ocean_grid)
  poly_1994 <- over(mg_1994, ocean_grid)
  poly_1995 <- over(mg_1995, ocean_grid)
  poly_1996 <- over(mg_1996, ocean_grid)
  poly_1997 <- over(mg_1997, ocean_grid)
  poly_1998 <- over(mg_1998, ocean_grid)
  poly_1999 <- over(mg_1999, ocean_grid)
}
#2000s
{ poly_2000 <- over(mg_2000, ocean_grid)
  poly_2001 <- over(mg_2001, ocean_grid)
  poly_2002 <- over(mg_2002, ocean_grid)
  poly_2003 <- over(mg_2003, ocean_grid)
  poly_2004 <- over(mg_2004, ocean_grid)
  poly_2005 <- over(mg_2005, ocean_grid)
  poly_2006 <- over(mg_2006, ocean_grid)
  poly_2007 <- over(mg_2007, ocean_grid)
  poly_2008 <- over(mg_2008, ocean_grid)
  poly_2009 <- over(mg_2009, ocean_grid)
}
#2010s
{ poly_2010 <- over(mg_2010, ocean_grid)
  poly_2011 <- over(mg_2011, ocean_grid)
  poly_2012 <- over(mg_2012, ocean_grid)
  poly_2013 <- over(mg_2013, ocean_grid)
  poly_2014 <- over(mg_2014, ocean_grid)
  poly_2015 <- over(mg_2015, ocean_grid)
  poly_2016 <- over(mg_2016, ocean_grid)
  poly_2017 <- over(mg_2017, ocean_grid)
  poly_2018 <- over(mg_2018, ocean_grid)
  poly_2019 <- over(mg_2019, ocean_grid)
}
#2020s
{ poly_2020 <- over(mg_2020, ocean_grid)
  poly_2021 <- over(mg_2021, ocean_grid)
}

# Remove duplicate polygons each year -------------------------------------

#1980s
{ poly_1982 <- poly_1982[!duplicated(poly_1982), ]
  poly_1983 <- poly_1983[!duplicated(poly_1983), ]
  poly_1984 <- poly_1984[!duplicated(poly_1984), ]
  poly_1985 <- poly_1985[!duplicated(poly_1985), ]
  poly_1986 <- poly_1986[!duplicated(poly_1986), ]
  poly_1987 <- poly_1987[!duplicated(poly_1987), ]
  poly_1988 <- poly_1988[!duplicated(poly_1988), ]
  poly_1989 <- poly_1989[!duplicated(poly_1989), ]
}
#1990s
{ poly_1990 <- poly_1990[!duplicated(poly_1990), ]
  poly_1991 <- poly_1991[!duplicated(poly_1991), ]
  poly_1992 <- poly_1992[!duplicated(poly_1992), ]
  poly_1993 <- poly_1993[!duplicated(poly_1993), ]
  poly_1994 <- poly_1994[!duplicated(poly_1994), ]
  poly_1995 <- poly_1995[!duplicated(poly_1995), ]
  poly_1996 <- poly_1996[!duplicated(poly_1996), ]
  poly_1997 <- poly_1997[!duplicated(poly_1997), ]
  poly_1998 <- poly_1998[!duplicated(poly_1998), ]
  poly_1999 <- poly_1999[!duplicated(poly_1999), ]
}
#2000s
{ poly_2000 <- poly_2000[!duplicated(poly_2000), ]
  poly_2001 <- poly_2001[!duplicated(poly_2001), ]
  poly_2002 <- poly_2002[!duplicated(poly_2002), ]
  poly_2003 <- poly_2003[!duplicated(poly_2003), ]
  poly_2004 <- poly_2004[!duplicated(poly_2004), ]
  poly_2005 <- poly_2005[!duplicated(poly_2005), ]
  poly_2006 <- poly_2006[!duplicated(poly_2006), ]
  poly_2007 <- poly_2007[!duplicated(poly_2007), ]
  poly_2008 <- poly_2008[!duplicated(poly_2008), ]
  poly_2009 <- poly_2009[!duplicated(poly_2009), ]
}
#2010s
{ poly_2010 <- poly_2010[!duplicated(poly_2010), ]
  poly_2011 <- poly_2011[!duplicated(poly_2011), ]
  poly_2012 <- poly_2012[!duplicated(poly_2012), ]
  poly_2013 <- poly_2013[!duplicated(poly_2013), ]
  poly_2014 <- poly_2014[!duplicated(poly_2014), ]
  poly_2015 <- poly_2015[!duplicated(poly_2015), ]
  poly_2016 <- poly_2016[!duplicated(poly_2016), ]
  poly_2017 <- poly_2017[!duplicated(poly_2017), ]
  poly_2018 <- poly_2018[!duplicated(poly_2018), ]
  poly_2019 <- poly_2019[!duplicated(poly_2019), ]
}
#2020s
{ poly_2020 <- poly_2020[!duplicated(poly_2020), ]
  poly_2021 <- poly_2021[!duplicated(poly_2021), ]
}

# Remove NA values --------------------------------------------------------

#1980s
{ poly_1982 <- filter(poly_1982, !is.na(ID))
  poly_1983 <- filter(poly_1983, !is.na(ID))
  poly_1984 <- filter(poly_1984, !is.na(ID))
  poly_1985 <- filter(poly_1985, !is.na(ID))
  poly_1986 <- filter(poly_1986, !is.na(ID))
  poly_1987 <- filter(poly_1987, !is.na(ID))
  poly_1988 <- filter(poly_1988, !is.na(ID))
  poly_1989 <- filter(poly_1989, !is.na(ID))
}
#1990s
{ poly_1990 <- filter(poly_1990, !is.na(ID))
  poly_1991 <- filter(poly_1991, !is.na(ID))
  poly_1992 <- filter(poly_1992, !is.na(ID))
  poly_1993 <- filter(poly_1993, !is.na(ID))
  poly_1994 <- filter(poly_1994, !is.na(ID))
  poly_1995 <- filter(poly_1995, !is.na(ID))
  poly_1996 <- filter(poly_1996, !is.na(ID))
  poly_1997 <- filter(poly_1997, !is.na(ID))
  poly_1998 <- filter(poly_1998, !is.na(ID))
  poly_1999 <- filter(poly_1999, !is.na(ID))
}
#2000s
{ poly_2000 <- filter(poly_2000, !is.na(ID))
  poly_2001 <- filter(poly_2001, !is.na(ID))
  poly_2002 <- filter(poly_2002, !is.na(ID))
  poly_2003 <- filter(poly_2003, !is.na(ID))
  poly_2004 <- filter(poly_2004, !is.na(ID))
  poly_2005 <- filter(poly_2005, !is.na(ID))
  poly_2006 <- filter(poly_2006, !is.na(ID))
  poly_2007 <- filter(poly_2007, !is.na(ID))
  poly_2008 <- filter(poly_2008, !is.na(ID))
  poly_2009 <- filter(poly_2009, !is.na(ID))
}
#2010s
{ poly_2010 <- filter(poly_2010, !is.na(ID))
  poly_2011 <- filter(poly_2011, !is.na(ID))
  poly_2012 <- filter(poly_2012, !is.na(ID))
  poly_2013 <- filter(poly_2013, !is.na(ID))
  poly_2014 <- filter(poly_2014, !is.na(ID))
  poly_2015 <- filter(poly_2015, !is.na(ID))
  poly_2016 <- filter(poly_2016, !is.na(ID))
  poly_2017 <- filter(poly_2017, !is.na(ID))
  poly_2018 <- filter(poly_2018, !is.na(ID))
  poly_2019 <- filter(poly_2019, !is.na(ID))
}
#2020s
{ poly_2020 <- filter(poly_2020, !is.na(ID))
  poly_2021 <- filter(poly_2021, !is.na(ID))
}

# Set up blank data frame -------------------------------------------------

Area_NET <- as.data.frame(1982:2021)
colnames(Area_NET) <- "Year"
Area_NET$Area <- NA

# Calculate Number of grid cells per year ----------------------------

#1980s
{ Area_NET[1,2] <- nrow(poly_1982)
Area_NET[2,2] <- nrow(poly_1983)
Area_NET[3,2] <- nrow(poly_1984)
Area_NET[4,2] <- nrow(poly_1985)
Area_NET[5,2] <- nrow(poly_1986)
Area_NET[6,2] <- nrow(poly_1987)
Area_NET[7,2] <- nrow(poly_1988)
Area_NET[8,2] <- nrow(poly_1989)
}
#1990s
{ Area_NET[9,2] <- nrow(poly_1990)
  Area_NET[10,2] <- nrow(poly_1991)
  Area_NET[11,2] <- nrow(poly_1992)
  Area_NET[12,2] <- nrow(poly_1993)
  Area_NET[13,2] <- nrow(poly_1994)
  Area_NET[14,2] <- nrow(poly_1995)
  Area_NET[15,2] <- nrow(poly_1996)
  Area_NET[16,2] <- nrow(poly_1997)
  Area_NET[17,2] <- nrow(poly_1998)
  Area_NET[18,2] <- nrow(poly_1999)
}
#2000s
{ Area_NET[19,2] <- nrow(poly_2000)
  Area_NET[20,2] <- nrow(poly_2001)
  Area_NET[21,2] <- nrow(poly_2002)
  Area_NET[22,2] <- nrow(poly_2003)
  Area_NET[23,2] <- nrow(poly_2004)
  Area_NET[24,2] <- nrow(poly_2005)
  Area_NET[25,2] <- nrow(poly_2006)
  Area_NET[26,2] <- nrow(poly_2007)
  Area_NET[27,2] <- nrow(poly_2008)
  Area_NET[28,2] <- nrow(poly_2009)
}
#2010s
{ Area_NET[29,2] <- nrow(poly_2010)
  Area_NET[30,2] <- nrow(poly_2011)
  Area_NET[31,2] <- nrow(poly_2012)
  Area_NET[32,2] <- nrow(poly_2013)
  Area_NET[33,2] <- nrow(poly_2014)
  Area_NET[34,2] <- nrow(poly_2015)
  Area_NET[35,2] <- nrow(poly_2016)
  Area_NET[36,2] <- nrow(poly_2017)
  Area_NET[37,2] <- nrow(poly_2018)
  Area_NET[38,2] <- nrow(poly_2019)
}
#2020s
{ Area_NET[39,2] <- nrow(poly_2020)
  Area_NET[40,2] <- nrow(poly_2021)
}

write.csv(Area_NET, "./Data/Area of occupancy/Individual countries/AOO_NET.csv")

# Plot --------------------------------------------------------------------

Area_NET <- read.csv("./Data/Area of occupancy/Individual countries/AOO_NET.csv")

# Make quick plot
ggplot(Area_NET, aes(x=Year, y=Area))+
  geom_point()

# -------------------------------------------------------------------------
# --------------------------------- Norway --------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

table(NOR$Year)

#2000s
{ mg_2001 <- filter(NOR, Year<=2001)
  mg_2002 <- filter(NOR, Year<=2002)
  mg_2003 <- filter(NOR, Year<=2003)
  mg_2004 <- filter(NOR, Year<=2004)
  mg_2005 <- filter(NOR, Year<=2005)
  mg_2006 <- filter(NOR, Year<=2006)
  mg_2007 <- filter(NOR, Year<=2007)
  mg_2008 <- filter(NOR, Year<=2008)
  mg_2009 <- filter(NOR, Year<=2009)
}
#2010s
{ mg_2010 <- filter(NOR, Year<=2010)
  mg_2011 <- filter(NOR, Year<=2011)
  mg_2012 <- filter(NOR, Year<=2012)
  mg_2013 <- filter(NOR, Year<=2013)
  mg_2014 <- filter(NOR, Year<=2014)
  mg_2015 <- filter(NOR, Year<=2015)
  mg_2016 <- filter(NOR, Year<=2016)
  mg_2017 <- filter(NOR, Year<=2017)
  mg_2018 <- filter(NOR, Year<=2018)
  mg_2019 <- filter(NOR, Year<=2019)
}
#2020s
{ mg_2020 <- filter(NOR, Year<=2020)
  mg_2021 <- filter(NOR, Year<=2021)
}

## Separate the lon and lat values -----------------------------------------

#2000s
{ mg_2001 <- mg_2001[, 8:7]
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

## Convert points to spdf ---------------------------------------------------

#2000s
{ mg_2001 <- SpatialPointsDataFrame(coords=mg_2001[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_2001)
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

## Move points off of land -------------------------------------------------

#2010s
{ mg_2016 <- points2nearestcell(mg_2016, bathy)
  mg_2017 <- points2nearestcell(mg_2017, bathy)
  mg_2018 <- points2nearestcell(mg_2018, bathy)
  mg_2019 <- points2nearestcell(mg_2019, bathy)
}
#2020s
{ mg_2020 <- points2nearestcell(mg_2020, bathy)
  mg_2021 <- points2nearestcell(mg_2021, bathy)
}

# Load country-specific coast grid ----------------------------------------

setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/(4) Results/(Fig 3) Spread panel/Coastal grids/Norway")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Calculate the number of polygons with data for each year ----------------

#2000s
{ poly_2001 <- over(mg_2001, ocean_grid)
  poly_2002 <- over(mg_2002, ocean_grid)
  poly_2003 <- over(mg_2003, ocean_grid)
  poly_2004 <- over(mg_2004, ocean_grid)
  poly_2005 <- over(mg_2005, ocean_grid)
  poly_2006 <- over(mg_2006, ocean_grid)
  poly_2007 <- over(mg_2007, ocean_grid)
  poly_2008 <- over(mg_2008, ocean_grid)
  poly_2009 <- over(mg_2009, ocean_grid)
}
#2010s
{ poly_2010 <- over(mg_2010, ocean_grid)
  poly_2011 <- over(mg_2011, ocean_grid)
  poly_2012 <- over(mg_2012, ocean_grid)
  poly_2013 <- over(mg_2013, ocean_grid)
  poly_2014 <- over(mg_2014, ocean_grid)
  poly_2015 <- over(mg_2015, ocean_grid)
  poly_2016 <- over(mg_2016, ocean_grid)
  poly_2017 <- over(mg_2017, ocean_grid)
  poly_2018 <- over(mg_2018, ocean_grid)
  poly_2019 <- over(mg_2019, ocean_grid)
}
#2020s
{ poly_2020 <- over(mg_2020, ocean_grid)
  poly_2021 <- over(mg_2021, ocean_grid)
}

# Remove duplicate polygons each year -------------------------------------

#2000s
{ poly_2001 <- poly_2001[!duplicated(poly_2001), ]
  poly_2002 <- poly_2002[!duplicated(poly_2002), ]
  poly_2003 <- poly_2003[!duplicated(poly_2003), ]
  poly_2004 <- poly_2004[!duplicated(poly_2004), ]
  poly_2005 <- poly_2005[!duplicated(poly_2005), ]
  poly_2006 <- poly_2006[!duplicated(poly_2006), ]
  poly_2007 <- poly_2007[!duplicated(poly_2007), ]
  poly_2008 <- poly_2008[!duplicated(poly_2008), ]
  poly_2009 <- poly_2009[!duplicated(poly_2009), ]
}
#2010s
{ poly_2010 <- poly_2010[!duplicated(poly_2010), ]
  poly_2011 <- poly_2011[!duplicated(poly_2011), ]
  poly_2012 <- poly_2012[!duplicated(poly_2012), ]
  poly_2013 <- poly_2013[!duplicated(poly_2013), ]
  poly_2014 <- poly_2014[!duplicated(poly_2014), ]
  poly_2015 <- poly_2015[!duplicated(poly_2015), ]
  poly_2016 <- poly_2016[!duplicated(poly_2016), ]
  poly_2017 <- poly_2017[!duplicated(poly_2017), ]
  poly_2018 <- poly_2018[!duplicated(poly_2018), ]
  poly_2019 <- poly_2019[!duplicated(poly_2019), ]
}
#2020s
{ poly_2020 <- poly_2020[!duplicated(poly_2020), ]
  poly_2021 <- poly_2021[!duplicated(poly_2021), ]
}

# Remove NA values --------------------------------------------------------

#2000s
{ poly_2001 <- filter(poly_2001, !is.na(ID))
  poly_2002 <- filter(poly_2002, !is.na(ID))
  poly_2003 <- filter(poly_2003, !is.na(ID))
  poly_2004 <- filter(poly_2004, !is.na(ID))
  poly_2005 <- filter(poly_2005, !is.na(ID))
  poly_2006 <- filter(poly_2006, !is.na(ID))
  poly_2007 <- filter(poly_2007, !is.na(ID))
  poly_2008 <- filter(poly_2008, !is.na(ID))
  poly_2009 <- filter(poly_2009, !is.na(ID))
}
#2010s
{ poly_2010 <- filter(poly_2010, !is.na(ID))
  poly_2011 <- filter(poly_2011, !is.na(ID))
  poly_2012 <- filter(poly_2012, !is.na(ID))
  poly_2013 <- filter(poly_2013, !is.na(ID))
  poly_2014 <- filter(poly_2014, !is.na(ID))
  poly_2015 <- filter(poly_2015, !is.na(ID))
  poly_2016 <- filter(poly_2016, !is.na(ID))
  poly_2017 <- filter(poly_2017, !is.na(ID))
  poly_2018 <- filter(poly_2018, !is.na(ID))
  poly_2019 <- filter(poly_2019, !is.na(ID))
}
#2020s
{ poly_2020 <- filter(poly_2020, !is.na(ID))
  poly_2021 <- filter(poly_2021, !is.na(ID))
}

# Set up blank data frame -------------------------------------------------

Area_NOR <- as.data.frame(2001:2021)
colnames(Area_NOR) <- "Year"
Area_NOR$Area <- NA

# Calculate Number of grid cells per year ----------------------------

#2000s
{ Area_NOR[1,2] <- nrow(poly_2001)
Area_NOR[2,2] <- nrow(poly_2002)
Area_NOR[3,2] <- nrow(poly_2003)
Area_NOR[4,2] <- nrow(poly_2004)
Area_NOR[5,2] <- nrow(poly_2005)
Area_NOR[6,2] <- nrow(poly_2006)
Area_NOR[7,2] <- nrow(poly_2007)
Area_NOR[8,2] <- nrow(poly_2008)
Area_NOR[9,2] <- nrow(poly_2009)
}
#2010s
{ Area_NOR[10,2] <- nrow(poly_2010)
  Area_NOR[11,2] <- nrow(poly_2011)
  Area_NOR[12,2] <- nrow(poly_2012)
  Area_NOR[13,2] <- nrow(poly_2013)
  Area_NOR[14,2] <- nrow(poly_2014)
  Area_NOR[15,2] <- nrow(poly_2015)
  Area_NOR[16,2] <- nrow(poly_2016)
  Area_NOR[17,2] <- nrow(poly_2017)
  Area_NOR[18,2] <- nrow(poly_2018)
  Area_NOR[19,2] <- nrow(poly_2019)
}
#2020s
{ Area_NOR[20,2] <- nrow(poly_2020)
  Area_NOR[21,2] <- nrow(poly_2021)
}

write.csv(Area_NOR, "./Data/Area of occupancy/Individual countries/AOO_NOR.csv")

# Plot --------------------------------------------------------------------

Area_NOR <- read.csv("./Data/Area of occupancy/Individual countries/AOO_NOR.csv")

# Make quick plot
ggplot(Area_NOR, aes(x=Year, y=Area))+
  geom_point()

# -------------------------------------------------------------------------
# ---------------------------------- Sweden -------------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

table(SWE$Year)

#2000s
{ mg_2007 <- filter(SWE, Year<=2007)
  mg_2008 <- filter(SWE, Year<=2008)
  mg_2009 <- filter(SWE, Year<=2009)
}
#2010s
{ mg_2010 <- filter(SWE, Year<=2010)
  mg_2011 <- filter(SWE, Year<=2011)
  mg_2012 <- filter(SWE, Year<=2012)
  mg_2013 <- filter(SWE, Year<=2013)
  mg_2014 <- filter(SWE, Year<=2014)
  mg_2015 <- filter(SWE, Year<=2015)
  mg_2016 <- filter(SWE, Year<=2016)
  mg_2017 <- filter(SWE, Year<=2017)
  mg_2018 <- filter(SWE, Year<=2018)
  mg_2019 <- filter(SWE, Year<=2019)
}
#2020s
{ mg_2020 <- filter(SWE, Year<=2020)
  mg_2021 <- filter(SWE, Year<=2021)
}

## Separate the lon and lat values -----------------------------------------

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

## Move points off of land -------------------------------------------------

#2020s
{ mg_2021 <- points2nearestcell(mg_2021, bathy)
}

# Load country-specific coast grid ----------------------------------------

setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/(4) Results/(Fig 3) Spread panel/Coastal grids/Sweden")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Calculate the number of polygons with data for each year ----------------

#2000s
{ poly_2007 <- over(mg_2007, ocean_grid)
poly_2008 <- over(mg_2008, ocean_grid)
poly_2009 <- over(mg_2009, ocean_grid)
}
#2010s
{ poly_2010 <- over(mg_2010, ocean_grid)
  poly_2011 <- over(mg_2011, ocean_grid)
  poly_2012 <- over(mg_2012, ocean_grid)
  poly_2013 <- over(mg_2013, ocean_grid)
  poly_2014 <- over(mg_2014, ocean_grid)
  poly_2015 <- over(mg_2015, ocean_grid)
  poly_2016 <- over(mg_2016, ocean_grid)
  poly_2017 <- over(mg_2017, ocean_grid)
  poly_2018 <- over(mg_2018, ocean_grid)
  poly_2019 <- over(mg_2019, ocean_grid)
}
#2020s
{ poly_2020 <- over(mg_2020, ocean_grid)
  poly_2021 <- over(mg_2021, ocean_grid)
}

# Remove duplicate polygons each year -------------------------------------

#2000s
{ poly_2007 <- poly_2007[!duplicated(poly_2007), ]
poly_2008 <- poly_2008[!duplicated(poly_2008), ]
poly_2009 <- poly_2009[!duplicated(poly_2009), ]
}
#2010s
{ poly_2010 <- poly_2010[!duplicated(poly_2010), ]
  poly_2011 <- poly_2011[!duplicated(poly_2011), ]
  poly_2012 <- poly_2012[!duplicated(poly_2012), ]
  poly_2013 <- poly_2013[!duplicated(poly_2013), ]
  poly_2014 <- poly_2014[!duplicated(poly_2014), ]
  poly_2015 <- poly_2015[!duplicated(poly_2015), ]
  poly_2016 <- poly_2016[!duplicated(poly_2016), ]
  poly_2017 <- poly_2017[!duplicated(poly_2017), ]
  poly_2018 <- poly_2018[!duplicated(poly_2018), ]
  poly_2019 <- poly_2019[!duplicated(poly_2019), ]
}
#2020s
{ poly_2020 <- poly_2020[!duplicated(poly_2020), ]
  poly_2021 <- poly_2021[!duplicated(poly_2021), ]
}

# Remove NA values --------------------------------------------------------

#2000s
{ poly_2007 <- filter(poly_2007, !is.na(ID))
poly_2008 <- filter(poly_2008, !is.na(ID))
poly_2009 <- filter(poly_2009, !is.na(ID))
}
#2010s
{ poly_2010 <- filter(poly_2010, !is.na(ID))
  poly_2011 <- filter(poly_2011, !is.na(ID))
  poly_2012 <- filter(poly_2012, !is.na(ID))
  poly_2013 <- filter(poly_2013, !is.na(ID))
  poly_2014 <- filter(poly_2014, !is.na(ID))
  poly_2015 <- filter(poly_2015, !is.na(ID))
  poly_2016 <- filter(poly_2016, !is.na(ID))
  poly_2017 <- filter(poly_2017, !is.na(ID))
  poly_2018 <- filter(poly_2018, !is.na(ID))
  poly_2019 <- filter(poly_2019, !is.na(ID))
}
#2020s
{ poly_2020 <- filter(poly_2020, !is.na(ID))
  poly_2021 <- filter(poly_2021, !is.na(ID))
}

# Set up blank data frame -------------------------------------------------

Area_SWE <- as.data.frame(2007:2021)
colnames(Area_SWE) <- "Year"
Area_SWE$Area <- NA

# Calculate Number of grid cells per year ----------------------------

#2000s
{ Area_SWE[1,2] <- nrow(poly_2007)
Area_SWE[2,2] <- nrow(poly_2008)
Area_SWE[3,2] <- nrow(poly_2009)
}
#2010s
{ Area_SWE[4,2] <- nrow(poly_2010)
  Area_SWE[5,2] <- nrow(poly_2011)
  Area_SWE[6,2] <- nrow(poly_2012)
  Area_SWE[7,2] <- nrow(poly_2013)
  Area_SWE[8,2] <- nrow(poly_2014)
  Area_SWE[9,2] <- nrow(poly_2015)
  Area_SWE[10,2] <- nrow(poly_2016)
  Area_SWE[11,2] <- nrow(poly_2017)
  Area_SWE[12,2] <- nrow(poly_2018)
  Area_SWE[13,2] <- nrow(poly_2019)
}
#2020s
{ Area_SWE[14,2] <- nrow(poly_2020)
  Area_SWE[15,2] <- nrow(poly_2021)
}

write.csv(Area_SWE, "./Data/Area of occupancy/Individual countries/AOO_SWE.csv")

# Plot --------------------------------------------------------------------

Area_SWE <- read.csv("./Data/Area of occupancy/Individual countries/AOO_SWE.csv")

# Make quick plot
ggplot(Area_SWE, aes(x=Year, y=Area))+
  geom_point()

# -------------------------------------------------------------------------
# ----------------------------- United Kingdom ----------------------------
# -------------------------------------------------------------------------
## Subset the data by year -------------------------------------------------

table(UK$Year)

#1970s
{ mg_1974 <- filter(UK, Year<=1974)
  mg_1975 <- filter(UK, Year<=1975)
  mg_1976 <- filter(UK, Year<=1976)
  mg_1977 <- filter(UK, Year<=1977)
mg_1978 <- filter(UK, Year<=1978)
mg_1979 <- filter(UK, Year<=1979)
}
#1980s
{ mg_1980 <- filter(UK, Year<=1980)
  mg_1981 <- filter(UK, Year<=1981)
  mg_1982 <- filter(UK, Year<=1982)
  mg_1983 <- filter(UK, Year<=1983)
  mg_1984 <- filter(UK, Year<=1984)
  mg_1985 <- filter(UK, Year<=1985)
  mg_1986 <- filter(UK, Year<=1986)
  mg_1987 <- filter(UK, Year<=1987)
  mg_1988 <- filter(UK, Year<=1988)
  mg_1989 <- filter(UK, Year<=1989)
}
#1990s
{ mg_1990 <- filter(UK, Year<=1990)
  mg_1991 <- filter(UK, Year<=1991)
  mg_1992 <- filter(UK, Year<=1992)
  mg_1993 <- filter(UK, Year<=1993)
  mg_1994 <- filter(UK, Year<=1994)
  mg_1995 <- filter(UK, Year<=1995)
  mg_1996 <- filter(UK, Year<=1996)
  mg_1997 <- filter(UK, Year<=1997)
  mg_1998 <- filter(UK, Year<=1998)
  mg_1999 <- filter(UK, Year<=1999)
}
#2000s
{ mg_2000 <- filter(UK, Year<=2000)
  mg_2001 <- filter(UK, Year<=2001)
  mg_2002 <- filter(UK, Year<=2002)
  mg_2003 <- filter(UK, Year<=2003)
  mg_2004 <- filter(UK, Year<=2004)
  mg_2005 <- filter(UK, Year<=2005)
  mg_2006 <- filter(UK, Year<=2006)
  mg_2007 <- filter(UK, Year<=2007)
  mg_2008 <- filter(UK, Year<=2008)
  mg_2009 <- filter(UK, Year<=2009)
}
#2010s
{ mg_2010 <- filter(UK, Year<=2010)
  mg_2011 <- filter(UK, Year<=2011)
  mg_2012 <- filter(UK, Year<=2012)
  mg_2013 <- filter(UK, Year<=2013)
  mg_2014 <- filter(UK, Year<=2014)
  mg_2015 <- filter(UK, Year<=2015)
  mg_2016 <- filter(UK, Year<=2016)
  mg_2017 <- filter(UK, Year<=2017)
  mg_2018 <- filter(UK, Year<=2018)
  mg_2019 <- filter(UK, Year<=2019)
}
#2020s
{ mg_2020 <- filter(UK, Year<=2020)
  mg_2021 <- filter(UK, Year<=2021)
}

## Separate the lon and lat values -----------------------------------------

#1970s
{ mg_1974 <- mg_1974[, 8:7]
mg_1975 <- mg_1975[, 8:7]
mg_1976 <- mg_1976[, 8:7]
mg_1977 <- mg_1977[, 8:7]
mg_1978 <- mg_1978[, 8:7]
mg_1979 <- mg_1979[, 8:7]
}
#1980s
{ mg_1980 <-mg_1980[, 8:7]
  mg_1981 <-mg_1981[, 8:7]
  mg_1982 <-mg_1982[, 8:7]
  mg_1983 <-mg_1983[, 8:7]
  mg_1984 <-mg_1984[, 8:7]
  mg_1985 <-mg_1985[, 8:7]
  mg_1986 <-mg_1986[, 8:7]
  mg_1987 <-mg_1987[, 8:7]
  mg_1988 <-mg_1988[, 8:7]
  mg_1989 <-mg_1989[, 8:7]
}
#1990s
{ mg_1990 <- mg_1990[, 8:7]
  mg_1991 <- mg_1991[, 8:7]
  mg_1992 <- mg_1992[, 8:7]
  mg_1993 <- mg_1993[, 8:7]
  mg_1994 <- mg_1994[, 8:7]
  mg_1995 <- mg_1995[, 8:7]
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

## Convert points to spdf ---------------------------------------------------

#1970s
{ mg_1974 <- SpatialPointsDataFrame(coords=mg_1974[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1974)
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

## Move points off of land -------------------------------------------------

#1990s
{ mg_1998 <- points2nearestcell(mg_1998, bathy)
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

# Load country-specific coast grid ----------------------------------------

setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/(4) Results/(Fig 3) Spread panel/Coastal grids/United Kingdom")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Calculate the number of polygons with data for each year ----------------

#1970s
{ poly_1974 <- over(mg_1974, ocean_grid)
  poly_1975 <- over(mg_1975, ocean_grid)
  poly_1976 <- over(mg_1976, ocean_grid)
  poly_1977 <- over(mg_1977, ocean_grid)
  poly_1978 <- over(mg_1978, ocean_grid)
  poly_1979 <- over(mg_1979, ocean_grid)
}
#1980s
{ poly_1980 <- over(mg_1980, ocean_grid)
  poly_1981 <- over(mg_1981, ocean_grid)
  poly_1982 <- over(mg_1982, ocean_grid)
  poly_1983 <- over(mg_1983, ocean_grid)
  poly_1984 <- over(mg_1984, ocean_grid)
  poly_1985 <- over(mg_1985, ocean_grid)
  poly_1986 <- over(mg_1986, ocean_grid)
  poly_1987 <- over(mg_1987, ocean_grid)
  poly_1988 <- over(mg_1988, ocean_grid)
  poly_1989 <- over(mg_1989, ocean_grid)
}
#1990s
{ poly_1990 <- over(mg_1990, ocean_grid)
  poly_1991 <- over(mg_1991, ocean_grid)
  poly_1992 <- over(mg_1992, ocean_grid)
  poly_1993 <- over(mg_1993, ocean_grid)
  poly_1994 <- over(mg_1994, ocean_grid)
  poly_1995 <- over(mg_1995, ocean_grid)
  poly_1996 <- over(mg_1996, ocean_grid)
  poly_1997 <- over(mg_1997, ocean_grid)
  poly_1998 <- over(mg_1998, ocean_grid)
  poly_1999 <- over(mg_1999, ocean_grid)
}
#2000s
{ poly_2000 <- over(mg_2000, ocean_grid)
  poly_2001 <- over(mg_2001, ocean_grid)
  poly_2002 <- over(mg_2002, ocean_grid)
  poly_2003 <- over(mg_2003, ocean_grid)
  poly_2004 <- over(mg_2004, ocean_grid)
  poly_2005 <- over(mg_2005, ocean_grid)
  poly_2006 <- over(mg_2006, ocean_grid)
  poly_2007 <- over(mg_2007, ocean_grid)
  poly_2008 <- over(mg_2008, ocean_grid)
  poly_2009 <- over(mg_2009, ocean_grid)
}
#2010s
{ poly_2010 <- over(mg_2010, ocean_grid)
  poly_2011 <- over(mg_2011, ocean_grid)
  poly_2012 <- over(mg_2012, ocean_grid)
  poly_2013 <- over(mg_2013, ocean_grid)
  poly_2014 <- over(mg_2014, ocean_grid)
  poly_2015 <- over(mg_2015, ocean_grid)
  poly_2016 <- over(mg_2016, ocean_grid)
  poly_2017 <- over(mg_2017, ocean_grid)
  poly_2018 <- over(mg_2018, ocean_grid)
  poly_2019 <- over(mg_2019, ocean_grid)
}
#2020s
{ poly_2020 <- over(mg_2020, ocean_grid)
  poly_2021 <- over(mg_2021, ocean_grid)
}

# Remove duplicate polygons each year -------------------------------------

#1970s
{ poly_1974 <- poly_1974[!duplicated(poly_1974), ]
  poly_1975 <- poly_1975[!duplicated(poly_1975), ]
  poly_1976 <- poly_1976[!duplicated(poly_1976), ]
  poly_1977 <- poly_1977[!duplicated(poly_1977), ]
  poly_1978 <- poly_1978[!duplicated(poly_1978), ]
  poly_1979 <- poly_1979[!duplicated(poly_1979), ]
}
#1980s
{ poly_1980 <- poly_1980[!duplicated(poly_1980), ]
  poly_1981 <- poly_1981[!duplicated(poly_1981), ]
  poly_1982 <- poly_1982[!duplicated(poly_1982), ]
  poly_1983 <- poly_1983[!duplicated(poly_1983), ]
  poly_1984 <- poly_1984[!duplicated(poly_1984), ]
  poly_1985 <- poly_1985[!duplicated(poly_1985), ]
  poly_1986 <- poly_1986[!duplicated(poly_1986), ]
  poly_1987 <- poly_1987[!duplicated(poly_1987), ]
  poly_1988 <- poly_1988[!duplicated(poly_1988), ]
  poly_1989 <- poly_1989[!duplicated(poly_1989), ]
}
#1990s
{ poly_1990 <- poly_1990[!duplicated(poly_1990), ]
  poly_1991 <- poly_1991[!duplicated(poly_1991), ]
  poly_1992 <- poly_1992[!duplicated(poly_1992), ]
  poly_1993 <- poly_1993[!duplicated(poly_1993), ]
  poly_1994 <- poly_1994[!duplicated(poly_1994), ]
  poly_1995 <- poly_1995[!duplicated(poly_1995), ]
  poly_1996 <- poly_1996[!duplicated(poly_1996), ]
  poly_1997 <- poly_1997[!duplicated(poly_1997), ]
  poly_1998 <- poly_1998[!duplicated(poly_1998), ]
  poly_1999 <- poly_1999[!duplicated(poly_1999), ]
}
#2000s
{ poly_2000 <- poly_2000[!duplicated(poly_2000), ]
  poly_2001 <- poly_2001[!duplicated(poly_2001), ]
  poly_2002 <- poly_2002[!duplicated(poly_2002), ]
  poly_2003 <- poly_2003[!duplicated(poly_2003), ]
  poly_2004 <- poly_2004[!duplicated(poly_2004), ]
  poly_2005 <- poly_2005[!duplicated(poly_2005), ]
  poly_2006 <- poly_2006[!duplicated(poly_2006), ]
  poly_2007 <- poly_2007[!duplicated(poly_2007), ]
  poly_2008 <- poly_2008[!duplicated(poly_2008), ]
  poly_2009 <- poly_2009[!duplicated(poly_2009), ]
}
#2010s
{ poly_2010 <- poly_2010[!duplicated(poly_2010), ]
  poly_2011 <- poly_2011[!duplicated(poly_2011), ]
  poly_2012 <- poly_2012[!duplicated(poly_2012), ]
  poly_2013 <- poly_2013[!duplicated(poly_2013), ]
  poly_2014 <- poly_2014[!duplicated(poly_2014), ]
  poly_2015 <- poly_2015[!duplicated(poly_2015), ]
  poly_2016 <- poly_2016[!duplicated(poly_2016), ]
  poly_2017 <- poly_2017[!duplicated(poly_2017), ]
  poly_2018 <- poly_2018[!duplicated(poly_2018), ]
  poly_2019 <- poly_2019[!duplicated(poly_2019), ]
}
#2020s
{ poly_2020 <- poly_2020[!duplicated(poly_2020), ]
  poly_2021 <- poly_2021[!duplicated(poly_2021), ]
}

# Remove NA values --------------------------------------------------------

#1970s
{ poly_1974 <- filter(poly_1974, !is.na(ID))
  poly_1975 <- filter(poly_1975, !is.na(ID))
  poly_1976 <- filter(poly_1976, !is.na(ID))
  poly_1977 <- filter(poly_1977, !is.na(ID))
  poly_1978 <- filter(poly_1978, !is.na(ID))
  poly_1979 <- filter(poly_1979, !is.na(ID))
}
#1980s
{ poly_1980 <- filter(poly_1980, !is.na(ID))
  poly_1981 <- filter(poly_1981, !is.na(ID))
  poly_1982 <- filter(poly_1982, !is.na(ID))
  poly_1983 <- filter(poly_1983, !is.na(ID))
  poly_1984 <- filter(poly_1984, !is.na(ID))
  poly_1985 <- filter(poly_1985, !is.na(ID))
  poly_1986 <- filter(poly_1986, !is.na(ID))
  poly_1987 <- filter(poly_1987, !is.na(ID))
  poly_1988 <- filter(poly_1988, !is.na(ID))
  poly_1989 <- filter(poly_1989, !is.na(ID))
}
#1990s
{ poly_1990 <- filter(poly_1990, !is.na(ID))
  poly_1991 <- filter(poly_1991, !is.na(ID))
  poly_1992 <- filter(poly_1992, !is.na(ID))
  poly_1993 <- filter(poly_1993, !is.na(ID))
  poly_1994 <- filter(poly_1994, !is.na(ID))
  poly_1995 <- filter(poly_1995, !is.na(ID))
  poly_1996 <- filter(poly_1996, !is.na(ID))
  poly_1997 <- filter(poly_1997, !is.na(ID))
  poly_1998 <- filter(poly_1998, !is.na(ID))
  poly_1999 <- filter(poly_1999, !is.na(ID))
}
#2000s
{ poly_2000 <- filter(poly_2000, !is.na(ID))
  poly_2001 <- filter(poly_2001, !is.na(ID))
  poly_2002 <- filter(poly_2002, !is.na(ID))
  poly_2003 <- filter(poly_2003, !is.na(ID))
  poly_2004 <- filter(poly_2004, !is.na(ID))
  poly_2005 <- filter(poly_2005, !is.na(ID))
  poly_2006 <- filter(poly_2006, !is.na(ID))
  poly_2007 <- filter(poly_2007, !is.na(ID))
  poly_2008 <- filter(poly_2008, !is.na(ID))
  poly_2009 <- filter(poly_2009, !is.na(ID))
}
#2010s
{ poly_2010 <- filter(poly_2010, !is.na(ID))
  poly_2011 <- filter(poly_2011, !is.na(ID))
  poly_2012 <- filter(poly_2012, !is.na(ID))
  poly_2013 <- filter(poly_2013, !is.na(ID))
  poly_2014 <- filter(poly_2014, !is.na(ID))
  poly_2015 <- filter(poly_2015, !is.na(ID))
  poly_2016 <- filter(poly_2016, !is.na(ID))
  poly_2017 <- filter(poly_2017, !is.na(ID))
  poly_2018 <- filter(poly_2018, !is.na(ID))
  poly_2019 <- filter(poly_2019, !is.na(ID))
}
#2020s
{ poly_2020 <- filter(poly_2020, !is.na(ID))
  poly_2021 <- filter(poly_2021, !is.na(ID))
}

# Set up blank data frame -------------------------------------------------

Area_UK <- as.data.frame(1974:2021)
colnames(Area_UK) <- "Year"
Area_UK$Area <- NA

# Calculate Number of grid cells per year ----------------------------

#1970s
{ Area_UK[1,2] <- nrow(poly_1974)
Area_UK[2,2] <- nrow(poly_1975)
Area_UK[3,2] <- nrow(poly_1976)
Area_UK[4,2] <- nrow(poly_1977)
Area_UK[5,2] <- nrow(poly_1978)
Area_UK[6,2] <- nrow(poly_1979)
}
#1980s
{ Area_UK[7,2] <- nrow(poly_1980)
  Area_UK[8,2] <- nrow(poly_1981)
  Area_UK[9,2] <- nrow(poly_1982)
  Area_UK[10,2] <- nrow(poly_1983)
  Area_UK[11,2] <- nrow(poly_1984)
  Area_UK[12,2] <- nrow(poly_1985)
  Area_UK[13,2] <- nrow(poly_1986)
  Area_UK[14,2] <- nrow(poly_1987)
  Area_UK[15,2] <- nrow(poly_1988)
  Area_UK[16,2] <- nrow(poly_1989)
}
#1990s
{ Area_UK[17,2] <- nrow(poly_1990)
  Area_UK[18,2] <- nrow(poly_1991)
  Area_UK[19,2] <- nrow(poly_1992)
  Area_UK[20,2] <- nrow(poly_1993)
  Area_UK[21,2] <- nrow(poly_1994)
  Area_UK[22,2] <- nrow(poly_1995)
  Area_UK[23,2] <- nrow(poly_1996)
  Area_UK[24,2] <- nrow(poly_1997)
  Area_UK[25,2] <- nrow(poly_1998)
  Area_UK[26,2] <- nrow(poly_1999)
}
#2000s
{ Area_UK[27,2] <- nrow(poly_2000)
  Area_UK[28,2] <- nrow(poly_2001)
  Area_UK[29,2] <- nrow(poly_2002)
  Area_UK[30,2] <- nrow(poly_2003)
  Area_UK[31,2] <- nrow(poly_2004)
  Area_UK[32,2] <- nrow(poly_2005)
  Area_UK[33,2] <- nrow(poly_2006)
  Area_UK[34,2] <- nrow(poly_2007)
  Area_UK[35,2] <- nrow(poly_2008)
  Area_UK[36,2] <- nrow(poly_2009)
}
#2010s
{ Area_UK[37,2] <- nrow(poly_2010)
  Area_UK[38,2] <- nrow(poly_2011)
  Area_UK[39,2] <- nrow(poly_2012)
  Area_UK[40,2] <- nrow(poly_2013)
  Area_UK[41,2] <- nrow(poly_2014)
  Area_UK[42,2] <- nrow(poly_2015)
  Area_UK[43,2] <- nrow(poly_2016)
  Area_UK[44,2] <- nrow(poly_2017)
  Area_UK[45,2] <- nrow(poly_2018)
  Area_UK[46,2] <- nrow(poly_2019)
}
#2020s
{ Area_UK[47,2] <- nrow(poly_2020)
  Area_UK[48,2] <- nrow(poly_2021)
}

write.csv(Area_UK, "./Data/Area of occupancy/Individual countries/AOO_UK.csv")

# Plot --------------------------------------------------------------------

Area_UK <- read.csv("./Data/Area of occupancy/Individual countries/AOO_UK.csv")

# Make quick plot
ggplot(Area_UK, aes(x=Year, y=Area))+
  geom_point()

