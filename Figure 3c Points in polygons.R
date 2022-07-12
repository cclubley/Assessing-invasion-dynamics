# Set the working directory ------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Load the required libraries ---------------------------------------------
{ library(rgdal)
  library(sdmpredictors)
  library(dplyr)
  library(rSDM)
  library(ggplot2)
}

# Load the distribution data ----------------------------------------------

mg <- read.csv("./Data/Data - 2022/Final spreadsheet/Total_data_Atlantic_April_2022_Final.csv")

# Load an ocean grid ------------------------------------------------------

# Load in a high resolution coastal grid for Europe
# This grid has a resolution of 0.5 degrees and already has polygon IDs
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/(4) Results/(Fig 3) Spread panel/Coastal grids/Europe")
ocean_grid <- readOGR(dsn=".", layer="coast_grid")
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

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

# Subset the data by year (cumulatively) -----------------------------------

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
{  mg_1970 <- SpatialPointsDataFrame(coords=mg_1970[,c("Longitude", "Latitude")], proj4string=CRS(proj4string(bathy)), data=mg_1970)
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

# 1960s, 1970s, 1980s and initial introduction points are all within raster already
#1990s
{mg_1995 <- points2nearestcell(mg_1995, bathy)
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

Area <- as.data.frame(1965:2021)
colnames(Area) <- "Year"
Area$Area <- NA

# Calculate Number of grid cells per year ----------------------------

#1960s
{ Area[1,2] <- nrow(poly_1965)
Area[2,2] <- nrow(poly_1966)
Area[3,2] <- nrow(poly_1967)
Area[4,2] <- nrow(poly_1968)
Area[5,2] <- nrow(poly_1969)
}
#1970s
{ Area[6,2] <- nrow(poly_1970)
Area[7,2] <- nrow(poly_1971)
Area[8,2] <- nrow(poly_1972)
Area[9,2] <- nrow(poly_1973)
Area[10,2] <- nrow(poly_1974)
Area[11,2] <- nrow(poly_1975)
Area[12,2] <- nrow(poly_1976)
Area[13,2] <- nrow(poly_1977)
Area[14,2] <- nrow(poly_1978)
Area[15,2] <- nrow(poly_1979)
}
#1980s
{ Area[16,2] <- nrow(poly_1980)
  Area[17,2] <- nrow(poly_1981)
  Area[18,2] <- nrow(poly_1982)
  Area[19,2] <- nrow(poly_1983)
  Area[20,2] <- nrow(poly_1984)
  Area[21,2] <- nrow(poly_1985)
  Area[22,2] <- nrow(poly_1986)
  Area[23,2] <- nrow(poly_1987)
  Area[24,2] <- nrow(poly_1988)
  Area[25,2] <- nrow(poly_1989)
}
#1990s
{ Area[26,2] <- nrow(poly_1990)
  Area[27,2] <- nrow(poly_1991)
  Area[28,2] <- nrow(poly_1992)
  Area[29,2] <- nrow(poly_1993)
  Area[30,2] <- nrow(poly_1994)
  Area[31,2] <- nrow(poly_1995)
  Area[32,2] <- nrow(poly_1996)
  Area[33,2] <- nrow(poly_1997)
  Area[34,2] <- nrow(poly_1998)
  Area[35,2] <- nrow(poly_1999)
}
#2000s
{ Area[36,2] <- nrow(poly_2000)
  Area[37,2] <- nrow(poly_2001)
  Area[38,2] <- nrow(poly_2002)
  Area[39,2] <- nrow(poly_2003)
  Area[40,2] <- nrow(poly_2004)
  Area[41,2] <- nrow(poly_2005)
  Area[42,2] <- nrow(poly_2006)
  Area[43,2] <- nrow(poly_2007)
  Area[44,2] <- nrow(poly_2008)
  Area[45,2] <- nrow(poly_2009)
}
#2010s
{ Area[46,2] <- nrow(poly_2010)
  Area[47,2] <- nrow(poly_2011)
  Area[48,2] <- nrow(poly_2012)
  Area[49,2] <- nrow(poly_2013)
  Area[50,2] <- nrow(poly_2014)
  Area[51,2] <- nrow(poly_2015)
  Area[52,2] <- nrow(poly_2016)
  Area[53,2] <- nrow(poly_2017)
  Area[54,2] <- nrow(poly_2018)
  Area[55,2] <- nrow(poly_2019)
}
#2020s
{ Area[56,2] <- nrow(poly_2020)
  Area[57,2] <- nrow(poly_2021)
}

# Save as .csv file -------------------------------------------------------

write.csv(Area, "./Data/Area of occupancy/AOO_All.csv", row.name=FALSE)

# Plot --------------------------------------------------------------------

# Read in the saved data
Area <- read.csv("./Data/Area of occupancy/AOO_All.csv")

# Make quick plot for visual purposes
ggplot(Area, aes(x=Year, y=Area))+
  geom_point()

# Equation fitting and final plot created in MatLab