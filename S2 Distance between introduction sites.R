# --------------------------------------------------------------------------
# ------ Script for plotting the distance between the farthest points ------
# --------------------------------------------------------------------------
# Set the working directory ------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of Spread")

# Load the required libraries ---------------------------------------------
{ library(dplyr)
library(sdmpredictors)
library(rSDM)
library(gdistance)
library(SDraw)
}

# Load the initial introduction data ---------------------------------------

mg <- read.csv("./Data/First records/Initial_introductions_no_angulata_new_data.csv")

# Separate the database and literature data
mg_d <- filter(mg, type=="database")
mg_l <- filter(mg, type=="introduction")

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

# Subset the data by country -----------------------------------
#table(mg$country)

# Separate by country for the database data
{ BELd <- filter(mg_d, country=="Belgium")
DENd <- filter(mg_d, country=="Denmark")
FRAd <- filter(mg_d, country=="France")
GERd <- filter(mg_d, country=="Germany")
IREd <- filter(mg_d, country=="Ireland")
NETd <- filter(mg_d, country=="Netherlands")
NORd <- filter(mg_d, country=="Norway")
SWEd <- filter(mg_d, country=="Sweden")
UKd <- filter(mg_d, country=="UK")
}
# Separate by country for the literature data
{BELl <- filter(mg_l, country=="Belgium")
DENl <- filter(mg_l, country=="Denmark")
FRAl <- filter(mg_l, country=="France")
GERl <- filter(mg_l, country=="Germany")
IREl <- filter(mg_l, country=="Ireland")
NETl <- filter(mg_l, country=="Netherlands")
NORl <- filter(mg_l, country=="Norway")
SWEl <- filter(mg_l, country=="Sweden")
UKl <- filter(mg_l, country=="UK")
}

# Convert points to spdf ---------------------------------------------------

{BELd_sp <- SpatialPointsDataFrame(coords=BELd[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=BELd)
DENd_sp <- SpatialPointsDataFrame(coords=DENd[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=DENd)
FRAd_sp <- SpatialPointsDataFrame(coords=FRAd[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=FRAd)
GERd_sp <- SpatialPointsDataFrame(coords=GERd[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=GERd)
IREd_sp <- SpatialPointsDataFrame(coords=IREd[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=IREd)
NETd_sp <- SpatialPointsDataFrame(coords=NETd[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=NETd)
NORd_sp <- SpatialPointsDataFrame(coords=NORd[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=NORd)
SWEd_sp <- SpatialPointsDataFrame(coords=SWEd[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=SWEd)
UKd_sp <- SpatialPointsDataFrame(coords=UKd[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=UKd)
}
{BELl_sp <- SpatialPointsDataFrame(coords=BELl[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=BELl)
  DENl_sp <- SpatialPointsDataFrame(coords=DENl[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=DENl)
  FRAl_sp <- SpatialPointsDataFrame(coords=FRAl[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=FRAl)
  GERl_sp <- SpatialPointsDataFrame(coords=GERl[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=GERl)
  IREl_sp <- SpatialPointsDataFrame(coords=IREl[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=IREl)
  NETl_sp <- SpatialPointsDataFrame(coords=NETl[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=NETl)
  NORl_sp <- SpatialPointsDataFrame(coords=NORl[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=NORl)
  SWEl_sp <- SpatialPointsDataFrame(coords=SWEl[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=SWEl)
  UKl_sp <- SpatialPointsDataFrame(coords=UKl[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=UKl)
}

# Move points off of land -------------------------------------------------

# Only Denmark has an issue
DENl_sp <- points2nearestcell(DENl_sp, bathy)

# Convert projection of points to UTM -------------------------------------

{ BELd_sp <- spTransform(BELd_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
DENd_sp <- spTransform(DENd_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
FRAd_sp <- spTransform(FRAd_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
GERd_sp <- spTransform(GERd_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
IREd_sp <- spTransform(IREd_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
NETd_sp <- spTransform(NETd_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
NORd_sp <- spTransform(NORd_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
SWEd_sp <- spTransform(SWEd_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
UKd_sp <- spTransform(UKd_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
}
{BELl_sp <- spTransform(BELl_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
DENl_sp <- spTransform(DENl_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
FRAl_sp <- spTransform(FRAl_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
GERl_sp <- spTransform(GERl_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
IREl_sp <- spTransform(IREl_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
NETl_sp <- spTransform(NETl_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
NORl_sp <- spTransform(NORl_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
SWEl_sp <- spTransform(SWEl_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
UKl_sp <- spTransform(UKl_sp, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))
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

# Add distance column to data ---------------------------------------------

BELd$Distance <- NA
DENd$Distance <- NA
FRAd$Distance <- NA
GERd$Distance <- NA
IREd$Distance <- NA
NETd$Distance <- NA
NORd$Distance <- NA
SWEd$Distance <- NA
UKd$Distance <- NA

# Calculate the distances ---------------------------------------------------

for (i in 1:length(BELd_sp)) {
  AtoB_BEL <- shortestPath(trb, BELd_sp, BELl_sp, output="SpatialLines") 
  BELd$Distance <- max(lineLength(AtoB_BEL, byid=TRUE)/1000) 
}
for (i in 1:length(DENd_sp)) {
  AtoB_DEN <- shortestPath(trb, DENd_sp, DENl_sp, output="SpatialLines") 
  DENd$Distance <- max(lineLength(AtoB_DEN, byid=TRUE)/1000) 
}
for (i in 1:length(FRAd_sp)) {
  AtoB_FRA <- shortestPath(trb, FRAd_sp, FRAl_sp, output="SpatialLines") 
  FRAd$Distance <- max(lineLength(AtoB_FRA, byid=TRUE)/1000) 
}
for (i in 1:length(GERd_sp)) {
  AtoB_GER <- shortestPath(trb, GERd_sp, GERl_sp, output="SpatialLines") 
  GERd$Distance <- max(lineLength(AtoB_GER, byid=TRUE)/1000) 
}
for (i in 1:length(IREd_sp)) {
  AtoB_IRE <- shortestPath(trb, IREd_sp, IREl_sp, output="SpatialLines") 
  IREd$Distance <- max(lineLength(AtoB_IRE, byid=TRUE)/1000) 
}
for (i in 1:length(NETd_sp)) {
  AtoB_NET <- shortestPath(trb, NETd_sp, NETl_sp, output="SpatialLines") 
  NETd$Distance <- max(lineLength(AtoB_NET, byid=TRUE)/1000) 
}
for (i in 1:length(NORd_sp)) {
  AtoB_NOR <- shortestPath(trb, NORd_sp, NORl_sp, output="SpatialLines") 
  NORd$Distance <- max(lineLength(AtoB_NOR, byid=TRUE)/1000) 
}
for (i in 1:length(SWEd_sp)) {
  AtoB_SWE <- shortestPath(trb, SWEd_sp, SWEl_sp, output="SpatialLines") 
  SWEd$Distance <- max(lineLength(AtoB_SWE, byid=TRUE)/1000) 
}
for (i in 1:length(UKd_sp)) {
  AtoB_UK <- shortestPath(trb, UKd_sp, UKl_sp, output="SpatialLines") 
  UKd$Distance <- max(lineLength(AtoB_UK, byid=TRUE)/1000) 
}

# Create a data frame -----------------------------------------------------

Distances <- rbind(BELd, DENd, FRAd, GERd, IREd, NETd, NORd, SWEd, UKd)

# Plot the lines -------------------------------------

# Re-load the bathymetry raster
bathy <- load_layers("MS_bathy_5m", equalarea=FALSE)
bathy <- bathy$MS_bathy_5m

# Crop raster to desired boundaries
min_lon <- -15
max_lon <- 15
min_lat <- 43
max_lat <- 65
geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)
Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))
coordinates(Sites.grid) <- ~ lon_bound + lat_bound
bathy <- crop(bathy, extent(Sites.grid))

rm(Sites.grid, geo_bounds, max_lat, max_lon, min_lat, min_lon)

{ AtoB_BEL <- spTransform(AtoB_BEL, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  AtoB_DEN <- spTransform(AtoB_DEN, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  AtoB_FRA <- spTransform(AtoB_FRA, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  AtoB_GER <- spTransform(AtoB_GER, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  AtoB_IRE <- spTransform(AtoB_IRE, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  AtoB_NET <- spTransform(AtoB_NET, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  AtoB_NOR <- spTransform(AtoB_NOR, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  AtoB_SWE <- spTransform(AtoB_SWE, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  AtoB_UK <- spTransform(AtoB_UK, CRS("+proj=longlat +datum=WGS84 +no_defs"))
}

bathy[!is.na(bathy)] <- -999
bathy[bathy>-999] <- NA
bathy[bathy==-999] <- 1

mg_dl <- SpatialPointsDataFrame(coords=mg_d[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=mg_d)
mg_ll <- SpatialPointsDataFrame(coords=mg_l[,c("lon", "lat")], proj4string=CRS(proj4string(bathy)), data=mg_l)

plot(bathy)
plot(AtoB_BEL, add=TRUE)
plot(AtoB_DEN, add=TRUE)
plot(AtoB_FRA, add=TRUE)
plot(AtoB_GER, add=TRUE)
plot(AtoB_IRE, add=TRUE)
plot(AtoB_NET, add=TRUE)
plot(AtoB_NOR, add=TRUE)
plot(AtoB_SWE, add=TRUE)
plot(AtoB_UK, add=TRUE)
points(mg_dl, col="red")
points(mg_ll, col="blue")

# zoom in on Belgium and the Netherlands
plot(bathy, xlim=c(2, 5), ylim=c(51, 52))
plot(AtoB_BEL, add=TRUE)
plot(AtoB_NET, add=TRUE)
points(mg_dl, col="red")
points(mg_ll, col="blue")

# zoom in on Ireland
plot(bathy, xlim=c(-8.8, -7.8), ylim=c(51.4, 52))
plot(AtoB_IRE, add=TRUE)
points(mg_dl, col="red")
points(mg_ll, col="blue")

# zoom in on Germany
plot(bathy, xlim=c(8, 9), ylim=c(54.8, 55.2))
plot(AtoB_GER, add=TRUE)
plot(AtoB_DEN, add=TRUE)
points(mg_dl, col="red")
points(mg_ll, col="blue")

# Edit the figure and create a panel in Inkscape
