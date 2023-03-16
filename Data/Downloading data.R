# Set wd and load libraries ------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread/Data/Data - 2022")

{ library(dplyr)
  library(tidyverse)
  library(sp)
  library(rSDM)
  library(geosphere)
  library(sf)
  library(raster)
  library(rgeos)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(devtools)
  library(robis)
  library(rgbif)
  library(dplyr)
  library(tidyverse)
  library(sp)
  library(ggplot2)
  library(ccplot)
}

# -------------------------------------------------------------------------
# ---------------------------------- GBIF ---------------------------------
# -------------------------------------------------------------------------
# Download GBIF records -------------------------------------------------------

# Download data labelled 'M. gigas'
gbif_mg <- occ_search(scientificName="Magallana gigas", limit=100000) 
gbif_mg <- gbif_mg$data # 14457 records (06/04/2022)

# Download data labelled 'C. gigas'
gbif_cg <- occ_search(scientificName = "Crassostrea gigas", limit=100000) 
gbif_cg <- gbif_cg$data # 6460 records (06/04/2022)

# Remove records dated after July 2021 ------------------------------------

# Separate the 2021 values from the overall data frame
gbif_mg_2021 <- filter(gbif_mg, year==2021)
gbif_mg <- filter(gbif_mg, year<=2020 | is.na(year))
# Remove 2021 records from after July
gbif_mg_2021 <- filter(gbif_mg_2021, month<=7 | is.na(month))
# Re-join the dataframes
gbif_mg <- rbind(gbif_mg, gbif_mg_2021) # 13527 records
# Removed 930 records from after July 2021

gbif_cg_2021 <- filter(gbif_cg, year==2021)
gbif_cg <- filter(gbif_cg, year<=2020 | is.na(year))
gbif_cg_2021 <- filter(gbif_cg_2021, month<=7 | is.na(month))
gbif_cg <- rbind(gbif_cg, gbif_cg_2021) # 6382 records
# Removed 78 records from after July 2021

rm(gbif_mg_2021, gbif_cg_2021)

# Remove records from outside of Europe -----------------------------------

# First use the continent column
gbif_mg <- filter(gbif_mg, continent=="EUROPE" | is.na(continent)) # Removed 112 records
gbif_cg <- filter(gbif_cg, continent=="EUROPE" | is.na(continent)) # Removed 72 records

# Then the country column
gbif_mg <- filter(gbif_mg, country=="Belgium"|country=="Croatia"|country=="Denmark"
               |country=="France"|country=="Germany"|country=="Greece"|country=="Guernsey"
               |country=="Ireland"|country=="Isle of Man"|country=="Italy"|country=="Jersey"
               |country=="Netherlands"|country=="Norway"|country=="Portugal"|country=="Slovenia"
               |country=="Spain"|country=="Sweden"|country=="United Kingdom of Great Britain and Northern Ireland"
               |country=="unknown or invalid"|is.na(country)) # Removed 4630 records

gbif_cg <- filter(gbif_cg, country=="Belgium"|country=="Croatia"|country=="Denmark"
                  |country=="France"|country=="Germany"|country=="Ireland"|country=="Italy"
                  |country=="Netherlands"|country=="Norway"|country=="Portugal"|country=="Slovenia"
                  |country=="Spain"|country=="Sweden"|country=="United Kingdom of Great Britain and Northern Ireland"
                  |country=="unknown or invalid"|is.na(country)) # Removed 2866 records

# Remove absence records --------------------------------------------------

gbif_mg <- filter(gbif_mg, occurrenceStatus=="PRESENT"|is.na(occurrenceStatus)) # Removed 2297 rows 
gbif_cg <- filter(gbif_cg, occurrenceStatus=="PRESENT"|is.na(occurrenceStatus)) # Removed 1600 rows 

# Remove records missing lat or lon values --------------------------------

gbif_mg <- filter(gbif_mg, !is.na(decimalLatitude)) # Removed 824 rows
gbif_mg <- subset(gbif_mg, !is.na(decimalLongitude))

gbif_cg <- subset(gbif_cg, !is.na(decimalLatitude)) # removes 570 rows
gbif_cg <- subset(gbif_cg, !is.na(decimalLongitude))

# Remove fossil and preserved specimens -----------------------------------

gbif_mg <- filter(gbif_mg, basisOfRecord=="HUMAN_OBSERVATION"|basisOfRecord=="MATERIAL_SAMPLE"
               |basisOfRecord=="OCCURRENCE"|is.na(basisOfRecord)) # removed 106 records

gbif_cg <- filter(gbif_cg, basisOfRecord=="HUMAN_OBSERVATION"|basisOfRecord=="MATERIAL_SAMPLE"
                  |basisOfRecord=="OCCURRENCE"|is.na(basisOfRecord)) # removed 52 records

# Remove any columns with only NA values -----------------------------------

gbif_mg <- gbif_mg[colSums(!is.na(gbif_mg)) > 0] # Removed 51 columns
gbif_cg <- gbif_cg[colSums(!is.na(gbif_cg)) > 0] # Removed 62 columns

# Flag records that fall >100m from the coast -------

# Load a medium resolution shapefile of the world.
world <- ne_countries(scale = "medium", returnclass = "sp")

# Crop the shapefile to boundaries of the data
min_lon <- -15
max_lon <- 25
min_lat <- 30
max_lat <- 65

geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)
Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))
coordinates(Sites.grid) <- ~ lon_bound + lat_bound
map_crop <- crop(world, extent(Sites.grid)) 

# Convert the projection to meters
world <- spTransform(map_crop, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))

# Implement a 100m buffer
map_buf <- gBuffer(world, width=-100)

# Restore the original projection
map <- spTransform(map_buf, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Convert the projection of the data
gbif_mg.sp <- SpatialPointsDataFrame(coords = gbif_mg[,c("decimalLongitude", "decimalLatitude")], 
                                  proj4string = CRS(proj4string(map)), data=gbif_mg)

# Plot to check
plot(map)
plot(gbif_mg.sp, col="red", add=TRUE)

# Determine whether any points fall on land
on_land <- as.data.frame(over(gbif_mg.sp, map))
colnames(on_land) <- "on_land"
sum(is.na(on_land)) # 3568 records fall over the ocean, 1990 fall 'on land'

# Add as a column to the original data frame
gbif_mg$on_land <- on_land$on_land # 1=on land, 0=in water

# Convert the projection of the data
gbif_cg.sp <- SpatialPointsDataFrame(coords = gbif_cg[,c("decimalLongitude", "decimalLatitude")], 
                                     proj4string = CRS(proj4string(map)), data=gbif_cg)

# Plot to check
plot(map)
plot(gbif_cg.sp, col="red", add=TRUE)

# Determine whether any points fall on land
on_land <- as.data.frame(over(gbif_cg.sp, map))
colnames(on_land) <- "on_land"
sum(is.na(on_land)) # 830 records fall over the ocean, 392 fall 'on land'

# Add as a column to the original data frame
gbif_cg$on_land <- on_land$on_land

# Save mg and cg, combine into one data frame in excel and remove duplicates -------------

gbif_mg$Database <- "GBIF"
gbif_cg$Database <- "GBIF"

# Save as .csv files and align the columns correctly
write.csv(gbif_mg, "./Raw data files/GBIF_mg_raw_April_2022.csv", row.names = FALSE)
write.csv(gbif_cg, "./Raw data files/GBIF_cg_raw_April_2022.csv", row.names = FALSE)

# Read the files back in
gbif_mg <- read.csv("./Raw data files/GBIF_mg_raw_April_2022.csv")
gbif_cg <- read.csv("./Raw data files/GBIF_cg_raw_April_2022.csv")
# They now have matching column numbers

gbif <- rbind(gbif_mg, gbif_cg) #6780 rows total

# Remove duplicate records
gbif <- gbif[!duplicated(gbif), ] # Removed 1133 rows

# Write out the final gbif file -----------------------------------------------

write.csv(gbif, "./Raw data files/GBIF_combined_raw_April_2022.csv", row.names=FALSE)
#Link to data - https://doi.org/10.15468/dd.n2y3vj

# -------------------------------------------------------------------------
# ---------------------------------- OBIS ---------------------------------
# -------------------------------------------------------------------------
# Download OBIS records -------------------------------------------------------

# Download data labelled as 'M. gigas'
obis_mg <- occurrence("Magallana gigas") # 3760 records (06/04/2022)
# Download data labelled as 'C. gigas'
obis_cg <- occurrence("Crassostrea gigas") #3760 records (06/04/2022)

# Combine to one data frame
obis <- rbind(obis_mg, obis_cg)
rm(obis_mg, obis_cg)

# Remove duplicate records
obis <- obis[!duplicated(obis), ] # Removed 3760 rows
# Records for M. gigas and C. gigas were the same

# Remove records dated after July 2021 ------------------------------------

# Separate the 2021 values from the overall data frame
obis_2021 <- filter(obis, year==2021) # No records dated from 2021
rm(obis_2021)

# Remove records from outside of Europe -----------------------------------

# First use the continent column
obis <- filter(obis, continent=="Europe"|continent=="North Sea"|is.na(continent)) # Removed 3 records

# Then the country column
obis <- filter(obis, country=="DE"|country=="France"|country=="GB"
               |country=="Ireland"|country=="Italy"|country=="Netherlands"
               |country=="Norway"|country=="Sweden"|country=="UK"
               |country=="United Kingdom"|country=="unknown or invalid"
               |is.na(country)) # Removed 742 records

# Remove absence records --------------------------------------------------

obis <- filter(obis, occurrenceStatus=="present"|is.na(occurrenceStatus)) # Removed 0 rows 

# Remove records missing lat or lon values --------------------------------

obis <- filter(obis, !is.na(decimalLatitude)) # Removed 0 rows
obis <- filter(obis, !is.na(decimalLongitude)) # Removed 0 rows

# Remove fossil and preserved specimens -----------------------------------

obis <- filter(obis, basisOfRecord=="HumanObservation"|basisOfRecord=="Occurrence"
               |is.na(basisOfRecord)) # removed 12 records

# Remove any columns with only NA values -----------------------------------

obis <- obis[colSums(!is.na(obis)) > 0] # Removed 34 columns

# Flag records that fall >100m from the coast -------

# Load a medium resolution shapefile of the world.
world <- ne_countries(scale = "medium", returnclass = "sp")

# Crop the shapefile to boundaries of the data
min_lon <- -15
max_lon <- 25
min_lat <- 30
max_lat <- 65

geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)
Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))
coordinates(Sites.grid) <- ~ lon_bound + lat_bound
map_crop <- crop(world, extent(Sites.grid)) 

# Convert the projection to meters
world <- spTransform(map_crop, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))

# Implement a 100m buffer
map_buf <- gBuffer(world, width=-100)

# Restore the original projection
map <- spTransform(map_buf, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Convert the projection of the data
obis.sp <- SpatialPointsDataFrame(coords = obis[,c("decimalLongitude", "decimalLatitude")], 
                                     proj4string = CRS(proj4string(map)), data=obis)

# Plot to check
plot(map)
plot(obis.sp, col="red", add=TRUE)

# Determine whether any points fall on land
on_land <- as.data.frame(over(obis.sp, map))
colnames(on_land) <- "on_land"
sum(is.na(on_land)) # 2317 records fall over the ocean, 686 fall 'on land'

# Add as a column to the original data frame
obis$on_land <- on_land$on_land # 1=on land, 0=in water

# Remove duplicates and save --------------------------------------------------

obis$Database <- "OBIS"

# Remove duplicate records
obis <- obis[!duplicated(obis), ] # Removed 0 rows

# Save as .csv file 
write.csv(obis, "./Raw data files/OBIS_raw_April_2022.csv", row.names = FALSE)

# -------------------------------------------------------------------------
# ----------------------------- Marine Recorder ---------------------------
# -------------------------------------------------------------------------
# Load the Marine Recorder data -------------------------------------------------

mrc <- read.csv("./Raw data files/Marine_Recorder.csv")

# Remove records missing lat or lon values --------------------------------

mrc <- filter(mrc, !is.na(Lat)) # Removed 1 row
mrc <- filter(mrc, !is.na(Lon))

# Remove duplicates -----------------------------------------------------------

# Remove duplicate records
mrc <- mrc[!duplicated(mrc), ] # Removed 424 rows

# Remove any columns with only NA values -----------------------------------

mrc <- mrc[colSums(!is.na(mrc)) > 0] # Removed 0 columns

# Flag records that fall >100m from the coast -------

# Load a medium resolution shapefile of the world.
world <- ne_countries(scale = "medium", returnclass = "sp")

# Crop the shapefile to boundaries of the data
min_lon <- -15
max_lon <- 25
min_lat <- 30
max_lat <- 65

geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)
Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))
coordinates(Sites.grid) <- ~ lon_bound + lat_bound
map_crop <- crop(world, extent(Sites.grid)) 

# Convert the projection to meters
world <- spTransform(map_crop, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))

# Implement a 100m buffer
map_buf <- gBuffer(world, width=-1000)

# Restore the original projection
map <- spTransform(map_buf, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Convert the projection of the data
mrc.sp <- SpatialPointsDataFrame(coords = mrc[,c("Lon", "Lat")], 
                                  proj4string = CRS(proj4string(map)), data=mrc)

# Plot to check
plot(map)
plot(mrc.sp, col="red", add=TRUE)

# Determine whether any points fall on land
on_land <- as.data.frame(over(mrc.sp, map))
colnames(on_land) <- "on_land"
sum(is.na(on_land)) # 458 records fall over the ocean, 225 fall 'on land'

# Add as a column to the original data frame
mrc$on_land <- on_land$on_land # 1=on land, 0=in water

# Write out the final Marine recorder file -----------------------------------------------

# Add a column to show which database the records came from
mrc$Database <- "MarineRecorder"

write.csv(mrc, "./Raw data files/MarineRecorder_raw_April_2022.csv", row.names=FALSE)

# -------------------------------------------------------------------------
# ------------------------------- NBN Atlas -------------------------------
# -------------------------------------------------------------------------
# Load the NBN Atlas data -------------------------------------------------

nbn <- read.csv("./Raw data files/NBN/records-2022-03-23.csv") # 983 records (23/03/2022)

# Remove records dated after July 2021 ------------------------------------

nbn_2021 <- filter(nbn, year.processed==2021) 
nbn <- filter(nbn, year.processed<=2020 | is.na(year.processed))
nbn_2021 <- filter(nbn_2021, month.processed<=7 | is.na(month.processed)) 
nbn <- rbind(nbn, nbn_2021) # 981 records
# Removed 2 records from after July 2021
rm(nbn_2021)

# Remove records missing lat or lon values --------------------------------

nbn <- filter(nbn, !is.na(decimalLatitude.processed)) # Removed 1 row
nbn <- filter(nbn, !is.na(decimalLongitude.processed))

# Remove duplicates -----------------------------------------------------------

# Remove duplicate records
nbn <- nbn[!duplicated(nbn), ] # Removed 0 rows

# Remove any columns with only NA values -----------------------------------

nbn <- nbn[colSums(!is.na(nbn)) > 0] # Removed 55 columns

# Flag records that fall >100m from the coast -------

# Load a medium resolution shapefile of the world.
world <- ne_countries(scale = "medium", returnclass = "sp")

# Crop the shapefile to boundaries of the data
min_lon <- -15
max_lon <- 25
min_lat <- 30
max_lat <- 65

geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)
Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))
coordinates(Sites.grid) <- ~ lon_bound + lat_bound
map_crop <- crop(world, extent(Sites.grid)) 

# Convert the projection to meters
world <- spTransform(map_crop, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"))

# Implement a 100m buffer
map_buf <- gBuffer(world, width=-1000)

# Restore the original projection
map <- spTransform(map_buf, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Convert the projection of the data
nbn.sp <- SpatialPointsDataFrame(coords = nbn[,c("decimalLongitude.processed", "decimalLatitude.processed")], 
                                 proj4string = CRS(proj4string(map)), data=nbn)

# Plot to check
plot(map)
plot(nbn.sp, col="red", add=TRUE)

# Determine whether any points fall on land
on_land <- as.data.frame(over(nbn.sp, map))
colnames(on_land) <- "on_land"
sum(is.na(on_land)) # 665 records fall over the ocean, 315 fall 'on land'

# Add as a column to the original data frame
nbn$on_land <- on_land$on_land # 1=on land, 0=in water

# Write out the final NBN Atlas file -----------------------------------------------

# Add a column to show which database the records came from
nbn$Database <- "NBNAtlas"

write.csv(nbn, "./Raw data files/NBN_raw_April_2022.csv", row.names=FALSE)

# -------------------------------------------------------------------------
# --------------------- Edit the spreadsheets in Excel --------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# --------- Combine the edited spreadsheets and do final cleaning ---------
# -------------------------------------------------------------------------
# Reload the edited spreadsheets ------------------------------------------

# Read in the spreadsheets
gbif <- read.csv("./Excel edited files/GBIF_combined_edited_April_2022_new.csv")
obis <- read.csv("./Excel edited files/OBIS_edited_April_2022.csv")
nbn <- read.csv("./Excel edited files/NBN_edited_April_2022.csv")
mrc <- read.csv("./Excel edited files/MarineRecorder_edited_April_2022.csv")

# Remove duplicate records from each
gbif <- gbif[!duplicated(gbif), ] # Removed 148 rows
obis <- obis[!duplicated(obis), ] # Removed 0 rows
nbn <- nbn[!duplicated(nbn), ] # Removed 109 rows
mrc <- mrc[!duplicated(mrc), ] # Removed 0 rows

# Final number of records per database
# GBIF = 4184
# OBIS = 1765
# NBN = 724
# MRC = 662
# TOTAL = 7335

# Combine to one spreadsheet ----------------------------------------------

Total_data <- rbind(gbif, obis, nbn, mrc)

# Save
write.csv(Total_data, "./Final Spreadsheet/Total_data_all_countries_April_2022.csv", row.names=FALSE)

# Remove records from countries outside of study area ---------------------

table(Total_data$Country)
Total_data <- filter(Total_data, Country=="Belgium"|Country=="Denmark"|
                       Country=="France"|Country=="Germany"|Country=="Guernsey"|
                       Country=="Ireland"|Country=="Jersey"|Country=="Netherlands"|
                       Country=="Norway"|Country=="Sweden"|Country=="United Kingdom")
# Removes 92 records from the other countries

# Save and remove records from S France in Excel
#write.csv(Total_data, "./Final Spreadsheet/Total_data_Atlantic_April_2022.csv", row.names=FALSE)

# Read back in 
Total_data <- read.csv("./Final Spreadsheet/Total_data_Atlantic_April_2022.csv")
# Removed 99 records

# Plot to check
mg.sp <- SpatialPointsDataFrame(coords = Total_data[,c("Longitude", "Latitude")], 
                                     proj4string = CRS(proj4string(map)), data=Total_data)
plot(map)
plot(mg.sp, col="red", add=TRUE)

# Removed 191 records in total

# Remove duplicate records across databases --------------------------------

Total_data <- Total_data[!duplicated(Total_data[, c(1:7)]), ] # Removed 450 rows

# Write the final file
write.csv(Total_data, "./Final Spreadsheet/Total_data_Atlantic_April_2022_Final.csv", row.names=FALSE)
# The final database has 6693 records total

# If you were to remove duplicate coorindates within the same year...
# Order the distribution data from the earliest record to latest
Total_data <- Total_data[order(Total_data$Year, Total_data$Month, Total_data$Day, decreasing=FALSE),]
# Remove duplicates based on the longitude and latitude
Total_data <- Total_data[!duplicated(Total_data[c("Longitude", "Latitude", "Year")]), ]
# Removes 932 records


# -------------------------------------------------------------------------
# ------------ Check records are labelled as the correct country ----------
# -------------------------------------------------------------------------
# Load the spreadsheet and a map ------------------------------------------

mg <- read.csv("./Final spreadsheet/Total_data_Atlantic_April_2022_Final.csv")

# Load a medium resolution shapefile of the world.
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter the data by country ----------------------------------------------

#table(mg$Country)

BEL <- filter(mg, Country=="Belgium")
DEN <- filter(mg, Country=="Denmark")
FRA <- filter(mg, Country=="France")
GER <- filter(mg, Country=="Germany")
GUE <- filter(mg, Country=="Guernsey")
IRE <- filter(mg, Country=="Ireland")
JER <- filter(mg, Country=="Jersey")
NET <- filter(mg, Country=="Netherlands")
NOR <- filter(mg, Country=="Norway")
SWE <- filter(mg, Country=="Sweden")
UKI <- filter(mg, Country=="United Kingdom")

# Plot the records for each country to check ------------------------------

# Belgium
ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(2, 5), ylim=c(50, 52), expand=FALSE)+ 
  cc_theme()+
  geom_point(data=BEL, aes(x=Longitude, y=Latitude), size=2, colour="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))
# These all seem fine - the two points to the far right are in waterways in Antwerp, so not actually on land

# Denmark
ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(5, 15), ylim=c(53, 60), expand=FALSE)+ 
  cc_theme()+
  geom_point(data=DEN, aes(x=Longitude, y=Latitude), size=2, colour="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))
# One offshore record looks as though it could belong to Germany (the farthest west point)
# Double checked on Google maps and it was Germany - changed this in the spreadsheet so it is now correct

# France
ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(1, 5), ylim=c(50, 52), expand=FALSE)+ 
  cc_theme()+
  geom_point(data=FRA, aes(x=Longitude, y=Latitude), size=2, colour="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))
# These all seem fine 

# Germany
ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(5, 15), ylim=c(51, 56), expand=FALSE)+ 
  cc_theme()+
  geom_point(data=GER, aes(x=Longitude, y=Latitude), size=2, colour="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))
# The points to the west seem as though they could be the Netherlands
# I double checked them on Google maps and they are all definitey Germany - a lot of them are German islands in the Wadden

# Guernsey
ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-5, -1), ylim=c(46, 50), expand=FALSE)+ 
  cc_theme()+
  geom_point(data=GUE, aes(x=Longitude, y=Latitude), size=2, colour="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))
# These all seem fine

# Ireland
ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-11, -5), ylim=c(51, 56), expand=FALSE)+ 
  cc_theme()+
  geom_point(data=IRE, aes(x=Longitude, y=Latitude), size=2, colour="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))
# Some of the points in the north could be Northern Ireland (UK)
# ~3 were the UK, so I changed this on the spreadsheet

# Jersey
ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-5, -1), ylim=c(46, 50), expand=FALSE)+ 
  cc_theme()+
  geom_point(data=JER, aes(x=Longitude, y=Latitude), size=2, colour="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))
# These all seem fine

# Netherlands
ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(3, 8), ylim=c(51, 54), expand=FALSE)+ 
  cc_theme()+
  geom_point(data=NET, aes(x=Longitude, y=Latitude), size=2, colour="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))
# These all seem fine

# Norway
ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(4, 13), ylim=c(56, 63), expand=FALSE)+ 
  cc_theme()+
  geom_point(data=NOR, aes(x=Longitude, y=Latitude), size=2, colour="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))
# Some of the far east points could be Sweden
# Checked them on Google Maps and they are all definitely in Norway

# Sweden
ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(9, 15), ylim=c(55, 61), expand=FALSE)+ 
  cc_theme()+
  geom_point(data=SWE, aes(x=Longitude, y=Latitude), size=2, colour="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))
# These all seem fine - the point in the middle of the country is in a lake (don't include in spread measurements)

# United Kingdom
ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-9, -5), ylim=c(53, 56), expand=FALSE)+ 
  cc_theme()+
  geom_point(data=UKI, aes(x=Longitude, y=Latitude), size=2, colour="red")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))
# Mainland UK is all fine
# One of the lowest points in N Ireland could be Ireland
# Two points were Ireland, so I changed these i the spreadsheet
