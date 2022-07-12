# --------------------------------------------------------------------------
# ------- Script for plotting the decadal spread of Pacific oysters --------
# ------------------- in their introduced European range -------------------
# --------------------------------------------------------------------------
# Set the working directory ------------------------------------------------
setwd("C:/Users/cclubley/OneDrive - University of Plymouth/Data chapters/(1) Rate of spread")

# Load the required packages -----------------------------------------------

{ library(dplyr)
  library(rnaturalearth)
  library(RColorBrewer)
  library(ggplot2)
  library(ccplot)
  library(ggstar)
  library(ggspatial)
}

# Load the data sets -------------------------------------------------------

# Distribution data for populations in northern and western Europe
mydata <- read.csv("./Data/Data - 2022/Final spreadsheet/Total_data_Atlantic_April_2022_Final.csv")

# The first record from within each country based on the database
firstrec <- read.csv("./Data/First records/Initial_introductions_no_angulata_new_data.csv")

# Isolate the data based only on the database
databaserec <- filter(firstrec, type=="database")

# Subset the distribution data by decade -----------------------------------

# Subset the distribution data into each decade (cumulatively)

#table(mydata$Decade)
mydata_1960s <- filter(mydata, Decade=="1960s")
mydata_1970s <- filter(mydata, Decade=="1960s"|Decade=="1970s")
mydata_1980s <- filter(mydata, Decade=="1960s"|Decade=="1970s"|Decade=="1980s") 
mydata_1990s <- filter(mydata, Decade=="1960s"|Decade=="1970s"|Decade=="1980s"|
                         Decade=="1990s")
mydata_2000s <- filter(mydata, Decade=="1960s"|Decade=="1970s"|Decade=="1980s"|
                         Decade=="1990s"|Decade=="2000s")
mydata_2010s <- filter(mydata, Decade=="1960s"|Decade=="1970s"|Decade=="1980s"|
                         Decade=="1990s"|Decade=="2000s"|Decade=="2010s")
mydata_2020s <- filter(mydata, Decade=="1960s"|Decade=="1970s"|Decade=="1980s"|
                         Decade=="1990s"|Decade=="2000s"|Decade=="2010s"|
                         Decade=="2020s")

# Subset the introduction data by decade -----------------------------------

# Subset the database introduction data into each decade (cumulatively)

#table(databaserec$year)
databaserec_1960s <- filter(databaserec, year=="1965")
databaserec_1970s <- filter(databaserec, year=="1965"|year=="1974")
databaserec_1980s <- filter(databaserec, year=="1965"|year=="1974"|
                           year=="1986"|year=="1989") 
databaserec_1990s <- filter(databaserec, year=="1965"|year=="1974"|
                           year=="1986"|year=="1989"|year=="1995")
databaserec_2000s <- filter(databaserec, year=="1965"|year=="1974"|
                           year=="1986"|year=="1989"|year=="1995"|
                           year=="2001"|year=="2006"|year=="2007"|
                           year=="2008"|year=="2009")
databaserec_2010s <- filter(databaserec, year=="1965"|year=="1974"|
                              year=="1986"|year=="1989"|year=="1995"|
                              year=="2001"|year=="2006"|year=="2007"|
                              year=="2008"|year=="2009"|year=="2011"|
                              year=="2014")
databaserec_2020s <- filter(databaserec, year=="1965"|year=="1974"|
                              year=="1986"|year=="1989"|year=="1995"|
                              year=="2001"|year=="2006"|year=="2007"|
                              year=="2008"|year=="2009"|year=="2011"|
                              year=="2014")

# Load a country shape file ------------------------------------------------

# Load a medium resolution shapefile of the world.
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create the decade maps ---------------------------------------------------

# Create a colour palette with enough colours for one for each decade.
cc_pal <- brewer.pal(n=9, "Spectral")[3:9]

# 1960s map
map_60s <- ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-15, 15), ylim=c(43, 65), expand=FALSE)+ #Crop map to desired area
  cc_theme()+ 
  geom_point(data=mydata_1960s, aes(x=Longitude, y=Latitude, col=Decade), 
             size=2)+                                      #Colour points by decade
  scale_colour_manual(values=cc_pal)+
  scale_fill_discrete(drop=F)+
  geom_star(data=databaserec_1960s, aes(x=lon, y=lat), size=4, 
            colour="black", fill="gold")+                  #Add gold star for first record
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  annotation_scale(location = "tl", line_width = 0.5, 
                   bar_cols=c("black", "dark grey"))+
  scale_x_continuous(breaks=seq(-15, 25, 5))+
  ggtitle("1960s")
map_60s

# 1970s map
map_70s <- ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-15, 15), ylim=c(43, 65), expand=FALSE)+
  cc_theme()+
  geom_point(data=mydata_1970s, aes(x=Longitude, y=Latitude, colour=Decade), 
             size=2)+
  scale_colour_manual(values=cc_pal)+
  scale_fill_discrete(drop=F)+
  geom_star(data=databaserec_1970s, aes(x=lon, y=lat), size=4, 
            colour="black", fill="gold")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  annotation_scale(location = "tl", line_width = 0.5, 
                   bar_cols=c("black", "dark grey"))+
  ggtitle("1970s")
map_70s

# 1980s map
map_80s <- ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-15, 15), ylim=c(43, 65), expand=FALSE)+
  cc_theme()+
  geom_point(data=mydata_1980s, aes(x=Longitude, y=Latitude, colour=Decade), 
             size = 2)+
  scale_colour_manual(values=cc_pal)+
  scale_fill_discrete(drop=F)+
  geom_star(data=databaserec_1980s, aes(x=lon, y=lat), size=4, 
            colour="black", fill="gold")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  annotation_scale(location = "tl", line_width = 0.5, 
                   bar_cols=c("black", "dark grey"))+
  ggtitle("1980s")
map_80s

# 1990s map
map_90s <- ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-15, 15), ylim=c(43, 65), expand=FALSE)+
  cc_theme()+
  geom_point(data=mydata_1990s, aes(x=Longitude, y=Latitude, colour=Decade), 
             size = 2)+
  scale_colour_manual(values=cc_pal)+
  scale_fill_discrete(drop=F)+
  geom_star(data=databaserec_1990s, aes(x=lon, y=lat), size=4, 
            colour="black", fill="gold")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  annotation_scale(location = "tl", line_width = 0.5, 
                   bar_cols=c("black", "dark grey"))+
  ggtitle("1990s")
map_90s

# 2000s map
map_00s <- ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-15, 15), ylim=c(43, 65), expand=FALSE)+
  cc_theme()+
  geom_point(data=mydata_2000s, aes(x=Longitude, y=Latitude, colour=Decade), 
             size = 2)+
  scale_colour_manual(values=cc_pal)+
  scale_fill_discrete(drop=F)+
  geom_star(data=databaserec_2000s, aes(x=lon, y=lat), size=4, 
            colour="black", fill="gold")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  annotation_scale(location = "tl", line_width = 0.5, 
                   bar_cols=c("black", "dark grey"))+
  ggtitle("2000s")
map_00s

# 2010s map
map_10s <- ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-15, 15), ylim=c(43, 65), expand=FALSE)+
  cc_theme()+
  geom_point(data=mydata_2010s, aes(x=Longitude, y=Latitude, colour=Decade), 
             size = 2)+
  scale_colour_manual(values=cc_pal)+
  scale_fill_discrete(drop=F)+
  geom_star(data=databaserec_2010s, aes(x=lon, y=lat), size=4, 
            colour="black", fill="gold")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  annotation_scale(location = "tl", line_width = 0.5, 
                   bar_cols=c("black", "dark grey"))+
  ggtitle("2010s")
map_10s

#2020s map
map_20s <- ggplot(data=world)+
  geom_sf(colour="black", fill="light grey")+
  coord_sf(xlim=c(-15, 15), ylim=c(43, 65), expand=FALSE)+
  cc_theme()+
  geom_point(data=mydata_2020s, aes(x=Longitude, y=Latitude, colour=Decade), 
             size = 2)+
  scale_colour_manual(values=cc_pal)+
  scale_fill_discrete(drop=F)+
  geom_star(data=databaserec_2020s, aes(x=lon, y=lat), size=4, 
            colour="black", fill="gold")+
  theme(legend.position="right", legend.title=element_text(face="bold"))+
  annotation_scale(location = "tl", line_width = 0.5, 
                   bar_cols=c("black", "dark grey"))+
  ggtitle("2020s")
map_20s

# Edit the maps externally in Inkscape and combine to create a panel.

