#https://r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
#if(!require("rgdal")) install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

renv::snapshot()# takes a snapshot of packages that work at that time and downloads a copy to the project directory

#https://geocompr.robinlovelace.net/adv-map.html####
#This website uses the tm_ commands used in this script#
##https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html#
#other website#

if(!require("sf")) install.packages("sf")
if(!require("raster")) install.packages("raster")
if(!require("dplyr")) install.packages("dplyr")

#In addition, it uses the following visualization packages (also install shiny if you want to develop interactive mapping applications):
if(!require("tmap")) install.packages("tmap")    # for static and interactive maps
if(!require("leaflet")) install.packages("leaflet") # for interactive maps
if(!require("ggplot2")) install.packages("ggplot2") # tidyverse data visualization package
if(!require("spData")) install.packages("spData")
if(!require("spDataLarge")) install.packages("spDataLarge")

###Bring in data####
necoast<-st_read("G:/1_LAB OPERATIONS_authorized access only/ADU Reference/Software/R/Mapping/Mapping/Shapefiles/ne 10m coastline/ne_10m_coastline.shp")
neshape<-st_read("G:/1_LAB OPERATIONS_authorized access only/ADU Reference/Software/R/Mapping/Mapping/Shapefiles/ne_10m_land/ne_10m_land.shp")
neriver<-st_read("G:/1_LAB OPERATIONS_authorized access only/ADU Reference/Software/R/Mapping/Mapping/Shapefiles/ne 10m rivers/ne_10m_rivers_lake_centerlines.shp")
gfstat<-st_read("G:/1_LAB OPERATIONS_authorized access only/ADU Reference/Software/R/Mapping/Mapping/Shapefiles/Groundfish_Statistical_Areas_2001/PVG_Statewide_2001_Present_GCS_WGS1984.shp")

library(readxl)

#install.packages("beepr")
#library(beepr)
#beep() #makes sound at end of code

data <- read_excel("G:/1_LAB OPERATIONS_authorized access only/Projects_ADU/Groundfish Chemical profile pilot project/Data/Final Data for NPRB/Yelloweye_Hormone_Isotope_Based_on_Master_with lat long iso.xlsb.xlsx", 
                   sheet = "RawCarbon_Nitrogen")
#data <-read_excel( file.choose())

data.raw<-data
#data<-filter(data.raw, ADFG.Species.Code %in% c("142", "152","143"))
data<-filter(data,long != is.na(data$long))
data<-filter(data,lat != is.na(data$lat))
data$lat<-as.numeric(data$lat)
data$long<-as.numeric(data$long)
data$long<-data$long*-1 #set the longitude to western hemisphere
#colnames(data)[which(names(data) == "Latitude d.min.")] <- "lat"
#colnames(data)[which(names(data) == "Longitude d.min")] <- "long"
#data = read.table("clipboard", header=T, sep="\t")#pull data into 
aggC <- aggregate(list(z=data$d13c.cor),list(x=data$long,y=data$lat),mean)
aggN <- aggregate(list(z=data$`δ15N (‰ Air N2)`),list(x=data$long,y=data$lat),mean)

######Assign a projection####
coordinates(data)=~long+lat
proj4string(data)=CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

coordinates(aggC)=~x+y
proj4string(aggC)=CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

coordinates(aggN)=~x+y
proj4string(aggN)=CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

CRS.new = CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
#CRS.old = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
##Reproject to the same AK Albers canonical
datasp = spTransform(data, CRS.new)
aggCsp= spTransform(aggC, CRS.new)
aggNsp= spTransform(aggN, CRS.new)

# map of AK####
tmap_mode("plot") #view or plot


#Setting the map size and location to the area of interest

bbox_new<-st_bbox(alaska) #larger map of alaska that gives you everything in the N hemisphere
 xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
 yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
 bbox_new[1] <- bbox_new[1] * 0.10  # xmin - left
 bbox_new[3] <- bbox_new[3] * 0.85  # xmax - right
 bbox_new[2] <- bbox_new[2] * 0.50 # ymin - bottom
 bbox_new[4] <- bbox_new[4] * 0.60 # ymax - top
bbox_new<-st_as_sfc(bbox_new)

#Species code
tm_shape(necoast,bbox = bbox_new)+
  tm_lines()+
  tm_graticules(col ="gray80", alpha = 0.3,n.x=5,n.y=5)+
  tm_xlab("Longitude", size = 0.8)+tm_ylab("Latitude", size = 0.8)+
  tm_shape(datasp)+
  #tm_dots(size=.5,shape=21,col="ADFG.Species.Code")+
  tm_bubbles("d13c.cor")
  #tm_shape(aggCsp)+
  #tm_dots(col="z",style="cont",size=2,shape=21,title="d13C")+
  tm_layout(legend.outside = TRUE)


tm_shape(neshape,bbox = bbox_new)+
  tm_fill(col="blanchedalmond")+
  tm_graticules(col ="gray80", alpha = 0.3,n.x=5,n.y=5)+
  tm_xlab("Longitude", size = 0.8)+tm_ylab("Latitude", size = 0.8)+
  tm_shape(datasp)+
  tm_dots(col="ADFG.Species.Code",size=1,shape=21,style="cont")+
  #tm_bubbles("d13c.cor")
  #tm_shape(aggCsp)+
  #tm_dots(col="z",style="cont",size=2,shape=21,title="d13C")+
  tm_layout(legend.outside = TRUE)


tm_shape(neshape,bbox = bbox_new)+
  tm_fill(col="blanchedalmond")+
  tm_graticules(col ="gray80", alpha = 0.3,n.x=5,n.y=5)+
  tm_xlab("Longitude", size = 0.8)+tm_ylab("Latitude", size = 0.8)+
  tm_shape(datasp)+
  tm_dots(size=1,shape=21)+
  #tm_bubbles("d13c.cor")
  #tm_shape(aggCsp)+
  #tm_dots(col="z",style="cont",size=2,shape=21,title="d13C")+
  tm_layout(legend.outside = TRUE)+
tm_shape(neriver)+
  tm_lines(col="blue")

#Average value by location set different values to not run by placing pound in front
tm_shape(neshape,bbox = bbox_new)+
  tm_fill(col="blanchedalmond")+
  tm_graticules(col ="gray80", alpha = 0.3,n.x=5,n.y=5)+
  tm_xlab("Longitude", size = 0.8)+tm_ylab("Latitude", size = 0.8)+
  #tm_shape(datasp)+
  #tm_bubbles("d13c.cor")
  tm_shape(aggCsp)+
  #tm_dots(col="z",style="cont",size=2,shape=21,title=expression(paste(delta ^13, "C")))+
  #tm_shape(aggNsp)+
  tm_dots(col="z",style="cont",size=2,shape=21,title=expression(paste(delta ^15, "N")))+
  tm_layout(legend.outside = TRUE)

#save active map as a file
tmap_save(mp15,"15Nmap.png",height=4, dpi=300)

#Plot geom ridges for distribution data####
theme_set(theme_bw())
library(sf)
library(devtools)
library(ggplot2)
library(ggridges)
#install.packages("ggridges")
#install.packages(Rcpp)
ggplot(data, aes(x=`δ15N (‰ Air N2)`,y=factor(lat)))+
   geom_density_ridges(xlim=c(-12,-16))+
  xlab(expression(paste(delta ^15, "N")))+
  ylab("Lattitude")+
  theme_classic()

ggplot(data, aes(x=`δ15N (‰ Air N2)`,y=factor(Depth1)))+
  geom_density_ridges(xlim=c(-12,-16))+
  xlab(expression(paste(delta ^15, "N")))+
  ylab("Depth")+
  theme_classic()

ggplot(data, aes(x=`δ15N (‰ Air N2)`,y=factor(lat)))+
  geom_density_ridges(xlim=c(-12,-16))+
  xlab(expression(paste(delta ^15, "N")))+
  ylab("Lattitude")+
  theme_classic()

ggplot(data, aes(x=d13c.cor,y=Mat))+
  geom_point()+
  xlab(expression(paste(delta ^15, "N")))+
  ylab("Depth")+
  theme_classic()

#Plot stat areas####
tmap_mode("plot") #view or plot

bbox_new<-st_bbox(alaska) #larger map of alaska that gives you everything in the N hemisphere
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
bbox_new[1] <- bbox_new[1] * 0.10  # xmin - left
bbox_new[3] <- bbox_new[3] * 0.85  # xmax - right
bbox_new[2] <- bbox_new[2] * 0.50 # ymin - bottom
bbox_new[4] <- bbox_new[4] * 0.60 # ymax - top
bbox_new<-st_as_sfc(bbox_new)
gfstat$REGION_COD

tmap_options(check.and.fix = TRUE)
tm_basemap("Esri.WorldStreetMap")+
tm_shape(gfstat)+
  tm_polygons(col=gfstat$REGION_COD)
  tm_graticules(col ="gray80", alpha = 0.3,n.x=5,n.y=5)+
  tm_xlab("Longitude", size = 0.8)+tm_ylab("Latitude", size = 0.8)+
  tm_shape(datasp)+
  #tm_dots(size=.5,shape=21,col="ADFG.Species.Code")+
  tm_bubbles("d13c.cor")
#tm_shape(aggCsp)+
#tm_dots(col="z",style="cont",size=2,shape=21,title="d13C")+
tm_layout(legend.outside = TRUE)