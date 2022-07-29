#https://r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
if(!require("rgdal")) install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

renv::snapshot()

#https://geocompr.robinlovelace.net/adv-map.html####

if(!require("sf")) install.packages("sf")
if(!require("raster")) install.packages("raster")
if(!require("dplyr")) install.packages("dplyr")
if(!require("spData")) install.packages("spData")
if(!require("spDataLarge")) install.packages("spDataLarge")
#In addition, it uses the following visualization packages (also install shiny if you want to develop interactive mapping applications):
if(!require("tmap")) install.packages("tmap")    # for static and interactive maps
if(!require("leaflet")) install.packages("leaflet") # for interactive maps
if(!require("ggplot2")) install.packages("ggplot2") # tidyverse data visualization package


###Bring in data####
necoast<-st_read("G:/1_LAB OPERATIONS_authorized access only/ADU Reference/Software/R/Mapping/Mapping/Shapefiles/ne 10m coastline/ne_10m_coastline.shp")

library(readxl)
data <- read_excel("G:/1_LAB OPERATIONS_authorized access only/Projects_ADU/Groundfish Chemical profile pilot project/Data/Final Data for NPRB/Yelloweye_Hormone_Isotope_Based_on_Master_with lat long iso.xlsb.xlsx", 
                   sheet = "RawCarbon_Nitrogen", col_types = c("text", "numeric", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "skip"))

#data = read.table("clipboard", header=T, sep="\t")
aggC <- aggregate(list(z=data$d13c.cor),list(x=data$long,y=data$lat),mean)
aggN <- aggregate(list(z=data$`δ15N (‰ Air N2)`),list(x=data$long,y=data$lat),mean)
######Assign a projection
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
# map of AK
tmap_mode("plot") #view or plot

bbox_new<-st_bbox(alaska)
#st_bbox(bbox_new)
 xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
 yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
 bbox_new[1] <- bbox_new[1] * 0.10  # xmin - left
 bbox_new[3] <- bbox_new[3] * 0.85  # xmax - right
 bbox_new[2] <- bbox_new[2] * 0.50 # ymin - bottom
 bbox_new[4] <- bbox_new[4] * 0.60 # ymax - top
bbox_new<-st_as_sfc(bbox_new)

tm_shape(necoast,bbox = bbox_new)+
  tm_lines()+
  tm_graticules(col ="gray80", alpha = 0.3,n.x=5,n.y=5)+
  tm_xlab("Longitude", size = 0.8)+tm_ylab("Latitude", size = 0.8)+
  #tm_shape(datasp)+
  #tm_bubbles("d13c.cor")
  tm_shape(aggCsp)+
  tm_dots(col="z",style="cont",size=2,shape=21,title="d13C")+
  tm_layout(legend.outside = TRUE)

tm_shape(necoast,bbox = bbox_new)+
  tm_lines()+
  tm_graticules(col ="gray80", alpha = 0.3,n.x=5,n.y=5)+
  tm_xlab("Longitude", size = 0.8)+tm_ylab("Latitude", size = 0.8)+
  #tm_shape(datasp)+
  #tm_bubbles("d13c.cor")
  tm_shape(aggNsp)+
  tm_dots(col="z",style="cont",size=2,shape=21,title="d15N")+
  tm_layout(legend.outside = TRUE)


# Add border layer to nz shape
tm_shape(alaska) +
  tm_borders() 

# Add fill and border layers to nz shape
tm_shape(bathy)+tm_raster("layer", palette = "-RdBu", legend.show = T, style = "cont")+
  tm_shape(World)+tm_borders()+
  tm_grid(col ="gray80", alpha = 0.3)+tm_fill(col = "black")+
  tm_xlab("Longitude", size = 0.8)+tm_ylab("Latitude", size = 0.8)+
  tm_layout(title = "(a) n = 19", scale = 0.7, main.title.size = 0.8,title.position = c('left','bottom'))

#ggplot
ggplot(alaska)+
  geom_sf(alaska, aes(fill="light gray"))

tmap_tip()

theme_set(theme_bw())
library(sf)
library(devtools)
library(ggplot2)
library(ggridges)
install.packages("ggridges")
install.packages(Rcpp)
ggplot(data, aes(x=d13c.cor,y=factor(long)))+
   geom_density_ridges()

??geom_density_ridges


