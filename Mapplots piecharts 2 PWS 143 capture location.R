####################Pie Charts######################
##################Kevin McNeel######################

#install.packages("mapplots")
#install.packages("shapefiles")
#install.packages("RColorBrewer")
library(rgdal)
library(sp)
library(maptools) 
library(shapefiles)
library(mapplots)
library(RColorBrewer)
library(maps)

####Help file##########
#https://cran.r-project.org/web/packages/mapplots/mapplots.pdf


####Put shape files into R folder in the Program directory in the mapplot/extdata folder

shp.file <- file.path(system.file(package = "mapplots", "extdata"), "ne_10m_land")
irl <- read.shapefile(shp.file)

#####Get data##########

#Input Stat areas
pws_stat.rg <- readOGR("G:ZZ McNeel/R/Shapefiles/PWS_gf_zip","pvg_PWS_stat_areas")
ogrInfo("G:ZZ McNeel/R/Shapefiles/PWS_gf_zip","pvg_PWS_stat_areas")
print(proj4string(pws_stat.rg))
#get lat and long centroid for each area and area label
getSpPPolygonsLabptSlots(pws_stat.rg)
data.frame(pws_stat.rg$STAT_AREA)
#creat pivot of count of specimen per stat area with labeled lat and lon
######plot stat areas with labels#
#plot(pws_stat.rg)
#invisible(text(getSpPPolygonsLabptSlots(pws_stat.rg), labels=as.character(data.frame(pws_stat.rg$STAT_AREA)), cex=0.4))


####Data input, headers Lat, Lon, Site, Count

Recoveries = read.table("clipboard", header=T, sep="\t") #####Copy data from excel into R
#Recoveries$lncount<-log(Recoveries$Count) ##log scale

######Mapping

xlim <- c(-150,-145)
ylim <- c(59,62)
#xyz <- make.xyz(Recoveries$Lon,Recoveries$Lat,Recoveries$Count,Recoveries$STAT_AREA)
agg <- aggregate(list(z=Recoveries$Count),list(x=Recoveries$Lon,y=Recoveries$Lat),sum)
#col <- c(brewer.pal(9,"Set1"),brewer.pal(11,"Set3"))

pdf("S:/ADU/ZZ McNeel/R/Thornyhead_14C_Recovery.pdf",width=7,height=6)
??basemap
basemap(xlim, ylim, main = "Shortspine Thornyhead\n14C Recovery Site")
draw.shape(irl, col="cornsilk")
draw.bubble(agg$x, agg$y, agg$z, maxradius=.1, pch=21, bg="#00FF0050")
legend.z <- round(max(agg$z),0)
legend.bubble("topright", z=legend.z, maxradius=0.1, inset=0.04, bg="lightblue", txt.cex=0.6,
pch=21, pt.bg="#00FF0050")

#text(-143.95, 61.2, "Count",col = "black", font = 4)
text(-146.99, 60.70, "Prince William\nSound", col = "white", font = 4,cex=.75)
text(-147, 60.71, "Prince William\nSound", col = "black", font = 4,cex=.75)
text(-145.99, 59.79, "Gulf of Alaska",col = "white", font = 4)
text(-146, 59.8, "Gulf of Alaska",col = "black", font = 4)

par(plt = c(0.70, 0.90, 0.15, 0.5), new = TRUE)

plot.window(xlim=c(-170,-135),ylim=c(50,75))
polygon(c(-175,-175,-120,-120),c(50,75,75,50),col="white") # fill the box with white
map(xlim=c(-170,-135),ylim=c(40,75), interior=FALSE,add=TRUE,fill=TRUE, col="blanchedalmond")# draw the map
polygon(x=c(-149.5,-143.5,-143.5,-149.5,-149.5),y=c(59.25,59.25,61.51,61.51,59.25),border="red",lwd=3)# draw a teeny little box showing where the big map is sampled from
??map

dev.off()