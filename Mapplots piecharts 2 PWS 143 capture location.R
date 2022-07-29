####################Pie Charts######################
##################Kevin McNeel######################

#install.packages("mapplots")
#install.packages("shapefiles")
#install.packages("RColorBrewer")
if(!require("rgdal"))   install.packages("rgdal")
if(!require("sp"))   install.packages("sp")
if(!require("maptools"))   install.packages("maptools")
if(!require("shapefiles"))   install.packages("shapefiles")
if(!require("mapplots"))   install.packages("mapplots")
if(!require("RColorBrewer"))   install.packages("RColorBrewer")
if(!require("maps"))   install.packages("maps")


####Help file##########
#https://cran.r-project.org/web/packages/mapplots/mapplots.pdf


####Put shape files into R folder in the Program directory in the mapplot/extdata folder

#shp.file <- file.path(system.file(package = "mapplots", "extdata"), "ne_10m_land")
irl <- read.shapefile("G:/1_LAB OPERATIONS_authorized access only/ADU Reference/Software/R/Mapping/Mapping/Shapefiles/ne 10m coastline/ne_10m_coastline")

#####Get data##########

#Input Stat areas
pws_stat.rg <- readOGR("Shapefiles/PWS_gf_zip","pvg_PWS_stat_areas")
ogrInfo("Shapefiles/PWS_gf_zip","pvg_PWS_stat_areas")
print(proj4string(pws_stat.rg))

#get lat and long centroid for each area and area label
getSpPPolygonsLabptSlots(pws_stat.rg)
data.frame(pws_stat.rg$STAT_AREA)

#creat pivot of count of specimen per stat area with labeled lat and lon
######plot stat areas with labels#

plot(pws_stat.rg)
#invisible(text(getSpPPolygonsLabptSlots(pws_stat.rg), labels=as.character(data.frame(pws_stat.rg$STAT_AREA)), cex=0.4))


####Data input, headers Lat, Lon, Site, Count

Recoveries = read.table("clipboard", header=T, sep="\t") #####Copy data from excel into R


######Mapping

xlim <- c(-150,-142)
ylim <- c(52,66)

#xyz <- make.xyz(Recoveries$Lon,Recoveries$Lat,Recoveries$Count,Recoveries$STAT_AREA)
agg <- aggregate(list(z=Recoveries$Count),list(x=Recoveries$Lon,y=Recoveries$Lat),sum)
agg <- aggregate(list(z=data$d13c.cor),list(x=data$long,y=data$lat),mean)
?aggregate
#col <- c(brewer.pal(9,"Set1"),brewer.pal(11,"Set3"))
agg
pdf("Thornyhead_14C_Recovery.pdf",width=7,height=6)
basemap(xlim, ylim, main = "Shortspine Thornyhead\n14C Recovery Site", bg="white")
?basemap
draw.shape(irl)

draw.bubble(agg$x, agg$y, abs(agg$z), maxradius=.1, pch=21, bg="#00FF0050")
abs(agg$z)
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


dev.off()
