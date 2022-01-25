####https://cran.r-project.org/web/packages/marmap/vignettes/marmap-DataAnalysis.pdf###
####https://cran.r-project.org/web/packages/marmap/vignettes/marmap.pdf
###R Bathymetry
library(marmap)

#load NOAA map
papoue <- getNOAA.bathy(lon1 = -149, lon2 = -145.5,lat1 = 59, lat2 = 61.5, resolution = .7)
summary(papoue)

#Creating color palettes
###blues <- c("lightsteelblue4", "lightsteelblue3","lightsteelblue2", "lightsteelblue1")
###greys <- c(grey(0.6), grey(0.93), grey(0.99))
blues <- colorRampPalette(c("red","purple","blue","cadetblue1","white"))

#############Plot map##############
####################################


#blues <- c("lightsteelblue4", "lightsteelblue3","lightsteelblue2", "lightsteelblue1")
#greys <- c(grey(0.6), grey(0.93), grey(0.99))
#plot(papoue, image = TRUE, land = TRUE, lwd = 0.03,
#bbpal = list(c(0, max(papoue), greys),
#c(min(papoue), 0, blues)))
#plot(papoue, n = 1, lwd = 0.4, add = TRUE)
#scaleBathy(papoue, deg = 1, x = "topleft", inset = 5)

#plot(papoue, image = TRUE)

#blues <- colorRampPalette(c("red","purple","blue","cadetblue1","white"))
#plot(papoue, image = TRUE, bpal = blues(100),
#deep = c(-9000, -3000, 0),
#shallow = c(-3000, -10, 0),
#step = c(1000, 1000, 0),
#lwd = c(0.8, 0.8, 1), lty = c(1, 1, 1),
#col = c("lightgrey", "darkgrey", "black"),
#drawlabel = c(FALSE, FALSE, FALSE))
#scaleBathy(papoue, deg = 1, x = "topleft", inset = 5)
??shallow

##### Add coastline######
#plot(papoue, deep = 0, shallow = 0, step = 0,lwd = 0.4, add = TRUE)


######Add Scale Bar######
#scaleBathy(papoue, deg = 1, x = "topleft", inset = 5)

#################################################
#####Plot points and text on map see below####3
###############################################

###Export image
tiff()


########################Data########################
####################################################

#####Get depths by clicking####
#####Click map and stop to get data#
get.depth(papoue, distance = TRUE)

####Get depths from table######
##data file head (x,y,station)

#Plot points and text on map see below####
blues <- c("lightsteelblue4", "lightsteelblue3","lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))

library("colorspace")
pal <- choose_palette()
file.remove("S:/ADU/ZZ McNeel/152/PWS_bath_map2.pdf")
pdf("S:/ADU/ZZ McNeel/152/PWS_bath_map2.pdf",width=7,height=6)


plot(papoue, image = TRUE, land = TRUE,n=0,col=grey(0.93), 
bpal = list(c(0, max(papoue), greys),
c(min(papoue), 0, blues)))

rockfish <- get.area(papoue, level.inf = -500, level.sup = -400)
plotArea(rockfish, col = pal(12)[11])
rockfish <- get.area(papoue, level.inf = -400, level.sup = -300)
plotArea(rockfish, col = pal(12)[10])
rockfish <- get.area(papoue, level.inf = -300, level.sup = -200)
plotArea(rockfish, col = pal(12)[9])

plot(papoue, deep=-5000, shallow=-1300, step=1000, lwd=0.5, add=TRUE)
plot(papoue, deep=-500, shallow=-100, step=100,cex=.5,font=2,lwd=1, add=TRUE, drawlabel=TRUE, col="lightsteelblue4")

text(-146.99, 60.70, "Prince William\nSound", col = "white", font = 4,cex=.75)
text(-147, 60.71, "Prince William\nSound", col = "black", font = 4,cex=.75)
text( -147.4662, 60.04346, "Montague Is.",col = "white", font = 4,srt=35,cex=.75)
text(  -146.4672, 60.41150, "Hinchinbrook\nIs.",col = "white", font = 4,cex=.50)
text(-147.8067, 59.98212, "Montague Strait",col = "white", font = 4, srt=35, cex=.64)
text(-147.8167, 59.98212, "Montague Strait",col = "black", font = 4, srt=35, cex=.64)
text(-146.81, 60.23624, "Hinch. Channel",col = "white", font = 4,srt=-70, cex=.64)
text(-146.8, 60.23624, "Hinch. Channel",col = "black", font = 4,srt=-70, cex=.64)
text(-145.99, 59.79, "Gulf of Alaska",col = "white", font = 4)
text(-146, 59.8, "Gulf of Alaska",col = "black", font = 4)

scaleBathy(papoue, deg = 1, x = "topright", inset = 10,font=2,lwd=2)

dev.off()

###################################################
# add sampling points, and add text to the plot:
##################################################

lon= c(-149.466667
lat= c(59.835278
station= c(GAK1
sampling=data.frame(lon,lat,station

points(sampling$lon, sampling$lat, pch = 21, col = "black",
bg = "yellow", cex = 1)

###############################################
#add colored region
###############################################

col.rf <- c(rgb(0.7, 0, 0, 0.8),rgb(0.7, 0, 0, 0.6),rgb(0.7, 0, 0, 0.3))

rockfish <- get.area(papoue, level.inf = -500, level.sup = -400)
plotArea(rockfish, col = col.rf)
rockfish <- get.area(papoue, level.inf = -500, level.sup = -300)
plotArea(rockfish, col = col.rf)
rockfish <- get.area(papoue, level.inf = -500, level.sup = -200)
plotArea(rockfish, col = col.rf)