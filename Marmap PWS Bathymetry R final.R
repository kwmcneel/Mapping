####https://cran.r-project.org/web/packages/marmap/vignettes/marmap-DataAnalysis.pdf###
####https://cran.r-project.org/web/packages/marmap/vignettes/marmap.pdf
###R Bathymetry
library(marmap)
library(maps)
library(mapdata)
library(mapproj)

#######Reset defaults#######

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}
par(resetPar())     ## reset the pars to defaults

#######################
###Load raster data####
######################

library(raster)

r <- raster("S:/ADU/Prince_William_Sound_DEM_1862/prince_william_sound_ak_8s.asc")
r<-rotate(r)
#plot(r, main='RasterLayer from file')
pws<-as.bathy(r)

####################
#######Set Colors###
####################

blues <- c("lightsteelblue4", "lightsteelblue3","lightsteelblue2", "lightsteelblue1")
greys <- c(grey(0.6), grey(0.93), grey(0.99))

library("colorspace")
pal <- choose_palette()

##################
###Export image###
##################

file.remove("S:/ADU/ZZ McNeel/152/PWS_bath_map2.pdf")
tiff("S:/ADU/ZZ McNeel/152/PWS_bath_map2.tiff",width=6.9,height=6.1,units="in",res=72)
?tiff()
#############Plot map##############
####################################

plot(pws, image = TRUE, land = TRUE,n=0,col="lightsteelblue1", 
bpal = list(c(0, max(pws), greys),
c(min(pws), 0, blues)))

plot(pws, deep=-5000, shallow=-1300, step=1000, lwd=0.5, add=TRUE)
plot(pws, deep=-500, shallow=-100, step=100,cex=.5,font=2,lwd=1, add=TRUE, drawlabel=TRUE, col="lightsteelblue4")

rockfish1 <- get.area(pws, level.inf = -1200, level.sup = -500)
plotArea(rockfish1, col = pal(12)[12])
rockfish2 <- get.area(pws, level.inf = -500, level.sup = -400)
plotArea(rockfish2, col = pal(12)[11])
rockfish3 <- get.area(pws, level.inf = -400, level.sup = -300)
plotArea(rockfish3, col = pal(12)[10])
rockfish4 <- get.area(pws, level.inf = -300, level.sup = -200)
plotArea(rockfish4, col = pal(12)[9])

text(-146.99, 60.70, "Prince William\nSound", col = "white", font = 4,cex=.75)
text(-147, 60.71, "Prince William\nSound", col = "black", font = 4,cex=.75)
text( -147.4662, 60.04346, "Montague Is.",col = "white", font = 4,srt=35,cex=.75)
text(  -146.4672, 60.41150, "Hinchinbrook\nIs.",col = "white", font = 4,cex=.64)
text(-147.8067, 59.98212, "Montague Strait",col = "white", font = 4, srt=35, cex=.64)
text(-147.8167, 59.98212, "Montague Strait",col = "black", font = 4, srt=35, cex=.64)
text(-146.81, 60.23624, "Hinch. Channel",col = "white", font = 4,srt=-70, cex=.64)
text(-146.8, 60.23624, "Hinch. Channel",col = "black", font = 4,srt=-70, cex=.64)
text(-145.99, 59.79, "Gulf of Alaska",col = "white", font = 4)
text(-146, 59.8, "Gulf of Alaska",col = "black", font = 4)

scaleBathy(pws, deg = 1, x = "topright", inset = 10,font=2,lwd=2)


############Create Inset#############

par(plt = c(0.70, 0.90, 0.15, 0.37), new = TRUE)

plot.window(xlim=c(-170,-135),ylim=c(50,75))
polygon(c(-175,-175,-120,-120),c(50,75,75,50),col="white") # fill the box with white
map(xlim=c(-170,-135),ylim=c(50,75), interior=FALSE,add=TRUE,fill=TRUE, col="blanchedalmond")# draw the map
polygon(x=c(-149,-145,-145,-149,-149),y=c(58.5,58.5,61.5,61.5,58.5),border="red",lwd=3)# draw a teeny little box showing where the big map is sampled from

##########Close PDF#################

dev.off()


###################################################################################################
#################################################################################################
#######################################Garbage dump############################################

#load NOAA map
#pws <- getNOAA.bathy(lon1 = -149, lon2 = -145.5,lat1 = 59, lat2 = 61.5, resolution = .7)
#summary(pws)

#blues <- c("lightsteelblue4", "lightsteelblue3","lightsteelblue2", "lightsteelblue1")
#greys <- c(grey(0.6), grey(0.93), grey(0.99))
#plot(pws, image = TRUE, land = TRUE, lwd = 0.03,
#bbpal = list(c(0, max(pws), greys),
#c(min(pws), 0, blues)))
#plot(pws, n = 1, lwd = 0.4, add = TRUE)
#scaleBathy(pws, deg = 1, x = "topleft", inset = 5)

#plot(pws, image = TRUE)

#blues <- colorRampPalette(c("red","purple","blue","cadetblue1","white"))
#plot(pws, image = TRUE, bpal = blues(100),
#deep = c(-9000, -3000, 0),
#shallow = c(-3000, -10, 0),
#step = c(1000, 1000, 0),
#lwd = c(0.8, 0.8, 1), lty = c(1, 1, 1),
#col = c("lightgrey", "darkgrey", "black"),
#drawlabel = c(FALSE, FALSE, FALSE))
#scaleBathy(pws, deg = 1, x = "topleft", inset = 5)

##### Add coastline######
#plot(pws, deep = 0, shallow = 0, step = 0,lwd = 0.4, add = TRUE)


######Add Scale Bar######
#scaleBathy(pws, deg = 1, x = "topleft", inset = 5)

#################################################
#####Plot points and text on map see below####3
###############################################

###Export image
tiff()


########################Data########################
####################################################

#####Get depths by clicking####
#####Click map and stop to get data#
get.depth(pws, distance = TRUE)

####Get depths from table######
##data file head (x,y,station)
plot(pws, image = TRUE, land = TRUE,n=0,col="lightsteelblue1", 
bpal = list(c(0, max(pws), greys),
c(min(pws), 0, blues)))

plot(pws, deep=-5000, shallow=-1300, step=1000, lwd=0.5, add=TRUE)
plot(pws, deep=-500, shallow=-100, step=100,cex=.5,font=2,lwd=1, add=TRUE, drawlabel=TRUE, col="lightsteelblue4")

rockfish1 <- get.area(pws, level.inf = -1200, level.sup = -500)
plotArea(rockfish1, col = pal(12)[12])
rockfish2 <- get.area(pws, level.inf = -500, level.sup = -400)
plotArea(rockfish2, col = pal(12)[11])
rockfish3 <- get.area(pws, level.inf = -400, level.sup = -300)
plotArea(rockfish3, col = pal(12)[10])
rockfish4 <- get.area(pws, level.inf = -300, level.sup = -200)
plotArea(rockfish4, col = pal(12)[9])

text(-146.99, 60.70, "Prince William\nSound", col = "white", font = 4,cex=.75)
text(-147, 60.71, "Prince William\nSound", col = "black", font = 4,cex=.75)
text( -147.4662, 60.04346, "Montague Is.",col = "white", font = 4,srt=35,cex=.75)
text(  -146.4672, 60.41150, "Hinchinbrook\nIs.",col = "white", font = 4,cex=.64)
text(-147.8067, 59.98212, "Montague Strait",col = "white", font = 4, srt=35, cex=.64)
text(-147.8167, 59.98212, "Montague Strait",col = "black", font = 4, srt=35, cex=.64)
text(-146.81, 60.23624, "Hinch. Channel",col = "white", font = 4,srt=-70, cex=.64)
text(-146.8, 60.23624, "Hinch. Channel",col = "black", font = 4,srt=-70, cex=.64)
text(-145.99, 59.79, "Gulf of Alaska",col = "white", font = 4)
text(-146, 59.8, "Gulf of Alaska",col = "black", font = 4)

par(plt=c(x1,x2,y1,y2),new=TRUE)

scaleBathy(pws, deg = 1, x = "topright", inset = 10,font=2,lwd=2)


dev.off()

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}
par(resetPar())     ## reset the pars to defaults

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

rockfish <- get.area(pws, level.inf = -500, level.sup = -400)
plotArea(rockfish, col = col.rf)
rockfish <- get.area(pws, level.inf = -500, level.sup = -300)
plotArea(rockfish, col = col.rf)
rockfish <- get.area(pws, level.inf = -500, level.sup = -200)
plotArea(rockfish, col = col.rf)