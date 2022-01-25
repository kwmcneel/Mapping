########From Kevin: other useful packages#######
##rworldmap
##plyr
##ggmap
###############From Lorna#################
##sp
##maptools
##rgdal


#####Get package sp here:
#http://cran.r-project.org/web/packages/sp/index.html

#####Get package maptools here:
#http://cran.r-project.org/web/packages/maptools/index.html

#####Get some shapefiles:
#http://www.naturalearthdata.com/

#####Load shapefiles
library(sp)
library(maptools)
#install.packages("rgdal") #for changing projection
library(rgdal)

#Must have all files along with .shp file (.dbf, .prj., .shx ...)
#Choose the .shp file
countries.shp = readShapeLines(file.choose())
places.shp = readShapePoints(file.choose())
AKCoast.shp = readShapePoly(file.choose())
areas.shp = readShapePoly(file.choose())

###Check to see if reference system is defined:
proj4string(countries.shp)

#####If not, then define it (to be able to transform it):
## Use http://www.spatialreference.org/ to find the required string
proj4string(countries.shp) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
proj4string(places.shp) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
proj4string(AKCoast.shp) = CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")


####Choose the projection you want
CRS.new = CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
####Apply the projection to shapefiles
ak.countries = spTransform(countries.shp, CRS.new)
ak.places = spTransform(places.shp, CRS.new)
AKCoast = spTransform(AKCoast.shp, CRS.new)

###Bring in AHRP data
Recoveries = read.table("clipboard", header=T, sep="\t")
Recoveries[,1]  #streams
ls(Recoveries)
######Assign the recovery data a projection
coordinates(Recoveries)=~RecLng+RecLat
proj4string(Recoveries)=CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
##Reproject to the same AK Albers canonical

data_name<-"ak.13"
assign(data_name,spTransform(Recoveries, CRS("+proj=longlat")))
data_name<-"ak.countries"
assign(data_name,spTransform(countries.shp, CRS("+proj=longlat")))
data_name<-"ak.places"
assign(data_name,spTransform(places.shp, CRS("+proj=longlat")))
data_name<-"AKCoast"
assign(data_name,spTransform(AKCoast.shp, CRS("+proj=longlat")))

##To get legend and labels to fit on the figure we can change the size of the
bBox <- bbox(ak.13)
ynudge <- 0.5
xnudge <- 3
xlim <- c(bBox[1,1] + xnudge , bBox[1,2] - xnudge)
ylim <- c(bBox[2,1] - ynudge, bBox[2,2] )
ak.13
########Location map
plot(ak.13, axes=TRUE, xlim=xlim, ylim=ylim, pch=1, cex=.5, col="red")
text(ak.13,labels=Recoveries$ADU, pos=runif(30,1,4), col="blue", offset=0.5, cex=.7)
plot(AKCoast, col="seashell2", add=T)
lines(ak.countries, col="black")
compassRose(bBox[1,1] + xnudge,bBox[2,1] - ynudge,rot=0,cex=1)
??random

compassRose<-function(x,y,rot=0,cex=1) {
 oldcex<-par(cex=cex)
 mheight<-strheight("M")
 xylim<-par("usr")
 plotdim<-par("pin")
 xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1]
 point.angles<-seq(0,2*pi,by=pi/4)+pi*rot/180
 crspans<-rep(c(mheight*3,mheight/2),length.out=9)
 xpoints<-cos(point.angles)*crspans*xmult+x
 ypoints<-sin(point.angles)*crspans+y
 for(point in 1:8) {
  pcol<-ifelse(point%%2,"black","white")
  polygon(c(xpoints[c(point,point+1)],x),c(ypoints[c(point,point+1)],y),col=pcol)
 }
 txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.2*crspans[1]*xmult+x
 txtypoints<-sin(point.angles[c(1,3,5,7)])*1.2*crspans[1]+y
 text(txtxpoints,txtypoints,c("E","N","W","S"))
 par(oldcex)
}

