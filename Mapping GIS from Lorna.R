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
install.packages("rgdal") #for changing projection
library(rgdal)

#Must have all files along with .shp file (.dbf, .prj., .shx ...)
#Choose the .shp file
countries.shp = readShapeLines(file.choose())
places.shp = readShapePoints(file.choose())
AKCoast.shp = readShapePoly(file.choose())

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

######Assign the recovery data a projection
coordinates(Recoveries)=~Long+Lat
proj4string(Recoveries)=CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
##Reproject to the same AK Albers canonical
ak.13 = spTransform(Recoveries, CRS.new)

####Seperate Areas, Pedigree
outNIO = ak.13$Area=="NIO"
outNII = ak.13$Area=="NII"
outNO = ak.13$Area=="NO"
outSI = ak.13$Area=="SI"
outP = ak.13$Area=="P"
NIO = ak.13[outNIO,]
NII = ak.13[outNII,]
NO = ak.13[outNO,]
SI = ak.13[outSI,]
P = ak.13[outP,]

########Stream map
plot(ak.13, col="white")
plot(AKCoast, col="seashell2", add=T)
points(NIO, pch=19, cex=1.5, col="red")
points(NII, pch=19, cex=1.5, col="blue")
points(NO, pch=19, cex=1.5, col="green")
points(SI, pch=19, cex=1.55, col="orange")
points(P, pch=17, cex=1.5, col="blue")
points(ak.places, col = "black", pch=1, cex=1)


########Pedigree plot
plot(P, col="white")
plot(AKCoast, col="seashell2", add=T)
points(P, pch=17, cex=2, col="blue")
points(ak.places, col = "black", pch=1, cex=1.5)

##Inset
plot(ak.13, col="white")
lines(ak.countries, col="black")


