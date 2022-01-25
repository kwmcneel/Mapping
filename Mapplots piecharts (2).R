####################Pie Charts######################
##################Kevin McNeel######################

#install.packages("mapplots")
#install.packages("shapefiles")
#install.packages("RColorBrewer")

####Help file##########
#https://cran.r-project.org/web/packages/mapplots/mapplots.pdf

library(mapplots)
library(shapefiles)
library(RColorBrewer)

####Put shape files into R folder in the Program directory in the mapplot/extdata folder

shp.file <- file.path(system.file(package = "mapplots", "extdata"), "ne_10m_land")
irl <- read.shapefile(shp.file)

####Data input, headers Lat, Lon, Site, Count

Recoveries = read.table("clipboard", header=T, sep="\t") #####Copy data from excel into R
Recoveries$lncount<-log(Recoveries$Count) ##log scale

######Mapping

xlim <- c(-137,-130.315)
ylim <- c(54.8,58.72)
xyz <- make.xyz(Recoveries$Lon,Recoveries$Lat,Recoveries$lncount,Recoveries$Site)
col <- c(brewer.pal(9,"Set1"),brewer.pal(11,"Set3"))

basemap(xlim, ylim, main = "Release Site")
draw.shape(irl, col="cornsilk")
draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.2, col=col)
legend.pie(-130.6,58.2,labels=c("AH","AB","BC","BH","CB","DI","GaC","GC","K","KB","KeB","LI","MA","NI","NB","PA","NR","SC","TB","TP"), radius=0.4, bty="n", col=col,
cex=0.8, label.dist=1.3)
legend.z <- round(max(xyz$z),0)
legend.bubble(-130.6,57,z=legend.z,round=1,maxradius=0.3,bty="n",txt.cex=0.6)
text(-130.3,56.5,"ln(Count)",cex=0.8)