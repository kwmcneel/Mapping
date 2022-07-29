####################Pie Charts######################
##################Kevin McNeel######################

#install.packages("mapplots")
#install.packages("shapefiles")
#install.packages("RColorBrewer")

####Help file##########
#https://cran.r-project.org/web/packages/mapplots/mapplots.pdf

if(!require("mapplots")) install.packages("mapplots")
if(!require("shapefiles")) install.packages("shapefiles")
if(!require("RColorBrewer")) install.packages("RColorBrewer")

####Put shape files into R folder in the Program directory in the mapplot/extdata folder

shp.file <- file.path(system.file(package = "mapplots", "extdata"), "ne_10m_land")
necoast<-read.shapefile("G:/1_LAB OPERATIONS_authorized access only/ADU Reference/Software/R/Mapping/Mapping/Shapefiles/ne 10m coastline/ne_10m_coastline")
gfstat<-read.shapefile("G:/1_LAB OPERATIONS_authorized access only/ADU Reference/Software/R/Mapping/Mapping/Shapefiles/Groundfish_Statistical_Areas_2001/PVG_Statewide_2001_Present_GCS_WGS1984")



####Data input, headers Lat, Lon, Site, Count

Recoveries = read.table("clipboard", header=T, sep="\t") #####Copy data from excel into R
Recoveries$lncount<-log(Recoveries$Count) ##log scale

######Mapping

xlim <- c(-180,-130.315)
ylim <- c(54.0,70.0)
xyz <- make.xyz(Recoveries$Lon,Recoveries$Lat,Recoveries$lncount,Recoveries$Site)
col <- c(brewer.pal(9,"Set1"),brewer.pal(11,"Set3"))

basemap(xlim, ylim, main = "Release Site")
draw.shape(necoast, col="cornsilk")
draw.shape(gfstat,fill=REGION_CODE)

?gfstat

draw.pie(xyz$x, xyz$y, xyz$z, radius = 0.2, col=col)
legend.pie(-130.6,58.2,labels=c("AH","AB","BC","BH","CB","DI","GaC","GC","K","KB","KeB","LI","MA","NI","NB","PA","NR","SC","TB","TP"), radius=0.4, bty="n", col=col,
cex=0.8, label.dist=1.3)
legend.z <- round(max(xyz$z),0)
legend.bubble(-130.6,57,z=legend.z,round=1,maxradius=0.3,bty="n",txt.cex=0.6)
text(-130.3,56.5,"ln(Count)",cex=0.8)