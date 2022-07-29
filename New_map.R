#References
#https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
#https://learning.nceas.ucsb.edu/2020-11-RRCourse/session-13-geospatial-analysis-in-r.html
#https://www.r-bloggers.com/2019/04/zooming-in-on-maps-with-sf-and-ggplot2/
install.packages(c("cowplot", "googleway", "ggplot2",
                   "ggrepel", "ggspatial", "sf", "rnaturalearth", "rnaturalearthdata","ggmap"))
#devtools::install_github("r-spatial/lwgeom")

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("dplyr")
library("leaflet")
library("scales")
library("ggmap")

#remove.packages("cli")
#install.packages("cli",dependencies = TRUE)

#Tools####
world <- ne_countries(scale = "medium", returnclass = "sf")
ak_regions <- read_sf("G:/1_LAB OPERATIONS_authorized access only/ADU Reference/Software/R/Mapping/Mapping/Shapefiles/resource_map_urn_uuid_fa761459_7b69_4ff1_b389_25abf775094e/shapefile_demo_data/ak_regions_simp.shp")
class(world)
ak_cropped <- st_crop(world, xmin = -2, xmax = -140,
                          ymin = 45, ymax = 70)

ggplot(data = world) +
  geom_sf()+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))



st_crs(ak_regions)
st_crs(world)
ak_regions_3338 <- ak_regions %>%
  st_transform(crs = 3338)

st_crs(ak_regions_3338)

ak_regions_3338 %>%
  select(region)
ak_regions_3338 %>%
  filter(region == "Southeast")

#GIS calc

#pop <- read.csv("shapefiles/alaska_population.csv")
#pop_4326 <- st_as_sf(pop, 
#                     coords = c('lng', 'lat'),
#                     crs = 4326,
#                     remove = F)
#pop_3338 <- st_transform(pop_4326, crs = 3338)
#pop_joined <- st_join(pop_3338, ak_regions_3338, join = st_within)


#Working Map####
nc <- sf::st_read("G:/1_LAB OPERATIONS_authorized access only/ADU Reference/Software/R/Mapping/Mapping/Shapefiles/Groundfish_Statistical_Areas_2001/PVG_Statewide_2001_Present_GCS_WGS1984.shp")
head(nc)

nc_3338 <-  sf::st_transform(nc,crs = 3338)
world_3338 <- sf::st_transform(world,crs = 3338)

#zoom_to <- c(-166.54, 53.8946)  # ~ dutch harbor
#zoom_to <- c(-152.4, 53.8946)  # ~ kodiak
#zoom_to <- c(-161.7694, 60.7943)  # ~ bethel
zoom_to <- c(-163.7694, 60.7943)  # ~ rand
zoom_level <- 3.25

zoom_to_xy <- st_transform(st_sfc(st_point(zoom_to), crs = 4326),
                           crs = 3338)
C <- 40075016.686   # ~ circumference of Earth in meters
x_span <- C / 2^zoom_level
y_span <- C / 2^(zoom_level+.7)

disp_window <- st_sfc(
  st_point(st_coordinates(zoom_to_xy - c(x_span / 2, y_span / 2))),
  st_point(st_coordinates(zoom_to_xy + c(x_span / 2, y_span / 2))),
  crs = 3338
)
ggplot(nc_3338) +
  geom_sf(aes(fill = factor(REGION_COD))) +
  geom_sf(data=world_3338,fill="cornsilk")+
  coord_sf(xlim = st_coordinates(disp_window)[,'X'],
           ylim = st_coordinates(disp_window)[,'Y']) +
  theme_bw() +
  scale_fill_discrete(labels=c('1 Southeast (232)','2 Central (184)','3 AYK (114)','4 Westward (1160)','| Donut Hole (48)'))+
  scale_x_continuous(breaks = seq(-180, -130, by = 10)) +
  theme(legend.background=element_rect(fill = alpha("white", 0.7)),
            legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.title.align =0.9)+
  labs(fill = expression(atop(bold('ADF&G Statistical Area Map'),paste("" [By] [Region])))) 
ggsave("plots/ADFG_statmap.jpeg",dpi=300)
 # scale_fill_continuous(low = "khaki", high =  "firebrick", labels = comma)
#?legend.title
