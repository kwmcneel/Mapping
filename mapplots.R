####GGPLOT bubblemaps#

#https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2.html

# Libraries
library(ggplot2)
if(!require("dplyr"))   install.packages("dplyr")

# Get the world polygon and extract UK
if(!require("maps"))   install.packages("maps")
unique()
UK <- map_data("world2") %>% filter(region=="Alaska")

?map_data
identify(map("world", fill = TRUE, col = 0)) #"state"

# Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
data <- world.cities %>% filter(country.etc=="Alaska")


ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey") +
  geom_point( data=data, aes(x=long, y=lat)) +
  theme_void() + ylim(50,59) + coord_map() 

# Second graphic with names of the 10 biggest cities
library(ggrepel)
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + ylim(50,59) + coord_map() +
  theme(legend.position="none")