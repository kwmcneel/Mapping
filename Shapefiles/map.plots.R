plot.new()
?map

###load package mapdata
library(maps)
library(mapdata)

#######USA doesn't include Alaska
map('usa')
map('state', boundary =TRUE)
map('rivers', add=TRUE, names = T)
map.axes()


###World map with major world rivers
map('world', fill = F, xlim=c(-165, -110), ylim=c(41, 67))
map('rivers', add=TRUE, col = 4)
map("state", add=TRUE)
map.axes()


###Pacific Northwest
map('world2Hires', xlim=c(180, 250), ylim=c(35, 72))

?world2Hires

