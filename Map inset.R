library(maps)

  # more high-resolution world data (you might not need but what the heck)
  library(mapdata)

  # extra stuff like gridlines, etc.
  library(mapproj)

# Next, we create a new graphics space in the lower-right hand corner.  The numbers are proportional distances within the graphics window (xmin,xmax,ymin,ymax) on a scale of 0 to 1.
# "plt" is the key parameter to adjust
par(plt = c(0.70, 0.90, 0.2, 0.42), new = TRUE)

# I think this is the key command from http://www.stat.auckland.ac.nz/~paul/RGraphics/examples-map.R
    plot.window(xlim=c(-170,-135),ylim=c(50,75))

  # fill the box with white
    polygon(c(-175,-175,-120,-120),c(50,75,75,50),col="white")

  # draw the map
    map(xlim=c(-170,-135),ylim=c(50,75), interior=FALSE,
          add=TRUE,fill=TRUE, col="blanchedalmond")

  # draw a teeny little box showing where the big map is sampled from
    polygon(x=c(-149,-145,-145,-149,-149),
        y=c(58.5,58.5,61.5,61.5,58.5),border="red",lwd=3)


dev.off()

