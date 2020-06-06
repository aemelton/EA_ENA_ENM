### AE Melton
# This script uses functions from dismo and geospatial packages to sample n background points
# within a d sized buffer around occurrence points.

#
# usr.occ == data.frame with occurrence data
# radius == size of radius for buffer within which the background points will be sampled
# n == number of background points to sample
# usr.env == raster or raster stack of environmental variables
#

GetBackgroundPoints <- function(usr.occ, radius, n, usr.env, samp.method){

if(samp.method == "near.points"){
  x <- circles(SpatialPoints(usr.occ), d = radius, lonlat=TRUE)
  pol <-  gUnaryUnion(x@polygons)
  buffer.raster <- mask(usr.env, pol)
  xy <- sampleRandom(buffer.raster, size=n, xy=TRUE)
  colnames(xy)[1:2] <- c(colnames(usr.occ))
  bg.pts <- as.data.frame(xy[,1:2])
}

if(samp.method == "all.m"){
  xy <- sampleRandom(usr.env, size=n, xy=TRUE)
  colnames(xy)[1:2] <- c(colnames(usr.occ))
  bg.pts <- as.data.frame(xy[,1:2])
}

return(bg.pts)
}
