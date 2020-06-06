### AE Melton
# This script will sample an environmental raster or raster stack at points sampled from a binary
# predicted presence/absence raster to generate hypervolumes.
# Future iterations of this script will add in hypervolume comparisons and overlap analyses

#
# bin.proj == a binary predicted presence/absence raster
# usr.env == a raster or raster stack of environmental data
#

GetHypervolume <- function(bin.proj, usr.env, do.scale = FALSE, do.center = FALSE, set.scale = FALSE) {
  
  bin.proj[bin.proj == 0] <- NA
  
  dist.points <-  rasterToPoints(bin.proj)
  
  hv.dat <- extract(usr.env, dist.points[,1:2]);
  
  hv.dat <- hv.dat[complete.cases(hv.dat),];
  
  if (do.scale == TRUE) {

     hv.dat <- scale(hv.dat, center = do.center, scale = set.scale)

  }

  #estimate_bandwidth(enaEnvt, method = "silverman")
  
  #enaExp <- expectation_ball(enaEnvt)
  
  hv <- hypervolume(data = hv.dat, method = "box")
  
  return(hv)
}
