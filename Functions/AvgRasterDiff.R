### AE Melton
# This script will extract suitability scores for two predictions rasters and calculate an average # difference between them.
#
# r1 == prediction raster for species a
# r2 == prediction raster for species b
# plot == logical; T will return a raster plot of the difference per cell
#

AvgRasterDiff <- function(r1, r2, plot = F) {
 
  diff <- abs(r1 - r2)
  
  if (plot == TRUE) {
  diff.plot <- plot(diff)
  } else {
  diff.plot <- NULL
  }
  
  diff <- as.data.frame(diff)
  
  sum.diff <- sum(diff, na.rm = T)
  
  avg.diff <- sum.diff/(ncell(r1)-freq(r1, value=NA))
  
  return(list(diff.plot, avg.diff))
}
