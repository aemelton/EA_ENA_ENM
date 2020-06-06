### AE Melton
# This function reads in two rasters and plots density curves of their suitability scores.
# It also performs a T-test. This function requires GGplot2.

#
# @r1 == raster file for "species 1"
# @r2 == raster file for "species 2"
# @sp1 == character - name for species 1
# @sp2 == character - name for species 2
#

CompareSuitability <- function(r1, r2, sp1, sp2){
  
  r1.dat <- as.data.frame(r1)
  r2.dat <- as.data.frame(r2)
  colnames(r1.dat) <- "score"
  colnames(r2.dat) <- "score"
  r1.dat$Projection <- sp1
  r2.dat$Projection <- sp2
  r.dat <- rbind(r1.dat, r2.dat)
  colnames(r.dat) <- c("Predicted Suitability Score", "Projection")
  p <- ggplot(r.dat, aes(`Predicted Suitability Score`, fill = Projection)) +
    xlab("Predicted Suitability Score") +
    geom_density(alpha = 0.2) +
    facet_wrap(~Projection)
  t <- t.test(r.dat$`Predicted Suitability Score`)
  return((list(p, t)))
}
