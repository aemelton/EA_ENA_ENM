### AE Melton
# Calculates mahalanobis distance between two sets of rasters.

#
# @usr.ref == raster file; I use a prediction raster here
# @usr.proj == raster file; I generally use a future climate or novel environment raster here
#

CalcMahalanobisDist <- function(usr.ref, usr.proj){

refdat <- as.data.frame(usr.ref)
head(refdat)

projdat <- as.data.frame(usr.proj)
head(projdat)

# Calculate the average and covariance matrix of the variables 
# in the reference set

ref.av <- colMeans(refdat, na.rm=TRUE)
ref.cov <- var(refdat, na.rm=TRUE)

# Calculate the mahalanobis distance of each raster 
# cell to the environmental center of the reference 
# set for both the reference and the projection data 
# set and calculate the ratio between the two.

mah.ref <- mahalanobis(x=refdat, center=ref.av, cov=ref.cov, tol=1e-20)
mah.pro <- mahalanobis(x=projdat, center=ref.av, cov=ref.cov, tol=1e-20)
mah.max <- max(mah.ref[is.finite(mah.ref)])
nt2 <- as.data.frame(mah.pro / mah.max)

# Create and plot the raster layer
NT2 <- usr.proj[[1]]
NT2@data <- nt2
NT2rast <- raster(NT2)
plot(NT2rast, col=rainbow(100))
}
