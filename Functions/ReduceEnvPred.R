### AE Melton
# Get correlations for raster layers and figure out which ones to remove.

#
# usr.env == raster stack
# corr.method == Character; statistics include 'cov' (covariance), 'weighted.cov' (weighted      # covariance), or 'pearson' (correlation coefficient) ### RIGHT NOW IT JUST USES PEARSON
# corr.cutoff == numeric; what is the threshold to be removed?
#

ReduceEnvPred <- function(usr.env = NULL, stat = NULL, corr.cutoff = NULL){

corr <- layerStats(usr.env, stat, na.rm = TRUE)

c <- corr$`pearson correlation coefficient`
#write.csv(c, "correlationBioclim.csv") 

c.matrix <- data.matrix(c)

c.abs <- abs(c.matrix)

envtCor <- findCorrelation(c.abs, cutoff = corr.cutoff, names = TRUE, exact = TRUE)

cor.out.sort <- sort(envtCor)

red.envStack <- dropLayer(x = usr.env, i = cor.out.sort)

return(red.envStack)
}
