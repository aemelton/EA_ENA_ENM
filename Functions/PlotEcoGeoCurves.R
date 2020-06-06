### AE Melton
# This function will extract data from environmental rasters at random points
# within a species estimated distribution and plot out a histogram of the
# predicted realized niche space for the variable.

#
# method == "points" or "sdm"
# usr.env == a raster or raster stack with environmental data to be sampled.
# usr.occs == data frame of occurrence data.
# usr.sdm == a binary presence/absence raster.
# usr.variable == If inputting raster stack, which variable do you want to plot? Will make it plot all variables at some point....
#

PlotEcoGeoCurves <- function(method = NULL,
usr.env = NULL, 
usr.occs = NULL, 
usr.raster = NULL,
usr.variable = NULL){

	if(method == "points"){
	env.dat <- extract(usr.env, usr.occs[,1:2])
	env.dat <- env.dat[complete.cases(env.dat),]
	env.dat <- as.data.frame(env.dat)
	}
	
	if(method == "sdm"){
	dist.points <-  rasterToPoints(usr.sdm)
	env.dat <- extract(usr.env, dist.points[,1:2])
	env.dat <- env.dat[complete.cases(env.dat),]
	env.dat <- as.data.frame(env.dat)
	}
	
	p <- ggplot(env.dat, aes(x = env.dat[[usr.variable]]))+
  			geom_density(color="red", fill="grey") +
  			theme()
	plot(p)
}
