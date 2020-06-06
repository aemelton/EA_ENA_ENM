### AE Melton
# This function will rarefy occurrence data. First, it makes sure there is only one
# occurrence per raster cell using a command from the sp (???) package. Then, it uses
# the thin.max.r script from Dan Warren to rarefy in g-space.

#
# usr.occs == data frame of occurrence data to be rarefied.
# usr.ref == raster to be used as reference for rarefication
# usr.max == What is the maximum number of occurrences to keep?

RarefyOccurrences <- function(usr.occ, usr.ref, usr.max = nrow(usr.occ)){

	usr.occ.thin <- gridSample(xy = usr.occ, r = usr.ref[[1]], n = 1)

	if (nrow(usr.occ.thin) > usr.max) {
  		usr.occ.thin <- thin.max(usr.occ.thin, c("longitude", "latitude"), usr.max)
	}

	#if (usr.output == TRUE){
	#write.csv(x = usr.occ.thin, file = usr.output)
	#}
	
	return(usr.occ.thin)
}
