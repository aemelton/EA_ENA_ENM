### AE Melton
# This function is used to query databases for occurrence data and then clean the data
# to get rid of unlikely or poor quality occurrences.

#
# method == How do you want to get and clean data? Only using spocc package now.
# usr.query == a species name or list of species name to make query with.
# usr.db == Which databases fo you want to query? See spocc package for more details.
# usr.ref == a raster file used to ensure all occurrence data are in a cell that contains environmental data.
# usr.output == a csv with occurrence data that passed this QC.
#

GetOccurrenceData <- function(method = "scrubr", usr.query, usr.db, usr.output){
	print("This function currently uses scrubr to clean data. Later additions will expand upon just running a bunch of commands together. Maybe...")
	
	if(method == "scrubr"){
	spocc <- occ(query = usr.query, from = usr.db, has_coords = T)
  	
  	spocc <- fixnames(spocc, how = "query")

	allSpocc <- occ2df(spocc)
	
	allSpocc <- date_standardize(allSpocc, "%d%b%Y")
	
	paste("Raw points from spocc query: ", nrow(allSpocc), " points.", sep = "")

	#Step 1: Using scrubr
	scrubbed <- coord_incomplete(allSpocc) # Removes data with incomplete coordinates
	
	scrubbed <- coord_unlikely(scrubbed) # Removes data with unlikely coordinates (i.e. 0,0)

	paste("Step 1: Post scrubr: ", nrow(scrubbed), " points left.", sep = "")

	#Step 2: Removing duplicate dates and localities
	scrubbed <- scrubbed[,-6] # Removes unique, database-specific keys
	
	unique <- unique(scrubbed[,-4]) # Removes duplicate points (not considering data provider)

	paste("Step 2: Removing duplicate dates and localities: ", nrow(unique), " points left.", sep = "")

	#Step 3: Removing points with no environmental data
#	G_SPECIESExtract <- extract(usr.ref, sapply(unique[2:3], as.numeric))

#	G_SPECIESExtract<- cbind(unique, G_SPECIESExtract)

	#cleanExtract <- G_SPECIESExtract[complete.cases(G_SPECIESExtract[,5]),]

#	paste("Step 3: Remove points with no environmental data: ", nrow(cleanExtract), " points left.", sep = "")

#	#Step 4: Reduce points to resolution of reference raster
#	rasterResolution <- max(res(usr.ref))
#
#	while(min(nndist(cleanExtract[,2:3])) < rasterResolution){
# 		nnD <- nndist(cleanExtract[,2:3])
#  		cleanExtract <- cleanExtract[-(which(min(nnD) == nnD)[1]),]
#		}
#
#	row.names(cleanExtract) <- seq(nrow(cleanExtract))
#
#	paste("Step 4: Resolution reduction: ", nrow(cleanExtract), " points left.", sep = "")

	write.csv(unique[,1:4], file = usr.output, row.names = F)
	}
}
