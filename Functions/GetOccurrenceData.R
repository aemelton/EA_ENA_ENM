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

	if(method == "scrubr"){
	spocc <- occ(query = usr.query, from = usr.db, has_coords = T)
  	
  spocc <- fixnames(spocc, how = "query")

	allSpocc <- occ2df(spocc)
	
	allSpocc <- date_standardize(allSpocc, "%d%b%Y")

	#Step 1: Using scrubr
	scrubbed <- coord_incomplete(allSpocc) # Removes data with incomplete coordinates
	
	scrubbed <- coord_unlikely(scrubbed) # Removes data with unlikely coordinates (i.e. 0,0)

	paste("Step 1: Post scrubr: ", nrow(scrubbed), " points left.", sep = "")

	#Step 2: Removing duplicate dates and localities
	scrubbed <- scrubbed[,-6] # Removes unique, database-specific keys
	
	unique <- unique(scrubbed[,-4]) # Removes duplicate points (not considering data provider)

	write.csv(unique[,1:4], file = usr.output, row.names = F)
	}
}
