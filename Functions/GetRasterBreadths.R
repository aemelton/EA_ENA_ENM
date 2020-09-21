### AE Melton
# This script cycles through folders to calculate raster breadth using the enmtool R package

#
GetRasterBreadths <- function(pattern = ".tif"){

# make a list of model predictions and prepare a data frame to fill with results
enm.list <- list.files(pattern = pattern, full.names = TRUE) # Make a list of all prediction rasters
df <- data.frame(species = character(), B1 = character(), B2 = character()) # Generate empty data frame to put results in

# For loop to loop over the folders
for(i in 1:length(enm.list)){
  tmp.raster <- raster(enm.list[i])
  species <- enm.list[i]
  species <- gsub(pattern = "./", replacement = '', x = species)
  species <- gsub(pattern = ".tif", replacement = '', x = species)
  species <- gsub(pattern = "_", replacement = ' ', x = species)
  tmp.breadth <- rbind(raster.breadth(tmp.raster))
  tmp <- cbind(species, tmp.breadth)
  df <- rbind(df, tmp)
}
#

# prepare object ofr delivery to outside of loop
df$species <- unlist(df$species)
df$B1 <- unlist(df$B1)
df$B2<- unlist(df$B2)
return(df)
#
}
