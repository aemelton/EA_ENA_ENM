### AE Melton, 2020
# This script cycles through binary prediction rasters to calculate breadth in geographic space.

#
GetGeographicAreaSizes <- function(pattern = ".tif"){
  
file.list <- list.files(pattern = pattern, full.names = TRUE)

df <- NULL
tmp <- NULL

for(i in 1:length(file.list)){
  Species <- gsub(pattern = "_BIN.tif", replacement = "", x =  file.list[i])
  Species <- gsub(pattern = "_", replacement = " ", x = Species)
  Species <- gsub(pattern = "./", replacement = '', x = Species)
  rast.file <- raster(file.list[i])
  cellAreas <- area(rast.file)*rast.file
  cellAreaMeasures <- rasterToPoints(cellAreas)
  tmp <- data.frame(Species, sum(cellAreaMeasures[,3]))
  df <- rbind(df, tmp)
  }
colnames(df) <- c("Species", "Geographic Area")
return(df)
}
