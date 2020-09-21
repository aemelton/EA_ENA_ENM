### AE Melton
# This command will mask and crop environmental rasters and write out layers for use
# in other analyses. I recommend the "buff_poly" method. "buff.size" will need to
# determined on a per-species basis, made with consideration to the species dispersal
# abilities and geographic/ecological restrictions.
RasterLayerProcessingByLoopMaskByShape <- function(usr.proj = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs",
                                        shape = NULL,
                                        output.format = "GTiff",
                                        na.flag,
                                        rasters.folder,
                                        output.folder){
  # Since some files are so big, we can read them in and edit them one-by-one.
  setwd(rasters.folder)
  env.files <- list.files(pattern = ".tif", full.names = TRUE)
  for(i in 1:length(env.files)){
    setwd(rasters.folder)
    tmp.file <- raster(env.files[i])
    training.region <- mask(tmp.file, shape)
    training.region <- crop(x = training.region, y = shape)
    training.region <- setMinMax(training.region)
    setwd(output.folder)
    file.name <- names(tmp.file)
    writeRaster(x = training.region, filename = file.name,
                format = output.format, NAFlag = na.flag, overwrite = T)
  }
}
