### AE Melton
# This command will mask and crop environmental rasters and write out layers for use
# in other analyses. I recommend the "buff_poly" method. "buff.size" will need to
# determined on a per-species basis, made with consideration to the species dispersal
# abilities and geographic/ecological restrictions.
RasterLayerProcessingByLoop <- function(usr.proj = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs",
                                  #mask.method = "buffers",
                                  usr.occs,
                                  buffer.size = NULL,
                                  shape = NULL,
                                  usr.extent = NULL,
                                  output.format = "GTiff",
                                  path.to.rasters){
  # Since some files are so big, we can read them in and edit them one-by-one.
  usr.occs <- SpatialPoints(usr.occs)
  x <- circles(p = usr.occs)
  pol.a <-  gUnaryUnion(x@polygons)
  hull_buffer <-  gBuffer(pol.a, width = buffer.size)
  setwd(path.to.rasters)
  env.files <- list.files(pattern = ".tif", full.names = TRUE)
  for(i in 1:length(env.files)){
    setwd(path.to.rasters)
    tmp.file <- raster(env.files[i])
    training.region <- mask(tmp.file, hull_buffer)
    training.region <- crop(x = training.region, y = hull_buffer)
    training.region <- setMinMax(training.region)
    setwd(main.folder)
    setwd(Species.Layers.folder)
    setwd(species.folder)
    filename <- names(tmp.file)
    writeRaster(x = training.region, filename = filename,
                format = output.format, NAFlag = "-9999", overwrite = T)
  }
}