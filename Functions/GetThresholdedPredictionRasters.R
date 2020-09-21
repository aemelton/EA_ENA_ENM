### AE Melton, 2020
#Loop over folders to make binary [rediction rasters]
#
GetThresholdedPredictionRasters <- function(pattern = ".tif", tmp.raster, usr.occs, method, output.type, raster.output.suffix = "_BIN", pdf.output.suffix = "_BinPlot.pdf", main.folder = main.folder){
#
folder.list <- list.files(pattern = "*", full.names = TRUE) # Make a list of all species output folders
folder.list
species.list <- gsub(pattern = "./", replacement = '', x = folder.list)
#

#
for(i in 1:length(folder.list)){
  #
  setwd(folder.list[i])
  
  # Load points
  tmp.pts <- read.csv("rare_pts.csv")[,2:3]
  colnames(tmp.pts) <- c("longitude", "latidude")
  
  # Load raster and binarize
  file.name <- paste0(species.list[i], pattern, sep = "")
  tmp.raster <- raster(file.name)
  bin.mod <- ThresholdModel(usr.raster = tmp.raster, usr.occs = tmp.pts, method = "95pct", output.type = "binary")
  file.name.to.write <- paste0(species.list[i], raster.output.suffix)
  writeRaster(x = bin.mod, filename = file.name.to.write, format = "GTiff", NAFlag = "-9999", overwrite = T)
  
  # Make a plot
  pdf.file.name.to.write <-  paste0(species.list[i], pdf.output.suffix)
  pdf(pdf.file.name.to.write)
  plot(bin.mod) # logistic output
  points(tmp.pts, pch = 19, cex = 0.25)
  dev.off()
  
  # Go home to start the loop over
  setwd(main.folder)
  setwd("ENMEval_Outputs/")
  #
  }
}
