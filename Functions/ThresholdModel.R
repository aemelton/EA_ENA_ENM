### AE Melton
# This function will take a suitability score raster and generate a binary predicted
# occurrence/absence map. I recommend using the minimum presence threshold ("MPT").

#
# usr.raster == ENM output raster with suitability scores
# usr.occs == data frame with occurrence data
# method == How do you want to set the threshold? Currently have: MPT (minimum presence threshold), 90 or 95% thresholds.
# output.type == "binary" or keep suitability scores with "suitability.scores"
#

ThresholdModel <- function(usr.raster, usr.occs, method, output.type){
  usr.raster <- setMinMax(usr.raster)
  
  SuitabilityScores <- extract(usr.raster, usr.occs)
  
  SuitabilityScores <- SuitabilityScores[complete.cases(SuitabilityScores)]
  
  if(method == "MPT"){
    threshold <- min(SuitabilityScores)
  } else if(method == "95pct"){
    threshold <- sort(SuitabilityScores, decreasing = T)[round(length(SuitabilityScores)*.95,0)] 
  } else if(method == "90pct"){
    threshold <- sort(SuitabilityScores, decreasing = T)[round(length(SuitabilityScores)*.90,0)] 
  }
  
  if(output.type == "suitability.scores"){
    usr.raster[usr.raster < threshold] <- 0
    return(usr.raster)
  } else if(output.type == "binary"){
  M <- c(0, threshold, 0,  threshold, 1, 1); 
  rclmat <- matrix(M, ncol = 3, byrow = TRUE); 
  Dist <- reclassify(usr.raster, rcl = rclmat);
  }
}
