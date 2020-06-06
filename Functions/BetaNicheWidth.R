### AE Melton 
# This function will sample each cell of the two extremes that overlap with SDM and take 
# the difference of the max and min. The SDM should be a binary raster (mine are derived 
# from thresholded MaxEnt outputs). You also have the option of using point data.
#
# usr.occs == a data.frame of occurrence data
# usr.sdm == a binary presence/absence raster
# method == either "sdm" or "points" ; if sdm, environmental data will be extracted for all predicted presence cells. Id points, data will only be extracted at points in the usr.occs file
# Temp == raster for mean annual temperature
# Prcp == raster for annual precipitation
# Alt == raster for elevation
#

BetaNicheWidth <- function(usr.occs = NULL, usr.sdm = NULL, method, usr.raster = NULL, Temp = NULL, Prcp = NULL, Alt = NULL){
  
  if(method == "sdm"){
    usr.sdm <- bin.mod
    usr.sdm[usr.sdm==0] <- NA
    usr.raster <- envStack
    usr.raster.sdm <- usr.sdm*usr.raster
    #usr.raster.sdm[usr.raster.sdm==0] <- NA
    usr.raster.max <- maxValue(usr.raster.sdm)
    usr.raster.min <- minValue(usr.raster.sdm)
    usr.raster.width <- usr.raster.max - usr.raster.min
    usr.raster.width <- as.data.frame(usr.raster.width)
    rownames(usr.raster.width) <- names(usr.raster)
    return(usr.raster.width)
  }
  
#  if(method == "sdm"){
#    Temp.sdm <- usr.sdm*Temp
#    Temp.sdm[Temp.sdm==0] <- NA
#    temp.max <- maxValue(Temp.sdm)
#    temp.min <- minValue(Temp.sdm)
#    temp.width <- temp.max - temp.min
#  
#    Prcp.sdm <- usr.sdm*Prcp
#    Prcp.sdm[Prcp.sdm==0] <- NA
#    precip.max <- maxValue(Prcp.sdm)
#    precip.min <- minValue(Prcp.sdm)
#    precip.width <- precip.max - precip.min
#  
#    Alt.sdm <- usr.sdm*Alt
#    Alt.sdm[Alt.sdm==0] <- NA
#    alt.max <- maxValue(Alt.sdm)
#    alt.min <- minValue(Alt.sdm)
#    alt.width <- alt.max - alt.min
#  
#    width <- data.frame(temp.width, precip.width, alt.width)
#  
#    return(width)
#    }
  
  if(method == "points"){
    temp.occs <- extract(x = Temp, y = occs)
    temp.width <- (max(temp.occs) - min(temp.occs))
    
    prcp.occs <- extract(x = Prcp, y = occs)
    prcp.width <- (max(prcp.occs) - min(prcp.occs))
  
    alt.occs <- extract(x = Alt, y = occs)
    alt.width <- (max(alt.occs) - min(alt.occs))
  
    width <- data.frame(temp.width, prcp.width, alt.width)
  
    return(width)
    }
}
