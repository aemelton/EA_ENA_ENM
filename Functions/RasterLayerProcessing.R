### AE Melton
# This command will mask and crop environmental rasters and write out layers for use
# in other analyses. I recommend the "buff_poly" method. "buff.size" will need to
# determined on a per-species basis, made with consideration to the species dispersal
# abilities and geographic/ecological restrictions.

#
# usr.env == a raster stack to be trimmed
# usr.proj == projection definition
# mask.method == How do you want to define and trim training region? Options include "extent", "shapefile", "convex_hull", and "buff_poly".
# usr.occs == data frame with occurrence data
# buff.size = how large to make buffers around points or convex hulls?
# shape == shapefile
# usr.extent == extent object
# raster.output == name for writing raster output files. Currently null - direct to object.
# output.format == format of output raster files. Currently null - direct to object.
#

RasterLayerProcessing <- function(usr.env,
							usr.proj = "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs",
							mask.method = "buff_poly",
							usr.occs,
							buff.size = NULL,
							shape = NULL,
							usr.extent = NULL,
							raster.output.name,
							output.format = "GTiff"){
  
  usr.occs <- SpatialPoints(usr.occs)
	
	if(mask.method == "convex_hull"){
		hull <- gConvexHull(usr.occs)
		hull_buff <- gBuffer(hull, width = buff.size)
	}

	if(mask.method == "shapefile"){
		hull_buff <- shape
		#projection(hull_buff) <- usr.proj
		}
	
	if(mask.method == "extent"){
		hull_buff <- extent(usr.extent)
	}
  
  	if(mask.method == "buff_poly"){
    		x <- circles(usr.occs)
    		pol.a <-  gUnaryUnion(x@polygons)
    		hull_buff <-  gBuffer(pol.a, width=buff.size)
  	}
	
	crs(hull_buff) <- crs(usr.env)
	
	training.region <- mask(usr.env, hull_buff)
	
	training.region <- crop(x = training.region, y = hull_buff)
	
	training.region <- setMinMax(training.region)
	
	#writeRaster(training.region,
	#	filename = raster.output.name, 
  #      format = output.format,
  #      bylayer = T,
  #      suffix=names(usr.env),
  #      NAFlag = "-9999",
  #      overwrite = T)
	
	return(training.region)
}
