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
							mask.method = "buffers",
							usr.occs,
							buffer.size,
							shape,
							usr.extent,
							raster.output.name,
							output.format = "GTiff"){
	
	if(mask.method == "convex_hull"){
	  usr.occs <- SpatialPoints(usr.occs)
		hull <- gConvexHull(usr.occs)
		hull_buffer <- gBuffer(hull, width = buffer.size)
	}

	if(mask.method == "shapefile"){
		hull_buffer <- shape
		}
	
	if(mask.method == "extent"){
		hull_buffer <- extent(usr.extent)
	}
  
  if(mask.method == "buffers"){
      usr.occs <- SpatialPoints(usr.occs)
  		x <- circles(usr.occs)
  		pol.a <-  gUnaryUnion(x@polygons)
  		hull_buffer <-  gBuffer(pol.a, width=buffer.size)
  }
  crs(hull_buffer) <- crs(usr.env)
  training.region <- mask(usr.env, hull_buffer)
  training.region <- crop(x = training.region, y = hull_buffer)
  training.region <- setMinMax(training.region)
  return(training.region)
}
