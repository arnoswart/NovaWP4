#####
# Global variables, to be used all over the project
#####


# Default CRS and bounding box to be used
crs.default    <- st_crs( "+init=epsg:4326 +proj=longlat +datum=WGS84" )
bbox.default   <- c( xmin=-10, xmax=5, ymin=35, ymax=44 )
#raster.default <- raster( nrow=400, ncol=400, crs=crs.default,ext=extent( bbox.default) )
raster.default <- raster( nrow=400, ncol=400, crs=crs.default$proj4string,ext=extent( bbox.default) )