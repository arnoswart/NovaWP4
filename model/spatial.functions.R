
my_rasterize <- function( map.sf, raster.default, field, filename, background=NA, my.fun="mean" ){
  the.raster <- rasterize( map.sf, raster.default, 
                           field=field, fun=my.fun,
                           background=background,
                           progress="text" )
  names( the.raster ) <- field
  writeRaster( the.raster, str_c("./data/processed/", filename, ".grd", sep=""), overwrite=TRUE)
  return( the.raster )
}
