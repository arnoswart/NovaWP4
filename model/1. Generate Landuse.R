################################Make the land use map of Spain########################################

library(sf)
library(rgdal)
library(tidyverse)
library(raster)
library(rasterVis)
library(here)
library(viridis)

setwd(here())

source( "./model/globals.R" )
source( "./model/plot.functions.R")

downsample.factor <- 25

# load the shapefile of Spain that will be used to mask the raster
# the shapefile of Spain follows the national coordinate system of Spain: ED50 
shapeSpain <- read_sf( dsn="./data/original/ESP_adm/", layer="ESP_adm2") %>%
  st_transform( crs.default) %>%
  st_crop( bbox.default )

# load the 100x100 m raster layer, which I downloaded from CORINE (https://land.copernicus.eu/faq/clc-1)  
if( !file.exists("./data/original/corine/CLC2018_CLC2018_V2018_20.tif")){
  stop( "Download corine map from hhttps://land.copernicus.eu/pan-european/corine-land-cover/clc2018?tab=download,
         and put the tiff file in ../data/original
         You will need to creat a copernicus account for this." )
}

spainraster <- raster("./data/original/corine/CLC2018_CLC2018_V2018_20.tif ")
corine.legend <- read.csv("./data/original/corine/Legend/CLC2018_CLC2018_V2018_20b2_QGIS.txt",
                          header = FALSE, col.names=c("code",rep("NULL",4), "class"), 
                          colClasses=c("integer", rep("NULL",4), "character"  ) )

# Need to crop to Spain only, take the default bounding box, project to raster CRS
bbox.raster <- raster( ext=extent(bbox.default), crs=crs.default$proj4string ) %>%
  projectRaster( crs=crs( spainraster) )

# Then crop
spainraster <- spainraster %>% raster::crop( bbox.raster )

# Then reproject to default CRS
spainraster <- raster::projectRaster( spainraster, crs = crs.default$proj4string,
                                      progress="text", method="ngb") %>% 
  raster::crop( bbox.default )

# What levels exist in the raster? Remove 999 it is 'no data'
spainraster.levels <- unique( spainraster, progress="text" )

code.exclude <- c( 111, 123, 124, 131, 132, 133, 141, 142, 241, 
                   331, 332, 334, 335, 411, 412, 421, 422, 423, 521, 522, 523, 999 )

spainraster.levels <- spainraster.levels[ !(spainraster.levels %in% code.exclude) ]

corine.legend %>% filter( code %in% spainraster.levels )

# each resulting layer contains the fraction of area within that cell in which value of original raster is N. 
layers <- brick( map( spainraster.levels, 
                        .f=function( y, na.rm ) { 
                          r <- raster::aggregate(spainraster, 
                              downsample.factor, 
                              fun=function(x, na.rm=T) {mean(x==y, na.rm=na.rm)},
                              progress="text")
                          r <- raster::mask( r, shapeSpain )
                          names(r) <- corine.legend %>% filter( code==y ) %>% pull( class )
                          return(r)} ) )

# Now resample on the smaller brick to the exact resolution
layers <- resample( layers, raster.default, progress="text" )

# See all of them
plot( layers )
names( layers )

writeRaster( layers, "./data/processed/corine/corine", format="raster", overwrite=TRUE)

for( i in 1:length( names(layers))){
  plot_landuse(i)
  ggsave(str_c("./figures/rasterplots/landuse.", names(layers)[i], ".png" ) )
}
