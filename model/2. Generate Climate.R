################################Read and plot raster maps of climate variables########################################
library( sf )
library( rgdal )
library( tidyverse )
library( rlang )
library( here )
library( openxlsx )
library( raster )
library( ncdf4 )

setwd(here())

source( "./model/globals.R" )
source( "./model/plot.functions.R")
source( "./model/spatial.functions.R" )

# Rasterbrick of all climate covariates
climate.br <- brick()

# Load the administrative borders of Spain, to put as borders in the raster
# use file.path to build paths that are correct, independently of the operating system you work on
path_adm <-file.path( ".", "data", "original","ESP_adm" )
adm.sf <- read_sf( dsn=path_adm, layer="ESP_adm2" ) %>%  
  st_transform( crs.default ) %>%
  st_crop( bbox.default )

# Climate data downloaded from Copernicus: https://cds.climate.copernicus.eu/#!/home 
# Climate data from Copernicus are in NetCDF format, format created to store large arrays of gridded data.
# They are already raster of data, I can read them with the function stack from the raster
# In the Copernicus website there are many datasets available and many different variables from different models.
# I downloaded climate variables from E-OBS daily gridded meteorological data for Europe from 1950 to present derived from in-situ observations 
# Reference: https://cds.climate.copernicus.eu/cdsapp#!/dataset/insitu-gridded-observations-europe?tab=overview
# Variables selected: Max, Min, Mean of Air Temperature, Precipitation amount, Sea level pressure, Surface shortwave downwelling radiation
# 0.1 degree
# Time period selected: 01-01-2011 until now, but the time frame available varies per variable

# Mean Air Temperature: from 2011-01-01 until 2016-11-02 
path_mean_temp <- file.path( ".", "data", "original","climate_data","tg_ens_mean_0.1deg_reg_2011-2019_v20.0e.nc" )

# Arno: currently downloading from copernicus didn't work for me.
if( !file.exists(path_mean_temp)){
  stop( "Download map from https://cds.climate.copernicus.eu/cdsapp#!/dataset/insitu-gridded-observations-europe?tab=overview
         and put the nc file in ../data/original/climate_data
         You will need to creat a copernicus account for this." )
}

# read the raster and crop the raster to the size of Spain
meantemp.raster <- stack ( path_mean_temp ) %>% crop( bbox.default )
# project the raster to have the same crs system than the default raster
meantemp.raster <- projectRaster( meantemp.raster, crs = crs.default$proj4string,
                       progress="text", method="ngb" ) 

# calculate the average of all measurements of all years: yearly year average
# it is also possible to calculate the yearly season or yearly month average(e.g average of spring months over the years, or average of all january's over the years).
year.average.meantemp.raster <- mean( meantemp.raster ) 
year.average.meantemp.raster@data@names <- "Mean_temp_2011_2016"
plot.raster( adm.sf, year.average.meantemp.raster,Mean_temp_2011_2016,file="mean_temp_average2011_2016" )

# to be able to add the raster to the brick, the climate raster has to have the same dimensions than the default raster
# The default raster is larger, I need to extrapolate the data
year.average.meantemp.raster.ext <-resample( year.average.meantemp.raster, raster.default )

# add it to the raster brick 
climate.br <- addLayer( climate.br,year.average.meantemp.raster.ext )

# Min Air Temperature from 2011-01-01 until 2016-11-02
path_min_temp <- file.path( ".", "data", "original", "climate_data","tn_ens_mean_0.1deg_reg_2011-2019_v20.0e.nc" )
# read the raster and crop the raster to the size of Spain
mintemp.raster <- stack ( path_min_temp ) %>% crop( bbox.default )
# project the raster to have the same crs system than the default raster
mintemp.raster <- projectRaster( mintemp.raster, crs = crs.default$proj4string,
                                  progress="text", method="ngb" ) 

# calculate the average of all measurements of all years: yearly year average
# it is also possible to calculate the yearly season or yearly month average(e.g average of spring months over the years, or average of all january's over the years).
year.average.mintemp.raster <- mean( mintemp.raster ) 

year.average.mintemp.raster@data@names <- "Min_temp_2011_2016"
plot.raster( adm.sf, year.average.mintemp.raster,Min_temp_2011_2016,file="min_temp_average2011_2016" )

# to be able to add the raster to the brick, the climate raster has to have the same dimensions than the default
# The default raster it is larger, I need to extrapolate the data
year.average.mintemp.raster.ext <- resample( year.average.mintemp.raster, raster.default, progress="text" ) 

# add it to the raster brick 
climate.br <- addLayer( climate.br,year.average.mintemp.raster.ext )

# Max Air Temperature from 2011-01-01 until 2016-11-02 
path_max_temp <- file.path( ".", "data", "original", "climate_data","tx_ens_mean_0.1deg_reg_2011-2019_v20.0e.nc" )
# read the raster and crop the raster to the size of Spain
maxtemp.raster <- stack ( path_max_temp ) %>% crop( bbox.default )
# project the raster to have the same crs system than the default raster
maxtemp.raster <- projectRaster( maxtemp.raster, crs = crs.default$proj4string,
                                  progress="text", method="ngb") 

# calculate the average of all measurements of all years: yearly year average
# it is also possible to calculate the yearly season or yearly month average(e.g average of spring months over the years, or average of all january's over the years).
year.average.maxtemp.raster <- mean( maxtemp.raster ) 
year.average.maxtemp.raster@data@names <- "Max_temp_2011_2016"
plot.raster( adm.sf, year.average.maxtemp.raster,Max_temp_2011_2016,file="max_temp_average2011_2016" )

# to be able to add the raster to the brick, the climate raster has to have the same dimensions than the default
# The default raster it is larger, I need to extrapolate the data
year.average.maxtemp.raster.ext <- resample( year.average.maxtemp.raster, raster.default )
# add it to the raster brick 
climate.br <- addLayer( climate.br,year.average.maxtemp.raster.ext )

# Precipitation amount from 2011-01-01 until 2016-11-02 
path_prec <- file.path( ".", "data", "original", "climate_data","rr_ens_mean_0.1deg_reg_2011-2019_v20.0e.nc" )
# read the raster and crop the raster to the size of Spain
prec.raster <- stack ( path_prec ) %>% crop( bbox.default )
# project the raster to have the same crs system than the default raster
prec.raster <- projectRaster( prec.raster, crs = crs.default$proj4string,
                                  progress="text", method="ngb") 
# calculate the average of all measurements of all years: yearly year average
# it is also possible to calculate the yearly season or yearly month average(e.g average of spring months over the years, or average of all january's over the years).
year.average.prec.raster <- mean( prec.raster )
year.average.prec.raster@data@names <- "Prec_2011_2016"
plot.raster( adm.sf, year.average.prec.raster,Prec_2011_2016,file="prec_average2011_2016" )

# to be able to add the raster to the brick, the climate raster has to have the same dimensions than the default
# The default raster it is larger, I need to extrapolate the data
year.average.prec.raster.ext <- resample( year.average.prec.raster, raster.default )
# add it to the raster brick 
climate.br <- addLayer( climate.br,year.average.prec.raster.ext )

# Sea level pressure
path_press <- file.path( ".", "data", "original", "climate_data","pp_ens_mean_0.1deg_reg_2011-2019_v20.0e.nc" )
# read the raster and crop the raster to the size of Spain
press.raster <- stack ( path_press ) %>% crop( bbox.default )
# project the raster to have the same crs system than the default raster
press.raster <- projectRaster( press.raster, crs = crs.default$proj4string,
                                  progress="text", method="ngb") 

# calculate the average of all measurements of all years: yearly year average
# it is also possible to calculate the yearly season or yearly month average(e.g average of spring months over the years, or average of all january's over the years).
year.average.press.raster <- mean( press.raster ) 
year.average.press.raster@data@names <- "Press_2011_2016"
plot.raster( adm.sf, year.average.press.raster,Press_2011_2016,file="press_average2011_2016" )

# to be able to add the raster to the brick, the climate raster has to have the same dimensions than the default
# The default raster it is larger, I need to extrapolate the data
year.average.press.raster.ext <- resample( year.average.press.raster, raster.default )
# add it to the raster brick 
climate.br <- addLayer( climate.br,year.average.press.raster.ext )

# Surface shortwave downwelling radiation
path_rad <- file.path( ".", "data", "original", "climate_data","qq_ens_mean_0.1deg_reg_2011-2019_v20.0e.nc" )
# read the raster and crop the raster to the size of Spain
rad.raster <- stack ( path_rad ) %>% crop( bbox.default )
# project the raster to have the same crs system than the default raster
rad.raster <- projectRaster( rad.raster, crs = crs.default$proj4string,
                                  progress="text", method="ngb" ) 

# calculate the average of all measurements of all years: yearly year average
# it is also possible to calculate the yearly season or yearly month average(e.g average of spring months over the years, or average of all january's over the years).
#year.average.rad.raster <- mean( rad.raster )  # something strange occurs here. To be fixed later. If I calculated the mean I get only part of the map
# If I first subset the data, by selecting all the 2134 timeseries grid then I get a complete grid
sel <- subset( rad.raster, 1:2134 )
year.average.rad.raster <- mean( sel )
year.average.rad.raster@data@names <- "Rad_2011_2016"
plot.raster( adm.sf, year.average.rad.raster,Rad_2011_2016,file="rad_average2011_2016" )

# to add the raster to the brick, the climate raster has to have the same dimensions than the default
# The default raster it is larger, I need to extrapolate the data
year.average.rad.raster.ext <- resample( year.average.rad.raster, raster.default)
# add it to the raster brick 
climate.br <- addLayer( climate.br,year.average.rad.raster.ext )

# save the brick
writeRaster( climate.br, "./data/processed/climate/climate.grd", overwrite=TRUE )
