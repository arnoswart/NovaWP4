library( sf )
library( rgdal )
library( tidyverse )
library( rlang )
library( here )
library( openxlsx )
library( raster )

# Note: need to open this R script via the Project file for here() to work
setwd( here() )

source( "./model/globals.R" )
source( "./model/plot.functions.R" )
source( "./model/spatial.functions.R" )

# I'm going to collect all variables in a table, and write to CSV,
# to facilitate translation of the columns
all.df <- tibble()

# Rasterbrick of all covariates
maps.br <- brick()

#
# administrative borders
#

# We can see what layers exist
# use file.path to build paths that are correct, independently of the operating system you work on
path_adm <-file.path(".", "data", "original","ESP_adm")
adm.sf <- read_sf( dsn=path_adm, layer="ESP_adm2") %>%  
  st_transform( crs.default ) %>%
  st_crop( bbox.default )
plot.shape( adm.sf, NULL, my_fill=NAME_1, my_color=NULL, filename="adm", "Administrative Areas")

#
# Spain Areas: Not used, it only contains the south
# 
path_area <-file.path(".", "data", "original","areas")
area.sf <- read_sf( dsn=path_area) %>%
  st_transform( crs.default ) %>% 
  st_crop( bbox.default )
#all.df <- rbind( all.df, tibble( map="area.sf", columns=colnames(area.sf)))
plot.shape( area.sf, adm.sf, my_fill=HECTAREAS, my_color=NULL, filename="area", "Areas")
area.raster <- my_rasterize( area.sf, raster.default, "HECTAREAS", "area")
plot.raster( adm.sf, area.raster, HECTAREAS, "area" )  # Comment of Elisa: the map rasterplot inside figures was not present. I made it
rm( area.sf )

#
# Hunting areas: : Not used, it only contains the south
#
path_hunting <-file.path(".", "data", "original","hunting_ground")
st_layers( dsn=path_hunting) 
hunting.sf <- read_sf( dsn=path_hunting) %>%
  st_transform( crs.default ) %>%
  st_crop( bbox.default ) %>% 
  mutate( TIPO=as.factor(TIPO))
colnames(hunting.sf)
#all.df <- rbind( all.df, tibble( map="hunting.sf", columns=colnames(hunting.sf)))
plot.shape( hunting.sf, adm.sf, my_fill=PN_PORCEN, my_color=NULL, filename="hunting", "hunting ground")


# This is how to deal with factor levels
# hunting.raster.type <- my_rasterize( hunting.sf, raster.default, "TIPO", "hunting.type", my.fun=last)
# hunting.raster.type<-
#   rasterize( hunting.sf , raster.default, 
#            field="TIPO",
#            fun='last',
#            progress="text" )
# hunting.raster.type<-ratify(hunting.raster.type)
# rat <- levels( hunting.raster.type )[[1]]
# rat$type <- levels( hunting.sf$TIPO )
# rat$code <- 1:length(levels( hunting.sf$TIPO ))
# levels( hunting.raster.type ) <- rat
# hunting.raster.type
# plot( hunting.raster.type )

hunting.raster <- my_rasterize( hunting.sf, raster.default, "PN_PORCEN", "hunting")
plot.raster( adm.sf, hunting.raster, PN_PORCEN, "hunting" )
rm( hunting.sf )

#
# pig production intensive
#


path_pig_int <-file.path(".", "data", "original","pig_production_intensive")
st_layers( dsn=path_pig_int) 
pig.int.sf <- read_sf( dsn=path_pig_int) %>%
  st_transform( crs.default ) %>% 
  st_crop( bbox.default ) %>%
  mutate( dens_censu = log10( dens_censu + 1e-5)) %>%
  rename( fat_pigs=cebo,
          piglets=lechones,
          post_weaning=recria,
          sows=cerdas,
          repl_sows=reposicion,
          boars=verracos)

all.df <- rbind( all.df, tibble( map="pig.int.sf", columns=colnames(pig.int.sf)))

# What layers exist?
colnames(pig.int.sf)

plot.shape( pig.int.sf, adm.sf, my_fill=densfarm, my_color=NULL, filename="densfarm", "Intensive Pig Farming, Farms" )
plot.shape( pig.int.sf, adm.sf, my_fill=dens_censu, my_color=NULL, filename="dens_censu", "Intensive Pig Farming, Pigs" )
plot.shape( pig.int.sf, adm.sf, my_fill=fat_pigs, my_color=NULL, filename="fattening_pigs", "Intensive Pig Farming, Fat. Pigs" )

pig.intensive.densfarm.raster <- my_rasterize( pig.int.sf, raster.default, "densfarm", "pig.intensive.farm")
pig.intensive.dens_censu.raster <- my_rasterize( pig.int.sf, raster.default, "dens_censu", "pig.intensive.pig")
pig.intensive.fat_pig.raster <- my_rasterize( pig.int.sf, raster.default, "fat_pigs", "pig.intensive.fattening")
pig.intensive.piglets.raster <- my_rasterize( pig.int.sf, raster.default, "piglets", "pig.intensive.piglets")
pig.intensive.sows.raster <- my_rasterize( pig.int.sf, raster.default, "sows", "pig.intensive.sows")
pig.intensive.repl_sows.raster <- my_rasterize( pig.int.sf, raster.default, "repl_sows", "pig.intensive.repl_sows")
pig.intensive.boars.raster <- my_rasterize( pig.int.sf, raster.default, "boars", "pig.intensive.boars")

maps.br <- addLayer( maps.br, pig.intensive.densfarm.raster,
                     pig.intensive.dens_censu.raster,
                     pig.intensive.fat_pig.raster,
                     pig.intensive.piglets.raster,
                     pig.intensive.sows.raster,
                     pig.intensive.repl_sows.raster,
                     pig.intensive.boars.raster )
rm( pig.int.sf )
rm(list=ls(pattern="pig.intensive."))

#
# pig production extensive
#
path_pig_ext <-file.path(".", "data", "original","pig_production_extensive")
st_layers( dsn=path_pig_ext) 
pig.ext.sf <- read_sf(dsn=path_pig_ext) 

pig.ext.sf <- pig.ext.sf %>%
  st_transform( crs.default ) %>% 
  st_crop( bbox.default )
colnames( pig.ext.sf )

all.df <- rbind( all.df, tibble( map="pig.ext.sf", columns=colnames(pig.ext.sf)))

plot.shape( pig.ext.sf, adm.sf, my_fill=densfarm, my_color=NULL, filename="pig.extensive", "Extensive Pig Farming" )

pig.ext.raster.farms <- my_rasterize( pig.ext.sf, raster.default, "densfarm", "Extensive pig farming, Farms", background = 0 )
pig.ext.raster.pigs <- my_rasterize( pig.ext.sf, raster.default, "dens_censu", "Extensive pig farming, Pigs", background = 0 )

plot.raster( adm.sf, pig.ext.raster.pigs, dens_censu, "pig.extensive" )
plot.raster( adm.sf, pig.ext.raster.farms, densfarm, "pig.extensive.farms" )

maps.br <- addLayer( maps.br,pig.ext.raster.farms,pig.ext.raster.pigs)
rm( pig.ext.sf )
rm(list=ls(pattern="pig.ext"))
              
#
# Wild Boar density
#
path_wildb_dens <- file.path(".", "data", "original","wild_boar_densitiy")
if( length(dir( path_wildb_dens ))==0){
  stop("Wild boar data does not exist.")
}
st_layers( dsn=path_wildb_dens) 
wildboar.sf <- read_sf( dsn=path_wildb_dens) %>% 
  st_transform( crs.default ) %>% 
  st_crop( bbox.default ) %>%
  rename( prv=CATEGORIAS )
colnames(wildboar.sf)

all.df <- rbind( all.df, tibble( map="wildboar.sf", columns=colnames(wildboar.sf)))

plot.shape( wildboar.sf, adm.sf, my_fill=Dens_hunt, my_color=NULL, filename="wild.boar", "Wild Boar Density" )

wildboar.raster.hunt <- my_rasterize( wildboar.sf, raster.default, "Dens_hunt", "wildboar.hunt", background = 0)
wildboar.raster.prv <- my_rasterize( wildboar.sf, raster.default, "prv", "wildboar.prv", background = 0)
maps.br <- addLayer( maps.br,wildboar.raster.hunt,wildboar.raster.prv)

plot.raster( adm.sf, wildboar.raster.prv, prv, "wildboar_prv" )
plot.raster( adm.sf, wildboar.raster.hunt, Dens_hunt, "wildboar_hunt", my_title="Wild Boar Abundance" )
rm(list=ls(pattern="wildboar"))

#
# Wild Boar Salmonella
#
path_wildb_sal <- file.path(".", "data", "original","wild_boar_sal_result")
st_layers( dsn=path_wildb_sal) 
wildboar.sal.sf <- read_sf( dsn=path_wildb_sal, layer="POSALL_DATA_reproject") %>% 
  st_transform( crs.default ) %>% 
  st_crop( bbox.default )

all.df <- rbind( all.df, tibble( map="wildboar.sal.sf", columns=colnames(wildboar.sal.sf)))
plot.shape( wildboar.sal.sf, adm.sf, Salmonella, Salmonella, filename="wildboar.salmonella", "Wild Boar Salmonella" )
# No rasterize here, it is point data

writeRaster(maps.br, "./data/processed/all.maps.grd", overwrite=TRUE )

#
# Write data translation spreadsheet
#
if( !file.exists("./data/processed/datatranslation.xlsx")){       # Comment from Elisa. The map generated does not exists. It exists the map processed. I changed in Arno's code 
  all.df <- all.df %>% mutate( english.name="-", description="-")
  write.xlsx( all.df, "./data/processed/datatranslation.xlsx" )
}
