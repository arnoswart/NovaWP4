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

# Adm1 = province
adm.sf <- read_sf( dsn="./data/original/ESP_adm/", layer="ESP_adm1") %>% 
  st_transform( crs.default ) %>%
  st_crop( bbox.default )

plot.shape( adm.sf, NULL, my_fill=NAME_1, my_color=NULL, filename="adm", "Administrative Areas")

corine <- brick( "./data/processed/corine/corine.grd" )
plot( corine )

plot_landuse_province <- function( lu, corine_name ){
  my_fill <- sym(corine_name)
  
  Spain_landuse <-ggplot( lu ) +
    geom_sf( aes(fill=!!my_fill) ) +
    theme(plot.background = element_blank())+
    theme_bw()+
    theme(axis.title = element_blank())+
    ggtitle(names( corine )[nr]) +
    guides(fill=guide_legend(title="fraction landuse") )+ 
    scale_fill_viridis_c(option = "plasma")
  return( Spain_landuse )
}


# Average the raster values over the provinces
for( nr in 1:length( names(corine))  ){
  lu <- raster::extract( corine[[nr]], adm.sf, fun=mean, sp=TRUE )
  plot_landuse_province( st_as_sf(lu), names(corine)[nr] )
  ggsave(str_c("./figures/rasterplots/province.landuse.", names(corine)[nr], ".png" ) )
}
