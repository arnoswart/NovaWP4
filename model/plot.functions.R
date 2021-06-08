plot.shape <- function( front.shape, back.shape, my_fill, my_color, filename=NA, title){
  my_fill  <- enquo( my_fill )
  my_color <- enquo( my_color )
  
  p<-ggplot( ) +
    geom_sf( data = back.shape ) +
    ifelse( is.null( my_color),
      geom_sf( data = front.shape, aes( fill=!!my_fill ), size=0.05),
      geom_sf( data = front.shape, aes( fill=!!my_fill, color=!!my_color ))) +
    xlab("Longitude") + ylab("Latitude") +
    ggtitle( title )+
    theme( legend.title=element_text(size=6),   # I improved the legend. It was too big!
    legend.text=element_text(size=6),
    legend.key.height=unit(0.5,"cm"),legend.key.width=unit(0.3,"cm"))
     
  if( !is.na(filename) ){
    ggsave( filename=str_c("./figures/shapeplots/", filename,".png", sep=""), plot=p )
  }
  return( p )
}

plot.raster <- function( shape, raster, my_fill, filename=NA, my_title="rasterized" ){
  my_fill <- enquo( my_fill )
  
  p <- ggplot( shape ) +
    geom_raster(data=raster %>% 
                  as.data.frame( xy=T, na.rm=T ),
                inherit.aes = FALSE,
                aes(
                  x = x,
                  y = y,
                  fill = !!my_fill 
                )) +
    geom_sf( fill=NA ) +
    theme(plot.background = element_blank())+
    theme_bw()+
    theme(axis.title = element_blank())+
    ggtitle( my_title ) +
    theme( legend.title=element_text(size=6),   # I improved the legend. It was too big!
           legend.text=element_text(size=6),
           legend.key.height=unit(0.5,"cm"),legend.key.width=unit(0.3,"cm"))+
   # guides(fill=guide_legend(title=as_label(my_fill)) )+ # with this command I get a discrete legend, which should not be
    scale_fill_viridis_c(option = "plasma")
  
  if( !is.na(filename) ){
    ggsave( filename=str_c("./figures/rasterplots/", filename,".png", sep=""), plot=p )
  }
  return(p)
}

# make the plot of the raster of Spain and its contours 
plot_landuse <- function( nr ){
  my_fill <- sym(names( layers)[nr] )
  
  Spain_landuse <-ggplot( shapeSpain ) +
    geom_raster(data=layers[[nr]] %>% 
                  as.data.frame( xy=T, na.rm=T ),
                inherit.aes = FALSE,
                aes(x = x,y = y, fill = !!my_fill )) +
    geom_sf( fill=NA ) +
    theme(plot.background = element_blank())+
    theme_bw()+
    theme(axis.title = element_blank())+
    ggtitle(names( layers)[nr]) +
    guides(fill=guide_legend(title="fraction landuse") )+ 
    scale_fill_viridis_c(option = "plasma")
  return( Spain_landuse )
}