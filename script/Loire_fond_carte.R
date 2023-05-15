map <- function(shape_name = c('coastline', 'tidal_level'), EPSG = 4326){
  
  # load shapefiles into R environment
  shape <- glue::glue('shape/{shape_name}.shp') %>%
    sapply(sf::read_sf)
  
  # project data according to EPSG
  shape_proj <- sapply(shape, sf::st_transform, EPSG)
  
  names(shape_proj) <- shape_name
  
  ggmap <- list(geom_sf(data = shape_proj$tidal_level, colour = NA,
                        fill = c('#C3D6F3',
                                 '#D0EAA2',
                                 '#EAE3BF')),
                geom_sf(data = shape_proj$coastline, fill = NA),
                ggspatial::annotation_north_arrow(location = 'tl',
                                                  which_north = 'true',
                                                  height = unit(2.5, 'cm'),
                                                  width = unit(2.5, 'cm'),
                                                  pad_x = unit(1, 'cm'),
                                                  pad_y = unit(.5, 'cm'),
                                                  style = ggspatial::north_arrow_fancy_orienteering(
                                                    fill = c('black',
                                                             'white'),
                                                    line_col = 'black',
                                                    text_size = 18)),
                ggspatial::annotation_scale(location = 'br',
                                            width_hint = .15,
                                            bar_cols = c('black',
                                                         'white'),
                                            pad_x = unit(1.5, 'cm'),
                                            pad_y = unit(1, 'cm'),
                                            text_pad = unit(.5, 'cm'),
                                            text_cex = 1.2,
                                            line_width = 2),
                scale_x_continuous(name = 'Longitude',
                                   limits = c(-2.45, -1.87),
                                   expand = c(0, 0)),
                scale_y_continuous(name = 'Latitude',
                                   limits = c(47.12, 47.33),
                                   expand = c(0, 0)),
                theme_bw(),
                theme(axis.text = element_text(size = 16),
                      axis.title = element_text(size = 25)))
  
  return(ggmap)
  
}