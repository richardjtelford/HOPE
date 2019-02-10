plot_region_map <- function(regions, sites_meta){
  mp <- map_data("world")
  detach("package:maps")#conflicts with purrr
  ggplot(regions, aes(xmin = long_min, xmax = long_max, ymin = lat_min, ymax = lat_max, fill = region)) +
    geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey50", colour = "grey30") +
    geom_rect(alpha  = 0.5, show.legend = FALSE) + 
    geom_point(aes(x = long, y = lat), data = sites_meta, colour = scales::muted("red"), inherit.aes = FALSE) +
    geom_text(aes(x = (long_min + long_max)/2, y = (lat_min + lat_max)/2, label = region)) + 
    coord_quickmap() + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "°E", y = "°N")
}