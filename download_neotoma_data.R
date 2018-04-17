## load packages ####
library("tidyverse")
library("neotoma")

#register parallel backend
doMC::registerDoMC(cores = 3)

## get map
mp <- map_data("world")
detach("package:maps")

## region of interest
europe <- list(
  datasettype = "pollen",
  loc = c(lonW = -20, latS = 30, lonE = 45, latN = 89)
  
)

#get dataset list
sites <- europe %>% 
  do.call(what = get_dataset, args = .)

#extract site information
sites_meta <- sites %>% 
  map("site.data") %>% 
  map_df(as_data_frame)

#map sites
europe$loc %>% 
  as.list() %>% 
  as.data.frame() %>% 
  ggplot(aes(xmin = lonW, xmax = lonE, ymin = latS, ymax = latN)) +
  geom_map(data = mp, map = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey60") +
  geom_rect(colour = "red", fill = NA) +
  geom_point(data = sites_meta, aes(x = long, y = lat), inherit.aes = FALSE)


