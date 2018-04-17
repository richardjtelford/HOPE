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
  loc = c(lonW = 4, latS = 55, lonE = 37, latN = 81)
)

#get dataset list
sites <- europe %>% 
  do.call(what = get_dataset, args = .)

#extract site information
sites_meta <- sites %>% 
  map("site.data") %>% 
  map_df(as_data_frame)



#download_data
european_data <- sites %>% get_download()
save(european_data, file = "european.Rdata")


age_control <- european_data %>% 
  map(ages) %>% 
  map_df(function(x){
    x %>% summarise(age_min = min(age), age_max = max(age), n = n(), res = n/(age_max - age_min) * 1000)
    },
    .id = "dataset.id") %>% 
  as_tibble()

age_control <- age_control %>% mutate(length = case_when(
  is.na(age_min) ~ "none?",
  age_max - age_min < 4000 ~ "short",
  age_min < 2000 & age_max > 8000 ~ "Most Holocene",
  age_min > 8000 ~ "Late Glacial",
  TRUE ~"part Holocene"
))
  
sites_meta <- sites_meta %>% bind_cols(age_control)
##TODO - get dataset.id into site meta and left_join

#map sites
sites_meta %>% 
ggplot(aes(x = long, y = lat, colour = length)) +
  geom_map(data = mp, map = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey80") +
  geom_rect(data = europe$loc %>% 
    as.list() %>% 
    as.data.frame(), 
    mapping = aes(xmin = lonW, xmax = lonE, ymin = latS, ymax = latN), colour = "red", fill = NA, inherit.aes = FALSE) +
  geom_point() +
  coord_quickmap() +
  scale_y_continuous(expand = c(0.01, 0))



x <- get_download(sites[[1]])
ages(x)
x %>% neotoma:::Stratiplot.download(yaxis = "depth")

x
