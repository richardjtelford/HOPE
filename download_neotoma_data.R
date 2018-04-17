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
  map_df(as_data_frame) %>% 
  bind_cols(sites %>% 
              map_df("dataset.meta"))



#download_data
european_data <- sites %>% get_download()

### download environment
DepEnvtTypes <- get_table("DepEnvtTypes")
CollectionUnits <- get_table("CollectionUnits")

## save downloaded data
save(sites, european_data, DepEnvtTypes, CollectionUnits, file = "european.Rdata")


age_control <- european_data %>% 
  map(ages) %>% 
  map_df(function(x){
    x %>% summarise(age_min = min(age), age_max = max(age), n = n(), res = n/(age_max - age_min) * 1000)
    },
    .id = "dataset.id") %>% 
  mutate(dataset.id = as.numeric(dataset.id)) %>% 
  as_tibble()

age_control <- age_control %>% mutate(length = case_when(
  is.na(age_min) ~ "none?",
  age_max - age_min < 4000 ~ "short",
  age_min < 2000 & age_max > 8000 ~ "Most Holocene",
  age_min > 8000 ~ "Late Glacial",
  TRUE ~"part Holocene"
))
  
sites_meta <- sites_meta %>% inner_join(age_control)

#map sites
g <- sites_meta %>% 
ggplot(aes(x = long, y = lat, colour = length)) +
  geom_map(data = mp, map = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey80") +
  geom_rect(data = europe$loc %>% 
    as.list() %>% 
    as.data.frame(), 
    mapping = aes(xmin = lonW, xmax = lonE, ymin = latS, ymax = latN), colour = "red", fill = NA, inherit.aes = FALSE) +
  geom_point() +
  coord_quickmap() +
  scale_y_continuous(expand = c(0.01, 0))

g

## get environment data
DepEnvtTypes <- DepEnvtTypes %>% 
  left_join(DepEnvtTypes, by = c("DepEnvtHigherID" = "DepEnvtID"), suffix = c("", ".1")) %>% 
  left_join(DepEnvtTypes, by = c("DepEnvtHigherID.1" = "DepEnvtID"), suffix = c("", ".2")) %>%
  left_join(DepEnvtTypes, by = c("DepEnvtHigherID.2" = "DepEnvtID"), suffix = c("", ".3")) %>% 
  left_join(DepEnvtTypes, by = c("DepEnvtHigherID.3" = "DepEnvtID"), suffix = c("", ".4")) %>% 
  select(-starts_with("RecDate")) %>% 
  as_tibble()

CollectionUnits <- CollectionUnits %>% 
  as_tibble() %>% 
  left_join(DepEnvtTypes, by = "DepEnvtID") %>% 
  select(-starts_with("RecDate"))


## merge depo environement with  site_meta
sites_meta <- sites_meta %>% 
  left_join(CollectionUnits, by = c("collection.handle" = "Handle"))

g %+% (sites_meta %>% filter(length == "Most Holocene")) +
         aes(colour = DepEnvt.4)

save(sites_meta, file = "sites_meta.Rdata")
