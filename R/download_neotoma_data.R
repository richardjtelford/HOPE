## load packages ####
library("drake")#load first to prevent conflicts
library("tidyverse")
library("neotoma")
library("readr")

#source functions
source("R/get_sites_meta.R")
source("R/get_pollen.R")
source("R/merge_pollen_by_region.R")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

import_neotoma_plan <- drake_plan(
  #get dataset list
  pollen_sites = get_dataset(datasettype = "pollen"),

  #download_data
  pollen_data = pollen_sites %>% get_download(), #SLOW
  
  ##rough age control
  rough_selection = pollen_data %>%
    map("sample.meta") %>% 
    map_df(~{
      as_tibble(.) %>%
        summarise(
          age_min = min(age, na.rm  = TRUE),
          age_max = max(age, na.rm = TRUE),
          n = n(),
          res = n/(age_max - age_min) * 1000)
    },
    .id = "dataset.id") %>%
    mutate(
      length = case_when(
        is.na(age_min) | is.infinite(age_min) ~ "none?",
        age_max - age_min < 4000 ~ "short",
        age_min < 2000 & age_max > 8000 ~ "Most Holocene",
        age_min > 8000 ~ "Late Glacial",
        TRUE ~"part Holocene"),
      rough_pass = length %in% c("Most Holocene", "part Holocene") & n > 16
      ),
  
  #drop datasets that fail rough_pass
  pollen_sites_clean = pollen_sites[rough_selection$dataset.id[rough_selection$rough_pass]],
  
  #download_data
  pollen_data_clean = pollen_data[rough_selection$dataset.id[rough_selection$rough_pass]], 
  

  ## get site_meta
  sites_meta = get_sites_meta(pollen_sites_clean, regions),
  
  
  
  #get best chronology
  
  #get geochron
  
  #selection criteria
  #weak criteria - omit hopeless sites
    #no chronology
    #v short
    #not Holocene
    #v low resolution (< 1/1000 yrs)
  
  #drop hopeless datasets
  
  #strict criteria
    #drop 

  #charcoal data-sets download

  ## ecological_groups
  ecol_groups = get_table("EcolGroupTypes") %>% 
    select(-starts_with("RecDate")),
  
  #ecological groups
  wanted_pollen = c("TRSH", "UPHE", "VACR", "SUCC", "PALM", "MANG"),
  
    
  #pull out pollen etc
  pollen = pollen_data_clean %>% map(get_pollen, wanted = wanted_pollen),
  
  fungal = map2(pollen_data_clean, pollen, get_group, wanted = "FUNG"),
 # charcoal = map2(pollen_data_clean, pollen, get_group, wanted = "CHAR") #no dataset has charcoal in counts - some have it in taxon.list
  
  #merge pollen types
  regions = read_csv(file_in("data/region_bounding_boxes.csv")),
  region_map = {mp <- map_data("world")
                detach("package:maps")#conflicts with purrr
                ggplot(regions, aes(xmin = long_min, xmax = long_max, ymin = lat_min, ymax = lat_max, fill = region)) +
                  geom_map(map = mp, data = mp, aes(map_id = region), inherit.aes = FALSE, fill = "grey50", colour = "grey30") +
                  geom_rect(alpha  = 0.5, show.legend = FALSE) + 
                  geom_point(aes(x = long, y = lat), data = sites_meta, colour = scales::muted("red"), inherit.aes = FALSE) +
                  geom_text(aes(x = (long_min + long_max)/2, y = (lat_min + lat_max)/2, label = region)) + 
                  coord_quickmap() + 
                  scale_x_continuous(expand = c(0, 0)) + 
                  scale_y_continuous(expand = c(0, 0)) +
                  labs(x = "°E", y = "°N")},
  taxonomic_merges = read_csv(file_in("data/region_merges.csv")),
  merged_pollen = merge_pollen_by_region(pollen, taxonomic_merges, sites_meta)
 #get merges - check sensible
)
#why do some dataset.id occur multiple times in sites_meta? - duplicate collection units??



import_conf <- drake_config(import_neotoma_plan)
download_again <- FALSE
make(import_neotoma_plan, trigger = trigger(condition = download_again))
vis_drake_graph(import_conf, targets_only = TRUE)

