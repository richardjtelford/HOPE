## load packages ####
library("drake")#load first to prevent conflicts
library("tidyverse")
library("neotoma")

#source functions
source("R/get_sites_meta.R")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

import_neotoma_plan <- drake_plan(
  #get dataset list
  pollen_sites = get_dataset(datasettype = "pollen"),

  #download_data
  pollen_data = pollen_sites %>% get_download(), #SLOW

  ## get site_meta
  sites_meta = get_sites_meta(pollen_sites),

  ## ecological_groups
  ecol_groups = get_table("EcolGroupTypes") %>% 
    select(-starts_with("RecDate")),
  
  #ecological groups
  wanted_pollen = c("TRSH", "UPHE", "VACR", "SUCC", "PALM", "MANG"),
  
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
  
  #pull out pollen etc
  pollen = pollen_data %>% map(get_pollen, wanted = wanted_pollen),
  
  ## need to subset away hopeless sites before next steps (datasets with no pollen in wanted ecological groups)
  fungal = pollen_data %>% map(get_group, wanted = "FUNG", pollen = pollen),
  charcoal = pollen_data %>% map(get_group, wanted = "CHAR", pollen = pollen)
)

import_conf <- drake_config(import_neotoma_plan)
download_again <- FALSE
make(import_neotoma_plan, trigger = trigger(condition = download_again))
vis_drake_graph(import_conf, targets_only = TRUE)

# ## save downloaded data
# save(sites_meta, pollen_sites, pollen_data, DepEnvtTypes, CollectionUnits, wanted, ecol_groups, file = "data/pollen.Rdata")


# orphaned
# ##age control
# age_control <- pollen_data %>% 
#   map(ages) %>% 
#   map_df(function(x){
#     x %>% 
#       summarise(
#         age_min = min(age, na.rm  = TRUE), 
#         age_max = max(age, na.rm = TRUE), 
#         n = n(), 
#         res = n/(age_max - age_min) * 1000)
#   },
#   .id = "dataset.id") %>% 
#   mutate(dataset.id = as.numeric(dataset.id)) %>% 
#   as_tibble() %>% 
#   mutate(length = case_when(
#     is.na(age_min) | is.infinite(age_min) ~ "none?",
#     age_max - age_min < 4000 ~ "short",
#     age_min < 2000 & age_max > 8000 ~ "Most Holocene",
#     age_min > 8000 ~ "Late Glacial",
#     TRUE ~"part Holocene"
#   ))
# 
# # join age_control to sites_meta
# sites_meta <- sites_meta %>% inner_join(age_control)
# 
# 
