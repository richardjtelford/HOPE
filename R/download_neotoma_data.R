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
  sites_meta = get_sites_meta(pollen_sites, pollen_data),

  ## ecological_groups
  ecol_groups = get_table("EcolGroupTypes") %>% 
    select(-starts_with("RecDate")),

  wanted = c("TRSH", "UPHE", "VACR", "SUCC", "PALM", "MANG")
)

import_conf <- drake_config(import_neotoma_plan)
make(import_neotoma_plan)
vis_drake_graph(import_conf, targets_only = TRUE)

# ## save downloaded data
# save(sites_meta, pollen_sites, pollen_data, DepEnvtTypes, CollectionUnits, wanted, ecol_groups, file = "data/pollen.Rdata")
