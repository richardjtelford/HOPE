## load packages ####
library("drake")#load first to prevent conflicts
library("maps")
library("tidyverse")
library("neotoma")
library("readr")
library("assertthat")

#source functions
source("R/get_sites_meta.R")
source("R/get_pollen.R")
source("R/general_plots.R")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#drake plan
import_neotoma_plan <- drake_plan(
  #get dataset list
  pollen_sites = get_dataset(datasettype = "pollen"),

  #download_data
  pollen_data = pollen_sites %>% get_download(), #SLOW
  
  collection_units = get_collection_unit_depo_type(),
  
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
        age_max < 5000 ~ "Late Holocene",
        age_min < 2000 & age_max > 8000 ~ "Most Holocene",
        age_min > 6000 ~ "Early Holocene",
        age_min > 9000 ~ "Late Glacial",
        TRUE ~"part Holocene"),
      rough_pass = length %in% c("Most Holocene", "part Holocene") & n > 16
      ),
  
  #drop datasets that fail rough_pass
  pollen_sites_clean = pollen_sites[rough_selection$dataset.id[rough_selection$rough_pass]],
  
  #drop pollen data that fail rough_pass
  pollen_data_clean = pollen_data[rough_selection$dataset.id[rough_selection$rough_pass]], 

  ## get metadata and make into nested tibble
  sites_meta = get_sites_meta(pollen_sites = pollen_sites_clean, regions = regions, collection_units = collection_units),
  
  #get best chronology
  
  #get geochron

  #charcoal data-sets download

  ## ecological_groups
  ecol_groups = get_table("EcolGroupTypes") %>% 
    select(-starts_with("RecDate")),
  
  #ecological groups
  wanted_pollen = c("TRSH", "UPHE", "SUCC", "PALM", "MANG"),

  #merge pollen types
  regions = read_csv(file_in("data/region_bounding_boxes.csv")),
  region_map = plot_region_map(regions, sites_meta),

  #taxonomic merges
  taxonomic_merges = read_csv(file_in("data/region_merges.csv")),
  merge_dictionary = mk_merge_dictionary(meta_pollen, taxonomic_merges),

  #process pollen
  meta_pollen = get_pollen_etc(sites_meta, pollen_data_clean, wanted),
  processed_pollen = process_meta_pollen(meta_pollen, merge_dictionary, wanted = wanted_pollen)
)#end of drake plan

#configure plan
import_conf <- drake_config(import_neotoma_plan)

#download fresh data (v slow)
download_again <- FALSE

#set up parallel processing for drake
future::plan(future::multiprocess) 

#make drake plan
make(import_neotoma_plan, trigger = trigger(condition = download_again), jobs = 2, parallelism = "future")

#show drake graph
vis_drake_graph(import_conf, targets_only = TRUE)


#known problems
  #Betula/Corylus/Myrica becomes Corylus (longest option) 