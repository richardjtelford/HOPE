get_collection_unit_depo_type <- function(){
  ### download environment
  DepEnvtTypes0 <- get_table("DepEnvtTypes")
  CollectionUnits <- get_table("CollectionUnits")
  
  ## get environment data
  DepEnvtTypes <- DepEnvtTypes0 %>% 
    left_join(DepEnvtTypes0, by = c("DepEnvtHigherID" = "DepEnvtID"), suffix = c("", ".1")) %>% 
    left_join(DepEnvtTypes0, by = c("DepEnvtHigherID.1" = "DepEnvtID"), suffix = c("", ".2")) %>%
    left_join(DepEnvtTypes0, by = c("DepEnvtHigherID.2" = "DepEnvtID"), suffix = c("", ".3")) %>% 
    left_join(DepEnvtTypes0, by = c("DepEnvtHigherID.3" = "DepEnvtID"), suffix = c("", ".4")) %>% 
    select(-starts_with("RecDate")) %>% 
    as_tibble()
  
  CollectionUnits <- CollectionUnits %>% 
    as_tibble() %>% 
    left_join(DepEnvtTypes, by = "DepEnvtID") %>% 
    select(-starts_with("RecDate"))
  
  return(CollectionUnits)
}
  


get_sites_meta <- function(pollen_sites, regions, collection_units){
  
  #extract site information
  sites_meta <- pollen_sites %>% 
    map("site.data") %>% 
    map_df(as_data_frame) %>% 
    bind_cols(pollen_sites %>% 
                map_df("dataset.meta"))
  
  ## merge depo environement with  site_meta
  sites_meta <- sites_meta %>% 
    left_join(collection_units, by = c("collection.handle" = "Handle", "site.id" = "SiteID"))
  
  ## find region
  sites_meta <- sites_meta %>% 
    mutate(region = map2_chr(long, lat, which_region, regions = regions))

  return(sites_meta) 
}

which_region <- function(regions, long, lat){
  region <- regions %>% 
    filter(long_min < long, long_max >= long, lat_min < lat, lat_max >= lat) %>% 
    pull(region)
  if(length(region) < 1) {region <- "Not in region"}
  if(length(region) > 1) {stop("Site in multiple regions")}
  return(region)
}

get_pollen_etc <- function(sites_meta, pollen_data){
  #check everything matches
  assert_that(are_equal(sites_meta$dataset.id, names(pollen_data)))
  
  #add pollen data
  #chronology
  #labdata
  sites_meta_pollen <- sites_meta %>% 
    mutate(
      raw_data = map(pollen_data, gather_data),
      chronologies = map(pollen_data, "chronologies"),
      lab_data = map(pollen_data, "lab.data"))
  return(sites_meta_pollen)
}


gather_data <- function(x){
  if(any(names(x$taxon.list) == "alias")){
    name_column <- "alias"
  } else {
    name_column <- "taxon.name"
  }
  
  counts(x) %>%
    as_tibble(rownames = "sampleID") %>% 
    gather(key = taxa, value = count, -sampleID) %>% 
    filter(count > 0) %>% 
    left_join(x$taxon.list, by = c("taxa" = name_column))
}
  
