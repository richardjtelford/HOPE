get_sites_meta <- function(pollen_sites, regions){
  #extract site information
  sites_meta <- pollen_sites %>% 
    map("site.data") %>% 
    map_df(as_data_frame) %>% 
    bind_cols(pollen_sites %>% 
                map_df("dataset.meta"))
  
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
  
  ## merge depo environement with  site_meta
  sites_meta <- sites_meta %>% 
    left_join(CollectionUnits, by = c("collection.handle" = "Handle"))
  
  ## find region
  sites_meta <- sites_meta %>% 
    rowwise() %>% 
    mutate(region = which_region(regions, long = long, lat = lat)) %>% 
  ungroup()

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
