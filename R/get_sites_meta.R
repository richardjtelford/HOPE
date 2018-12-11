get_sites_meta <- function(pollen_sites){
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

  return(sites_meta) 
}