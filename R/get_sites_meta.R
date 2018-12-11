get_sites_meta <- function(pollen_sites, pollen_data){
  #extract site information
  sites_meta <- pollen_sites %>% 
    map("site.data") %>% 
    map_df(as_data_frame) %>% 
    bind_cols(pollen_sites %>% 
                map_df("dataset.meta"))
  
  ### download environment
  DepEnvtTypes <- get_table("DepEnvtTypes")
  CollectionUnits <- get_table("CollectionUnits")
  
  ##age control
  age_control <- pollen_data %>% 
    map(ages) %>% 
    map_df(function(x){
      x %>% 
        summarise(
          age_min = min(age, na.rm  = TRUE), 
          age_max = max(age, na.rm = TRUE), 
          n = n(), 
          res = n/(age_max - age_min) * 1000)
    },
    .id = "dataset.id") %>% 
    mutate(dataset.id = as.numeric(dataset.id)) %>% 
    as_tibble() %>% 
    mutate(length = case_when(
      is.na(age_min) | is.infinite(age_min) ~ "none?",
      age_max - age_min < 4000 ~ "short",
      age_min < 2000 & age_max > 8000 ~ "Most Holocene",
      age_min > 8000 ~ "Late Glacial",
      TRUE ~"part Holocene"
    ))
  
  # join age_control to sites_meta
  sites_meta <- sites_meta %>% inner_join(age_control)
  
  
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