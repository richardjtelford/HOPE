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

mk_merge_dictionary <- function(meta_pollen, merges){
  #get all names in use
  old_names <- meta_pollen %>% 
    select(region, raw_thin_counts) %>% 
    unnest() %>% 
    filter(variable.element == "pollen") %>% 
    select(-sampleID, -count) %>% 
    distinct() %>% 
    group_by(region) %>% 
    nest(.key = taxon)
  
  #prepare regular expression
  merges <- merges %>% 
    group_by(region) %>% 
    summarise(expres = paste0(".*(", paste(merge, collapse = "|"), ").*"))
  
  #get merged names by region
  left_join(old_names, merges, by = "region") %>% 
    mutate(taxon = map2(taxon, expres, ~mutate(.x, new_taxa = gsub(.y, "\\1", taxa)))) %>% 
    unnest() %>% 
    filter(taxa != new_taxa) %>% 
    select(region, taxa, new_taxa) %>% 
    group_by(region) %>% 
    nest(.key = merges)
}

get_pollen_etc <- function(sites_meta, pollen_data, wanted){
  #check everything matches
  assert_that(are_equal(as.character(sites_meta$dataset.id), names(pollen_data)))
  
  #add pollen data
  #chronology
  #labdata
  sites_meta_pollen <- sites_meta %>% 
    mutate(
      raw_counts = map(pollen_data, counts),
      
      chronologies = map(pollen_data, "chronologies"),
      lab_data = map(pollen_data, "lab.data"),
      taxonomy = map(pollen_data, "taxon.list"), 
      raw_thin_counts = map2(raw_counts, taxonomy, mk_thin_pollen))
      
  return(sites_meta_pollen)
}

process_meta_pollen <- function(meta_pollen, merge_dictionary, wanted, min_count = 150){
  meta_pollen <- meta_pollen %>% 
    left_join(merge_dictionary, by = "region") %>% 
    mutate(
      merged_thin = map2(raw_thin_counts, merges, .f = merge_taxa),
      merged_thin = map(merged_thin, .f = calc_percent, wanted = wanted),
    #remove low counts
      merged_thin = map(merged_thin, ~(filter(., pollen_sum >= min_count)))
      ) %>% 
    select(-merges)
  return(meta_pollen)
}



#todo
#merge species by region
#calculate percent for tree shrub herb etcraw_thin_counts, region, merge_taxa
#calculate percent for ferns, charcoal etc
#find charcoal data

mk_thin_pollen <- function(counts, taxon.list){
  if(any(names(taxon.list) == "alias")){
    name_column <- "alias"
  } else {
    name_column <- "taxon.name"
  }
  
  counts %>%
    rownames_to_column(var = "sampleID") %>% 
    gather(key = taxa, value = count, -sampleID) %>% 
    filter(count > 0) %>% 
    left_join(taxon.list, by = c("taxa" = name_column)) %>% 
    
    #filter pollen/spores/ascospores - no charcoal in the counts 
    filter(variable.element %in% c("pollen", "spores", "ascospore")) %>% 
    as_tibble()
}
  
merge_taxa <- function(thin_pollen, merges){
  if(is.null(merges)){
    return(thin_pollen)
  }
  
  merged_pollen <- thin_pollen %>% 
    left_join(merges, by = "taxa") %>% 
    mutate(taxa = coalesce(new_taxa, taxa)) %>% 
    group_by(sampleID, taxa, variable.units, variable.element, variable.context, taxon.group, ecological.group) %>% 
    summarise(count = sum(count))
  return(merged_pollen)
}

calc_percent <- function(x, wanted){
  thin_percent <- x %>%
    filter(ecological.group %in% c(wanted, "VACR", "FUNG")) %>% 
    mutate(group = if_else(ecological.group %in% wanted, "Terrestrial_Pollen", as.character(ecological.group))) %>% 
    group_by(sampleID, group) %>%
    mutate(group_sum = sum(count)) %>% 
    group_by(sampleID) %>% 
    mutate(
      pollen_sum = first(group_sum[group == "Terrestrial_Pollen"]),
      pollen_group = if_else(group == "Terrestrial_Pollen", pollen_sum, group_sum + pollen_sum), 
      percent = count/pollen_group * 100)
  
  return(thin_percent)
}