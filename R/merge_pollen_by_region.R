merge_pollen_by_region <- function(pollen, taxonomic_merges, sites_meta){
  
  result <- unique(sites_meta$dataset.id) %>% 
    set_names() %>% 
    map(function(id){
      poll <- pollen[[as.character(id)]]
      region_ <- sites_meta %>% 
        filter(dataset.id == id) %>% 
        slice(1) %>% #deal with case of duplicate dataset.id (from collection units)
        pull(region)
      taxonomic_merges <- taxonomic_merges %>% 
        filter(region == region_)
      
      if(nrow(taxonomic_merges) == 0) return(poll)#no merges to be made
      old_names <- names(poll)
      names(poll) <- merge_names(names(poll), taxonomic_merges$merge)
      new_names <- names(poll)
      if(identical(old_names, new_names)) return(poll)#no change, avoid more work
      print(paste("changing", id))
      poll <- poll %>% 
        rownames_to_column(var = "sampID") %>% 
        gather(key = taxa, value = count, -sampID) %>% 
        group_by(sampID, taxa) %>% 
        summarise(count = sum(count)) %>% 
        spread(key = taxa, value = count) %>% 
        as.data.frame() %>% 
        column_to_rownames(var = "sampID")
      
    return(poll)
  })
  return(result)
}


merge_names <- function(names, merges){
  for(i in 1:length(merges)){
    names[grep(merges[i], names)] <- merges[i]
  }
  names
  
}