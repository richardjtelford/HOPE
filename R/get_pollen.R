get_wanted <- function(object, wanted, pollen_spore = FALSE){
  #extract counts from object
  counts <- counts(object)
  
  #get wanted ecological groups
  taxon.list <- (object$taxon.list) %>% 
    filter(ecological.group %in% wanted) %>% 
    arrange(ecological.group)
  
  #if pollen then only pollen/spores, not random plant parts
  if(pollen_spore){
    taxon.list <- taxon.list %>% 
      filter(variable.element %in% c("pollen", "pollen/spore", "spore"))
  }
  
  #some taxa
  if(any(colnames(taxon.list) == "alias")){#use alias if any
    taxa <- taxon.list$alias
  }else {
    taxa <- taxon.list$taxon.name
  }
  taxa <- as.character(taxa)
  
  counts <- counts[, taxa, drop = FALSE]
  return(counts)
}

get_pollen <- function(object, wanted_pollen){
  get_wanted(object, wanted_pollen, pollen_spore = TRUE)
}

get_group <- function(object, wanted, pollen){
  what <- get_wanted(object, wanted = wanted)
  if(ncol(what) == 0){
    return(NULL)
  } else {
    return(what/rowSums(pollen) * 100)
  }
}

