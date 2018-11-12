#data checks 

#duplicate names
pollen_data %>% map_df(~{print(.$dataset$dataset.meta$dataset.id)
#browser()
  taxon.list <- (.$taxon.list)
  
  if(any(colnames(taxon.list) == "alias")){#use alias if any
    taxon.list$taxon.name <- taxon.list$alias
  }
  taxon.list %>% count(taxon.name) %>% filter(n > 1)
})
#just Larix

## empty rows.

min_cnt <- pollen_data %>% map_df(~{
  counts <- counts(.)
  
  taxon.list <- (.$taxon.list) %>% 
      filter(ecological.group %in% wanted)
    
    if(any(colnames(taxon.list) == "alias")){#use alias if any
      taxon.list$taxon.name <- taxon.list$alias
    }
  
  #make names unique
  names(counts) <- make.names(names(counts), unique = TRUE)
  
  counts <- counts[, make.names(taxon.list$taxon.name, unique = TRUE)]
  data_frame(cnt = min(rowSums(counts)))
}, .id = "id") %>% filter(cnt < 100)

min_cnt %>% arrange(cnt) %>% count(cnt)
