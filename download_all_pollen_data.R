## ---- download

if(!file.exists("allPollenData.RData")){ #SLOW
  allPollenSites <- get_dataset(datasettype = "pollen")
  allPollenIDs <- sapply(allPollenSites, FUN = function(x) x$dataset$dataset.id)
  allPollenData <- get_download(allPollenIDs)
  save(allPollenSites, allPollenData, file = "allPollenData.RData")
}

## ---- allPollen
#keep only terrestrial pollen & spores
allPollenCounts <- plyr::llply(allPollenData, function(x){
  tax <- x$taxon.list %>% 
    filter(grepl("NISP", variable.units),#only count data
           grepl("pollen|spore", variable.element), #only pollen/spores (also megaspores etc)
           grepl("TRSH|UPHE|VACR|MANG|SUCC|VASC|PALM", ecological.group)
    )
  
  if(is.null(tax$alias)){
    spp <- tax$taxon.name
  } else {
    spp <- tax$alias
  }
  spp <- as.character(spp)
  counts(x) %>% select(one_of(spp))
}, 
.parallel = TRUE
)
#remove sites with no counts in required ecological groups
allPollenCounts <- allPollenCounts[sapply(allPollenCounts, ncol) > 0]

processPollen <- . %>% 
  rownames_to_column(var = "sampleID") %>% 
  gather(key = species, value = count, -sampleID)
allPollen <- plyr::ldply(allPollenCounts, processPollen, .parallel = TRUE)

assertthat::assert_that(!any(is.na(allPollen$count)))#NAs not allowed
assertthat::assert_that(min(allPollen$count) >= 0)#no negative values

allPollen <- allPollen %>% filter(count > 0)


#fix near integers
allPollen <- allPollen %>% 
  mutate(count = round(count, 3))

#find percent
pinus <- "^Pinus|^Picea|^Abies|^Pinaceae|^Tsuga|^Podocarp|^Acer"

notPinus <- allPollen %>%
  filter(!grepl(pinus, species)) %>% 
  mutate(int  = count %% 1 == 0)

many_not_int <- notPinus %>% 
  group_by(.id) %>% 
  summarise(Proportion = mean(!int), Number = sum(!int)) %>% 
  filter(Number > 0) %>% 
  arrange(desc(Proportion))

allPollen <- allPollen %>% 
  anti_join(many_not_int %>% filter(Proportion > 0.5)) %>% #remove probably % data
  mutate(count = ceiling(count))# round up
