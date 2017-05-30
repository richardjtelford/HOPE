#load packages
library("tidyverse")
library("neotoma")

#register parallel backend
doMC::registerDoMC(cores = 3)


####download pollen
allPollenSites <- get_dataset(datasettype = "pollen")
allPollenIDs <- sapply(allPollenSites, FUN = function(x) x$dataset$dataset.id)
allPollenData <- get_download(allPollenIDs)
save(allPollenSites, allPollenData, file = "allPollenData.RData")
load("allPollenData.RData")
allPollenCounts <- counts(allPollenData)


#find non-count data
z <- sapply(allPollenCounts, function(x){
  any(x[x > 0] < 0.5)
  #any(x<0)
})
which(z)


#find non-integer values
nonDec <- plyr::ldply(allPollenCounts, function(x){
  gather(x, key = taxon, value = count) %>% 
    mutate(mod = round(count, 5) %% 1) %>% #round to ignore ~integers
    filter(
      !mod %in% c(0, 0.5), #remove integer values
      !round(mod, 2) %in% c(0.33, 0.67)
    ) %>% 
    select(-mod)
}, .parallel = TRUE)
nonDec

nonDec %>% group_by(.id, taxon) %>% 
  mutate(n = n()) %>% 
  distinct(.id, taxon, .keep_all = TRUE) %>% 
  rename(example_count = count) %>% 
  write.table(file = "datasetID_taxa_n_non_integers.csv", sep = ",", row.names = FALSE)

nonDec %>% count(.id) %>% 
  write.table(file = "datasetID_n_non_integers.csv", sep = ",", row.names = FALSE)

#zap duff data
allPollenCounts$'16090' <- NULL #?pollen influx rates
allPollenCounts$`16210` <- NULL #duff
allPollenCounts$`16209` <- NULL #duff

combinedtaxonomy <- plyr::ldply(allPollenData, function(x)x$taxon.list) %>% 
  select(-.id) %>% distinct() %>% as_tibble()

z <- sapply(allPollenData, function(x){
  cnt <- counts(x)
  tax <- x$taxon.list
  ncol(cnt) == nrow(tax) 
})

setdiff(allPollenData[[1]]$taxon.list$taxon.name, colnames(allPollenData[[1]]$counts))
setdiff( colnames(allPollenData[[1]]$counts), allPollenData[[1]]$taxon.list$taxon.name)


#countSums, singletons
allCountSums <- plyr::ldply(allPollenData, function(x){
    cnt <- counts(x)
    tax <- x$taxon.list
    keep <- tax$taxon.name[tax$ecological.group %in% c("TRSH", "UPHE", "VACR")] %>% as.character()
    cnt <- cnt %>% select(one_of(keep))
    data_frame(
      n = 1:nrow(cnt),
      counts = rowSums(cnt),
      singletons = apply(cnt == 1, 1, any),
      minCount = apply(cnt, 1, function(r){ 
          if(sum(r) > 0) min(r[r > 0])
          else NA
        })
      )
  }, .parallel = TRUE)

#countsums
ggplot(allCountSums, aes(x = counts)) + 
  geom_histogram()

#singletons
allCountSums %>% count(singletons)
allCountSums %>% count(minCount)

ggplot(allCountSums, aes(x = minCount, y = counts)) + 
  geom_jitter(height = 0, width = 0.2) +
  lims(x = c(0, 20), y = c(0, 2000))

#rows without singletons
lapply(allPollenCounts, function(x){
  keep <- apply(x, 1, function(r) min(r[r > 0])) > 1
  x <- x[keep, ]
  if(nrow(x) > 0) 
    return(x)
  else 
    return(NULL)
})



