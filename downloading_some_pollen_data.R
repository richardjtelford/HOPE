#load packages
library("tidyverse")
library("neotoma")

#register parallel backend
doMC::registerDoMC(cores = 3)


####download pollen
if(FALSE){ #SLOW
  allPollenSites <- get_dataset(datasettype = "pollen")
  allPollenIDs <- sapply(allPollenSites, FUN = function(x) x$dataset$dataset.id)
  allPollenData <- get_download(allPollenIDs)
  save(allPollenSites, allPollenData, file = "allPollenData.RData")
}

#load data
load("allPollenData.RData")


#map of datasets
mp <- map_data("world")
plyr::ldply(allPollenData, function(x)x$dataset$site.data) %>% 
  ggplot(aes(x = long, y = lat)) + 
    geom_map(data = mp, map = mp, mapping = aes(map_id = region), fill = "grey80") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(limits = c(-70, NA), expand = c(0,0)) +
    geom_point() + 
    coord_quickmap() +
    labs(x = "Longitude E°", y = "Latiude N°")


#taxonomy
combinedtaxonomy <- plyr::ldply(allPollenData, function(x)x$taxon.list) %>% 
  select(-.id) %>% distinct() %>% as_tibble()

get_table("EcolGroupTypes")

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

#zap duff data
allPollenCounts$'16090' <- NULL #?pollen influx rates
allPollenCounts$`16210` <- NULL #?percent
allPollenCounts$`16209` <- NULL #?percent
allPollenCounts$`15696` <- NULL #?percent
allPollenCounts$`15059` <- NULL #?percent
allPollenCounts$`1558` <- NULL #?percent

allPollenCounts$`17391` <- NULL #Mostly OK, some weird data 0.01
allPollenCounts$`17357` <- NULL #Mostly OK, some weird data 0.1

allPollenCounts$'488' <- NULL #?pollen influx rates - all integers but far too high AUER



#find non-integer values
nonDec <- plyr::ldply(allPollenCounts, function(x){
  gather(x, key = taxon, value = count) %>% 
    mutate(mod = round(count, 4) %% 1) %>% #round to ignore ~integers
    filter(
      !mod %in% c(0, 0.5), #remove integer values
      !round(mod, 2) %in% c(0.33, 0.67)
    ) %>% 
    select(-mod)
}, .parallel = TRUE)

nonDec %>% group_by(.id, taxon) %>% 
  mutate(n = n()) %>% 
  distinct(.id, taxon, .keep_all = TRUE) %>% 
  rename(example_count = count) %>% print(n = Inf)
  #write.table(file = "datasetID_taxa_n_non_integers.csv", sep = ",", row.names = FALSE)

nonDec %>% count(.id) %>% arrange(n) %>% print(n = Inf) 
  #write.table(file = "datasetID_n_non_integers.csv", sep = ",", row.names = FALSE)

nonDec %>% filter(!grepl("Pinus|Picea|Abies", taxon)) %>% count(.id) %>% arrange(n) %>% print(n = Inf) 



#countSums, singletons
allCountSums <- plyr::ldply(allPollenCounts, function(x){
    x <- ceiling(x)
    x <- x[rowSums(x) > 0, , drop = FALSE] # drop samples with no pollen
    
    df <- data_frame(
      n = 1:nrow(x),
      counts = rowSums(x),
      singletons = apply(x == 1, 1, any),
      minCount = apply(x, 1, function(r){ 
          if(sum(r) > 0) min(r[r > 0])
          else NA
        }),
      mult = sapply(1:nrow(x), function(i) { #multiples
        if(singletons[i]) {
          return(NA)
        } else {
          x2 <- x[i, ] 
          x2 <- x2[x2 > 0] #only presences
          x3 <- x2 %% minCount[i]
          mean(x3 == 0)
        }
      }), 
      ntaxon = ncol(x),
      ptaxon = rowSums(x > 0)
    )
    
    df  
  }, .parallel = TRUE)

#countsums
ggplot(allCountSums, aes(x = counts)) + 
  geom_histogram()

allCountSums %>% filter(counts == 1000) %>% count(singletons)
allCountSums %>% filter(counts < 5000) %>% summarise(sum = sum(counts))

#ntaxa
allCountSums %>% ggplot(aes(x = ptaxon, fill = singletons)) + geom_density(alpha = 0.3)

allCountSums %>% ggplot(aes(x = counts, y = ntaxon,  colour = singletons)) + geom_jitter(alpha = 0.3) + scale_x_log10()



#singletons
allCountSums %>% group_by(spprich = ntaxon > 25) %>% count(singletons) %>% mutate(p = round(nn/sum(nn) * 100, 1))

allCountSums %>% group_by(spprich = cut(ntaxon, c(0, 20.1, 40.1, Inf), labels = c("< 25", "25 - 40",  "> 40"))) %>% 
  count(singletons) %>% mutate(sum = sum(nn), p = round(nn/sum(nn)*100, 1)) %>% filter(!singletons) %>% select(`No. taxa` = spprich, `No. samples` = sum, `Without singletons %` = p)

allCountSums %>% group_by(spprich = cut(ntaxon, c(0, 20.1, 40.1, Inf), labels = c("< 25", "25 - 40",  "> 40"))) %>% 
  count(singletons) %>% mutate(sum = sum(nn), p = round(nn/sum(nn)*100, 1)) %>% filter(!singletons) %>% select(`No. taxa` = spprich, `No. samples` = sum, `Without singletons %` = p)



allCountSums %>% group_by(about1000 = counts == 1000) %>% count(singletons) %>% mutate(p = nn/sum(nn))

allCountSums %>% filter(ntaxon > 25) %>% group_by(ntaxon) %>% count(singletons) %>% spread(key = singletons, value = nn, fill = 0) %>% ungroup() %>% mutate(F = cumsum(`FALSE`), T = cumsum(`TRUE`), tot = F + T, prop = F/tot) %>% ggplot(aes(x = ntaxon, y = prop, colour = ntaxon)) +geom_line()

allCountSums %>% count(minCount) %>% ggplot(aes(minCount, nn)) + geom_line() + scale_y_log10() 
allCountSums %>% mutate(rich = ifelse(ntaxon > 25, "> 25 taxa", "<= 25 taxa")) %>% ggplot(aes(x = counts, fill = singletons)) + 
  geom_bar(width = 1, position = "stack", show.legend = FALSE) + 
  scale_x_continuous(limits = c(0, 1200), expand = c(0, 0), breaks = seq(0, 1200, 100)) +
  facet_wrap(~rich, ncol = 1, scales = "free_y")    

allCountSums %>% ggplot(aes(x = ntaxon, fill = singletons)) + 
  geom_bar(width = 1, position = "stack", show.legend = FALSE) 
allCountSums %>% ggplot(aes(x = ntaxon, fill = singletons)) + 
  geom_bar(width = 1, position = "fill", show.legend = FALSE) 

allCountSums %>% ggplot(aes(x = ptaxon, fill = singletons)) + 
  geom_bar(width = 1, position = "stack", show.legend = FALSE) 

ggplot(allCountSums, aes(x = minCount, y = counts)) + 
  geom_jitter(height = 0, width = 0.2) +
  lims(x = c(0, 20), y = c(0, 2000))

ggplot(allCountSums, aes(x = ntaxon, y = ptaxon)) + 
  geom_jitter()

ggplot(allCountSums, aes(x = counts, fill = singletons)) + 
     geom_histogram() +
     xlim(0, 2000) +
  facet_wrap(~singletons, ncol = 1)

ggplot(allCountSums, aes(x = counts, y = singletons * 1)) + 
  geom_point() +
  geom_smooth(aes(group = 1), method = "gam", method.args = list(family = binomial)) +
  xlim(0, 2000)

##by dataset
allCountSums %>% group_by(.id) %>% summarise(mn = mean(singletons), n = n()) %>% group_by(mn == 1) %>% count()

allCountSums %>% group_by(.id, rich = ntaxon > 25) %>% summarise(mn = mean(singletons), n = n()) %>% 
  ggplot(aes(x = mn, fill = rich)) + geom_histogram() + facet_wrap(~rich)

allCountSums %>% group_by(.id) %>% filter(ntaxon > 25) %>% summarise(mn = mean(singletons), n = n()) %>% filter(mn < 0.9)


## last digit distributions
lastDigit <- plyr::ldply(allPollenCounts, function(x){ 
  x %>% 
    gather() %>% 
    filter(value > 0) %>% 
    mutate(value = ceiling(value), value = value %% 10) %>% 
    count(value)
})


lastDigit %>%  
  group_by(.id) %>% 
  spread(key = value, value = n) %>% 
  filter(`1` < `8`) %>% 
  gather(key = n, value = value, -.id)  %>% 
  mutate(n = factor(n, levels = c(1L:9L, 0L))) %>% 
  ggplot(aes(n, value, fill = as.factor(n))) + 
  geom_col() + scale_fill_brewer(palette = "Paired") + 
  facet_wrap(~.id, scales = "free_y")



## multiple
allCountSums %>% filter(ptaxon > 7) %>% ggplot(aes(x = mult)) + geom_histogram()

mean(allCountSums$mult == 1, na.rm = TRUE)
