#biome

library("tidyverse")

#load pollen

#load pollen-pft dictionary

#load pft biome assignment


pollen %>% 
  gather(key = taxon, value = percent, -sampleID) %>%
  filter(percent > 0.5) %>% 
  left_join(pollen_pft) %>% 
  group_by(sampleID, pft) %>% 
  summarise(pft_sum = sum(sqrt(percent))) %>% 
  left_join(pft_biome) %>% 
  group_by(sampleID, biome) %>% 
  summarise(affinity = sum(pft_sum)) %>% 
  arrange(desc(affinity))



