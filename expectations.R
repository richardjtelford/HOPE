#packages
library("tidyverse")
library("vegan")

#data
data(BCI)



#distribution of last digits in full dataset
BCI %>% 
  gather() %>% 
  filter(value > 0) %>% 
  mutate(
    value = value %% 10,
    value = factor(value, levels = c(1:9, 0))
  ) %>%
  ggplot(aes(x = value)) + 
    geom_bar()


#effect of limited taxonomic breadth and count size
BC_1 <- colSums(BCI)
BC_low <- sort(BC_1, decreasing = TRUE)[1:25]

sizes <- c(50, 100, 200, 500)

plyr::ldply(list(full = BC_1, low = BC_low), function(dataset){
  plyr::ldply(setNames(sizes, sizes), function(size){
   rmultinom(1000, size = size, prob = dataset) %>% 
      as.data.frame() %>% 
      gather() %>% 
      filter(value > 0) %>% 
      mutate(
        value = value %% 10, 
        value = factor(value, levels = c(1:9, 0))
      ) %>% 
      count(value)
  }, .id = "countSize")
}, .id = "resolution") %>% 
  mutate(countSize = factor(countSize, levels = sizes)) %>% 
  ggplot(aes(x = value, y = n, colour = countSize, group = countSize)) + 
  geom_line()+
  geom_point() +
  facet_wrap(~resolution) + 
  labs(x = "Last digit", y = "Frequency", colour = "Count size")
