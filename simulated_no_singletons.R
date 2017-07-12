#
library("vegan")
data(BCI)

abu <- colSums(BCI)

mx.power <- 4
sapply(1:mx.power, function(p)max(abu^p)/ sum(abu^p))

sapply(1:mx.power, function(p)round(sort((abu^p)/ sum(abu^p), decreasing = TRUE)[1:10], 3)) 
sapply(1:mx.power, function(p)exp(diversity(abu^p)))

data_frame(
  rank = rep(rank(-abu, ties = "random"), mx.power), 
  power = as.factor(rep(1:mx.power, each = length(abu))), 
  abu = unlist(lapply(1:mx.power, function(x) abu ^ x))) %>% 
ggplot(aes(x = rank, y = abu, colour = power)) + 
  geom_point() + 
  scale_y_log10()

mean.singletons <- expand.grid(size = c(10, 15, 20, 25, 30, 40, 50, 75, 100, 200, 300, 500, 1000), 
                               power = 1:mx.power,
                               nrep = 100000) %>% 
  group_by(size, power, nrep) %>% 
  do({
    rabu <- rmultinom(.$nrep, size = .$size, prob = abu ^ .$power)
    data.frame(singletons = mean(apply(rabu != 1, 2, all)),
               N1 = exp(diversity(abu ^ .$power)))
  })

mean.singletons %>% ggplot(aes(x = size, colour = as.factor(round(N1)), y = singletons, weight = nrep)) +
  geom_point()+ 
  geom_smooth(method = "gam", method.args = list(family = binomial), se = FALSE) + 
  scale_x_log10() + 
  labs(x = "Count sum", y = "Proportion of counts with no singletons", colour = "Hill's N1")

