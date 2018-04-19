library(rioja)
data(SWAP)

mod <- crossval(WA(SWAP$spec, SWAP$pH, mono = TRUE))
mat_mod <- MAT(SWAP$spec, SWAP$pH)

dists <- paldist(SWAP$spec)
thresh <- quantile(dists, c(0, 0.05, 0.1))
thresh

dat <- data_frame(resid = mod$residuals.cv[, "WA.m"], resid2 = abs(resid), dist = mat_mod$dist.n[, 1], pH = SWAP$pH)

dat %>% ggplot(aes(x = dist, y = resid2)) + 
  geom_point() + 
  geom_smooth(method  = "lm") +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = thresh))


mod1 <- lm(resid2 ~ dist, dat)


autoplot(mod1)
anova(mod1)
summary(mod1)


