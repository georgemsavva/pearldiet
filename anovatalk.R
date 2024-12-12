## Scripts for ANOVA talk


## should have set a seed for this one.

library(ggplot2)
library(ggbeeswarm)

group = rep(c("A", "B"),each=10)
y = rnorm(20)

dat <- data.frame(y,group)

ggplot(dat) + aes(x=group, y=y) + geom_beeswarm() + theme_bw() + 
  stat_summary(geom="errorbar", fun.max = mean, width=0.5)

summary(lm(data=dat, y~group))


