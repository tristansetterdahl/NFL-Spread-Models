library(glmnet)
library(tidyverse)
library(magrittr)
library(ranger)
library(ggplot2)
library(tree)

###Reading in data###
getwd()
dat <- read_csv("2020_nfl_spreads.csv", col_names = TRUE)
dat$Differential %>% head

#exploratory plots

dat %>% ggplot(mapping = aes(x = Differential)) + geom_freqpoly(binwidth = .5, color = 'green') +
  geom_freqpoly(aes(x = abs(Spread)), color = 'red', binwidth = .5)
  #we see that actual score differentials are somewhat normally distributed, centered around 5, skewed negatively
  #spreads are kept tight. obviously no negative values (abs)
summary(abs(dat$Spread))
summary(dat$Differential)
  #Similarly centered distributions, differential just has much larger tails

dat %>% ggplot(mapping = aes(x = abs(Spread), y = Differential, color = FavDiv)) +
  geom_point() + geom_abline(slope = 1, color = 'red', linetype = 'dashed')
  #cant tell a whole lot here. would be good if can make it clearer, but so many colors.
  #pretty much shows spread is 5050 to hit either way

dat %>% ggplot(mapping = aes(x = FavDiv, fill = SpreadWin)) + geom_bar(position = 'fill')
  #actually really like this one, shows that underdog covers more often, especially against nfc east teams
dat$SpreadWin %>% as.factor() %>% summary
  #confirmed, underdog covers 150/268 games in 2020, 55.9%



#messing around with models
glmod <- glm(as.factor(SpreadWin)~as.factor(FavDiv) + as.factor(UDDiv), family = 'binomial', data = dat)
glmod %>% summary

lmod <- lm(Differential ~ abs(Spread) + as.factor(Where), data = dat)
lmod %>% summary
#these are both pretty terrible

treemod <- tree(Differential ~ Spread + as.factor(FavDiv) + as.factor(UDDiv), dat)
treemod %>% summary
treemod %>% plot
text(treemod, pretty = 0)
