#script for data exploration

library(ggplot2)
library(magrittr)
library(reshape2)
getwd()
setwd('Sports and Data Science')

#loading in data
dat <- read.csv('all_data_2020.csv')


#making a correlation heat map
#first, need the numeric data
num_dat <- dat[,sapply(dat, typeof) %in% c('integer', 'double')] %>% drop_na()
num_dat %<>% select(-c('Fav.FUM', 'UD.FUM', 'X'))
cor_dat <- cor(num_dat)
cor_dat[lower.tri(cor_dat)] <- NA
#melting it
melted_cor_dat <- melt(cor_dat, na.rm = TRUE)
melted_cor_dat %>% nrow

ggplot(data = melted_cor_dat, mapping = aes(x = Var2, y = Var1, fill = value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


spread_corrs <- melted_cor_dat[melted_cor_dat['Var1'] == 'Spread',] %>% arrange(desc(abs(value))) 
diff_corrs <- melted_cor_dat[melted_cor_dat['Var1'] == 'Differential',] %>% arrange(desc(abs(value)))
fav_score_corrs <- melted_cor_dat[melted_cor_dat['Var1'] == 'FavScore',] %>% arrange(desc(abs(value)))
ud_score_corrs <- melted_cor_dat[melted_cor_dat['Var1'] == 'UDogScore',] %>% arrange(desc(abs(value)))


ud_score_corrs %>% head
fav_score_corrs %>% head

cor_dat %>% colnames



#histograms for scores and differentials
hist(dat$Differential)
hist(dat$FavScore)
hist(dat$UDogScore)


hist(dat$FavScore + dat$UDogScore) #this looks very normal(50, 14.12**2), maybe SLR would be good here
mean(dat$FavScore + dat$UDogScore)
sd(dat$FavScore + dat$UDogScore)
mean((dat$FavScore + dat$UDogScore) > mean(dat$FavScore + dat$UDogScore) + 2*sd(dat$FavScore + dat$UDogScore)) +
  mean((dat$FavScore + dat$UDogScore) < mean(dat$FavScore + dat$UDogScore) - 2*sd(dat$FavScore + dat$UDogScore))


     