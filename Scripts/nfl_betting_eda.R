#script for data exploration

library(ggplot2)
library(magrittr)
library(reshape2)

set.seed(1738)

#loading in data
dat18 <- read_csv("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Full/all_data_2018.csv")
dat19 <- read_csv("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Full/all_data_2019.csv")
dat20 <- read_csv("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Full/all_data_2020.csv")
dat21 <- read_csv("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Full/all_data_2021.csv")
dat <- rbind(dat18, dat19, dat20, dat21)
train_idxs <- sample(1:nrow(dat), .75*nrow(dat), replace = FALSE)
train <- dat[train_idxs, ]
test <- dat[-train_idxs,]


colnames(dat)
#response variables of interest:
##FavScore
##UDogScore
##Differential
##Total
##OverUnder
##SpreadWin
##Winner


#making a correlation heat map
#first, need the numeric data
num_dat <- train[,sapply(train, typeof) %in% c('integer', 'double')] %>% drop_na()
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



#Exploring Differential
hist(train$Differential)
abline(v = mean(train$Differential), col = 'red', lty = 2)
abline(v = mean(train$Differential) + sd(train$Differential), col = 'blue', lty = 2)
abline(v = mean(train$Differential) - sd(train$Differential), col = 'blue', lty = 2)
abline(v = mean(train$Differential) + 2*sd(train$Differential), col = 'green', lty = 2)
abline(v = mean(train$Differential) - 2*sd(train$Differential), col = 'green', lty = 2)


#Exploring Total
hist(train$Total) #this looks very normal(47, 14.21**2), maybe SLR would be good here
abline(v = mean(train$Total), col = 'red', lty = 2)
abline(v = mean(train$Total) + sd(train$Total), col = 'blue', lty = 2)
abline(v = mean(train$Total) - sd(train$Total), col = 'blue', lty = 2)
abline(v = mean(train$Total) + 2*sd(train$Total), col = 'green', lty = 2)
abline(v = mean(train$Total) - 2*sd(train$Total), col = 'green', lty = 2)

dat[dat$Where == 'Home',]$Total %>% summary #(37, 47.27, 57)
dat[dat$Where == 'Away',]$Total %>% summary #(37, 46.62, 56)
dat[dat$Where == 'Neutral',]$Total %>% summary #(35.25, 40.14, 44.5) #Where the game is played really only afffects total score on neutral fields.

dat[dat$FavDiv == 'NFC East' & dat$UDDiv == 'NFC East',]$Total 

mean(dat$OverUnder == 'O')
mean(dat$OverUnder == 'U') #Under hits 51% of the time as opposed to 47% of the time
dat$Total %>% hist 
abline(v = mean(dat$OULine))

#Looking at distribution for favscore
hist(train$FavScore, breaks = 7) #this looks kinda normal(25.92, 9.92**2)
abline(v = mean(train$FavScore), col = 'red', lty = 2)
abline(v = mean(train$FavScore) + sd(train$FavScore), col = 'blue', lty = 2)
abline(v = mean(train$FavScore) - sd(train$FavScore), col = 'blue', lty = 2)
abline(v = mean(train$FavScore) + 2*sd(train$FavScore), col = 'green', lty = 2)
abline(v = mean(train$FavScore) - 2*sd(train$FavScore), col = 'green', lty = 2)

#Looking at distribution for favscore
hist(train$UDogScore, breaks = 9) #this looks not very normal(21.06, 9.86**2)
abline(v = mean(train$UDogScore), col = 'red', lty = 2)
abline(v = mean(train$UDogScore) + sd(train$UDogScore), col = 'blue', lty = 2)
abline(v = mean(train$UDogScore) - sd(train$UDogScore), col = 'blue', lty = 2)
abline(v = mean(train$UDogScore) + 2*sd(train$UDogScore), col = 'green', lty = 2)
abline(v = mean(train$UDogScore) - 2*sd(train$UDogScore), col = 'green', lty = 2)

##how are the lines distributed?
summary(dat$Spread) #biggest is -22, avg is -5.6 and goes to 0. 75th percentile is -3
summary(dat$OULine) #smallest line is 35, mean is 46.61 and max is 63.5


#checking in on who wins 
mean(dat$Winner == 'Favorite') #Favorite wins 64.98% of the time
summary(dat[dat$Winner == 'Favorite',]$Spread) #Average spread when the favorite wins is -6.29
mean(dat$Winner == 'Underdog') #Underdog wins 34.59% of the time
summary(dat[dat$Winner == 'Underdog',]$Spread) #Average spread for upsets is -4.33. Biggest upset is -17.5

#who Covers?
mean(dat$SpreadWin == 'Favorite') #Favorite covers 45.4% of the time
summary(dat[dat$SpreadWin == 'Favorite',]$Spread) #Average spread when the favorite cove-0rs is -5.79, biggest cover is -22
sort(table(dat[dat$SpreadWin == 'Favorite',]$FavDiv) / table(dat$FavDiv)) #AFC south covers the least when favored, AFC East covers the most at 54.09%
sort(table(dat[dat$SpreadWin == 'Underdog',]$FavDiv) / table(dat$FavDiv)) #AFC south covers the least when favored, AFC East covers the most at 54.09%
sort(table(dat[dat$SpreadWin == 'Favorite',]$Favorite) / table(dat$Favorite)) #miami the best at covering when favored. 61%. jags suck at 28.57%


mean(dat$SpreadWin == 'Underdog') #Underdog covers 52.57% of the time
summary(dat[dat$SpreadWin == 'Underdog',]$Spread) #Average spread for upsets is -5.437. 
sort(table(dat[dat$SpreadWin == 'Favorite',]$UDDiv) / table(dat$UDDiv)) 
sort(table(dat[dat$SpreadWin == 'Underdog',]$UDDiv) / table(dat$UDDiv)) #NFC west covers 61% of the time when they are underdog. AFC east only 47.65%
sort(table(dat[dat$SpreadWin == 'Underdog',]$Underdog) / table(dat$Underdog)) #14 teams cover over 55% of the time when they are underdogs. crazy

#covers when wins?
mean(dat[dat$Winner == 'Favorite',]$SpreadWin == 'Underdog') #27% of the time when the favorite wins, the underdog covers
mean(dat[dat$Winner == 'Favorite',]$SpreadWin == 'Favorite') #69.82%% of the time when the favorite wins, the favorite covers
mean(dat[dat$Winner == 'Favorite',]$SpreadWin == 'Push') #3.11%% of the time when the favorite wins, the push covers

ggplot(data = dat) + geom_bar(mapping = aes(x = SpreadWin))
ggplot(data = dat) + geom_bar(mapping = aes(x = Winner, fill = SpreadWin))


mean(train$UDogScore)
sd(train$UDogScore)
mean((train$FavScore + train$UDogScore) > mean(train$FavScore + train$UDogScore) + 2*sd(train$FavScore + train$UDogScore)) +
  mean((train$FavScore + train$UDogScore) < mean(train$FavScore + train$UDogScore) - 2*sd(train$FavScore + train$UDogScore))



#does home field advantage mean anything?
ggplot(data = dat) + geom_bar(mapping = aes(x = Winner, fill = Where))
ggplot(data = dat) + geom_bar(mapping = aes(x = SpreadWin, fill = Where))





#Outliers to explore removing: check if medians and means move closer together. stuff like that. normalizing affects to the distributions
(1.5 * IQR(dat$Total)) + quantile(dat$Total, .75) #games with totals over 84.5. 11 games
quantile(dat$Total, .25) - (1.5 * IQR(dat$Total))  #games with less than 8.5. 1 game

(1.5 * IQR(dat$FavScore)) + quantile(dat$FavScore, .75) #games where favorite scores 52.5+. 5 games
quantile(dat$FavScore, .25) - (1.5 * IQR(dat$FavScore))  #favorites get blanked. 5 games

1.5 * IQR(dat$UDogScore) + quantile(dat$UDogScore, .75) #gunderdog scores 46.5+. 8 games
quantile(dat$UDogScore, .25) - (1.5 * IQR(dat$UDogScore))  #Nothing to drop here

1.5 * IQR(dat$Differential) + quantile(dat$Differential, .75) #games with diffs over 39.5. 10 games
quantile(dat$Differential, .25) - (1.5 * IQR(dat$Differential))  #games with diffs of -28.5 or less. 13 games

sum(dat$Differential < -28)
dat[dat$Differential < -28, c('Date', 'Favorite', 'Underdog', 'FavScore', 'UDogScore', 'Spread')]
     