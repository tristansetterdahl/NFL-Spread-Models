#script for data exploration
library(tidyverse)
library(ggplot2)
library(magrittr)
library(reshape2)
library(glmnet)

set.seed(1738)

#loading in data
dat18 <- read_csv("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Full/all_data_2018.csv")
dat19 <- read_csv("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Full/all_data_2019.csv")
dat20 <- read_csv("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Full/all_data_2020.csv")
dat21 <- read_csv("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Full/all_data_2021.csv")
dat <- rbind(dat18, dat19, dat20, dat21) %>% drop_na
dat <- dat[-1]
train_idxs <- sample(1:nrow(dat), .75*nrow(dat), replace = FALSE)
train <- dat[train_idxs, ]
test <- dat[-train_idxs,]
train %>% head
#data without outliers
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
abline(v = mean(train$Differential), col = 'red', lty = 2) + 
abline(v = median(train$Differential), col = 'orange', lty = 2)
abline(v = mean(train$Differential) + sd(train$Differential), col = 'blue', lty = 2)
abline(v = mean(train$Differential) - sd(train$Differential), col = 'blue', lty = 2)
abline(v = mean(train$Differential) + 2*sd(train$Differential), col = 'green', lty = 2)
abline(v = mean(train$Differential) - 2*sd(train$Differential), col = 'green', lty = 2)


#Exploring Total
mean(train$Total) #46.5
median(train$Total) #46
sd(train$Total)  #14.096
hist(train$Total) #this looks very normal(46.5, 14.1**2), maybe SLR would be good here
abline(v = mean(train$Total), col = 'red', lty = 2) + 
abline(v = median(train$Total), col = 'orange', lty = 2)
abline(v = mean(train$Total) + sd(train$Total), col = 'blue', lty = 2)
abline(v = mean(train$Total) - sd(train$Total), col = 'blue', lty = 2)
abline(v = mean(train$Total) + 2*sd(train$Total), col = 'green', lty = 2)
abline(v = mean(train$Total) - 2*sd(train$Total), col = 'green', lty = 2)

train[train$Where == 'Home',]$Total %>% summary #(37, 47.27, 57)
train[train$Where == 'Away',]$Total %>% summary #(36, 45, 53.25)
train[train$Where == 'Neutral',]$Total %>% summary #(35.25, 41.1, 44.5) #Where the game is played really only afffects total score on neutral fields.

train[train$FavDiv == 'NFC East' & train$UDDiv == 'NFC East',]$Total 

mean(train$OverUnder == 'O') #46.7%
mean(train$OverUnder == 'U') #Under hits 51% of the time 
train$Total %>% hist 
abline(v = mean(train$OULine))

#Looking at distribution for favscore
mean(train$FavScore)
median(train$FavScore)
sd(train$FavScore)
hist(train$FavScore, breaks = 15) #this looks kinda normal(25.8247, 9.80**2)
abline(v = mean(train$FavScore), col = 'red', lty = 2)
abline(v = mean(train$FavScore) + sd(train$FavScore), col = 'blue', lty = 2)
abline(v = mean(train$FavScore) - sd(train$FavScore), col = 'blue', lty = 2)
abline(v = mean(train$FavScore) + 2*sd(train$FavScore), col = 'green', lty = 2)
abline(v = mean(train$FavScore) - 2*sd(train$FavScore), col = 'green', lty = 2)

#Looking at distribution for udogcore
mean(train$UDogScore)
median(train$UDogScore)
sd(train$UDogScore)
hist(train$UDogScore, breaks = 9) #this looks not very normal(20.68, 9.91**2)
abline(v = mean(train$UDogScore), col = 'red', lty = 2)
abline(v = mean(train$UDogScore) + sd(train$UDogScore), col = 'blue', lty = 2)
abline(v = mean(train$UDogScore) - sd(train$UDogScore), col = 'blue', lty = 2)
abline(v = mean(train$UDogScore) + 2*sd(train$UDogScore), col = 'green', lty = 2)
abline(v = mean(train$UDogScore) - 2*sd(train$UDogScore), col = 'green', lty = 2)

##how are the lines distributed?
summary(train$Spread) #biggest is -22, avg is -5.64 and goes to 0. 75th percentile is -3
summary(train$OULine) #smallest line is 35, mean is 46.59 and max is 63.5


#checking in on who wins 
mean(train$Winner == 'Favorite') #Favorite wins 64.67% of the time
summary(train[train$Winner == 'Favorite',]$Spread) #Average spread when the favorite wins is -6.249
mean(train$Winner == 'Underdog') #Underdog wins 35.19% of the time
summary(train[train$Winner == 'Underdog',]$Spread) #Average spread for upsets is -4.53. Biggest upset is -17.5

#who Covers?
mean(train$SpreadWin == 'Favorite') #Favorite covers 46.61% of the time
summary(train[train$SpreadWin == 'Favorite',]$Spread) #Average spread when the favorite covers is -5.85, biggest cover is -22
sort(table(train[train$SpreadWin == 'Favorite',]$FavDiv) / table(train$FavDiv)) #AFC south covers the least when favored, AFC East covers the most at 54.09%
sort(table(train[train$SpreadWin == 'Underdog',]$FavDiv) / table(train$FavDiv)) #AFC south covers the least when favored, AFC East covers the most at 54.09%
sort(table(train[train$SpreadWin == 'Favorite',]$Favorite) / table(train$Favorite)) #miami the best at covering when favored. 61%. jags suck at 28.57%


mean(train$SpreadWin == 'Underdog') #Underdog covers 51.39% of the time
summary(train[train$SpreadWin == 'Underdog',]$Spread) #Average spread for upsets is -5.465 
sort(table(train[train$SpreadWin == 'Favorite',]$UDDiv) / table(train$UDDiv)) 
sort(table(train[train$SpreadWin == 'Underdog',]$UDDiv) / table(train$UDDiv)) #NFC west covers 61% of the time when they are underdog. AFC east only 47.65%
sort(table(train[train$SpreadWin == 'Underdog',]$Underdog) / table(train$Underdog)) #14 teams cover over 55% of the time when they are underdogs. crazy

#covers when wins?
mean(train[train$Winner == 'Favorite',]$SpreadWin == 'Underdog') #27% of the time when the favorite wins, the underdog covers
mean(train[train$Winner == 'Favorite',]$SpreadWin == 'Favorite') #69.82%% of the time when the favorite wins, the favorite covers
mean(train[train$Winner == 'Favorite',]$SpreadWin == 'Push') #3.11%% of the time when the favorite wins, the push covers

ggplot(traina = train) + geom_bar(mapping = aes(x = SpreadWin))
ggplot(traina = train) + geom_bar(mapping = aes(x = Winner, fill = SpreadWin))


mean(train$UDogScore)
sd(train$UDogScore)
mean((train$FavScore + train$UDogScore) > mean(train$FavScore + train$UDogScore) + 2*sd(train$FavScore + train$UDogScore)) +
  mean((train$FavScore + train$UDogScore) < mean(train$FavScore + train$UDogScore) - 2*sd(train$FavScore + train$UDogScore))



#does home field advantage mean anything?
ggplot(data = train) + geom_bar(mapping = aes(x = Winner, fill = Where))
table(train[train$Where == 'Home',]$Winner)
table(train[train$Where == 'Away',]$Winner)

ggplot(data = train) + geom_bar(mapping = aes(x = SpreadWin, fill = Where), freq = TRUE)
table(train[train$Where == 'Home',]$SpreadWin)
table(train[train$Where == 'Away',]$SpreadWin)

ggplot(data = train) + geom_bar(mapping = aes(x = Where, fill = SpreadWin))
table(train[train$SpreadWin == 'Favorite',]$Where)
table(train[train$SpreadWin == 'Underdog',]$Where)

ggplot(data = train) + geom_bar(mapping = aes(x = Where, fill = Winner))
table(train[train$Winner == 'Favorite',]$Where)
table(train[train$Winner == 'Underdog',]$Where)

#looking at overunder
barplot(train$OverUnder %>% table) 
table(train$OverUnder) #Under hits a little bit more than the over does.
mean(train$Total >= train$OULine + 10) #22.05% of games cover the over line by 10 or more
mean(train$Total <= train$OULine - 10) #24.04% of games fall 10 or more short of the line
mean(train$Total == train$OULine) #1.5% of games push the line, 11 games

#Outliers to explore removing: check if medians and means move closer together. stuff like that. normalizing effects to the distributions
(1.5 * IQR(train$Total)) + quantile(train$Total, .75) #games with totals over 84.5. 11 games
quantile(train$Total, .25) - (1.5 * IQR(train$Total))  #games with less than 8.5. 1 game

(1.5 * IQR(train$FavScore)) + quantile(train$FavScore, .75) #games where favorite scores 50
quantile(train$FavScore, .25) - (1.5 * IQR(train$FavScore))  #favorites get blanked. 5 games

1.5 * IQR(train$UDogScore) + quantile(train$UDogScore, .75) #gunderdog scores 48+. 8 games
quantile(train$UDogScore, .25) - (1.5 * IQR(train$UDogScore))  #Nothing to drop here

1.5 * IQR(train$Differential) + quantile(train$Differential, .75) #games with diffs over 39.5. 10 games
quantile(train$Differential, .25) - (1.5 * IQR(train$Differential))  #games with diffs of -28.5 or bigger 8 games

sum(train$Differential < -28)
train[train$Differential < -28, c('Date', 'Favorite', 'Underdog', 'FavScore', 'UDogScore', 'Spread')]

#also will probably remove ties and pushes
sum(train$SpreadWin == 'Push') #15 pushes
sum(train$Winner == 'Tie') #1 ties
sum(train$OverUnder == 'P') #11 Pushes


train %>% colnames
out_idxs <- (train$SpreadWin == 'Push' | train$Winner == 'Tie'| train$OverUnder == 'P') #|
               #train$Total > 84.5 | train$Total < 8.5 | train$FavScore > 50 | train$FavScore < .5 | 
               #train$UDogScore > 48 | train$Differential > 39.5 | train$Differential < -28.5) 

out_idxs %>% mean 
ggplot(train, aes(x = UD.PF, y = Fav.PF)) + geom_point(aes(color = Winner)) + geom_abline(slope = 1)
mean((train$Fav.PF < train$UD.PF), na.rm = TRUE)
mean((train$Fav.PF > train$UD.PF) & train$Winner == 'Favorite') #51% of games where favorites score more than underdog and win game
mean((train$Fav.PF < train$UD.PF) & train$Winner == 'Underdog')
plot(x = train$Fav.PF, y = train$FavScore)
plot(x = train$UD.PF, y = train$UDogScore)
ggplot(train, aes(x = UD.PF, y = Fav.PF)) + geom_point(aes(color = Total, shape = OverUnder)) 

totmod <- lm(Total~Fav.PF+UD.PF, train)
totmod %>% plot

favmod <- lm(FavScore~Fav.PF+UD.PA, train)
favmod %>% plot

udmod <- lm(UDogScore~UD.PF+Fav.PA, train)
udmod %>% summary


#logistic models
train <- train[-out_idxs,] #removing ties and pushes for purely binary responses
logwin <- glm(as.factor(Winner) ~ Fav.PF + Fav.PA + UD.PF + UD.PA, family = 'binomial', train)
logwin  %>% summary #doesn't look great. null deviance: 977.18, AIC 970.64. Only UD.PF and UD.PA significant

logOUmod <- glm(as.factor(OverUnder) ~ Fav.PF + Fav.PA + UD.PF + UD.PA, family = 'binomial', train)
logOUmod %>% summary #AIC 1035

logspreadmod <- glm(as.factor(SpreadWin) ~ Fav.PF + Fav.PA + UD.PF + UD.PA, family = 'binomial', train)
logspreadmod %>% summary
