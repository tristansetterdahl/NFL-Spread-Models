library(ranger)
library(glmnet)
library(ggplot2)
library(tree)
library(randomForest)

###reading in the data###
train <- read_csv('all_data_2020.csv') %>% drop_na() #shouldnt be NA, why are there?
test <- read_csv('all_data_2021.csv')

missRanger(train) %>% head
apply(is.na(train), 2, which) 
train$Fav.KRAvg[c(1, 8, 16, 20)]
train$Fav.PRAvg[c(5, 14, 33)]
train$Fav.PuntAvg[5]
train$UD.FGPct[9]
train$UD.XPPct[6]
train$UD.KRAvg[c(2, 3, 6, 9, 12, 15, 16, 17)]
train$UD.PRAvg[c(5, 7, 8, 11, 21, 26, 39)]
test %>% is.na %>% which

?ranger

train$Spread <- abs(train$Spread)
test$Spread <- abs(test$Spread)

#character values to factors
team_levels <- train$Underdog %>% as.factor %>% levels
div_levels <- train$FavDiv %>% as.factor %>% levels

train$Where %<>% as.factor
train$Favorite %<>% factor(levels = team_levels)
train$FavDiv %<>% factor(levels = div_levels)
train$Underdog %<>% factor(levels = team_levels)
train$UDDiv %<>% factor(levels = div_levels)
train$SpreadWin %<>% as.factor
train$OverUnder %<>% as.factor
train$OT %<>% as.factor


test$Where %<>% as.factor
test$Favorite %<>% factor(levels = team_levels)
test$FavDiv %<>% factor(levels = div_levels)
test$Underdog %<>% factor(levels = team_levels)
test$UDDiv %<>% factor(levels = div_levels)
test$SpreadWin %<>% as.factor
test$OverUnder %<>% as.factor
test$OT %<>% as.factor


train$UD.PRAvg %>% is.na %>% which
test %>% is.na %>% sum

train %>% str
test %>% nrow


###lets explore differential##

hist(train$Differential)

trainrf$SpreadWinTeam %>% levels
trainrf$Underdog %>% levels()
div_levels <- trainrf$FavDiv %>% levels

#random forest for classification
trainrf_class <- train %>% select(-c('Time (ET)', 'WinTeam', 'SpreadWinTeam', 'Winner', 'OverUnder', 
                               'Differential', 'OT', 'UDogScore', 'FavScore'))


testrf_class <- test %>% select(-c('Time (ET)', 'WinTeam', 'SpreadWinTeam', 'Winner', 'OverUnder', 
                             'Differential', 'OT', 'UDogScore', 'FavScore'))

trainrf_class %>% str

rfmod_class <- ranger(SpreadWin ~., data = trainrf_class, num.trees = 50000, mtry = 7)
rfmod_class

rf_preds <- predict(rfmod_class, testrf_class)
rf_preds$predictions[,1] %>% hist
rf_preds$predictions %>% summary
testrf_class$SpreadWin %>% summary
sum(rf_preds$predictions == testrf_class$SpreadWin) / length(testrf_class$SpreadWin)


#random forest for difference:
trainrf_diff <- train %>% select(-c('Time (ET)', 'WinTeam', 'SpreadWinTeam', 'Winner', 'OverUnder', 
                                    'SpreadWin', 'OT', 'UDogScore', 'FavScore'))

testrf_diff <- test %>% select(-c('Time (ET)', 'WinTeam', 'SpreadWinTeam', 'Winner', 'OverUnder', 
                                   'SpreadWin', 'OT', 'UDogScore', 'FavScore'))


rfmod_diff <- ranger(Differential~., data = trainrf_diff)
rfmod_diff

rf_diff_preds <- predict(rfmod_diff, testrf_diff)
rf_diff_preds$predictions %>% summary
testrf_diff$Differential %>% summary

(rf_diff_preds$predictions <= 0) %>% sum
(testrf_diff$Differential <= 0) %>% sum


##do these actually predict the same things?
sum((rf_preds$predictions == 'Favorite') == (rf_diff_preds$predictions > 0)) ##only the same 32/285 times. not great
#now lets compare to real stuff
mean((test$SpreadWin == 'Favorite') == (rf_preds$predictions == 'Favorite')) #correct 139/285
mean((test$SpreadWin == 'Favorite') == (rf_diff_preds$predictions > 0)) #correct 136. worse than spreadwin directly

#let's see if we can model individual scores: starting with predicting favorite
train_rf_favscore <- train %>% select(-c('Time (ET)', 'WinTeam', 'SpreadWinTeam', 'Winner', 'OverUnder', 
                                                         'SpreadWin', 'OT', 'UDogScore', 'Differential'))
test_rf_favscore <- test %>% select(-c('Time (ET)', 'WinTeam', 'SpreadWinTeam', 'Winner', 'OverUnder', 
                                        'SpreadWin', 'OT', 'UDogScore', 'Differential'))
favscore_rfmod <- ranger(FavScore ~., data = train_rf_favscore, num.trees = 500, mtry = 7)
favscore_preds <- predict(favscore_rfmod, test_rf_favscore)
favscore_preds$predictions %>% summary
test$FavScore %>% summary


train_rf_udscore <- train %>% select(-c('Time (ET)', 'WinTeam', 'SpreadWinTeam', 'Winner', 'OverUnder', 
                                         'SpreadWin', 'OT', 'FavScore', 'Differential'))
test_rf_udscore <- test %>% select(-c('Time (ET)', 'WinTeam', 'SpreadWinTeam', 'Winner', 'OverUnder', 
                                        'SpreadWin', 'OT', 'FavScore', 'Differential'))
udscore_rfmod <- ranger(UDogScore ~., data = train_rf_udscore, num.trees = 500, mtry = 7)
udscore_preds <- predict(udscore_rfmod, test_rf_udscore)
udscore_preds$predictions %>% summary
test$UDogScore %>% summary

#this gets us 152 right. 150 is magic number. I think this is the best way so far
mean(((favscore_preds$predictions - udscore_preds$predictions) > test$Spread) == (test$SpreadWin == 'Favorite'))




#Can we predict over with any reasonability?
train_rf_ou <- train %>% select(-c('Time (ET)', 'WinTeam', 'SpreadWinTeam', 'Winner', 'UDogScore', 
                                        'SpreadWin', 'OT', 'FavScore', 'Differential'))
test_rf_ou <- test %>% select(-c('Time (ET)', 'WinTeam', 'SpreadWinTeam', 'Winner', 'UDogScore', 
                                   'SpreadWin', 'OT', 'FavScore', 'Differential'))

rf_ou <- ranger(OverUnder~., data = train_rf_ou, mtry = 8) #8
rf_ou
rf_ou_preds <- predict(rf_ou, test_rf_ou)
(rf_ou_preds$predictions == test$OverUnder) %>% mean ##162 right, pretty good
(((favscore_preds$predictions + udscore_preds$predictions) >= test$OULine) == (test$OverUnder == 'O')) %>% mean #146, not as good. better with classification

#manages to work better for betting unders. interesting, why is that?
(((rf_ou_preds$predictions == 'U') %>% which) %in% ((test$OverUnder == 'U') %>% which)) %>% mean
(((rf_ou_preds$predictions == 'O') %>% which) %in% ((test$OverUnder == 'O') %>% which)) %>% mean

rf_ou_probs <- ranger(OverUnder~., data = train_rf_ou, probability = TRUE, mtry = 7)
rf_ou_probs_preds <- predict(rf_ou_probs, test_rf_ou)
(rf_ou_probs_preds$predictions %>% head)
rf_ou_preds$predictions %>% head
test$OverUnder %>% head

###trying to combine OU and spreadwin classifier. can we hedge to any benefit?
#both spreadwin correct and ou correct
mean(((favscore_preds$predictions - udscore_preds$predictions) > test$Spread) == (test$SpreadWin == 'Favorite') & (rf_ou_preds$predictions == test$OverUnder)) #86 total where both are right
#now, one or the other. hedged cases
mean(((favscore_preds$predictions - udscore_preds$predictions) > test$Spread) == (test$SpreadWin == 'Favorite') | (rf_ou_preds$predictions == test$OverUnder)) #225
#spread is right, ou is wrong
mean(((favscore_preds$predictions - udscore_preds$predictions) > test$Spread) == (test$SpreadWin == 'Favorite') & (rf_ou_preds$predictions != test$OverUnder)) #66 
#spread wrong, ou is right
mean(((favscore_preds$predictions - udscore_preds$predictions) > test$Spread) != (test$SpreadWin == 'Favorite') & (rf_ou_preds$predictions == test$OverUnder)) #73 total where both are right

#neither hits
mean(((favscore_preds$predictions - udscore_preds$predictions) > test$Spread) != (test$SpreadWin == 'Favorite') & (rf_ou_preds$predictions != test$OverUnder)) #60 
#####Linear model, should be lots of problems
lmod <- lm(Differential~., data = train) 
lmod %>% plot #super weird plots

lmod %>% summary #Very overfit



####decision trees for everythin
