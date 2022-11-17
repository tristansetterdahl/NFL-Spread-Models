library(tidyverse)
library(rvest)
library(magrittr)
library(stringr)
library(lubridate)

####Reading in data from 2021-2022 Season for training data###
#Starting with the betting data

#function to transform full team names to abbreviations

nfl_abb21 <- function(team_col){
  abbs <- c('ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL', 'DEN', 'DET', 'GB', 'HOU',
            'IND', 'JAC', 'KC', 'LV', 'LAC', 'LA', 'MIA', 'MIN', 'NE', 'NO', 'NYG', 'NYJ', 'PHI',
            'PIT', 'SF', 'SEA', 'TB', 'TEN', 'WAS')
  
  #year specific because of Washington's name change
  team_names21 <- c('Arizona Cardinals', 'Atlanta Falcons', 'Baltimore Ravens', 'Buffalo Bills', 'Carolina Panthers',
                    'Chicago Bears', 'Cincinnati Bengals', 'Cleveland Browns', 'Dallas Cowboys',
                    'Denver Broncos', 'Detroit Lions', 'Green Bay Packers', 'Houston Texans',
                    'Indianapolis Colts', 'Jacksonville Jaguars', 'Kansas City Chiefs', 'Las Vegas Raiders',
                    'Los Angeles Chargers', 'Los Angeles Rams', 'Miami Dolphins', 'Minnesota Vikings',
                    'New England Patriots', 'New Orleans Saints', 'New York Giants', 'New York Jets',
                    'Philadelphia Eagles', 'Pittsburgh Steelers', 'San Francisco 49ers', 'Seattle Seahawks',
                    'Tampa Bay Buccaneers', 'Tennessee Titans', 'Washington Football Team')
  
  #team names are reassigned to correct abbreviations based on first 13 characters (LA Teams make this needed) in names 
  #(keeps seeds out of playoff names, shortest team names are 13 characters)
  team_col_abbs <- abbs[match(team_col %>% str_sub(1, 13), team_names21 %>% str_sub(1,13))]
  return(team_col_abbs)
}

#creates columns to track each team's divisions for every game
nfl_divs21 <- function(df){
  
  nfc_north_teams <- c('CHI', 'DET', 'GB', 'MIN')
  nfc_south_teams <- c('ATL', 'TB', 'CAR', 'NO')
  nfc_east_teams <- c('PHI', 'DAL', 'NYG', 'WAS')
  nfc_west_teams <- c('ARI', 'LA', 'SF', 'SEA')
  
  afc_north_teams <- c('BAL', 'CIN', 'CLE', 'PIT')
  afc_south_teams <- c('IND', 'JAC', 'TEN', 'HOU')
  afc_east_teams <- c('BUF', 'MIA', 'NE', 'NYJ')
  afc_west_teams <- c('DEN', 'KC', 'LV', 'LAC')
  
  df %<>% mutate(FavDiv = case_when(
    Favorite %in% nfc_east_teams ~ 'NFC East',
    Favorite %in% nfc_west_teams ~ 'NFC West',
    Favorite %in% nfc_north_teams ~ 'NFC North',
    Favorite %in% nfc_south_teams ~ 'NFC South',
    Favorite %in% afc_east_teams ~ 'AFC East',
    Favorite %in% afc_west_teams ~ 'AFC West',
    Favorite %in% afc_north_teams ~ 'AFC North',
    Favorite %in% afc_south_teams ~ 'AFC South'
  ))
  
  #df$FavDiv %<>% as.factor()
  
  df %<>% mutate(UDDiv = case_when(
    Underdog %in% nfc_east_teams ~ 'NFC East',
    Underdog %in% nfc_west_teams ~ 'NFC West',
    Underdog %in% nfc_north_teams ~ 'NFC North',
    Underdog %in% nfc_south_teams ~ 'NFC South',
    Underdog %in% afc_east_teams ~ 'AFC East',
    Underdog %in% afc_west_teams ~ 'AFC West',
    Underdog %in% afc_north_teams ~ 'AFC North',
    Underdog %in% afc_south_teams ~ 'AFC South'
  ))
  
  #df$UDDive %<>% as.factor()
  
}



training_bets <- function(){
  #tables for weekly schedule 2021 on this website
  betting_2021_dat <- read_html('https://www.sportsoddshistory.com/nfl-game-season/?y=2021#1')
  betting_2021 <- betting_2021_dat %>% html_nodes('table')
  
  #scrapes tables week by week until the regular season is in one cohesive dataframe
  week = 1
  spread_dat_2021 <- tibble()
  for(i in 7:24){
    assign(paste0('betting_2021_wk', week), (betting_2021[[i]] %>% html_table())[1:19, 1:10] %>% as.data.frame() %>% drop_na())
    
    
    spread_dat_2021 <- rbind(spread_dat_2021, get(paste0('betting_2021_wk', week)))
    week = week + 1
  }
  
  #adding postseason to the dataframe
  post_21 <- (betting_2021[[25]] %>% html_table())[1:13, 1:11] %>% as.data.frame() #%>% drop_na()
  post_21 <- post_21[,-1]
  colnames(post_21) <- colnames(spread_dat_2021)
  
  spread_dat_2021 <- rbind(spread_dat_2021, post_21)
  
  return(spread_dat_2021)
}

#function to clean score data. takes the data generated in training_bets()
clean_scores <- function(dat){
  #eliminates day column and second location column
  dat <- dat[-c(1, 8)]
  
  #names blank colname
  colnames(dat)[colnames(dat) == ''] = 'Where'
  
  #more readable, relative to the favorite
  dat$Where[dat$Where == '@'] = 'Home'
  dat$Where[dat$Where == ''] = 'Away'
  dat$Where[dat$Where == 'N'] = 'LDN'
  
  dat$Where %<>% as.factor()
  
  #lubridate date & time
  dat$Date = mdy(dat$Date)
  
  #abbreviates team names
  #dat$Favorite %<>% nfl_abb20()
  #dat$Underdog %<>% nfl_abb20()
  
  #Columns with divisions
  #dat %<>% nfl_divs20()
  
  #New column with winner of spread
  dat %<>% mutate(SpreadWin = case_when(
    startsWith(Spread, 'W') ~ 'Favorite',
    startsWith(Spread, 'L') ~ 'Underdog',
    startsWith(Spread, 'P') ~ 'Push'
  ))
  
  #dat$SpreadWin %<>% as.factor()
  
  
  #New column with team who won spread
  dat %<>% mutate(SpreadWinTeam = case_when(
    SpreadWin == 'Favorite' ~ Favorite,
    SpreadWin == 'Underdog' ~ Underdog,
    SpreadWin == 'Push' ~ 'Push'
  ))
  
  #dat$SpreadWinTeam %<>% as.factor()
  
  dat$Spread <- str_sub(dat$Spread, 3, -1)
  dat$Spread[which(dat$Spread == 'PK')] <- 0
  dat$Spread %<>% as.numeric()
  
  #New column with Game winner
  dat %<>% mutate(Winner = case_when(
    startsWith(Score, 'W') ~ 'Favorite',
    startsWith(Score, 'L') ~ 'Underdog',
    startsWith(Score, 'T') ~ 'Tie'
  ))
  #dat$Winner %<>% as.factor()
  
  
  #New Column with game winning team
  dat %<>% mutate(WinTeam = case_when(
    Winner == 'Favorite' ~ Favorite,
    Winner == 'Underdog' ~ Underdog,
    Winner == 'Tie' ~ 'Tie'
  ))
  
  
  
  #dat$WinTeam %<>% as.factor()
  #dat$Favorite %<>% as.factor()
  #dat$Underdog %<>% as.factor()
  
  #New column for OT
  dat %<>% mutate(OT = case_when(
    endsWith(Score, '(OT)') ~ 'Yes',
    TRUE ~ 'No'
  ))
  dat$OT %<>% as.factor()
  
  #reformat scores, deleting win/loss and OT markers
  dat$Score[str_sub(dat$Score, -4, -1) == '(OT)'] <- dat$Score[str_sub(dat$Score, -4, -1) == '(OT)'] %>% 
    str_sub(1, -6)
  dat$Score <- str_sub(dat$Score, 3, -1)
  
  #splitting scores into two different columns. also adding a score differential column
  dat %<>% separate(Score, c('FavScore', 'UDogScore'), sep = '-')
  dat$FavScore %<>% as.numeric()
  dat$UDogScore %<>% as.numeric()
  dat %<>% mutate(Differential = FavScore - UDogScore)
  
  #Cleaning over/under data
  dat %<>% separate(`Over/Under`, c('OverUnder', 'OULine'), sep = ' ')
  dat$OULine %<>% as.numeric()
  #dat$OverUnder %<>% as.factor()
  
  return(dat)
}


games_2021 %>% head


###putting it all together
#need to get this all into one call, quite the puzzle at the moment
games_2021 <- training_bets()
games_2021$Favorite <- nfl_abb21(games_2021$Favorite)
games_2021$Underdog <- nfl_abb21(games_2021$Underdog)
games_2021.2 <- clean_scores(games_2021)
games_2021.2 <- nfl_divs20(games_2021.2)

##converting chr to factor cause didnt like when I tried inside the function. integrate this!
games_2021.2$Favorite %<>% as.factor()
games_2021.2$Underdog %<>% as.factor()
games_2021.2$OverUnder %<>% as.factor()
games_2021.2$SpreadWin %<>% as.factor()
games_2021.2$SpreadWinTeam %<>% as.factor()
games_2021.2$Winner %<>% as.factor()
games_2021.2$WinTeam %<>% as.factor()
games_2021.2$FavDiv %<>% as.factor()
games_2021.2$UDDiv %<>% as.factor()

games_2021 <- select(games_2021.2, c(Date, `Time (ET)`, Where, Favorite, FavDiv, Underdog, UDDiv, Spread, OULine,
                                     FavScore, UDogScore, OT, Differential, OverUnder, SpreadWin, Winner, SpreadWinTeam, WinTeam))
write.csv(games_2021, "2021_nfl_spreads.csv", row.names = FALSE)

getwd()
games_2021.2 %>% head
games_2021 %>% head
games_2021.2$Winner[24]

nfc_east_ud <- games_2021.2 %>% filter(UDDiv == 'NFC East')


games_2021 %>% tail

###SUPER GOOD VISUAL, KEEP DOING THIS
ggplot(games_2021.2, mapping = aes(x = FavDiv, fill = SpreadWin)) + geom_bar(position = 'fill')

summary(games_2021.2)





