library(tidyverse)
library(rvest)
library(magrittr)
library(stringr)
library(lubridate)


#function to transform full team names to abbreviations
nfl_abb <- function(team_col, year){
  #oakland
  abbs18 <- c('ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL', 'DEN', 'DET', 'GB', 'HOU',
            'IND', 'JAC', 'KC', 'LAC', 'LA', 'MIA', 'MIN', 'NE', 'NO', 'NYG', 'NYJ', 'OAK', 'PHI',
            'PIT', 'SF', 'SEA', 'TB', 'TEN', 'WAS')
  #LVR
  abbs20 <- c('ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL', 'DEN', 'DET', 'GB', 'HOU',
            'IND', 'JAC', 'KC', 'LV', 'LAC', 'LA', 'MIA', 'MIN', 'NE', 'NO', 'NYG', 'NYJ', 'PHI',
            'PIT', 'SF', 'SEA', 'TB', 'TEN', 'WAS')
  
  #year specific because of name changes
  #Redskins and oakland
  team_names18 <- c('Arizona Cardinals', 'Atlanta Falcons', 'Baltimore Ravens', 'Buffalo Bills', 'Carolina Panthers',
                    'Chicago Bears', 'Cincinnati Bengals', 'Cleveland Browns', 'Dallas Cowboys',
                    'Denver Broncos', 'Detroit Lions', 'Green Bay Packers', 'Houston Texans',
                    'Indianapolis Colts', 'Jacksonville Jaguars', 'Kansas City Chiefs',
                    'Los Angeles Chargers', 'Los Angeles Rams', 'Miami Dolphins', 'Minnesota Vikings',
                    'New England Patriots', 'New Orleans Saints', 'New York Giants', 'New York Jets', 
                    'Oakland Raiders', 'Philadelphia Eagles', 'Pittsburgh Steelers', 'San Francisco 49ers', 'Seattle Seahawks',
                    'Tampa Bay Buccaneers', 'Tennessee Titans', 'Washington Redskins')
  #WFT, LV Raiders
  team_names20 <- c('Arizona Cardinals', 'Atlanta Falcons', 'Baltimore Ravens', 'Buffalo Bills', 'Carolina Panthers',
                    'Chicago Bears', 'Cincinnati Bengals', 'Cleveland Browns', 'Dallas Cowboys',
                    'Denver Broncos', 'Detroit Lions', 'Green Bay Packers', 'Houston Texans',
                    'Indianapolis Colts', 'Jacksonville Jaguars', 'Kansas City Chiefs', 'Las Vegas Raiders',
                    'Los Angeles Chargers', 'Los Angeles Rams', 'Miami Dolphins', 'Minnesota Vikings',
                    'New England Patriots', 'New Orleans Saints', 'New York Giants', 'New York Jets',
                    'Philadelphia Eagles', 'Pittsburgh Steelers', 'San Francisco 49ers', 'Seattle Seahawks',
                    'Tampa Bay Buccaneers', 'Tennessee Titans', 'Washington Football Team')
 
  
  if(year < 2020){
    abbs <- abbs18
    team_names <- team_names18
  }
  else{
    abbs <- abbs20
    team_names <- team_names20
  }
    
  #team names are reassigned to correct abbreviations based on first 13 characters (LA Teams make this needed) in names 
  #(keeps seeds out of playoff names, shortest team names are 13 characters)
  team_col_abbs <- abbs[match(team_col %>% str_sub(1, 13), team_names %>% str_sub(1,13))]
  return(team_col_abbs)
}

#creates columns to track each team's divisions for every game
nfl_divs <- function(df, year){
  
  nfc_north_teams <- c('CHI', 'DET', 'GB', 'MIN')
  nfc_south_teams <- c('ATL', 'TB', 'CAR', 'NO')
  nfc_east_teams <- c('PHI', 'DAL', 'NYG', 'WAS')
  nfc_west_teams <- c('ARI', 'LA', 'SF', 'SEA')
  
  afc_north_teams <- c('BAL', 'CIN', 'CLE', 'PIT')
  afc_south_teams <- c('IND', 'JAC', 'TEN', 'HOU')
  afc_east_teams <- c('BUF', 'MIA', 'NE', 'NYJ')
  if(year < 2020){
    afc_west_teams <- c('DEN', 'KC', 'OAK', 'LAC')
  }
  else{
    afc_west_teams <- c('DEN', 'KC', 'LV', 'LAC')
  }
  
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


#scrapes spread data, takes the season as argument. (eg, for 2019-2020 season pass 2019)
training_bets <- function(year){
  
  url <- paste0('https://www.sportsoddshistory.com/nfl-game-season/?y=', year, '#1')
  betting_dat <- read_html(url)
  betting <- betting_dat %>% html_nodes('table')
  
  #scrapes tables week by week until the regular season is in one cohesive dataframe
  week = 1
  spread_dat <- tibble()
  if(year < 2021){
    tabs <- 23
  }
  else{
    tabs <- 24
  }
  for(i in 7:tabs){
    assign(paste0('week', week), (betting[[i]] %>% html_table())[1:19, 1:10] %>% as.data.frame() %>% drop_na())
    
    spread_dat <- rbind(spread_dat, get(paste0('week', week)))
    week = week + 1
  }
  
  #adding postseason to the dataframe
  post <- (betting[[tabs + 1]] %>% html_table())[1:19, 1:11] %>% as.data.frame() 
  post <- post[,-1]
  colnames(post) <- colnames(spread_dat)
  
  spread_dat <- rbind(spread_dat, post)
  
  return(spread_dat)
}

#function to clean score data. takes the data generated in training_bets()
clean_scores <- function(dat){
  #eliminates day column and second location column
  dat <- dat[-c(1, 8)]
  dat %<>% drop_na()
  
  #names blank colname
  colnames(dat)[colnames(dat) == ''] = 'Where'
  
  #more readable, relative to the favorite
  dat$Where[dat$Where == '@'] = 'Home'
  dat$Where[dat$Where == ''] = 'Away'
  dat$Where[dat$Where == 'N'] = 'Neutral'
  
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
  
  #splitting scores into two different columns. also adding a score differential column and totals
  dat %<>% separate(Score, c('FavScore', 'UDogScore'), sep = '-')
  dat$FavScore %<>% as.numeric()
  dat$UDogScore %<>% as.numeric()
  dat %<>% mutate(Differential = FavScore - UDogScore)
  dat %<>% mutate(Total = FavScore + UDogScore)
  
  #Cleaning over/under data
  dat %<>% separate(`Over/Under`, c('OverUnder', 'OULine'), sep = ' ')
  dat$OULine %<>% as.numeric()
  #dat$OverUnder %<>% as.factor()
  
  return(dat)
}


full_spreads <- function(year){
  games <- training_bets(year) #defined line 106
  games$Favorite <- nfl_abb(games$Favorite, year) #defined line 12
  games$Underdog <- nfl_abb(games$Underdog, year)
  games <- clean_scores(games) #defined line 139
  games <- nfl_divs(games, year) #defined line 59
  games <- select(games, c(Date, `Time (ET)`, Where, Favorite, FavDiv, Underdog, UDDiv, Spread, OULine,
                                       FavScore, UDogScore, OT, Differential, Total, OverUnder, SpreadWin, Winner, SpreadWinTeam, WinTeam))
  write.csv(games, paste0(year, "_nfl_spreads.csv"), row.names = FALSE)
  return(games)
}

for(year in 2018:2021){
  setwd("NFL Spread Models/Spreads")
  full_spreads(year) #defined line 263
}

getwd() 

