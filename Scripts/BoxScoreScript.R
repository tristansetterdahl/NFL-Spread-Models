library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(zoo)

#Want to take offensive rushing yards, passing yards (per game and per attempt), rushing and passing points (total and ppg)
#and same for defense. for each team. these numbers are entering the game. so in df, week 2 numbers are from week 1

#function to add years to dates, date col in format (m/dd) and chr, season is numeric, 2020 for 20-21 season
#use atomic vector for the column
with_year <- function(date_col, season){
  for(date in 1:length(date_col)){
    split_date <- str_split(date_col[date], "/")
    month <- as.numeric(split_date[[1]][1])
    if(month > 6){
      year = as.character(season)
    }
    else{
      year = as.character(season + 1)
    }
    date_col[date] <- paste0(split_date[[1]][1], '/', split_date[[1]][2], '/', year)
  }
  date_col <- mdy(date_col)
  return(date_col)
}

#cleans game info to get just the opposing team's abbreviation
clean_game <- function(name_col){
  #fox table has different abbreviations for some teams, need to find all and replace with this.
  bad_abbs <- c('LAR', 'JAX')
  good_abbs <- c('LA', 'JAC')
  new_col <- gsub("[^a-zA-Z]", "", name_col) %>% str_sub(1, -2)
  new_col[new_col %in% bad_abbs] <- good_abbs[new_col[new_col %in% bad_abbs] %>% match(bad_abbs)]
  
  return(new_col)
}

#scrapes data from internet
get_boxes <- function(abb, year){
  idx <- match(abb, abbs)
  abb %<>% tolower()
  skills_ls <- list(c('passing'), c('rushing'), c('defense'), c('kicking'), c('returning'), c('punting'),
                    c('downs'), c('yardage'), c('turnovers'))
  colnames_ls <- list(c('Date', 'Game', 'Comp', 'PassAtt', 'CompPCT', 'PassYds', 'PassYPA', 'PassYPR', 
                        'PassTD', 'OINT', 'OSCK', 'OSCKYDS', 'QBR', 'P1st', 'FUM', 'FUML'),
                      c('Date', 'Game', 'RushAtt', 'RushYds', 'RushYPA', 'RushTD', 'R1st'), 
                      c('Date', 'Game', 'PA', 'Tckl', 'Solo', 'AstTckl', 'DSck', 'DSCKYds', 'INT', 'INTTD',
                        'FF', 'FumRec', 'FumTD', 'PassDef', 'Sfty'),
                      c('Date', 'Game', 'KPts', 'FGM', 'FGA', 'FGPct', 'FGLng', 'XPM', 'XPA', 'XPPct'),
                      c('Date', 'Game', 'KR', 'KRYds', 'KRAvg', 'KRTD', 'KRLng', 'PR', 'PRYds', 'PRAvg',
                        'PRTD', 'PRLng'),
                      c('Date', 'Game', 'Punts', 'PuntYds', 'PuntAvg'), 
                      c('Date', 'Game', '1st', 'R1st', 'P1st', '3rdCon', '3rdAtt', '3rdPct', '4thCon',
                        '4thAtt', '4thPct', 'Pen', 'PenYds'), 
                      c('Date', 'Game', 'TotYds', 'RushYds', 'RushYPA', 'RushTD', 'PassYds', 'PassYPA',
                        'PassYPR', 'PassTD', 'KRYds', 'PRYds', 'PenYds'), 
                      c('Date', 'Game', 'PlusMinus', 'OINT', 'FumRec', 'INT', 'FUML'))
  #make urls
  passing_urls <- make_urls(team_names, 'passing', year)
  rushing_urls <- make_urls(team_names, 'rushing', year)
  defense_urls <- make_urls(team_names, 'defense', year)
  kicking_urls <- make_urls(team_names, 'kicking', year)
  punting_urls <- make_urls(team_names, 'punting', year)
  returning_urls <- make_urls(team_names, 'returning', year)
  downs_urls <- make_urls(team_names, 'downs', year)
  yardage_urls <- make_urls(team_names, 'yardage', year)
  turnovers_urls <- make_urls(team_names, 'turnovers', year)
  
  
  #list to be returned, contains all of the tables for the team
  team_skills_dfs <- list()
  
  #this loop puts the appropriate team's links in the list, preceded by the skill, then uses the link
  #to scrape the data and store it
  for(skill in 1:length(skills_ls)){
    
    skill_urls <- get(paste0(skills_ls[[skill]][1], '_urls'))
    #skill_urls[idx]
    skills_ls[[skill]][2] <- skill_urls[idx]
    
    team_skill_dat <- read_html(skills_ls[[skill]][2]) %>% html_nodes('table') %>% html_table()
    team_skill_dat <- team_skill_dat[[2]]
    colnames(team_skill_dat) <- colnames_ls[[skill]]
    #saving the data frame
    team_skills_dfs[[skill]] <- team_skill_dat
  }
  
  return(team_skills_dfs)
}


abbs <- c('ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL', 'DEN', 'DET', 'GB', 'HOU',
          'IND', 'JAC', 'KC', 'LV', 'LAC', 'LA', 'MIA', 'MIN', 'NE', 'NO', 'NYG', 'NYJ', 'PHI',
          'PIT', 'SF', 'SEA', 'TB', 'TEN', 'WAS')
team_names <- c('Arizona Cardinals', 'Atlanta Falcons', 'Baltimore Ravens', 'Buffalo Bills', 'Carolina Panthers',
                'Chicago Bears', 'Cincinnati Bengals', 'Cleveland Browns', 'Dallas Cowboys',
                'Denver Broncos', 'Detroit Lions', 'Green Bay Packers', 'Houston Texans',
                'Indianapolis Colts', 'Jacksonville Jaguars', 'Kansas City Chiefs', 'Las Vegas Raiders',
                'Los Angeles Chargers', 'Los Angeles Rams', 'Miami Dolphins', 'Minnesota Vikings',
                'New England Patriots', 'New Orleans Saints', 'New York Giants', 'New York Jets',
                'Philadelphia Eagles', 'Pittsburgh Steelers', 'San Francisco 49ers', 'Seattle Seahawks',
                'Tampa Bay Buccaneers', 'Tennessee Titans', 'Washington Commanders')
team_names_inf <- c('cardinals', 'falcons', 'ravens', 'bills', 'panthers', 'bears', 'broncos', 'lions',
                    'packers', 'texans', 'colts', 'jaguars', 'chiefs', 'raiders', 'chargers', 'rams',
                    'dolphins', 'vikings', 'patriots', 'saints', 'giants', 'jets', 'eagles', 'steelers',
                    '49ers', 'seahawks', 'buccaneers', 'titans', 'commanders')
team_names %<>% tolower()
team_names <- gsub(" ", "-", team_names)
?sub

make_urls <- function(teams, skill, year){
  url_ls <- c()
  
  for(i in 1:length(teams)){
    url <- paste0('https://www.foxsports.com/nfl/', teams[i], '-team-game-log?category=', skill, '&season=', year)
    url_ls[i] <- url
  }
  return(url_ls)
}





#function that merges all of the dataframes for a team into one, takes team abb, ex: CHI
team_merge <- function(team_ex, all_teams_all_skills){
  team_stuff <- all_teams_all_skills[[team_ex]]
  left <- team_stuff[[1]]
  left_suff <- paste0('.', names(team_stuff)[1])
  for(l in 2:length(team_stuff)){
    #
    right <- team_stuff[[l]]
    right_suff <- paste0('.', names(team_stuff)[l])
    new_df <- inner_join(left, right, by = c('Date', 'Game'), suffix = c(left_suff, right_suff))
    
    left <- new_df
    left_suff <- ''
  }
  
  return(new_df)
}


#function that combines all team data into on big tibble and adds team name
big_team <- function(dat, year){
  dat[[1]] <- dat[[1]] %>% mutate(Team = abbs[1])
  big_dat <- dat[[1]]
  for(i in 2:length(abbs)){
    if(year < 2020 & abbs[i] == 'LV'){
      tm = 'OAK'
    }
    else{
      tm = abbs[i]
    }
    dat[[i]] <- dat[[i]] %>% mutate(Team = tm)
    big_dat <- big_dat %>% rbind(dat[[i]])
  }
  return(big_dat)
}







###Need to add something to address character columns, instances of "-"
big_clean <- function(all_teams_merged, year){
  scores_dat <- read_csv(paste0("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Spreads/", year, '_nfl_spreads.csv'))
  clean_merged <- list()
  for(i in 1:length(abbs)){
    curr_team <- all_teams_merged[[abbs[i]]] 
    
    
    
    #cleaning character columns, making them integer columns
    curr_team[, 3:ncol(curr_team1)] %<>% mutate_if(is.character, as.double)
    curr_team %<>% filter(!is.na(Comp)) #this removes games where data wasn't collected (gb vs oak 8/22/2019)
    
    #remove dublicate columns:
    #columns that were duplicated, double checked no information deleted
    dupl_cols <- c('R1st.downs', 'P1st.downs', 'RushYds.yardage', 'RushYPA.yardage', 'RushTD.yardage',
                   'PassYds.yardage', 'PassYPA.yardage', 'PassYPR.yardage', 'PassTD.yardage', 'KRYds.yardage',
                   'PRYds.yardage', 'PenYds.yardage', 'OINT.turnovers', 'FumRec.turnovers', 'INT.turnovers',
                   'FUML', 'FUM')
    curr_team %<>% select(-all_of(dupl_cols))
    
    #adds year and makes date
    
    curr_team$Date <- with_year(curr_team$Date, season = year)
    
    #adding team's scores
      #quick fix for when fn feeds LV but needs to find oak
    if(year < 2020 & abbs[i] == 'LV'){
      tm <- 'OAK'
    }
    else{
      tm <- abbs[i]
    }
    underdog_scores <- scores_dat %>% select(c('Date', 'UDogScore', 'Underdog')) %>% rename(Team = Underdog, PF = UDogScore) %>% filter(Team == tm)
    
    favorite_scores <- scores_dat %>% select(c('Date', 'FavScore', 'Favorite')) %>% rename(Team = Favorite, PF = FavScore) %>% filter(Team == tm)
    scores <- rbind(underdog_scores, favorite_scores) %>% arrange(Date) %>% select(-Team)
    
    curr_team <- left_join(curr_team, scores, by = 'Date')
    
    #Reverse order
    curr_team <- curr_team[nrow(curr_team):1,]
    
    #rolling averages: why is this broken?
    curr_team[, 3:ncol(curr_team)] <- lag(rollapplyr(curr_team[, 3:ncol(curr_team)], 25, mean, na.rm = TRUE, partial = TRUE))
    
    
    
    #Cleans opponent info
    curr_team$Game <- clean_game(curr_team$Game)
    
    #saving in new list
    clean_merged[[i]] <- curr_team
  }
  names(clean_merged) <- names(all_teams_merged)
  return(clean_merged)
}







#####How the pipeline should proceed, sloppy as fuck above#####
boxpipe <- function(year){

  
  

  #making a list, 32 elements which each contain the 9 dataframes for each team
  #all_teams_all_skills <- list()
  #for(team in 1:length(abbs)){
    #all_teams_all_skills[[team]] <- get_boxes(abbs[team], year)
    #names(all_teams_all_skills[[team]]) <- c('passing', 'rushing', 'defense', 'kicking', 'returning','punting',
     #                                        'downs', 'yardage', 'turnovers')
  #}
  #naming the elements of the list with the team abbreviations
  #names(all_teams_all_skills) <- abbs
  
  
  #now loop through the team_merge function, list again to hold 32 big dataframes and name conveniently
  #all_teams_merged <- list()
  #for(team in 1:length(abbs)){
   # merged_team <- team_merge(abbs[team], all_teams_all_skills)
    #all_teams_merged[[team]] <- merged_team
  #}
  #names(all_teams_merged) <- abbs
  
  #saving all merged dataframes so never have to scrape for them again
  #for(df in 1:length(all_teams_merged)){
   # dat.fm <- all_teams_merged[[df]] %>% as.data.frame()
    #named <- paste0("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Raw/", year, '/', tolower(abbs[df]), '_', year, '_box_data.csv')
    #write.csv(dat.fm, named, row.names = FALSE)
  #}
  
  #for after has been scraped
  all_teams_merged <- list()
  for(abb in 1:length(abbs)){
    to_Read <- paste0("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Raw/", year, '/', tolower(abbs[abb]), '_', year, '_box_data.csv')
    team_dat <- read.csv(to_Read)
    all_teams_merged[[abb]] <- team_dat
    names(all_teams_merged)[abb] <- abbs[abb]
    }
  
  
  ###now, lets clean. 
  ##needs to be reordered by date, currently backwards, DONE####
  ##duplicate columns ( double check before deleting!!) DONE####
  ##name and date info should be cleaned up, will be hard to merge with spread data currently ###DONE###
  ##lastly, all needs to be averaged DONE#####
  
  #(all_teams_merged$CHI %>% colnames)
  #all_teams_merged$CHI$R1st == all_teams_merged$CHI$R1st.downs  #SAME
  #all_teams_merged$CHI$P1st.downs == all_teams_merged$CHI$P1st #SAME
  #all_teams_merged$CHI$RushYds.yardage == all_teams_merged$CHI$RushYds #SAME
  #all_teams_merged$CHI$RushYPA.yardage == all_teams_merged$CHI$RushYPA #SAME
  #all_teams_merged$CHI$RushTD == all_teams_merged$CHI$RushTD.yardage #SAME
  #all_teams_merged$CHI$PassYDS == all_teams_merged$CHI$PassYds #SAME
  #all_teams_merged$CHI$PassYPA.yardage == all_teams_merged$CHI$PassYPA #SAME
  #all_teams_merged$CHI$PassYPR.yardage == all_teams_merged$CHI$PassYPR #SAME
  #all_teams_merged$CHI$PassTD.yardage == all_teams_merged$CHI$PassTD #SAME
  #all_teams_merged$CHI$KRYds.yardage == all_teams_merged$CHI$KRYds #SAME
  #all_teams_merged$CHI$PRYds.yardage == all_teams_merged$CHI$PRYds #SAME
  #all_teams_merged$CHI$PenYds.yardage == all_teams_merged$CHI$PenYds #SAME
  #all_teams_merged$CHI$OINT.turnovers == all_teams_merged$CHI$OINT #SAME
  #all_teams_merged$CHI$FumRec.turnovers == all_teams_merged$CHI$FumRec #SAME
  #all_teams_merged$CHI$INT.turnovers == all_teams_merged$CHI$INT #SAME
  
  #####Creating a function combining all of the cleaning steps below. Loop through all_teams_merged
  
  
  
  clean_all_merged <- big_clean(all_teams_merged, year)
  
  
  #now saving em
  #saving all merged dataframes so never have to scrape for them again
  for(df in 1:length(clean_all_merged)){
    dat.fm <- clean_all_merged[[df]] %>% as.data.frame()
    named <- paste0("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Averaged/", year, '/', tolower(abbs[df]), '_', year, '_avgs.csv')
    
    write.csv(dat.fm, named, row.names = FALSE)
  }
  
  
  big_all_merged <- big_team(clean_all_merged, year)
  big_name <- paste0("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Averaged/", year, '_box_avgs.csv')
  write.csv(big_all_merged, big_name, row.names = FALSE)
  
  #return(big_all_merged)
  
  #now merging clean box scores with spread data
  
  #reading spread data
  spread_dat <- read_csv(paste0("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Spreads/", year, '_nfl_spreads.csv'))
  
  underdog_frame <- spread_dat %>% select(c('Date', 'Underdog')) %>% rename(Team = Underdog)
  #underdog_frame %>% head
  favorite_frame <- spread_dat %>% select(c('Date', 'Favorite')) %>% rename(Team = Favorite)
  
  underdogs <- left_join(underdog_frame, big_all_merged, by = c('Date', 'Team')) 
  favorites <- left_join(favorite_frame, big_all_merged, by = c('Date', 'Team'))
  colnames(underdogs)[c(2,3)] <- c('Underdog', 'Favorite')
  colnames(underdogs)[c(4:ncol(underdogs))] <- paste0('UD.', colnames(underdogs)[c(4:ncol(underdogs))])
  colnames(favorites)[c(2,3)] <- c('Favorite', 'Underdog')
  colnames(favorites)[c(4:ncol(favorites))] <- paste0('Fav.', colnames(favorites)[c(4:ncol(favorites))])
  
  write.csv(underdogs, paste0("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Full/", year,'_underdogs_avgs.csv'), row.names = FALSE)
  write.csv(favorites, paste0("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Full/", year, '_favorites_avgs.csv'), row.names = FALSE)
  
  
  
  full_data_left <- left_join(spread_dat, favorites, by = c('Date', 'Favorite', 'Underdog'))
  full_data <- left_join(full_data_left, underdogs, by = c('Date', 'Underdog', 'Favorite'))
  
  #oakland to las vegas name change accounted for here
  full_data$Favorite[which(full_data$Favorite == 'OAK')] <- 'LV'
  full_data$Underdog[which(full_data$Underdog == 'OAK')] <- 'LV'
  
  write.csv(full_data, paste0("/Users/tristansetterdahl/Sports and Data Science/NFL Spread Models/Box Scores/Full/all_data_", year, '.csv'))
  
  
}



for(i in 2018:2021){
  boxpipe(i)
}




