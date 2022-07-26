##NBASTATR
library("nbastatR")
library("tidyverse")
library("Dict")

Sys.setenv(VROOM_CONNECTION_SIZE = 500000)

NBATeamsDict <- dict(
  "ATL" =   "Atlanta Hawks",
  "BKN" =   "Brooklyn Nets",
  "BOS" =   "Boston Celtics",
  "CHA" =   "Charlotte Hornets",
  "CHI" =   "Chicago Bulls",
  "CLE" =   "Cleveland Cavaliers",
  "DAL" =   "Dallas Mavericks",
  "DEN" =   "Denver Nuggets",
  "DET" =   "Detroit Pistons",
  "GSW" =   "Golden State Warriors",
  "HOU" =   "Houston Rockets",
  "IND" =   "Indiana Pacers",
  "LAC" =   "Los Angeles Clippers",
  "LAL" =   "Los Angeles Lakers",
  "MEM" =   "Memphis Grizzlies",
  "MIA" =   "Miami Heat",
  "MIL" =   "Milwaukee Bucks",
  "MIN" =   "Minnesota Timberwolves",
  "NOP" =   "New Orleans Pelicans",
  "NYK" =   "New York Knicks",
  "OKC" =   "Oklahoma City Thunder",
  "ORL" =   "Orlando Magic",
  "PHI" =   "Philadelphia 76ers",
  "PHX" =   "Phoenix Suns",
  "POR" =   "Portland Trail Blazers",
  "SAC" =   "Sacramento Kings",
  "SAS" =   "San Antonio Spurs",
  "TOR" =   "Toronto Raptors",
  "UTA" =   "Utah Jazz",
  "WAS" =   "Washington Wizards")
  



NBATeams <- c("Atlanta Hawks",
                  "Brooklyn Nets",
                  "Boston Celtics",
                  "Charlotte Hornets",
                  "Chicago Bulls",
                  "Cleveland Cavaliers",
                  "Dallas Mavericks",
                  "Denver Nuggets",
                  "Detroit Pistons",
                  "Golden State Warriors",
                  "Houston Rockets",
                  "Indiana Pacers",
                  "Los Angeles Clippers",
                  "Los Angeles Lakers",
                  "Memphis Grizzlies",
                  "Miami Heat",
                  "Milwaukee Bucks",
                  "Minnesota Timberwolves",
                  "New Orleans Pelicans",
                  "New York Knicks",
                  "Oklahoma City Thunder",
                  "Orlando Magic",
                  "Philadelphia 76ers",
                  "Phoenix Suns",
                  "Portland Trail Blazers",
                  "Sacramento Kings",
                  "San Antonio Spurs",
                  "Toronto Raptors",
                  "Utah Jazz",
                  "Washington Wizards"  
                  )
# Teams Shots
allTeamsShots10 = teams_shots(teams = NBATeams, seasons = 2010)
allTeamsShots11 = teams_shots(teams = NBATeams, seasons = 2011)
allTeamsShots12 = teams_shots(teams = NBATeams, seasons = 2012)
allTeamsShots13 = teams_shots(teams = NBATeams, seasons = 2013)
allTeamsShots14 = teams_shots(teams = NBATeams, seasons = 2014)
allTeamsShots15 = teams_shots(teams = NBATeams, seasons = 2015)
allTeamsShots16 = teams_shots(teams = NBATeams, seasons = 2016)
allTeamsShots17 = teams_shots(teams = NBATeams, seasons = 2017)
allTeamsShots18 = teams_shots(teams = NBATeams, seasons = 2018)
allTeamsShots19 = teams_shots(teams = NBATeams, seasons = 2019)
allTeamsShots20 = teams_shots(teams = NBATeams, seasons = 2020)
allTeamsShots21 = teams_shots(teams = NBATeams, seasons = 2021)
allTeamsShots22 = teams_shots(teams = NBATeams, seasons = 2022)

allTeamsShots <- bind_rows(allTeamsShots10, allTeamsShots11, allTeamsShots12, allTeamsShots13, allTeamsShots14, allTeamsShots15, allTeamsShots16, allTeamsShots17, allTeamsShots18, allTeamsShots19, allTeamsShots20, allTeamsShots21, allTeamsShots22)

saveRDS(allTeamsShots10, file = "allTeamsShots10.rds")
saveRDS(allTeamsShots11, file = "allTeamsShots11.rds")
saveRDS(allTeamsShots12, file = "allTeamsShots12.rds")
saveRDS(allTeamsShots13, file = "allTeamsShots13.rds")
saveRDS(allTeamsShots14, file = "allTeamsShots14.rds")
saveRDS(allTeamsShots15, file = "allTeamsShots15.rds")
saveRDS(allTeamsShots16, file = "allTeamsShots16.rds")
saveRDS(allTeamsShots17, file = "allTeamsShots17.rds")
saveRDS(allTeamsShots18, file = "allTeamsShots18.rds")
saveRDS(allTeamsShots19, file = "allTeamsShots19.rds")
saveRDS(allTeamsShots20, file = "allTeamsShots20.rds")
saveRDS(allTeamsShots21, file = "allTeamsShots21.rds")
saveRDS(allTeamsShots22, file = "allTeamsShots22.rds")
saveRDS(allTeamsShots, file = "allTeamsShots.rds")


# Pulling play by play games
playByPlay10 <- play_by_play_v2(game_ids = unique(allTeamsShots10$idGame)) 
playByPlay11 <- play_by_play_v2(game_ids = unique(allTeamsShots11$idGame)) 
playByPlay12 <- play_by_play_v2(game_ids = unique(allTeamsShots12$idGame)) 
playByPlay13 <- play_by_play_v2(game_ids = unique(allTeamsShots13$idGame)) 
playByPlay14 <- play_by_play_v2(game_ids = unique(allTeamsShots14$idGame)) 
playByPlay15 <- play_by_play_v2(game_ids = unique(allTeamsShots15$idGame)) 
playByPlay16 <- play_by_play_v2(game_ids = unique(allTeamsShots16$idGame)) 
playByPlay17 <- play_by_play_v2(game_ids = unique(allTeamsShots17$idGame)) 
playByPlay18 <- play_by_play_v2(game_ids = unique(allTeamsShots18$idGame)) 
playByPlay19 <- play_by_play_v2(game_ids = unique(allTeamsShots19$idGame)) 
playByPlay20 <- play_by_play_v2(game_ids = unique(allTeamsShots20$idGame)) 
playByPlay21 <- play_by_play_v2(game_ids = unique(allTeamsShots21$idGame))
playByPlay22 <- play_by_play_v2(game_ids = unique(allTeamsShots22$idGame))

play_by_play_clean <- function(play_by_play_data){
  output <- play_by_play_data %>%
  rename(idEvent = numberEvent) %>%
    mutate(scoreHome = if_else(idEvent == 2, 0, scoreHome)) %>%
    mutate(scoreAway = if_else(idEvent == 2, 0, scoreAway)) %>%
    mutate(marginScore = if_else(idEvent == 2, 0, marginScore)) %>%
    fill(scoreHome, .direction = "down") %>%
    fill(scoreAway, .direction = "down") %>%
    fill(marginScore, .direction = "down")
    
  return(output)
  
} 

# Play by play
playByPlay10Cl <- play_by_play_clean(playByPlay10)
playByPlay11Cl <- play_by_play_clean(playByPlay11)
playByPlay12Cl <- play_by_play_clean(playByPlay12)
playByPlay13Cl <- play_by_play_clean(playByPlay13)
playByPlay14Cl <- play_by_play_clean(playByPlay14)
playByPlay15Cl <- play_by_play_clean(playByPlay15)
playByPlay16Cl <- play_by_play_clean(playByPlay16)
playByPlay17Cl <- play_by_play_clean(playByPlay17)
playByPlay18Cl <- play_by_play_clean(playByPlay18)
playByPlay19Cl <- play_by_play_clean(playByPlay19)
playByPlay20Cl <- play_by_play_clean(playByPlay20)
playByPlay21Cl <- play_by_play_clean(playByPlay21)
playByPlay22Cl <- play_by_play_clean(playByPlay22)


# Saving regular RDSs
saveRDS(playByPlay10, file = "playByPlay10.rds")
saveRDS(playByPlay11, file = "playByPlay11.rds")
saveRDS(playByPlay12, file = "playByPlay12.rds")
saveRDS(playByPlay13, file = "playByPlay13.rds")
saveRDS(playByPlay14, file = "playByPlay14.rds")
saveRDS(playByPlay15, file = "playByPlay15.rds")
saveRDS(playByPlay16, file = "playByPlay16.rds")
saveRDS(playByPlay17, file = "playByPlay17.rds")
saveRDS(playByPlay18, file = "playByPlay18.rds")
saveRDS(playByPlay19, file = "playByPlay19.rds")
saveRDS(playByPlay20, file = "playByPlay20.rds")
saveRDS(playByPlay21, file = "playByPlay21.rds")
saveRDS(playByPlay22, file = "playByPlay22.rds")


#Saving cleaned RDS
saveRDS(playByPlay10Cl, file = "playByPlay10Cl.rds")
saveRDS(playByPlay11Cl, file = "playByPlay11Cl.rds")
saveRDS(playByPlay12Cl, file = "playByPlay12Cl.rds")
saveRDS(playByPlay13Cl, file = "playByPlay13Cl.rds")
saveRDS(playByPlay14Cl, file = "playByPlay14Cl.rds")
saveRDS(playByPlay15Cl, file = "playByPlay15Cl.rds")
saveRDS(playByPlay16Cl, file = "playByPlay16Cl.rds")
saveRDS(playByPlay17Cl, file = "playByPlay17Cl.rds")
saveRDS(playByPlay18Cl, file = "playByPlay18Cl.rds")
saveRDS(playByPlay19Cl, file = "playByPlay19Cl.rds")
saveRDS(playByPlay20Cl, file = "playByPlay20Cl.rds")
saveRDS(playByPlay21Cl, file = "playByPlay21Cl.rds")
saveRDS(playByPlay22Cl, file = "playByPlay22Cl.rds")


shots_and_play_join <- function(shotData, playByPlayData){
  tempTibble = left_join(shotData, playByPlayData, by = c("idGame", "idEvent")) %>%
    mutate(shotValue = case_when(typeShot == "2PT Field Goal" ~ 2,
           typeShot == "3PT Field Goal" ~ 3)) %>%
    mutate(numericMakeMiss = case_when(typeEvent == "Missed Shot" ~ 0,
                                      typeEvent == "Made Shot" ~ 1)) %>%
    
    #mutate(homeTeamLong = recode(slugTeamHome, !!NBATeamsDict, .default = NA_character_))
    
    mutate(numericIsPlayerHome = if_else(slugTeamPlayer1 == slugTeamHome, 1, 0)) %>%
    mutate(numericIsPlayerAway = if_else(slugTeamPlayer1 == slugTeamAway, 1, 0)) %>%
    
    mutate(marginScoreHomePreShot = marginScore + (numericMakeMiss * shotValue * numericIsPlayerAway) - (numericMakeMiss * shotValue * numericIsPlayerHome)) %>%
    mutate(marginScoreAwayPreShot = - marginScoreHomePreShot) %>%
    mutate(marginScoreShootersTeam = case_when(numericIsPlayerHome == 1 ~ marginScoreHomePreShot, numericIsPlayerHome == 0 ~ marginScoreAwayPreShot)) %>%
      
    # Section to determine the shot distance and angle.
    # Appears the only hoop is located at Y: (-9, 9) ; X: (-9,9).
    # Hoop located at 0,0
    # 1x = 0.2ft 
    # 1y = 0.1ft
    mutate(shotAngle = case_when(locationY >= 0  ~ tan(abs(locationX) / 2*locationY), locationY < 0 ~ 90 + tan(abs(locationX) / 2*abs(locationY))))
      
  return(tempTibble)
}



# Matching Shots and plays
matched_10 = shots_and_play_join(allTeamsShots10, playByPlay10Cl)
matched_11 = shots_and_play_join(allTeamsShots11, playByPlay11Cl)
matched_12 = shots_and_play_join(allTeamsShots12, playByPlay12Cl)
matched_13 = shots_and_play_join(allTeamsShots13, playByPlay13Cl)
matched_14 = shots_and_play_join(allTeamsShots14, playByPlay14Cl)
matched_15 = shots_and_play_join(allTeamsShots15, playByPlay15Cl)
matched_16 = shots_and_play_join(allTeamsShots16, playByPlay16Cl)
matched_17 = shots_and_play_join(allTeamsShots17, playByPlay17Cl)
matched_18 = shots_and_play_join(allTeamsShots18, playByPlay18Cl)
matched_19 = shots_and_play_join(allTeamsShots19, playByPlay19Cl)
matched_20 = shots_and_play_join(allTeamsShots20, playByPlay20Cl)
matched_21 = shots_and_play_join(allTeamsShots21, playByPlay21Cl)
matched_22 = shots_and_play_join(allTeamsShots22, playByPlay22Cl)


# Saving Files
saveRDS(matched_10, file = "matched_10.rds")
saveRDS(matched_11, file = "matched_11.rds")
saveRDS(matched_12, file = "matched_12.rds")
saveRDS(matched_13, file = "matched_13.rds")
saveRDS(matched_14, file = "matched_14.rds")
saveRDS(matched_15, file = "matched_15.rds")
saveRDS(matched_16, file = "matched_16.rds")
saveRDS(matched_17, file = "matched_17.rds")
saveRDS(matched_18, file = "matched_18.rds")
saveRDS(matched_19, file = "matched_19.rds")
saveRDS(matched_20, file = "matched_20.rds")
saveRDS(matched_21, file = "matched_21.rds")
saveRDS(matched_22, file = "matched_22.rds")
  






















# Obselete function ideas
matches_splitter_play_by_play <- function(seasonShotData){
  seasonGames <- unique(seasonShotData$idGame)
  numberSeasonGames <- length(seasonGames)
  list1 <- seasonGames[1:numberSeasonGames/10]
  list2 <- seasonGames[numberSeasonGames/10:numberSeasonGames/5]
  list3 <- seasonGames[numberSeasonGames/5:3*numberSeasonGames/10]
  list4 <- seasonGames[3*numberSeasonGames/10:4*numberSeasonGames/10]
  list5 <- seasonGames[4*numberSeasonGames/10:numberSeasonGames/2]
  list6 <- seasonGames[numberSeasonGames/2:6*numberSeasonGames/10]
  list7 <- seasonGames[6*numberSeasonGames/10:7*numberSeasonGames/10]
  list8 <- seasonGames[7*numberSeasonGames/10:8*numberSeasonGames/10]
  list9 <- seasonGames[8*numberSeasonGames/10:9*numberSeasonGames/10]
  list10 <- seasonGames[9*numberSeasonGames:numberSeasonGames]
  
  for (i in 1:10) {
    play_by_play_v2(list + str(i),  )
    
  }
  
  return (seasonGamesSplit)
}

get_players <- function(data){
  return (unique(data$namePlayer1))
}

# Testing functions

#GSWShots20 = teams_shots(teams = "Golden State Warriors", seasons = 2020)

#GSW21900016PlayByPlay <- play_by_play_v2(game_ids = 21900016 ) %>%
#    rename(idEvent = numberEvent)
#GSW_matched = left_join(GSWShots20, GSW21900016PlayByPlay, by = c("idGame", "idEvent"))

#GSW_matched = shots_and_play_join(GSWShots20, GSW21900016PlayByPlay)


playByPlayTest <- playByPlay20 %>%
  # rename(idEvent = numberEvent) %>%
  mutate(scoreHome = if_else(idEvent == 2, 0, scoreHome)) %>%
  mutate(scoreAway = if_else(idEvent == 2, 0, scoreAway)) %>%
  mutate(marginScore = if_else(idEvent == 2, 0, marginScore)) %>%
  fill(scoreHome, .direction = "down") %>%
  fill(scoreAway, .direction = "down") %>%
  fill(marginScore, .direction = "down")



##matched_20 = left_join(allTeamsShots20, playByPlay20, by = c("idGame", "idEvent"))
