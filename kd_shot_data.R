rm(list=ls())
library(rjson)
library(tidyverse)
library(grid)
library(jpeg)
library(RCurl)
library(gridExtra)

giannis <- read_csv("https://raw.githubusercontent.com/joshdmark/shot_charts/master/giannis_shot_data.csv") %>% data.frame()
durant <- read_csv("https://raw.githubusercontent.com/joshdmark/shot_charts/master/kd_shot_data.csv") %>% data.frame()

 
## Get regular season shot data
seasons <- c('2007-08', ## SEA
             '2008-09', '2009-10', '2010-11', '2011-12', '2012-13', '2013-14', '2014-15', '2015-16',## OKC
             '2016-17', '2017-18', '2018-19') ## GSW
all_shots <- data.frame()
for (season_id in seasons){
  # print(season)
  season <- season_id 
  playerID <- 201142 # KD
  season_type <- 'Regular+Season'
  shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS="
                   ,season
                   ,"&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID="
                   ,playerID
                   ,"&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season="
                   ,season
                   ,"&SeasonSegment=&SeasonType="
                   ,season_type
                   ,"&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0"
                   ,sep = "")
  shotData <- fromJSON(file = shotURL, method="C")
  # unlist shot data, save into a data frame
  fga <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))
  # fix column names
  # colnames(fga) <- shotData$resultSets[[1]][[2]] %>% data.frame()
  fga$season_nbr <- season
  fga$season_type <- season_type
  all_shots <- bind_rows(all_shots, fga)
}

## column names from NBA JSON are invalid, remove those columns and rename
rm_season <- all_shots %>% dplyr::select(-season_nbr, -season_type) ## columns with invalid names
season_col <- all_shots %>% dplyr::select(season_nbr, season_type) ## season_nbr, season_type columns (manually created)
## get correct column names
proper_names <- shotData$resultSets[[1]][[2]] %>% as.vector()
## rename columns 
colnames(rm_season) <- proper_names
## re-combine data
reg_season <- cbind(rm_season, season_col) 
write_rds(reg_season, "C:/Users/jdmark/Downloads/reg_season.RDS") ## write output RDS file
######## END REGULAR SEASON SCRAPING ########

### Repeat scraping for Playoffs data
## Get regular season shot data
seasons <- c('2009-10', '2010-11', '2011-12', '2012-13', '2013-14', '2015-16',## OKC
             '2016-17', '2017-18', '2018-19') ## GSW
all_shots <- data.frame()
for (season_id in seasons){
  # print(season)
  season <- season_id 
  playerID <- 201142 # KD
  season_type <- 'Playoffs'
  shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS="
                   ,season
                   ,"&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID="
                   ,playerID
                   ,"&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season="
                   ,season
                   ,"&SeasonSegment=&SeasonType="
                   ,season_type
                   ,"&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0"
                   ,sep = "")
  shotData <- fromJSON(file = shotURL, method="C")
  # unlist shot data, save into a data frame
  fga <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))
  # fix column names
  # colnames(fga) <- shotData$resultSets[[1]][[2]] %>% data.frame()
  fga$season_nbr <- season
  fga$season_type <- season_type
  all_shots <- bind_rows(all_shots, fga)
}

## column names from NBA JSON are invalid, remove those columns and rename
rm_season <- all_shots %>% dplyr::select(-season_nbr, -season_type) ## columns with invalid names
season_col <- all_shots %>% dplyr::select(season_nbr, season_type) ## season_nbr, season_type columns (manually created)
## get correct column names
proper_names <- shotData$resultSets[[1]][[2]] %>% as.vector()
## rename columns 
colnames(rm_season) <- proper_names
## re-combine data
playoffs <- cbind(rm_season, season_col)
write_rds(playoffs, "C:/Users/jdmark/Downloads/playoffs.RDS")
# rm(rm_season, season_col)
######## END PLAYOFFS SCRAPING ########

### Combine playoffs and regular season
reg_season <- read_rds("C:/Users/jdmark/Downloads/reg_season.RDS")
playoffs <- read_rds("C:/Users/jdmark/Downloads/playoffs.RDS")
all_shots <- rbind(reg_season, playoffs)

## fix formatting on all_shots
all_shots$LOC_X <- as.numeric(as.character(all_shots$LOC_X))
all_shots$LOC_Y <- as.numeric(as.character(all_shots$LOC_Y))
all_shots$SHOT_DISTANCE <- as.numeric(as.character(all_shots$SHOT_DISTANCE))

## output all_shots (reg_season and playoffs)
write_rds(all_shots, "C:/Users/jdmark/Downloads/all_shots.RDS")


# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# simple plot using EVENT_TYPE to colour the dots
ggplot(all_shots, aes(x=LOC_X, y=LOC_Y)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = EVENT_TYPE))
