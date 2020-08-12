###################################################
### Catcher Framing Model                       ###
### Author: Judah Axelrod                       ###
###################################################

#SETUP
library(tidyverse)
library(RMySQL)
library(Lahman)
library(mgcv)
library(broom)
library(lme4)

`%nin%` = Negate(`%in%`)

#Query Data from MySQL
query1 <- "select gameID, Date, DoubleHeader, VisitingTeam, HomeTeam, ParkID, VisitorRunsScored, HomeRunsScore, UmpireHID, UmpireHName 
from retro_gamelogs where date > 20150101;"
r_umps <- dbGetQuery(db,query1) %>% mutate(Date = as.character(Date))

query2 <- "select name_lahman, MLBCODE from playerids;"
playerids <- dbGetQuery(db,query2)

query3 <- "select * from sc_all where game_year >= 2015;"
sc_all15 <- dbGetQuery(db,query3)

#########################
#Cleaning
#########################

#Home Team Name Cleaning
retro_teams <- unique(r_umps$HomeTeam)
sc_teams <- unique(sc_all15$home_team)
all_teams <- cbind(retro_teams %>% sort(), sc_teams %>% sort())

c0_sc_all15 <- sc_all15 %>%
  mutate(retro_team = case_when(
    home_team == 'LAA' ~ 'ANA',
    home_team == 'CWS' ~ 'CHA',
    home_team == 'CHC' ~ 'CHN',
    home_team == 'KC' ~ 'KCA',
    home_team == 'LAD' ~ 'LAN',
    home_team == 'NYM' ~ 'NYN',
    home_team == 'NYY' ~ 'NYA',
    home_team == 'SD' ~ 'SDN',
    home_team == 'SF' ~ 'SFN',
    home_team == 'STL' ~ 'SLN',
    home_team == 'TB' ~ 'TBA',
    home_team == 'WSH' ~ 'WAS',
    TRUE ~ home_team),
    retro_date = gsub("-","",game_date))

#Doubleheader Flag
dblhdr <- c0_sc_all15 %>% 
  select(game_date, home_team, game_pk) %>%
  distinct() %>%
  count(game_date, home_team) %>%
  mutate(n = as.double(n))

c1_sc_all15 <- c0_sc_all15 %>%
  left_join(dblhdr, by=c('game_date','home_team'))

scores <- c1_sc_all15 %>%
  filter(n>1 & game_pk %nin% c(447743,447731)) %>%
  group_by(retro_date, retro_team, game_pk) %>%
  summarize(away_score = max(post_away_score),
            home_score = max(post_home_score)) 

#post_home_score field incorrect for some walk-offs - manually adjust these
scores_fixed <- scores %>%
  mutate(home_score = case_when(
    retro_date == '20150707' & retro_team == 'KCA' & home_score == 5 ~ 9,
    retro_date == '20170427' & retro_team == 'SLN' & home_score == 4 ~ 8,
    retro_date == '20170610' & retro_team == 'TBA' & home_score == 5 ~ 6,
    retro_date == '20170813' & retro_team == 'WAS' & away_score == 2 ~ 6,
    retro_date == '20170902' & retro_team == 'SDN' & home_score == 5 ~ 6,
    retro_date == '20180417' & retro_team == 'TOR' & home_score == 4 ~ 5,
    retro_date == '20180420' & retro_team == 'DET' & away_score == 2 ~ 3,
    retro_date == '20180519' & retro_team == 'CIN' & home_score == 4 ~ 5,
    retro_date == '20180528' & retro_team == 'ATL' & home_score == 2 ~ 4,
    retro_date == '20180619' & retro_team == 'CHN' & home_score == 1 ~ 2,
    retro_date == '20180709' & retro_team == 'NYN' & home_score == 3 ~ 4,
    retro_date == '20180913' & retro_team == 'NYN' & home_score == 3 ~ 4,
    retro_date == '20190501' & retro_team == 'CHA' & home_score == 5 ~ 7,
    retro_date == '20190703' & retro_team == 'CHA' & home_score == 6 ~ 9,
    retro_date == '20190820' & retro_team == 'TEX' & home_score == 2 ~ 3,
    retro_date == '20190831' & retro_team == 'SLN' & home_score == 2 ~ 3,
    retro_date == '20190901' & retro_team == 'SLN' & away_score == 3 ~ 4,
    TRUE ~ home_score)) %>%
  left_join(r_umps, by = c('retro_date' = 'Date','retro_team' = 'HomeTeam','away_score' = 'VisitorRunsScored','home_score' = 'HomeRunsScore')) %>%
  select(retro_date, retro_team, game_pk, DoubleHeader)

c2_sc_all15 <- c1_sc_all15 %>%
  left_join(scores_fixed, by = c('retro_date','retro_team','game_pk')) %>%
  mutate(DoubleHeader = case_when(
    game_pk == 447743 ~ 1,
    game_pk == 447731 ~ 2,
    is.na(DoubleHeader) ~ 0,
    TRUE ~ DoubleHeader),
    game_ID = paste0(retro_team, retro_date, DoubleHeader)) %>%
    select(game_ID, everything())

#Creation of game ID to match retrosheet
f_sc_all15 <- c2_sc_all15 %>% 
  filter(description %in% c('ball','blocked_ball','called_strike') & !is.na(plate_x) & !is.na(plate_z) #Should remove 5,309 obs with missing px/pz
         & balls <= 3 & !is.na(fielder_2)) %>%
  filter(pitch_type %in% c('FF','SL','FT','CH','CU','SI','FC','KC','FS','KN')) %>% #Gets rid of unknown/Eephus/Forkball type pitches
  left_join(r_umps, by = c('game_ID' = 'gameID')) %>%
  mutate(count = as.factor(paste0(balls,'-',strikes)),
         stand = as.factor(stand),
         p_throws = as.factor(p_throws),
         inning_topbot = as.factor(inning_topbot),
         pitch_type = as.factor(pitch_type),
         pitcher = as.character(pitcher),
         batter = as.character(batter)) %>%
  select(game_ID, game_date, pitcher_name, pitcher, batter, fielder_2, UmpireHID, 
         inning_topbot, type, description, plate_x, plate_z, p_throws, stand, count, pitch_type)

missing <- f_sc_all15 %>%
  filter(is.na(UmpireHID)) %>%
  select(game_ID) %>%
  distinct()

#Clear up memory:
rm(c0_sc_all15,c1_sc_all15,c2_sc_all15,sc_all15)


#Create GAM to model strike probability
gam1 <- gam(type == 'S' ~ s(plate_x, plate_z), family = binomial, data = f_sc_all15)

f_sc_all15 <- f_sc_all15 %>% 
  mutate(.fitted = as.numeric(predict(gam1, type = 'response')),
         type_pred = factor(ifelse(.fitted > 0.5, 'S','B')))

table(f_sc_all15$type,f_sc_all15$type_pred)
mean(f_sc_all15$type==f_sc_all15$type_pred)

#Create mixed model to measure framing
mix.mod1 <- glmer(type == 'S' ~ .fitted + (1|pitcher_name) + (1|batter) + (1|fielder_2) + (1|UmpireHID) + inning_topbot + count + stand*p_throws + pitch_type,
                  data = f_sc_all15, family = binomial, nAGQ = 0)
View(tidy(mix.mod1, effects = 'fixed'))
tidy(mix.mod1, effects = 'ran_pars')

all_re <- mix.mod1 %>%
  ranef() %>%
  as_tibble()

pitch_re <- all_re %>%
  filter(grpvar == 'pitcher_name') %>%
  arrange(desc(condval))

catch_re <- all_re %>%
  filter(grpvar == 'fielder_2') %>%
  mutate(grp = as.character(grp)) %>%
  left_join(playerids, by = c('grp' = 'MLBCODE')) %>%
  arrange(desc(condval))
