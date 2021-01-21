library(tidyverse)
library(baseballr)
library(RMySQL)
db <- dbConnect(MySQL(),dbname = "statcast", user ="root", password = 'ENTER PASSWORD')

#Credit to Bill Petti for the methodology and much of the code seen below.
#To read his original documentation, refer to the URL here: 
#https://billpetti.github.io/2020-05-26-build-statcast-database-rstats-version-2.0/

#######################################
#Data Acquisition from Baseball Savant
######################################

annual_statcast_query <- function(season) {
  #This function scrapes one season's worth of pitch-level data from Baseball Savant
  dates <- seq.Date(as.Date(paste0(season, '-03-01')), #Get list of all weeks from March to December of each year (possible dates of season)
                    as.Date(paste0(season, '-12-01')), by = 'week')
  date_grid <- tibble(start_date = dates, 
                      end_date = dates + 6)
  safe_savant <- safely(scrape_statcast_savant) #If week fails to process, doesn't interrupt entire loop
  
  payload <- map2(.x = date_grid$start_date, #Scrapes week of pitch-level game data
                  .y = date_grid$end_date,
                  .f = function(x,y) {
                    message('\nScraping week of ', x, '...\n')
                    safe_savant(start_date = x, end_date = y, type = 'pitcher')
                  })
  payload_df <- map(payload, 'result') #Converts each weekly payload into a dataframe
  nonzero_rows <- which(map_dbl(payload_df, function(x) ifelse(is.null(nrow(x)),0,nrow(x))) > 0) #Filters out weeks with no games
  payload_df_reduced <- payload_df[nonzero_rows]
  combined <- payload_df_reduced %>% #Binds all weeks together into one yearly dataframe
    bind_rows()
  
  message('Sleeping and collecting garbage...') #If calling this function repeatedly, the downtime helps to not overload the API
  
  Sys.sleep(5*60)
  
  gc()
  
  return(combined)
  
}



#######################################
#Cleaning Scraped Baseball Savant Data
#######################################

format_sc <- function(df){
  df <- df %>% 
    #Add additional fields
    mutate(hit_type = 
             case_when(
               type == "X" & events == "single" ~ 1,
               type == "X" & events == "double" ~ 2,
               type == "X" & events == "triple" ~ 3,
               type == "X" & events == "home_run" ~ 4,
               TRUE ~ NA_real_),
           hit = 
             case_when(
               type == "X" & events %in% c("single","double","triple","home_run")  ~ 1,
               TRUE ~ NA_real_),
           fielding_team = ifelse(inning_topbot == "Bot", away_team, home_team),
           batting_team = ifelse(inning_topbot == "Bot", home_team, away_team),
           barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 
                           & launch_speed + launch_angle >= 124, 1, 0),
           spray_angle = round((atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75),1)) %>%
    filter(!is.na(game_year)) %>% #Remove rows with missing game year
    #Further name/type cleaning and sorting
    mutate(game_date = as.character(game_date)) %>%
    rename(batter_name = player_name) %>%
    arrange(game_date) %>%
    filter(!is.na(game_date))
  
  
  #Convert fielder ID data to numeric and deal with missing cases in early years
  cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
                         "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                         "fielder_8", "fielder_9")
  
  df <- df %>%
    mutate_at(.vars = cols_to_transform, as.numeric) %>%
    mutate_at(.vars = cols_to_transform, function(x) {
      ifelse(is.na(x), 999999999, x)
    })
  
  #This CSV contains a list of variables and their correct types.
  #Will make sure that each variable is of its corresponding type in the lookup
  data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  character_columns <- data_base_column_types %>%
    filter(class == "character") %>%
    pull(variable)
  
  numeric_columns <- data_base_column_types %>%
    filter(class == "numeric") %>%
    pull(variable)
  
  integer_columns <- data_base_column_types %>%
    filter(class == "integer") %>%
    pull(variable)
  
  df <- df %>%
    mutate_if(names(df) %in% character_columns, as.character) %>%
    mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
    mutate_if(names(df) %in% integer_columns, as.integer)
  
  df <- df %>%
    arrange(game_date, game_pk, at_bat_number, pitch_number)
  
  return(df)
}

#############################
#Getting Player IDs and Names
#############################

#Create a 'lookup' dataframe of each player's name and MLBAM ID
chadwick_ids = read_csv('https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv')
cw_lim <- chadwick_ids %>%
  mutate(name = paste(name_first, name_last)) %>%
  select(key_mlbam,name) %>%
  filter(!is.na(key_mlbam))

#This function takes in the cleaned, scraped data and adds the pitcher name from the 'lookup'.
#After applying this function, every row (i.e. pitch) in the dataframe will contain the names of the pitcher and batter.
getIDs <- function(df){
  df <- left_join(df, cw_lim, by = c('pitcher'='key_mlbam')) %>%
    mutate(rownum = row_number())
    select(game_date, game_pk, at_bat_number, pitch_number, pitcher_name = name, batter_name, everything())
  return(df)
}



#Run all the above code just the first year's worth of data and dump it into a new database called statcast
sc_2008 <- getIDs(format_sc(annual_statcast_query(2008))) #Scrapes, cleans, and adds player names to the first year of data
dbWriteTable(db, name = "sc_all",  value = sc_2008, overwrite = TRUE, row.names=FALSE) 

#Run the code for the remaining years and stack the results together. 
#Then append the data frame to the existing database table in MySQL
sc_rest <- map(2009:2020, annual_statcast_query) %>%
  bind_rows()
sc_rest <- getIDs(format_sc(sc_rest))
dbWriteTable(db, name = "sc_all",  value = sc_rest, append = TRUE, row.names=FALSE) 

#Create second database table with just the MLB player ID and player name
#This table can be joined to the main table any time the user wants to query the name corresponding to a certain playerID
dbWriteTable(db, name = "player_ids", value = cw_lim, overwrite = TRUE, row.names = FALSE)

#Create composite primary key
dbGetQuery(db, "ALTER TABLE sc_all ADD PRIMARY KEY(game_pk, at_bat_number, pitch_number)")


#Create indexes in sc_all table to make querying run faster
dbGetQuery(db, "CREATE INDEX statcast_date ON sc_all(game_date(10))") #Specifies the length of the game_date index to be 10 chars
dbGetQuery(db, "CREATE INDEX statcast_year ON sc_all(game_year)")
dbGetQuery(db, "CREATE INDEX statcast_game ON sc_all(game_type(1))")
dbGetQuery(db, "CREATE INDEX statcast_pitcher ON sc_all(pitcher)")
dbGetQuery(db, "CREATE INDEX statcast_batter ON sc_all(batter)")

