import pandas as pd
import os
import sqlalchemy
import json
import urllib

path = ['ENTER PATH']
os.chdir(path)

###########################################
#SETUP AND CONNECTION TO EXISTING DATABASE
###########################################

#This MySQL database houses pitch-level data for all games between 2008 and 2020.
#"Statcast Data Build.R" contains full code on creating the MySQL database.
#The Game IDs from the database are necessary in order to obtain the updated MLB GUMBO data.

#Connect to MySQL Database
db_username = 'root'
db_password = ['ENTER PASSWORD']
db_ip = 'localhost'
db_name = 'statcast'


engine = sqlalchemy.create_engine('mysql+mysqlconnector://{0}:{1}@{2}/{3}'.
                                               format(db_username, db_password, 
                                                      db_ip, db_name))
db = engine.raw_connection()

#Query all non-exhibition game IDs from 2020 season and the number of at-bats in each game
query = "SELECT DISTINCT game_pk, MAX(at_bat_number) as at_bat_number \
    FROM sc_all WHERE (game_year = 2020 AND game_type NOT IN ('E','S')) GROUP BY game_pk"; 
sc20 = pd.read_sql(query,engine)


##########################################
#SCRAPE 2020 GAME DATA FROM MLB GUMBO feed
##########################################

def pullGumbo(game_pk):
    '''
    This function pulls JSON data for a given game ID from
    Major League Baseball's GUMBO feed via the Stats API
    '''
    url = 'https://statsapi.mlb.com/api/v1.1/game/'+str(game_pk)+'/feed/live'
    response = urllib.request.urlopen(url)
    return json.loads(response.read())

def jsonToDf(json_file, nAB):
    '''
    This function takes in 
    (1) scraped JSON file for a given game ID and 
    (2) the number of at-bats in that game)
    and converts it to a pandas dataframe at the pitch level
    '''
    playEvents = pd.DataFrame()
    matchup = pd.DataFrame()
    result = pd.DataFrame()
    for i in range(nAB):
        play_temp = pd.json_normalize(json_file['liveData']['plays']['allPlays'][i]['playEvents'])
        play_temp['atBatNum'] = i+1
        playEvents = playEvents.append(play_temp)
        matchup_temp = pd.json_normalize(json_file['liveData']['plays']['allPlays'][i]['matchup'])
        matchup_temp['atBatNum'] = i+1
        matchup = matchup.append(matchup_temp)
        result_temp = pd.json_normalize(json_file['liveData']['plays']['allPlays'][i]['result'])
        result_temp['atBatNum'] = i+1
        result = result.append(result_temp)
   
    pitches = playEvents.merge(matchup.merge(result, how='inner', on='atBatNum'), how='inner', on='atBatNum')
    pitches['game_pk'] = json_file['gamePk']
    pitches['game_year'] = int(json_file['gameData']['game']['id'][0:4])
    pitches['game_date'] = json_file['gameData']['game']['id'][0:10].replace('/','-')
    pitches = pitches.reset_index().drop('index',axis=1)
    return pitches

gumbo_gms = list(map(pullGumbo, sc20['game_pk'].astype(int))) #Scrapes JSON files for all 2020 MLB games
gumbo_dfs = list(map(jsonToDf, gumbo_gms, sc20['at_bat_number'])) #Converts all JSON files to dataframes
gumbo_stacked = pd.concat(gumbo_dfs) #Concatenate all game-level dataframes into one season-level dataframe
print(gumbo_stacked.shape)


#The above 2 function calls take quite a long time to run, so export the resulting dataframe to .csv
#Will then re-import and clean the data as part of the analysis
gumbo_stacked.to_csv('sc_gumbo.csv',index=False)



