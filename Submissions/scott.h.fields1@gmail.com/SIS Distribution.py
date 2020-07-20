import pandas as pd
import numpy as np
import statistics as stats

# Read in data
data = pd.read_csv('AnalyticsChallenge2020Data.csv')

# on_off_df = pd.DataFrame(columns = ['position', 'epa on', 'epa off', 'diff', 'count_on', 'count_off])

# expand position types to include side of ball
data['TechniqueName'] = data['SideOfBall'].astype(str) + ' ' + data['TechniqueName']
data['PlayID'] = data['GameID'].astype(str) + ' ' + data['EventID'].astype(str) 
data = data[~data['OnFieldPosition'].str.contains('LB')]

# Create dict tracking how much each player played each position
position_set = set()
player_count_dict = {}
for index, row in data.iterrows():
    if row['Name'] + ' ' + row['TechniqueName'] not in player_count_dict:
        player_count_dict[row['Name'] + ' ' + row['TechniqueName']] = 1
    else:
        player_count_dict[row['Name'] + ' ' + row['TechniqueName']] += 1
    position_set.add(row['TechniqueName'])

# delete entries with less than 20 plays
for x in list(player_count_dict):
    if player_count_dict[x] < 10:
        del player_count_dict[x]

epa_dict = {}
for x in player_count_dict:
    epa_dict[x] = [[], '']

# make dictionary of epa lists
for index, row in data.iterrows():
    if row['Name'] + ' ' + row['TechniqueName'] in player_count_dict:
        epa_dict[row['Name'] + ' ' + row['TechniqueName']][0].append(row['EPA'])
        epa_dict[row['Name'] + ' ' + row['TechniqueName']][1] = row['TechniqueName']

# convert to average       
for x in epa_dict:
    avg = np.nanmean(epa_dict[x][0])
    epa_dict[x][0] = avg

# create standard deviation dict
sd_dict = {}
for x in position_set:
    sd_dict[x] = [[], 0]

# fill sd dict with lists of epa/ plays
for x in epa_dict:
    sd_dict[epa_dict[x][1]][0].append(epa_dict[x][0])
    sd_dict[epa_dict[x][1]][1] += 1

# convert to sd
for x in sd_dict:
    temp_sd_list = sd_dict[x][0]
    sd = stats.stdev(temp_sd_list)
    sd_dict[x][0] = sd

# convert dict to df
sd_df = pd.DataFrame.from_dict(sd_dict, orient = 'index')

    
