import pandas as pd
import numpy as np

# Read in data
data = pd.read_csv('AnalyticsChallenge2020Data.csv')


# expand position types to include side of ball
data['TechniqueName'] = data['SideOfBall'].astype(str) + ' ' + data['TechniqueName']
data['PlayID'] = data['GameID'].astype(str) + ' ' + data['EventID'].astype(str)


#create dictionay to keep track of which plays are associated with which techniques
position_set = set()
play_dict = {}
for index, row in data.iterrows():
    position_set.add(row['TechniqueName'])
    if row['TechniqueName'] in play_dict:
        play_dict[row['TechniqueName']].append(row['PlayID'])
    else:
        play_dict[row['TechniqueName']] = [row['PlayID']]


# create a blank spot in dict for each position
on_off_dict = {}
for x in position_set:
    on_off_dict[x] = [[],[],0,0,0]

# make list of epa on for each position
for index, row in data.iterrows():
    tech = row['TechniqueName']
    on_off_dict[tech][0].append(row['EPA'])
    on_off_dict[tech][3] += 1
    
# fill list of epa off
current_play = 0
for index, row in data.iterrows():
    if row['PlayID'] != current_play:
        current_play = row['PlayID']
        for position in play_dict:
            if current_play not in play_dict[position]:
                on_off_dict[position][1].append(row['EPA'])
                on_off_dict[position][4] += 1

# change lists to average
for x in on_off_dict:
    avg = np.nanmean(on_off_dict[x][0])
    on_off_dict[x][0] = avg
    avg2 = np.nanmean(on_off_dict[x][1])
    on_off_dict[x][1] = avg2
    on_off_dict[x][2] = avg - avg2

# convert dict to df
on_off_df = pd.DataFrame.from_dict(on_off_dict, orient = 'index', columns =['epa on', 'epa off', 'diff', 'count_on', 'count_off'])
