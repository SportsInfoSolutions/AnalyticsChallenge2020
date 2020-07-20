import pandas as pd
import os
from typing import AnyStr


def tackle_for_loss(yards):
    yards = float(yards)
    if yards >= 0:
        return 0
    else:
        return 1


def tackle_for_loss_player(tfl, solo, assist):
    player_tfl = 0
    if tfl == 1:
        if solo == 1:
            player_tfl = 1
        elif assist == 1:
            player_tfl = 0.5
    return player_tfl


def safety_player(safety, solo, assist):
    player_safety = 0
    if safety == 1:
        if solo == 1:
            player_safety = 1
        elif assist == 1:
            player_safety = 0.5
    return player_safety


def tackle_for_loss_yards(tfl, yards):
    tfl_yards = 0
    if tfl == 1:
        tfl_yards = abs(yards)

    return tfl_yards


def technique_l_r(technique, sideofball):
    technique = str(technique).lower()
    sideofball = str(sideofball).lower()

    if sideofball == 'nan':
        technique_name = f"{technique}"
    else:
        technique_name = f"{technique}_{sideofball}"
    return technique_name


def _output_split_run_pass(df, rush_cols, pass_cols):
    output_base = './data/02_intermediate'

    # Rush
    rush_df = df.loc[df['EventType'] == 'rush']
    # Remove pass specific columns from rushing dataframe
    try:
        rush_df = rush_df.drop(rush_cols, axis=1)
    except Exception as e:
        print(e)
        pass
    rush_output = f"{output_base}/rush.csv"
    rush_df.to_csv(rush_output, index=False)

    # Pass
    pass_df = df.loc[df['EventType'] == 'pass']
    # Remove rush specific columns from passing dataframe
    try:
        pass_df = pass_df.drop(pass_cols, axis=1)
    except Exception as e:
        print(e)
        pass
    pass_output = f"{output_base}/pass.csv"
    pass_df.to_csv(pass_output, index=False)


def def_side_of_field(sideoffield):
    def_side = None
    if sideoffield == 'Own':
        def_side = 'Oppo'
    if sideoffield == 'Oppo':
        def_side = 'Own'
    return def_side


def game_event_id(game, event):
    game_event = None
    try:
        game_event = f"{game}_{event}"
    except Exception as e:
        pass
    return game_event


def main(input_csv, rush_cols, pass_cols):
    df = pd.read_csv(input_csv)

    # Create key for sorting
    df['game_event'] = df[['GameID', 'EventID']].apply(
        lambda x: game_event_id(
            game=x['GameID'],
            event=x['EventID']),
        axis=1)

    # Create L or R position for each tech
    df['TechniqueNameLR'] = df[['TechniqueName', 'SideOfBall']].apply(
        lambda x: technique_l_r(
            technique=x['TechniqueName'],
            sideofball=x['SideOfBall']),
        axis=1)

    # Add tackle for loss boolean column
    df['tfl'] = df['OffensiveYardage'].apply(tackle_for_loss)

    # add tackle for loss yards column
    df['tfl_yards'] = df[['tfl', 'OffensiveYardage']].apply(
        lambda x: tackle_for_loss_yards(
            tfl=x['tfl'],
            yards=x['OffensiveYardage']),
        axis=1)

    df['PlayerSafety'] = df[['Safety', 'SoloTackle', 'AssistedTackle']].apply(
        lambda x: safety_player(
            safety=x['Safety'],
            solo=x['SoloTackle'],
            assist=x['AssistedTackle']),
        axis=1)

    df['TackleForLoss'] = df[['tfl', 'SoloTackle', 'AssistedTackle']].apply(
        lambda x: tackle_for_loss_player(
            tfl=x['tfl'],
            solo=x['SoloTackle'],
            assist=x['AssistedTackle']),
        axis=1)

    df['DefSideOfField'] = df['SideOfField'].apply(def_side_of_field)
    _output_split_run_pass(df=df, rush_cols=rush_cols, pass_cols=pass_cols)


if __name__ == '__main__':
    remove_for_run = ['Pressure', 'SoloSack', 'AssistedSack', 'PassBreakup', 'Interception']
    remove_for_pass = []
    input_csv = './AnalyticsChallenge2020/Data/AnalyticsChallenge2020Data.csv'
    main(input_csv=input_csv, rush_cols=remove_for_run, pass_cols=remove_for_pass)
