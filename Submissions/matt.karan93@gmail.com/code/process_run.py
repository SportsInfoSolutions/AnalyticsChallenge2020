import pandas as pd
from typing import Dict
import re


def _yards_diff_in_pos_swap(df):
    # If you move one tech to any other tech, whats diff in avg yards allowed
    df = df.subtract(df.T)
    print(df)


def _extract_dir(gap_str):
    gap_str = str(gap_str).lower()
    regex = re.compile(r'left|right|middle')
    gap_dir = None
    try:
        gap_dir = regex.match(gap_str).group(0)
    except Exception as e:
        print(e)
        pass

    return gap_dir


def _extract_gap(gap_str):
    gap_str = str(gap_str).lower()
    gap_regex = re.compile(r'(\s\w+)(?=\sgap)')

    gap = None
    if gap_str == 'middle':
        gap = 'middle'
    else:
        try:
            gap = gap_regex.findall(gap_str)[0].strip()
        except Exception as e:
            print(e)
    return gap


def run_gap_map(gap, direction):
    dir_dict = {'left': 'l',
                'right': 'r',
                'middle': 'middle'}
    direction = dir_dict[direction]

    if direction != 'middle':
        gap_key = f"{gap}_{direction}"
    else:
        gap_key = 'middle'

    gap_dict = {'a_l': ['0', '1_l', '2i_l', '2_l', '3_l'],
                'b_l': ['2i_l', '2_l', '3_l', '4i_l', '4_l', '5_l', '7_l'],
                'c_l': ['4i_l', '4_l', '5_l', '7_l', '6_l', '9_l'],
                'd_l': ['5_l', '7_l', '6_l', '9_l'],

                'a_r': ['0', '1_r', '2i_r', '2_r', '3_r'],
                'b_r': ['2i_r', '2_r', '3_r', '4i_r', '4_r', '5_r', '7_r'],
                'c_r': ['4i_r', '4_r', '5_r', '7_r', '6_r', '9_r'],
                'd_r': ['5_r', '7_r', '6_r', '9_r'],

                'middle': ['3_l', '2_l', '2i_l', '1_l', '0', '1_r', '2i_r', '2_r', '3_r']}

    opposing_linemen = gap_dict[gap_key]
    return opposing_linemen


def proximity_to_play(contribution_sum, tech_pos, opposing_linemen):

    play_affected = False
    if contribution_sum > 0:
        play_affected = True
    elif tech_pos in opposing_linemen:
        play_affected = True

    return play_affected


def yards_if_in_proximity(yards, in_proximity):
    # Only count yards if player was in proximity to the play
    yards_allowed = 0
    if in_proximity:
        yards_allowed = yards
    return yards_allowed


def _get_avg_player_contributions(df: pd.DataFrame) -> Dict:
    # Get AVG of yds allowed, and contributions per tech position
    group_tech_sum = df.groupby(
        ['TechniqueNameLR']).sum().reset_index()[
        ['TechniqueNameLR', 'counted_yards', 'SoloTackle', 'AssistedTackle', 'TackleForLoss', 'ForcedFumble',
         'RecoveredFumble']]

    # Get count of all snaps per tech position
    group_tech_count = df.groupby(
        ['TechniqueNameLR']).count().reset_index()[
        ['TechniqueNameLR', 'counted_yards']]

    # Combine dataframes
    group_tech = pd.merge(group_tech_sum, group_tech_count, on='TechniqueNameLR')
    group_tech.columns = ['TechniqueNameLR', 'yards_avg', 'solo_tackle_avg', 'ass_tackle_avg', 'tackle_for_loss_avg',
                          'forced_fumble_avg', 'recovered_fumble_avg', 'snaps_played'
                          ]

    total_snaps = group_tech_count['counted_yards'].sum()
    columns = [col for col in group_tech_sum.columns]
    columns.remove('TechniqueNameLR')
    group_tech_wavg = pd.DataFrame(group_tech_sum.sum()[columns])
    group_tech_wavg = group_tech_wavg.T
    group_tech_wavg = group_tech_wavg.div(total_snaps)

    # Get total (avg * num of snaps)
    # group_tech['total_yds'] = group_tech['yards_avg'] * group_tech['snaps_played']
    # group_tech['total_solo_tackle'] = group_tech['solo_tackle_avg'] * group_tech['snaps_played']
    # group_tech['total_ass_tackle'] = group_tech['ass_tackle_avg'] * group_tech['snaps_played']
    # group_tech['total_tfl_tackle'] = group_tech['tackle_for_loss_avg'] * group_tech['snaps_played']
    # group_tech['total_forced_fumble'] = group_tech['forced_fumble_avg'] * group_tech['snaps_played']
    # group_tech['total_recovered_fumble'] = group_tech['recovered_fumble_avg'] * group_tech['snaps_played']

    # Totals
    # total_snaps = group_tech['snaps_played'].sum()
    #
    # total_yards = group_tech['total_yds'].sum()
    # total_solo_tackles = group_tech['total_solo_tackle'].sum()
    # total_ass_tackles = group_tech['total_ass_tackle'].sum()
    # total_tfl_tackles = group_tech['total_tfl_tackle'].sum()
    # total_forced_fumbles = group_tech['total_forced_fumble'].sum()
    # total_recovered_fumbles = group_tech['total_recovered_fumble'].sum()

    # Weighted AVGs
    # avg_yds_allowed = total_yards / total_snaps
    # avg_solo_tackles = total_solo_tackles / total_snaps
    # avg_ass_tackles = total_ass_tackles / total_snaps
    # avg_tfl_tackles = total_tfl_tackles / total_snaps
    # avg_forced_fumble = total_forced_fumbles / total_snaps
    # avg_recovered_fumble = total_recovered_fumbles / total_snaps
    #
    # # Dict
    # contribution_dict = {'avg_yards_allowed': [avg_yds_allowed],
    #                      'avg_solo_tackles': [avg_solo_tackles],
    #                      'avg_ass_tackles': [avg_ass_tackles],
    #                      'avg_tfl_tackles': [avg_tfl_tackles],
    #                      'avg_forced_fumble': [avg_forced_fumble],
    #                      'avg_recovered_fumble': [avg_recovered_fumble]}
    #
    # contribution_df = pd.DataFrame(contribution_dict)

    # group_tech = group_tech.sort_values(by='OffensiveYardage', ascending=True)
    # print(group_tech_sum)
    # print(group_tech_count)
    return group_tech_wavg


def main():
    df = pd.read_csv('./data/02_intermediate/rush.csv')
    df = df.copy()
    print(df.keys())

    # Use this number to determine if player affected the play
    df['contribution_sum'] = df[
        ['SoloTackle', 'AssistedTackle', 'ForcedFumble',
         'RecoveredFumble', 'TackleForLoss']
    ].sum(axis=1)

    df['offense_neg_yds'] = df[['ForcedFumble', 'RecoveredFumble', 'TackleForLoss']].sum()

    # Make gap and run direction columns
    print(df['RunDirection'].unique())
    df['gap'] = df['RunDirection'].apply(_extract_gap)
    df['run_dir'] = df['RunDirection'].apply(_extract_dir)

    # Determine which positions were in proximity to the run gap
    df['play_tech_positions'] = df.apply(lambda x: run_gap_map(gap=x['gap'], direction=x['run_dir']), axis=1)
    # Was the current player in one of those tech positions defending the run gap
    df['proximity_to_play'] = df.apply(
        lambda x: proximity_to_play(
            contribution_sum=x['contribution_sum'],
            tech_pos=x['TechniqueNameLR'],
            opposing_linemen=x['play_tech_positions']),
        axis=1)
    # Only count yards if player was in proximity to the play
    df['counted_yards'] = df.apply(
        lambda x: yards_if_in_proximity(
            yards=x['OffensiveYardage'],
            in_proximity=x['proximity_to_play']),
        axis=1)

    df.to_csv('./data/03_primary/rush_processed.csv', index=False)

    player_contibution_df = _get_avg_player_contributions(df=df)
    player_contibution_df.to_csv('./data/03_primary/rush_avg_player_contribution.csv', index=False)

    print(player_contibution_df)


if __name__ == '__main__':
    main()

# sacks
#   where did ball start
#   sack = +1
#       + modifiers
#           start pos, outcome

# firstydown = yards to go /100 / remaining downs
