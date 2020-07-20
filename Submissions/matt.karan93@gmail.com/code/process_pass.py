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


def proximity_to_play(contribution_sum):
    play_affected = False
    if contribution_sum > 0:
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
        ['TechniqueNameLR', 'counted_yards', 'IsRushing', 'SoloTackle', 'AssistedTackle', 'Pressure', 'SoloSack',
         'AssistedSack', 'PassBreakup', 'Interception', 'TackleForLoss', 'ForcedFumble',
         'RecoveredFumble']]

    # Get count of all snaps per tech position
    group_tech_count = df.groupby(
        ['TechniqueNameLR']).count().reset_index()[
        ['TechniqueNameLR', 'counted_yards']]

    # Combine dataframes
    # group_tech = pd.merge(group_tech_sum, group_tech_count, on='TechniqueNameLR')
    # group_tech.columns = ['TechniqueNameLR', 'yards_avg', 'rushing_avg', 'solo_tackle_avg', 'ass_tackle_avg',
    #                       'pressure_avg', 'solo_sack_avg', 'ass_sack_avg', 'pass_breakup_avg', 'interception_avg',
    #                       'tackle_for_loss_avg', 'forced_fumble_avg', 'recovered_fumble_avg', 'snaps_played'
    #                       ]

    # Weighted AVGs
    total_snaps = group_tech_count['counted_yards'].sum()
    columns = [col for col in group_tech_sum.columns]
    columns.remove('TechniqueNameLR')
    group_tech_wavg = pd.DataFrame(group_tech_sum.sum()[columns])
    group_tech_wavg = group_tech_wavg.T
    group_tech_wavg = group_tech_wavg.div(total_snaps)

    # Get total (avg * num of snaps)
    # group_tech['total_yds'] = group_tech['yards_avg'] * group_tech['snaps_played']
    # group_tech['total_rushing'] = group_tech['rushing_avg'] * group_tech['snaps_played']
    # group_tech['total_solo_tackle'] = group_tech['solo_tackle_avg'] * group_tech['snaps_played']
    # group_tech['total_ass_tackle'] = group_tech['ass_tackle_avg'] * group_tech['snaps_played']
    # group_tech['total_pressure'] = group_tech['pressure_avg'] * group_tech['snaps_played']
    # group_tech['total_solo_sack'] = group_tech['solo_sack_avg'] * group_tech['snaps_played']
    # group_tech['total_ass_sack'] = group_tech['ass_sack_avg'] * group_tech['snaps_played']
    # group_tech['total_pass_breakup'] = group_tech['pass_breakup_avg'] * group_tech['snaps_played']
    # group_tech['total_interception'] = group_tech['interception_avg'] * group_tech['snaps_played']
    # group_tech['total_tfl_tackle'] = group_tech['tackle_for_loss_avg'] * group_tech['snaps_played']
    # group_tech['total_forced_fumble'] = group_tech['forced_fumble_avg'] * group_tech['snaps_played']
    # group_tech['total_recovered_fumble'] = group_tech['recovered_fumble_avg'] * group_tech['snaps_played']

    # Totals
    # total_snaps = group_tech['snaps_played'].sum()
    #
    # total_yards = group_tech['total_yds'].sum()
    # total_rushing = group_tech['total_rushing'].sum()
    # total_solo_tackles = group_tech['total_solo_tackle'].sum()
    # total_ass_tackles = group_tech['total_ass_tackle'].sum()
    # total_pressure = group_tech['total_pressure'].sum()
    # total_solo_sack = group_tech['total_solo_sack'].sum()
    # total_ass_sack = group_tech['total_ass_sack'].sum()
    # total_pass_breakup = group_tech['total_pass_breakup'].sum()
    # total_interception = group_tech['total_interception'].sum()
    # total_tfl_tackles = group_tech['total_tfl_tackle'].sum()
    # total_forced_fumbles = group_tech['total_forced_fumble'].sum()
    # total_recovered_fumbles = group_tech['total_recovered_fumble'].sum()

    # Weighted AVGs
    # avg_yds_allowed = total_yards / total_snaps
    # avg_is_rushing = total_rushing / total_snaps
    # avg_solo_tackles = total_solo_tackles / total_snaps
    # avg_ass_tackles = total_ass_tackles / total_snaps
    # avg_pressure = total_pressure / total_snaps
    # avg_solo_sack = total_solo_sack / total_snaps
    # avg_ass_sack = total_ass_sack / total_snaps
    # avg_pass_breakup = total_pass_breakup / total_snaps
    # avg_interception = total_interception / total_snaps
    # avg_tfl_tackles = total_tfl_tackles / total_snaps
    # avg_forced_fumble = total_forced_fumbles / total_snaps
    # avg_recovered_fumble = total_recovered_fumbles / total_snaps

    # Dict
    # contribution_dict = {'avg_yards_allowed': [avg_yds_allowed],
    #                      'avg_is_rushing': [avg_is_rushing],
    #                      'avg_solo_tackles': [avg_solo_tackles],
    #                      'avg_ass_tackles': [avg_ass_tackles],
    #                      'avg_pressure': [avg_pressure],
    #                      'avg_solo_sack': [avg_solo_sack],
    #                      'avg_ass_sack': [avg_ass_sack],
    #                      'avg_pass_breakup': [avg_pass_breakup],
    #                      'avg_interception': [avg_interception],
    #                      'avg_tfl_tackles': [avg_tfl_tackles],
    #                      'avg_forced_fumble': [avg_forced_fumble],
    #                      'avg_recovered_fumble': [avg_recovered_fumble]}

    # contribution_df = pd.DataFrame(contribution_dict)
    return group_tech_wavg


def main():
    df = pd.read_csv('./data/02_intermediate/pass.csv')
    df = df.copy()
    # print(df.keys())

    # Use this number to determine if player affected the play
    df['contribution_sum'] = df[
        ['IsRushing', 'SoloTackle', 'AssistedTackle', 'Pressure', 'SoloSack', 'AssistedSack', 'PassBreakup',
         'Interception', 'ForcedFumble', 'RecoveredFumble', 'TackleForLoss']
    ].sum(axis=1)

    df['offense_neg_yds'] = df[['SoloSack', 'AssistedSack', 'PassBreakup', 'Interception', 'ForcedFumble',
                                'RecoveredFumble', 'TackleForLoss']].sum()

    # Was the current player in one of those tech positions defending the run gap
    df['proximity_to_play'] = df.apply(
        lambda x: proximity_to_play(
            contribution_sum=x['contribution_sum']),
        axis=1)
    # Only count yards if player was in proximity to the play
    df['counted_yards'] = df.apply(
        lambda x: yards_if_in_proximity(
            yards=x['OffensiveYardage'],
            in_proximity=x['proximity_to_play']),
        axis=1)

    df.to_csv('./data/03_primary/pass_processed.csv', index=False)

    # TODO create contribution dictionary for each position
    player_contibution_df = _get_avg_player_contributions(df=df)
    player_contibution_df.to_csv('./data/03_primary/pass_avg_player_contribution.csv', index=False)

    print(player_contibution_df)


if __name__ == '__main__':
    main()

# sacks
#   where did ball start
#   sack = +1
#       + modifiers
#           start pos, outcome

# firstydown = yards to go /100 / remaining downs
