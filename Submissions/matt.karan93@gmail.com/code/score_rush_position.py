import pandas as pd


def pct_to_first(yds_to_go, offense_yards):
    """
    Percentage of progress towards first down on a play
    """
    pct = float(offense_yards) / float(yds_to_go)
    return pct


def pct_of_avg_yds_remaining(avg_yds_remain, offense_yds):
    """
    Percentage of avg yards per down needed for a first down that was allowed by the defense on a play
    Ex: 2nd & 6 = 2 yds needed per down (2nd, 3rd, and 4th down). A run for 1 yd would be 50% of the yards needed
    on that down.
    """
    pct_of_yds_remaining = float(offense_yds) / float(avg_yds_remain)
    return pct_of_yds_remaining


def avg_yds_per_down_to_first(down, yds_to_go):
    """
    AVG number of yards needed per down by the offense to get a first down
    Ex: 1st & 10 = 2.5yds per down
    """
    downs_remaining = 5 - down
    avg_yds_remaining_per_down = float(yds_to_go) / float(downs_remaining)
    return avg_yds_remaining_per_down


def tfl_pct_chage(tfl, yds_to_go, tfl_yds):
    tfl_yds = tfl_yds * tfl  # tfl = 1 if solo, 0.5 if assist
    new_yds_to_go = yds_to_go + tfl_yds

    pct_change = (new_yds_to_go - yds_to_go) / yds_to_go
    return pct_change


def contribute_in_redzone(player_contribution, start_yd):
    contribution_pt = 0
    if player_contribution > 0:
        if start_yd < 10:
            contribution_pt = 1
        elif start_yd < 20:
            contribution_pt = 0.5
    return contribution_pt


def pct_away_from_endzone(start_yd, tfl_yds, def_sideoffield):
    # Calculate distance from offense endzone
    if def_sideoffield == 'Oppo':
        start = start_yd
    if def_sideoffield == 'Own':
        start = 100 - start_yd
    pct_to_offense_endzone = tfl_yds / start
    return pct_to_offense_endzone


def pct_away_from_def_endzone(start_yd, offense_yds, def_sideoffield):
    # Calculate distance from offense endzone
    if def_sideoffield == 'Oppo':
        start = 100 - start_yd
    if def_sideoffield == 'Own':
        start = start_yd
    pct_to_offense_endzone = offense_yds / start
    return pct_to_offense_endzone


def yds_allow(start_yd, down, yds_to_go, touchdown, safety, player_safety, def_sideoffield, offense_yards,
              proximity_to_play, tfl, tfl_yds, contributions, f_fumble, rec_fumble, turnover, avgs):

    # AVG yards from ADI (Avg defensive impact)
    avg_yds = avgs['counted_yards'].iloc[0]

    # Can be positive or negative
    yds_allow_over_avg = offense_yards - avg_yds

    # AVG yards to go per down to get first
    avg_yds_per_down = avg_yds_per_down_to_first(down=down, yds_to_go=yds_to_go)

    # pct of ACG yards to go per down allowed by defense
    pct_of_avg_yds_per_down = pct_of_avg_yds_remaining(avg_yds_remain=avg_yds_per_down, offense_yds=offense_yards)

    # pct to first down
    pct_to_first_down = pct_to_first(yds_to_go=yds_to_go, offense_yards=offense_yards)

    redzone_contribution = contribute_in_redzone(player_contribution=contributions, start_yd=start_yd)

    # Add up points
    total_points = 0

    total_points += redzone_contribution

    if proximity_to_play is True:
        if touchdown == 1:
            total_points -= 6
        else:
            pass
        if safety == 1:
            total_points += 2 * player_safety  # solo = 1, assist = 0.5
        else:
            pass

        if f_fumble == 1:
            if turnover == 1:
                total_points += 2
                if rec_fumble == 1:
                    total_points += 1
        elif f_fumble == 0:
            if rec_fumble == 1:
                total_points += 1
        else:
            pass

        if tfl > 0:
            tmp_points = tfl_pct_chage(tfl=tfl,
                                       yds_to_go=yds_to_go,
                                       tfl_yds=tfl_yds)
            total_points += tmp_points

            # pct pushed away from defense endzone
            pct_away_endzone = pct_away_from_endzone(start_yd=start_yd, tfl_yds=tfl_yds, def_sideoffield=def_sideoffield)
            total_points += pct_away_endzone
        else:
            # If more than 100%, add all yds
            # Calc pct to first down
            tmp_yds_to_first = 0
            if pct_to_first_down > 1:
                tmp_yds_to_first -= 1
            elif 0 < pct_to_first_down < 1:
                tmp_yds_to_first -= pct_to_first_down

            # Calc pct to TD
            pct_towards_endzone = pct_away_from_def_endzone(start_yd=start_yd, offense_yds=offense_yards,
                                                            def_sideoffield=def_sideoffield)

            subtotal = tmp_yds_to_first + pct_towards_endzone
            total_points -= subtotal
    else:
        # Potentially add some other metrics here to determine how far away from potentially being involved in the play
        total_points -= 0

    return total_points


def score_plays(df, avgs):
    df = df.copy()
    df['yds_allow_points'] = df[
        ['StartYard', 'Down', 'ToGo', 'FirstDown', 'Touchdown', 'Safety', 'PlayerSafety', 'DefSideOfField', 'OffensiveYardage',
         'proximity_to_play', 'TackleForLoss', 'tfl_yards', 'offense_neg_yds', 'ForcedFumble', 'RecoveredFumble', 'Turnover']
    ].apply(
        lambda x: yds_allow(
            start_yd=x['StartYard'],
            down=x['Down'],
            yds_to_go=x['ToGo'],
            touchdown=x['Touchdown'],
            safety=x['Safety'],
            player_safety=x['PlayerSafety'],
            def_sideoffield=x['DefSideOfField'],
            offense_yards=x['OffensiveYardage'],
            proximity_to_play=x['proximity_to_play'],
            tfl=x['TackleForLoss'],
            tfl_yds=x['tfl_yards'],
            contributions=x['offense_neg_yds'],
            f_fumble=x['ForcedFumble'],
            rec_fumble=x['RecoveredFumble'],
            turnover=x['Turnover'],
            avgs=avgs),
        axis=1)

    return df


def main():
    df_avg = pd.read_csv('./data/03_primary/rush_avg_player_contribution.csv')
    df = pd.read_csv('./data/03_primary/rush_processed.csv')

    unique_positions = df['TechniqueNameLR'].unique()
    print(unique_positions)

    master_position_df = score_plays(df=df, avgs=df_avg)
    # print(master_position_df['yds_allow_points'].mean())
    # exit()
    pdiaa_dict = {}
    for position in unique_positions:
        df_pos = df.loc[df['TechniqueNameLR'] == position]
        df_pos = df_pos.copy()
        score_df = score_plays(df=df_pos, avgs=df_avg)
        # Positional Defensive Rush Impact Above Average
        pdiaa_avg = score_df['yds_allow_points'].mean()
        pdiaa_sum = score_df['yds_allow_points'].sum()
        pdiaa_count = score_df['yds_allow_points'].count()
        pdiaa_dict[position] = [pdiaa_avg, pdiaa_sum, pdiaa_count]

    # Convert dictionary to DataFrame
    pdiaa_df = pd.DataFrame.from_dict(pdiaa_dict, orient='index')
    pdiaa_df = pdiaa_df.reset_index()
    pdiaa_df.columns = ['position', 'pdira_avg', 'pdira_sum', 'pdira_count']
    pdiaa_df = pdiaa_df.sort_values(by=['position'], ascending=True)
    pdiaa_df.to_csv('./data/04_graph_inputs/pdira.csv', index=False)
    print(pdiaa_df)


if __name__ == '__main__':
    main()

# POSITIONAL
# PDIRA -- positional defensive impact rush avg
# PDIPA
# PDIA

# CUMULATIVE DEFENSE
# DRIA
# DPIA
# DIA

