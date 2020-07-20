import pandas as pd


def better_worse(diff):
    impact = None
    if diff > 0:
        impact = 'more_impactful'
    if diff < 0:
        impact = 'less_impactful'
    return impact


def main():
    rush_df = pd.read_csv('./data/04_graph_inputs/pdira.csv')
    pass_df = pd.read_csv('./data/04_graph_inputs/pdipa.csv')

    df = pd.merge(rush_df, pass_df, on=['position'], how='inner')
    # Calculate Positional
    # Positional Defensive Impact Weighted AVG
    df['pdia_w_avg'] = (df['pdira_sum'] + df['pdipa_sum']) / (df['pdira_count'] + df['pdipa_count'])

    # Calculate Cumulative Defense


    pdipa_sum = df['pdipa_sum'].sum()
    pdipa_count = df['pdipa_count'].sum()
    dipa_w_avg = pdipa_sum / pdipa_count

    # Defensive Impact Rush Weighted AVG


    pdira_sum = df['pdira_sum'].sum()
    pdira_count = df['pdira_count'].sum()
    dira_w_avg = pdira_sum / pdira_count



    # Defensive Impact Pass Weighted AVG

    dia = (dira_w_avg + dipa_w_avg) / 2

    df['dira_w_avg'] = dira_w_avg
    df['dipa_w_avg'] = dipa_w_avg
    df['dia'] = dia

    # Calculate differentials
    # Positional Defensive Impact Weighted AVG Differential
    df['pdiad_diff'] = df['pdia_w_avg'] - df['dia']

    # Rush
    df['pdiard_diff'] = df['pdira_avg'] - df['dira_w_avg']

    # Pass
    df['pdiapd_diff'] = df['pdipa_avg'] - df['dipa_w_avg']

    # This is hacky, I'm low on time, please don't judge
    graph_cols = ['dia', 'pdiad_diff', 'pdiard_diff', 'pdiapd_diff', 'dipa_w_avg', 'dira_w_avg', 'pdia_w_avg', 'pdipa_avg', 'pdira_avg']
    # for col in graph_cols:
    #    df[col] = df[col] * -1

    df['impact'] = df['pdiad_diff'].apply(better_worse)
    df['rush_impact'] = df['pdiard_diff'].apply(better_worse)
    df['pass_impact'] = df['pdiapd_diff'].apply(better_worse)


    print(df)
    df.to_csv('./data/04_graph_inputs/graph_input.csv', index=False)


if __name__ == '__main__':
    main()


