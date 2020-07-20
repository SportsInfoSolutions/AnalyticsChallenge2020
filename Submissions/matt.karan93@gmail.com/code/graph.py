import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

import seaborn as sns


def graph_pdiapd_diff(df):
    graph_df = df.copy()
    graph_df = graph_df.sort_values(by=['pdiad_diff'], ascending=True)
    sns.set(style='whitegrid')
    f, axes = plt.subplots(figsize=(20, 10))
    sns.despine()
    pal = ['salmon', 'steelblue']

    sns.barplot(data=graph_df, x='pdiad_diff', y='position', hue='impact', ax=axes, alpha=0.8, palette=pal)

    # sns.barplot(data=events, x='event', y='vel_avg', palette=pal, ax=axes)
    #axes.set_xticklabels(labels=axes.get_xticklabels(), rotation=45, horizontalalignment='right')
    f.suptitle("Positional Defensive Impact Average Differential", fontsize=24)
    axes.set_xlabel("More or Less Impactful", fontsize=14)
    axes.set_ylabel("Player Position", fontsize=14)

    plt.legend(loc='upper right')

    plt.savefig('./data/05_graphs/pdiapd_diff.png')


def graph_pdiapd_diff_rush(df):
    graph_df = df.copy()
    graph_df = graph_df.sort_values(by=['pdiard_diff'], ascending=True)
    sns.set(style='whitegrid')
    f, axes = plt.subplots(figsize=(20, 10))
    sns.despine()
    pal = ['salmon', 'steelblue']

    sns.barplot(data=graph_df, x='pdiard_diff', y='position', hue='rush_impact', ax=axes, alpha=0.8, palette=pal)

    # sns.barplot(data=events, x='event', y='vel_avg', palette=pal, ax=axes)
    #axes.set_xticklabels(labels=axes.get_xticklabels(), rotation=45, horizontalalignment='right')
    f.suptitle("Positional Defensive Impact Rush Average Differential", fontsize=24)
    axes.set_xlabel("More or Less Impactful", fontsize=14)
    axes.set_ylabel("Player Position", fontsize=14)

    plt.legend(loc='upper right')

    plt.savefig('./data/05_graphs/pdiapd_rush_diff.png')


def graph_pdiapd_diff_pass(df):
    graph_df = df.copy()
    graph_df = graph_df.sort_values(by=['pdiapd_diff'], ascending=True)
    sns.set(style='whitegrid')
    f, axes = plt.subplots(figsize=(20, 10))
    sns.despine()
    pal = ['salmon', 'steelblue']

    sns.barplot(data=graph_df, x='pdiapd_diff', y='position', hue='pass_impact', ax=axes, alpha=0.8, palette=pal)

    # sns.barplot(data=events, x='event', y='vel_avg', palette=pal, ax=axes)
    #axes.set_xticklabels(labels=axes.get_xticklabels(), rotation=45, horizontalalignment='right')
    f.suptitle("Positional Defensive Impact Rush Average Differential", fontsize=24)
    axes.set_xlabel("More or Less Impactful", fontsize=14)
    axes.set_ylabel("Player Position", fontsize=14)

    plt.legend(loc='upper right')

    plt.savefig('./data/05_graphs/pdiapd_pass_diff.png')


def graph_pdia_w_avg(df):
    graph_df = df.copy()

    graph_df = graph_df.sort_values(by=['pdia_w_avg'], ascending=True)

    sns.set(style='whitegrid')
    f, axes = plt.subplots(figsize=(20, 10))
    sns.despine()
    pal = ['steelblue']

    sns.barplot(data=graph_df, x='pdia_w_avg', y='position', color='steelblue', ax=axes, alpha=0.8)
    plt.axvline(0.044509, color='darkred')

    #sns.lineplot(data=graph_df, x='dia', y='position', color='orange', dashes=True, ax=axes)
    #axes.legend()
    pdia_patch = mpatches.Patch(color='steelblue', label='Positional Defensive Impact Weighted AVG')
    dia_patch = mpatches.Patch(color='darkred', label='Defensive Impact Weighted AVG')
    plt.legend(handles=[pdia_patch, dia_patch])

    # sns.barplot(data=events, x='event', y='vel_avg', palette=pal, ax=axes)
    #axes.set_xticklabels(labels=axes.get_xticklabels(), rotation=45, horizontalalignment='right')
    f.suptitle("Positional Defensive Impact Weighted Average", fontsize=24)
    axes.set_xlabel("More or Less Impactful", fontsize=14)
    axes.set_ylabel("Player Position", fontsize=14)

    plt.savefig('./data/05_graphs/pdia_w_avg.png')


def graph_pdira_w_avg(df):
    graph_df = df.copy()

    graph_df = graph_df.sort_values(by=['pdira_avg'], ascending=True)

    sns.set(style='whitegrid')
    f, axes = plt.subplots(figsize=(20, 10))
    sns.despine()
    pal = ['steelblue']

    sns.barplot(data=graph_df, x='pdira_avg', y='position', color='steelblue', ax=axes, alpha=0.8)
    plt.axvline(0.041576, color='darkred')

    #sns.lineplot(data=graph_df, x='dia', y='position', color='orange', dashes=True, ax=axes)
    #axes.legend()
    pdia_patch = mpatches.Patch(color='steelblue', label='Positional Defensive Impact Rush Weighted AVG')
    dia_patch = mpatches.Patch(color='darkred', label='Defensive Impact Weighted AVG')
    plt.legend(handles=[pdia_patch, dia_patch])

    # sns.barplot(data=events, x='event', y='vel_avg', palette=pal, ax=axes)
    #axes.set_xticklabels(labels=axes.get_xticklabels(), rotation=45, horizontalalignment='right')
    f.suptitle("Positional Defensive Impact Weighted Average", fontsize=24)
    axes.set_xlabel("More or Less Impactful", fontsize=14)
    axes.set_ylabel("Player Position", fontsize=14)

    plt.savefig('./data/05_graphs/pdira_avg.png')


def graph_pdipa_w_avg(df):
    graph_df = df.copy()

    graph_df = graph_df.sort_values(by=['pdipa_avg'], ascending=True)

    sns.set(style='whitegrid')
    f, axes = plt.subplots(figsize=(20, 10))
    sns.despine()
    pal = ['steelblue']

    sns.barplot(data=graph_df, x='pdipa_avg', y='position', color='steelblue', ax=axes, alpha=0.8)
    plt.axvline(0.047441, color='darkred')

    #sns.lineplot(data=graph_df, x='dia', y='position', color='orange', dashes=True, ax=axes)
    #axes.legend()
    pdia_patch = mpatches.Patch(color='steelblue', label='Positional Defensive Impact Rush Weighted AVG')
    dia_patch = mpatches.Patch(color='darkred', label='Defensive Impact Weighted AVG')
    plt.legend(handles=[pdia_patch, dia_patch])

    # sns.barplot(data=events, x='event', y='vel_avg', palette=pal, ax=axes)
    #axes.set_xticklabels(labels=axes.get_xticklabels(), rotation=45, horizontalalignment='right')
    f.suptitle("Positional Defensive Impact Weighted Average", fontsize=24)
    axes.set_xlabel("More or Less Impactful", fontsize=14)
    axes.set_ylabel("Player Position", fontsize=14)

    plt.savefig('./data/05_graphs/pdipa_avg.png')



def graph_pdia_w_avg_pos(df):
    graph_df = df.copy()
    sort_list = ['outside_l', '9_l', '6_l', '7_l', '5_l', '4_l', '4i_l',
                                                   '3_l', '2_l', '2i_l', '1_l', '0', '1_r', '2i_r', '2_r', '3_r',
                                                   '4i_r', '4_r', '5_r', '7_r', '6_r', '9_r', 'outside_r', 'off ball']
    sorterIndex = dict(zip(sort_list, range(len(sort_list))))
    graph_df['pos_rank'] = graph_df['position'].map(sorterIndex)
    graph_df = graph_df.sort_values(by=['pos_rank'])
    print(graph_df)


    sns.set(style='whitegrid')
    f, axes = plt.subplots(figsize=(20, 10))
    sns.despine()
    pal = ['steelblue']

    sns.barplot(data=graph_df, x='pdia_w_avg', y='position', color='steelblue', ax=axes, alpha=0.8)
    plt.axvline(0.044509, color='darkred')

    #sns.lineplot(data=graph_df, x='dia', y='position', color='orange', dashes=True, ax=axes)
    #axes.legend()
    pdia_patch = mpatches.Patch(color='steelblue', label='Positional Defensive Impact Weighted AVG')
    dia_patch = mpatches.Patch(color='darkred', label='Defensive Impact Weighted AVG')
    plt.legend(handles=[pdia_patch, dia_patch])

    # sns.barplot(data=events, x='event', y='vel_avg', palette=pal, ax=axes)
    #axes.set_xticklabels(labels=axes.get_xticklabels(), rotation=45, horizontalalignment='right')
    f.suptitle("Positional Defensive Impact Weighted Average", fontsize=24)
    axes.set_xlabel("More or Less Impactful", fontsize=14)
    axes.set_ylabel("Player Position", fontsize=14)

    plt.savefig('./data/05_graphs/pdia_w_avg_pos.png')


def main():
    df = pd.read_csv('./data/04_graph_inputs/graph_input.csv')
    print(df.head())
    graph_pdiapd_diff(df=df)
    graph_pdia_w_avg(df=df)
    graph_pdia_w_avg_pos(df=df)
    graph_pdira_w_avg(df=df)
    graph_pdipa_w_avg(df=df)
    graph_pdiapd_diff_rush(df=df)
    graph_pdiapd_diff_pass(df=df)


if __name__ == '__main__':
    main()