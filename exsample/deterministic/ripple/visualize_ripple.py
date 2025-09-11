import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# CSVファイル名
csv_dir = 'exsample/deterministic/ripple/result/csv/withoutStock/'
fig_dir = 'exsample/deterministic/ripple/result/fig/withoutStock/'

cases = ['default','default-added']


def plot_comparison_and_difference(csv_basename, ylabel=None):
    df1 = pd.read_csv(csv_dir + 'default/' + f'{csv_basename}.csv')
    df2 = pd.read_csv(csv_dir + 'default-added/' + f'{csv_basename}.csv')
    time = df1['Time']
    series_names = df1.columns[1:]
    num_series = len(series_names)
    ncols = 3
    nrows = (num_series + ncols - 1) // ncols

    # 比較グラフ
    fig, axes = plt.subplots(nrows, ncols, figsize=(15, 10))
    axes = axes.flatten()
    for idx, name in enumerate(series_names):
        axes[idx].plot(time, df2[name], color='red', label=f"{idx+1},'added'")
        axes[idx].plot(time, df1[name], color='blue', label=f"{idx+1},'normal'")
        axes[idx].set_title(str(name))
        axes[idx].grid(True)
        axes[idx].legend(loc='lower left', fontsize=8)
        if ylabel:
            axes[idx].set_ylabel(ylabel)
    for idx in range(num_series, nrows * ncols):
        axes[idx].axis('off')
    plt.suptitle(f"Comparison of {csv_basename} volume", fontsize=16, fontweight='bold')
    plt.tight_layout(rect=[0, 0, 1, 0.96])
    plt.savefig(fig_dir + f'default-added/Comparison_of_{csv_basename}_python.png')
    plt.close(fig)

    # 差分グラフ
    fig, axes = plt.subplots(nrows, ncols, figsize=(15, 10))
    axes = axes.flatten()
    for idx, name in enumerate(series_names):
        diff = df2[name] - df1[name]
        axes[idx].plot(time, diff, color='blue', label=f"{idx+1}")
        axes[idx].set_title(str(idx+1))
        axes[idx].grid(True)
        axes[idx].legend(loc='lower left', fontsize=8)
        if ylabel:
            axes[idx].set_ylabel(ylabel)
    for idx in range(num_series, nrows * ncols):
        axes[idx].axis('off')
    plt.suptitle(f"Difference in {csv_basename} volume", fontsize=16, fontweight='bold')
    plt.tight_layout(rect=[0, 0, 1, 0.96])
    plt.savefig(fig_dir + f'default-added/Difference_in_{csv_basename}_python.png')
    plt.close(fig)

# 各指標について実行
for name in ['production','stock', 'profit', 'sales', 'demand']:
    plot_comparison_and_difference(name)

# --- ここから追加: stock, profit, sales, demand の個別描画 ---
for csv_basename in ['production','stock', 'profit', 'sales', 'demand']:
    for c in cases:
        df = pd.read_csv(csv_dir + c + f'/{csv_basename}.csv')
        time = df['Time']
        series_names = df.columns[1:]
        num_series = len(series_names)
        ncols = 3
        nrows = (num_series + ncols - 1) // ncols
        fig, axes = plt.subplots(nrows, ncols, figsize=(15, 10))
        axes = axes.flatten()
        for idx, name in enumerate(series_names):
            axes[idx].plot(time, df[name], color='blue')
            axes[idx].set_title(str(name))
            axes[idx].grid(True)
        for idx in range(num_series, nrows * ncols):
            axes[idx].axis('off')
        plt.suptitle(csv_basename.capitalize(), fontsize=16, fontweight='bold')
        plt.tight_layout(rect=[0, 0, 1, 0.96])
        plt.savefig(fig_dir + c + f'/{csv_basename}_python.png')
        plt.close(fig)
# --- ここまで追加 ---

