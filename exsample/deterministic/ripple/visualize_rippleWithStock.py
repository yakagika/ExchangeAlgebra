import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os

# CSVファイル名
csv_dir = 'exsample/deterministic/ripple/result/csv/withStock/'
fig_dir = 'exsample/deterministic/ripple/result/fig/withStock/'

cases = ['default', 'default-added', 'smallstock', 'smallstock-added', 'largestock', 'largestock-added']

# --- 個別時系列グラフ ---
for csv_basename in ['production', 'stock', 'profit', 'sales', 'demand']:
    for c in cases:
        csv_path = os.path.join(csv_dir, c, f'{csv_basename}.csv')
        if not os.path.exists(csv_path):
            continue
        df = pd.read_csv(csv_path)
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
        outdir = os.path.join(fig_dir, c)
        os.makedirs(outdir, exist_ok=True)
        plt.savefig(os.path.join(outdir, f'{csv_basename}_python.png'))
        plt.close(fig)

# --- 比較・差分グラフ（default vs default-added, smallstock vs smallstock-added, largestock vs largestock-added）---
def plot_comparison_and_difference(csv_basename, case1, case2, label1, label2, outprefix):
    path1 = os.path.join(csv_dir, case1, f'{csv_basename}.csv')
    path2 = os.path.join(csv_dir, case2, f'{csv_basename}.csv')
    if not (os.path.exists(path1) and os.path.exists(path2)):
        return
    df1 = pd.read_csv(path1)
    df2 = pd.read_csv(path2)
    time = df1['Time']
    series_names = df1.columns[1:]
    num_series = len(series_names)
    ncols = 3
    nrows = (num_series + ncols - 1) // ncols
    # 比較
    fig, axes = plt.subplots(nrows, ncols, figsize=(15, 10))
    axes = axes.flatten()
    for idx, name in enumerate(series_names):
        axes[idx].plot(time, df2[name], color='red', label=f"{idx+1},'{label2}'")
        axes[idx].plot(time, df1[name], color='blue', label=f"{idx+1},'{label1}'")
        axes[idx].set_title(str(name))
        axes[idx].grid(True)
        axes[idx].legend(loc='lower left', fontsize=8)
    for idx in range(num_series, nrows * ncols):
        axes[idx].axis('off')
    plt.suptitle(f"Comparison of {csv_basename} volume", fontsize=16, fontweight='bold')
    plt.tight_layout(rect=[0, 0, 1, 0.96])
    outdir = os.path.join(fig_dir, case2)
    os.makedirs(outdir, exist_ok=True)
    plt.savefig(os.path.join(outdir, f'Comparison_of_{csv_basename}_python.png'))
    plt.close(fig)
    # 差分
    fig, axes = plt.subplots(nrows, ncols, figsize=(15, 10))
    axes = axes.flatten()
    for idx, name in enumerate(series_names):
        diff = df2[name] - df1[name]
        axes[idx].plot(time, diff, color='blue', label=f"{idx+1}")
        axes[idx].set_title(str(idx+1))
        axes[idx].grid(True)
        axes[idx].legend(loc='lower left', fontsize=8)
    for idx in range(num_series, nrows * ncols):
        axes[idx].axis('off')
    plt.suptitle(f"Difference in {csv_basename} volume", fontsize=16, fontweight='bold')
    plt.tight_layout(rect=[0, 0, 1, 0.96])
    plt.savefig(os.path.join(outdir, f'Difference_in_{csv_basename}_python.png'))
    plt.close(fig)

# 比較・差分を描画するペア
pairs = [
    ('default', 'default-added', 'normal', 'added'),
    ('smallstock', 'smallstock-added', 'normal', 'added'),
    ('largestock', 'largestock-added', 'normal', 'added'),
]
for csv_basename in ['production', 'stock', 'profit', 'sales', 'demand']:
    for case1, case2, label1, label2 in pairs:
        plot_comparison_and_difference(csv_basename, case1, case2, label1, label2, case2) 