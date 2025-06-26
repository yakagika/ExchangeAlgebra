import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# CSVファイル名
csv_dir = 'exsample/deterministic/ripple/result/csv/withoutStock/'
fig_dir = 'exsample/deterministic/ripple/result/fig/withoutStock/'

cases = ['default','default-added']


#分割グラフ
for c in cases:
    df = pd.read_csv(csv_dir + c + '/production.csv')
    time = df['Time']
    series_names = df.columns[1:]
    num_series = len(series_names)

    # サブプロットの行・列数（例: 3x3）
    ncols = 3
    nrows = (num_series + ncols - 1) // ncols

    fig, axes = plt.subplots(nrows, ncols, figsize=(15, 10))
    axes = axes.flatten()

    for idx, name in enumerate(series_names):
        axes[idx].plot(time, df[name], color='blue')
        axes[idx].set_title(str(name))
        axes[idx].grid(True)

    # 余ったサブプロットを非表示
    for idx in range(num_series, nrows * ncols):
        axes[idx].axis('off')

    plt.suptitle("Production", fontsize=16, fontweight='bold')
    plt.tight_layout(rect=[0, 0, 1, 0.96])
    plt.savefig(fig_dir + c + '/production_python.png')

# --- ここから比較・差分グラフの追加 ---

# 2つのケースのデータを読み込み
case1 = 'default'
case2 = 'default-added'
df1 = pd.read_csv(csv_dir + case1 + '/production.csv')
df2 = pd.read_csv(csv_dir + case2 + '/production.csv')
time = df1['Time']
series_names = df1.columns[1:]
num_series = len(series_names)

ncols = 3
nrows = (num_series + ncols - 1) // ncols

# --- 比較グラフ ---
fig, axes = plt.subplots(nrows, ncols, figsize=(15, 10))
axes = axes.flatten()
for idx, name in enumerate(series_names):
    axes[idx].plot(time, df2[name], color='red', label=f"{idx+1},'added'")
    axes[idx].plot(time, df1[name], color='blue', label=f"{idx+1},'normal'")
    axes[idx].set_title(str(name))
    axes[idx].grid(True)
    axes[idx].legend(loc='lower left', fontsize=8)
for idx in range(num_series, nrows * ncols):
    axes[idx].axis('off')
plt.suptitle("Comparison of production volume", fontsize=16, fontweight='bold')
plt.tight_layout(rect=[0, 0, 1, 0.96])
plt.savefig(fig_dir + 'default-added/Comparison_of_production_volume_python.png')
plt.close(fig)

# --- 差分グラフ ---
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
plt.suptitle("Difference in production volume", fontsize=16, fontweight='bold')
plt.tight_layout(rect=[0, 0, 1, 0.96])
plt.savefig(fig_dir + 'default-added/Difference_in_production_volume_python.png')
plt.close(fig)
# --- ここまで追加 ---

