import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# CSVファイル名
csv_dir = 'exsample/basic/result/csv/simulateEx1/'
fig_dir = 'exsample/basic/result/fig/simulateEx1/'

cases = ['default-prod','plus-prod']

for csv_basename in ['stock', 'profit']:
    for c in cases:
        df = pd.read_csv(csv_dir + c + f'/{csv_basename}.csv')
        time = df['Time']
        series_names = df.columns[1:]
        num_series = len(series_names)
        ncols = 3
        nrows = (num_series + ncols - 1) // ncols
        fig, axes = plt.subplots(nrows, ncols, figsize=(15, 10),sharey=True)
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

