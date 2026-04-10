import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# 1. CSVから投入係数行列を読み込む
#    CSVの1列目（行名）が部門名、1行目（列名）も部門名になっている前提
A_df = pd.read_csv('io_for_py.csv', index_col=0)

# DataFrame → NumPy 行列
A = A_df.values

# 2. レオンチーフ逆行列を計算
I = np.eye(A.shape[0])
L = np.linalg.inv(I - A)

# DataFrame に変換してわかりやすく
L_df = pd.DataFrame(
    L,
    index=A_df.index,
    columns=A_df.columns
)

# 3. 波及効果（総乗数 multiplier）を計算
#  レオンチェフ逆行列の各列の合計
multipliers = L_df.sum(axis=0)
multipliers_df = multipliers.to_frame(name='TotalMultiplier')

# 結果を表示
print("=== Leontief Inverse Matrix (L) ===")
print(L_df.round(4))
print("\n=== Sectoral Multipliers ===")
print(multipliers_df.round(4))

# 結果をCSVに出力
L_df.to_csv('leontief_inverse_py.csv')
multipliers_df.to_csv('sectoral_multipliers.csv')

# 4. 波及効果をバー・チャートで可視化
plt.figure(figsize=(10, 6))
multipliers_df['TotalMultiplier'].sort_values().plot(kind='bar')
plt.title('Sectoral Total Multipliers')
plt.ylabel('Multiplier')
plt.xlabel('Sector')
plt.tight_layout()
plt.show()