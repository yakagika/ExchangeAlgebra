import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os
import re
try:
    from scipy.stats import wilcoxon
    _has_scipy = True
except Exception:
    _has_scipy = False

# CSVファイル名
csv_dir = 'exsample/deterministic/ripple/result/csv/withStock/'
fig_dir = 'exsample/deterministic/ripple/result/fig/withStock/'

cases = ['default', 'default-added', 'small', 'small-added', 'large', 'large-added']

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
    ('small', 'small-added', 'normal', 'added'),
    ('large', 'large-added', 'normal', 'added'),
]
for csv_basename in ['production', 'stock', 'profit', 'sales', 'demand']:
    for case1, case2, label1, label2 in pairs:
        plot_comparison_and_difference(csv_basename, case1, case2, label1, label2, case2) 

# --- 3つのケース（default, smallstock, largestock）の比較グラフ ---
def plot_three_case_comparison(csv_basename):
    """default, small, largeの3つのケースを比較するグラフを作成"""
    cases_to_compare = ['small-added','default-added', 'large-added']
    colors = ['blue', 'red', 'green']
    labels = ['Small Stock', 'Default',  'Large Stock']
    
    # データの読み込みと存在確認
    dataframes = []
    for case in cases_to_compare:
        path = os.path.join(csv_dir, case, f'{csv_basename}.csv')
        if os.path.exists(path):
            dataframes.append((case, pd.read_csv(path)))
        else:
            print(f"Warning: {path} not found")
            return
    
    if len(dataframes) < 3:
        print(f"Warning: Not enough data for {csv_basename}")
        return
    
    # 時系列データの取得
    time = dataframes[0][1]['Time']
    series_names = dataframes[0][1].columns[1:]
    num_series = len(series_names)
    
    # グラフのレイアウト設定
    ncols = 3
    nrows = (num_series + ncols - 1) // ncols
    fig, axes = plt.subplots(nrows, ncols, figsize=(18, 12))
    axes = axes.flatten()
    
    # 各系列について3つのケースを比較
    for idx, name in enumerate(series_names):
        for i, (case, df) in enumerate(dataframes):
            axes[idx].plot(time, df[name], color=colors[i], label=labels[i], linewidth=2)
        
        axes[idx].set_title(f'{name}', fontsize=12, fontweight='bold')
        axes[idx].grid(True, alpha=0.3)
        axes[idx].legend(loc='upper left', fontsize=10)
        axes[idx].set_xlabel('Time')
        axes[idx].set_ylabel(csv_basename.capitalize())
    
    # 未使用のサブプロットを非表示
    for idx in range(num_series, nrows * ncols):
        axes[idx].axis('off')
    
    plt.suptitle(f'Comparison of {csv_basename.capitalize()} across Three Cases\n(Default vs Small Stock vs Large Stock)', 
                 fontsize=16, fontweight='bold')
    plt.tight_layout(rect=[0, 0, 1, 0.96])
    
    # 保存先ディレクトリの作成と保存
    outdir = os.path.join(fig_dir, 'comparison')
    os.makedirs(outdir, exist_ok=True)
    plt.savefig(os.path.join(outdir, f'Three_case_comparison_{csv_basename}.png'), 
                dpi=300, bbox_inches='tight')
    plt.close(fig)
    
    print(f"Created three-case comparison graph for {csv_basename}")

# --- 3つのケースのproduction差分を一つのグラフ上にプロット ---
def plot_production_differences():
    """small、default、largeのproduction差分を一つのグラフ上にプロット"""
    pairs = [
        ('small', 'small-added', 'Small Stock'),
        ('default', 'default-added', 'Default'),
        ('large', 'large-added', 'Large Stock')
    ]
    colors = ['blue', 'red', 'green']
    
    # データの読み込みと存在確認
    differences_data = {}
    time = None
    
    for i, (case1, case2, label) in enumerate(pairs):
        path1 = os.path.join(csv_dir, case1, 'production.csv')
        path2 = os.path.join(csv_dir, case2, 'production.csv')
        
        if not (os.path.exists(path1) and os.path.exists(path2)):
            print(f"Warning: {path1} or {path2} not found")
            continue
            
        df1 = pd.read_csv(path1)
        df2 = pd.read_csv(path2)
        
        if time is None:
            time = df1['Time']
        
        # 各エージェントの差分を計算
        series_names = df1.columns[1:]
        for name in series_names:
            diff = df2[name] - df1[name]
            if name not in differences_data:
                differences_data[name] = []
            differences_data[name].append((diff, label, colors[i]))
    
    if not differences_data:
        print("Warning: No data available for production differences")
        return
    
    # グラフの作成（10象限）
    series_names = list(differences_data.keys())
    ncols = 3
    nrows = 4  # 10個のエージェントを3列×4行で配置（最後の行は1個のみ）
    fig, axes = plt.subplots(nrows, ncols, figsize=(18, 12))
    axes = axes.flatten()
    
    # 各エージェントについて3つの差分を同時にプロット
    for idx, name in enumerate(series_names):
        if idx >= len(axes):
            break
            
        # 統計情報を計算して視覚化
        labels_with_stats = []
        for diff, label, color in differences_data[name]:
            # 最大値、最小値、振幅を計算
            max_val = diff.max()
            min_val = diff.min()
            amplitude = max_val - min_val
            
            # 統計情報付きのラベルを作成
            new_label = f"{label} (Max: {max_val:.3f}, Min: {min_val:.3f}, Amp: {amplitude:.3f})"
            
            # 折れ線グラフをプロット（ラベル付き）
            axes[idx].plot(time, diff, color=color, linewidth=2, label=new_label)
            
            # 最大値と最小値の位置を横線で表示（凡例なし）
            axes[idx].axhline(y=max_val, color=color, linestyle=':', alpha=0.7, linewidth=1)
            axes[idx].axhline(y=min_val, color=color, linestyle=':', alpha=0.7, linewidth=1)
            
            # 振幅の範囲を透過した幅として表示（凡例なし）
            axes[idx].axhspan(min_val, max_val, alpha=0.1, color=color)
        
        # 凡例を表示
        axes[idx].legend(loc='lower left', fontsize=7)
        
        axes[idx].set_title(f'{name}', fontsize=12, fontweight='bold')
        axes[idx].grid(True, alpha=0.3)
        axes[idx].set_xlabel('Time')
        axes[idx].set_ylabel('Production Difference')
        axes[idx].axhline(y=0, color='black', linestyle='--', alpha=0.5)
    
    # 未使用のサブプロットを非表示
    for idx in range(len(series_names), nrows * ncols):
        axes[idx].axis('off')
    
    plt.suptitle('Production Differences (Added Demand - Normal Demand)\nAcross Three Stock Out Rate Cases', 
                 fontsize=16, fontweight='bold')
    plt.tight_layout(rect=[0, 0, 1, 0.96])
    
    # 保存先ディレクトリの作成と保存
    outdir = os.path.join(fig_dir, 'comparison')
    os.makedirs(outdir, exist_ok=True)
    plt.savefig(os.path.join(outdir, 'Production_differences_comparison.png'), 
                dpi=300, bbox_inches='tight')
    plt.close(fig)
    
    print("Created production differences comparison graph")

# 3つのケースの比較グラフを作成（productionとinventoryのみ）
for csv_basename in ['production', 'stock']:
    plot_three_case_comparison(csv_basename)

# production差分の比較グラフを作成
plot_production_differences()

# --- seed 別CSVから差分の平均±SDエンベロープを作成 ---
def _collect_seed_files(case, basename):
    """case 配下の *_seed<INT>.csv を収集して {seed: path} を返す"""
    case_dir = os.path.join(csv_dir, case)
    if not os.path.isdir(case_dir):
        return {}
    pattern = re.compile(rf"^{re.escape(basename)}_seed(\d+)\.csv$")
    seed_map = {}
    for fname in os.listdir(case_dir):
        m = pattern.match(fname)
        if m:
            seed_map[int(m.group(1))] = os.path.join(case_dir, fname)
    return seed_map


def _load_diff_series(case_base, case_added, basename="production", use_abs=True):
    """seed ごとの差分時系列(added - base)を返す。各seedは行平均系列。"""
    base_files = _collect_seed_files(case_base, basename)
    added_files = _collect_seed_files(case_added, basename)
    common_seeds = sorted(set(base_files.keys()) & set(added_files.keys()))

    if not common_seeds:
        print(f"Warning: no common seeds for {case_base} vs {case_added}")
        return None, None

    diffs = []
    time = None
    for sd in common_seeds:
        df_base = pd.read_csv(base_files[sd])
        df_added = pd.read_csv(added_files[sd])

        # Time で揃え、共通列のみを使用して整合性を担保
        common_cols = [c for c in df_base.columns if c != "Time" and c in df_added.columns]
        if not common_cols:
            print(f"Warning: no common columns for seed {sd} in {case_base} vs {case_added}")
            continue

        df_base = df_base[["Time"] + common_cols]
        df_added = df_added[["Time"] + common_cols]
        merged = df_base.merge(df_added, on="Time", suffixes=("_base", "_added"))
        if merged.empty:
            print(f"Warning: no common Time rows for seed {sd} in {case_base} vs {case_added}")
            continue

        if time is None:
            time = merged["Time"].to_numpy()

        base_vals = merged[[f"{c}_base" for c in common_cols]].apply(pd.to_numeric, errors="coerce").to_numpy()
        added_vals = merged[[f"{c}_added" for c in common_cols]].apply(pd.to_numeric, errors="coerce").to_numpy()

        # 行平均の差分（各時点でエージェント平均との差分を平均）
        row_mean_diff = (added_vals - base_vals).mean(axis=1)
        diffs.append(np.abs(row_mean_diff) if use_abs else row_mean_diff)

    return time, np.vstack(diffs)


def plot_seeded_diff_envelope():
    pairs = [
        ("small", "small-added", "Small", "blue"),
        ("default", "default-added", "Default", "red"),
        ("large", "large-added", "Large", "green"),
    ]

    fig, ax = plt.subplots(1, 1, figsize=(12, 6))
    any_plotted = False
    mean_series_by_label = []

    for base, added, label, color in pairs:
        time, diff_matrix = _load_diff_series(base, added, "production", use_abs=True)
        if diff_matrix is None:
            continue
        mean = diff_matrix.mean(axis=0)
        sd = diff_matrix.std(axis=0, ddof=1) if diff_matrix.shape[0] > 1 else np.zeros_like(mean)
        ax.plot(time, mean, color=color, linewidth=2, label=f"{label} mean")
        ax.fill_between(time, mean - sd, mean + sd, color=color, alpha=0.2, label=f"{label} ±SD")
        mean_series_by_label.append((label, mean))
        any_plotted = True

    if not any_plotted:
        print("Warning: no seed-diff data available for envelope plot")
        return

    ax.axhline(y=0, color="black", linestyle="--", alpha=0.5)
    ax.set_title("Production Difference Envelope (Added - Normal)\nMean ± SD across seeds")
    ax.set_xlabel("Time")
    ax.set_ylabel("Production Difference (mean across agents)")
    # 差分は正負が混在するため symlog を使う
    ax.set_yscale("symlog", linthresh=0.1)
    ax.grid(True, alpha=0.3)
    ax.legend(loc="upper left")

    outdir = os.path.join(fig_dir, "seed_envelope")
    os.makedirs(outdir, exist_ok=True)
    plt.tight_layout()
    plt.savefig(os.path.join(outdir, "production_diff_envelope_mean_sd.png"), dpi=300)
    plt.close(fig)

    # mean 系列の箱ひげ図は別関数で作成


def plot_mean_boxplot_signed():
    """mean系列（符号付き）の箱ひげ図を作成する"""
    pairs = [
        ("small", "small-added", "Small"),
        ("default", "default-added", "Default"),
        ("large", "large-added", "Large"),
    ]
    mean_series_by_label = []
    for base, added, label in pairs:
        time, diff_matrix = _load_diff_series(base, added, "production", use_abs=False)
        if diff_matrix is None:
            continue
        mean = diff_matrix.mean(axis=0)
        mean_series_by_label.append((label, mean))

    if not mean_series_by_label:
        print("Warning: no data available for mean boxplot")
        return

    outdir = os.path.join(fig_dir, "seed_envelope")
    os.makedirs(outdir, exist_ok=True)
    fig, ax = plt.subplots(1, 1, figsize=(8, 5))
    labels = [lbl for lbl, _ in mean_series_by_label]
    data = [series for _, series in mean_series_by_label]
    ax.boxplot(data, tick_labels=labels, showfliers=True)
    ax.set_title("Distribution of Mean Production Difference (Added - Normal)\nAcross Time")
    ax.set_ylabel("Mean Production Difference")
    ax.axhline(y=0, color="black", linestyle="--", alpha=0.5)
    # 正負を含むため symlog を使用
    ax.set_yscale("symlog", linthresh=0.1)
    ax.grid(True, axis="y", alpha=0.3)
    plt.tight_layout()
    plt.savefig(os.path.join(outdir, "production_diff_mean_boxplot_signed.png"), dpi=300)
    plt.close(fig)


plot_seeded_diff_envelope()
plot_mean_boxplot_signed()

# --- Seedごとの振幅指標 → 対応検定 ---
def _load_mean_series_per_seed(case_name, basename="production"):
    """seedごとの行平均系列（Timeで整合）を {seed: series} で返す"""
    files = _collect_seed_files(case_name, basename)
    if not files:
        return {}
    series_by_seed = {}
    for sd, path in files.items():
        df = pd.read_csv(path)
        if "Time" not in df.columns:
            continue
        cols = [c for c in df.columns if c != "Time"]
        if not cols:
            continue
        vals = df[cols].apply(pd.to_numeric, errors="coerce").to_numpy()
        series_by_seed[sd] = vals.mean(axis=1)
    return series_by_seed


import numpy as np

# SciPy (Friedman / Wilcoxon)
_has_scipy = True
try:
    from scipy.stats import friedmanchisquare, wilcoxon
except Exception:
    _has_scipy = False


import numpy as np

# SciPy (Friedman / Wilcoxon)
_has_scipy = True
try:
    from scipy.stats import friedmanchisquare, wilcoxon
except Exception:
    _has_scipy = False


def _align_seed_series(base_series, added_series):
    common = sorted(set(base_series.keys()) & set(added_series.keys()))
    if not common:
        return [], []
    pairs = [(base_series[s], added_series[s]) for s in common]
    return common, pairs


# ===== 指標：差分系列 d(t)=added-normal の max|d(t)| =====

def _amplitude_maxabs(x):
    x = np.asarray(x, dtype=float)
    return float(np.nanmax(np.abs(x)))


def _amplitude_log_maxabs(x, eps=1e-12):
    return float(np.log(_amplitude_maxabs(x) + eps))


# ===== 多重比較補正 =====

def _holm_adjust(pvals):
    pvals = np.asarray(pvals, dtype=float)
    m = len(pvals)
    order = np.argsort(pvals)
    adj = np.empty(m, dtype=float)

    prev = 0.0
    for k, idx in enumerate(order):
        val = min(1.0, (m - k) * pvals[idx])
        val = max(prev, val)
        adj[idx] = val
        prev = val
    return adj


def _run_wilcoxon_greater(diffs):
    diffs = np.asarray(diffs, dtype=float)
    diffs = diffs[np.isfinite(diffs)]
    if len(diffs) == 0 or np.all(diffs == 0):
        return np.nan
    if not _has_scipy:
        return np.nan
    res = wilcoxon(diffs, alternative="greater", zero_method="zsplit")
    return float(res.pvalue)


# ===== Cliff's delta =====

def cliffs_delta(x, y):
    """
    Cliff's delta: δ = P(X>Y) - P(Y>X)
    x, y: 1D arrays (finite values expected). Ties contribute 0.
    Returns: (delta, n_x, n_y)
    """
    x = np.asarray(x, dtype=float)
    y = np.asarray(y, dtype=float)
    x = x[np.isfinite(x)]
    y = y[np.isfinite(y)]
    if len(x) == 0 or len(y) == 0:
        return np.nan, int(len(x)), int(len(y))

    # O(n*m) but fine for ~100 seeds; simple and transparent.
    gt = 0
    lt = 0
    for xi in x:
        gt += int(np.sum(xi > y))
        lt += int(np.sum(xi < y))

    n = len(x) * len(y)
    delta = (gt - lt) / n
    return float(delta), int(len(x)), int(len(y))


def _delta_label(delta):
    """慣例的な大きさカテゴリ（参考）"""
    if not np.isfinite(delta):
        return "NA"
    ad = abs(delta)
    if ad < 0.147:
        return "negligible"
    if ad < 0.33:
        return "small"
    if ad < 0.474:
        return "medium"
    return "large"


def run_amplitude_tests_friedman_maxabs_with_cliffs_delta():
    base_conds = ["small", "default", "large"]
    added_conds = ["small-added", "default-added", "large-added"]

    series_base = {c: _load_mean_series_per_seed(c, "production") for c in base_conds}
    series_added = {c: _load_mean_series_per_seed(c, "production") for c in added_conds}

    amp_max = {}
    amp_log = {}

    for base, added in zip(base_conds, added_conds):
        seeds, pairs = _align_seed_series(series_base[base], series_added[added])
        if not pairs:
            print(f"Warning: no common seeds for {base} vs {added}")
            continue

        max_map = {}
        log_map = {}
        for s, (x_base, x_added) in zip(seeds, pairs):
            d = np.asarray(x_added, dtype=float) - np.asarray(x_base, dtype=float)
            max_map[s] = _amplitude_maxabs(d)
            log_map[s] = _amplitude_log_maxabs(d)

        amp_max[base] = max_map
        amp_log[base] = log_map

    if not all(c in amp_max for c in base_conds):
        print("Warning: missing condition maps; cannot run Friedman.")
        return

    common_seeds = sorted(
        set(amp_max["small"]) &
        set(amp_max["default"]) &
        set(amp_max["large"])
    )
    if not common_seeds:
        print("Warning: no common seeds across all conditions.")
        return

    small_max = np.array([amp_max["small"][s] for s in common_seeds])
    def_max   = np.array([amp_max["default"][s] for s in common_seeds])
    large_max = np.array([amp_max["large"][s] for s in common_seeds])

    small_log = np.array([amp_log["small"][s] for s in common_seeds])
    def_log   = np.array([amp_log["default"][s] for s in common_seeds])
    large_log = np.array([amp_log["large"][s] for s in common_seeds])

    print(f"Common seeds: n={len(common_seeds)}")

    if not _has_scipy:
        print("Warning: scipy not available; cannot run Friedman/Wilcoxon.")
        return

    fr_max = friedmanchisquare(small_max, def_max, large_max)
    fr_log = friedmanchisquare(small_log, def_log, large_log)

    print(f"[Friedman] max|d(t)|: stat={fr_max.statistic:.6g}, p={fr_max.pvalue:.6g}")
    print(f"[Friedman] log(max|d|+eps): stat={fr_log.statistic:.6g}, p={fr_log.pvalue:.6g}")

    def posthoc_with_effects(label, a, b, c):
        # one-sided Wilcoxon for ordered alternative
        p1 = _run_wilcoxon_greater(b - a)  # Default > Small
        p2 = _run_wilcoxon_greater(c - b)  # Large > Default
        raw = np.array([p1, p2])
        adj = _holm_adjust(raw)

        # Cliff's delta (effect size) for the same comparisons
        d1, nx1, ny1 = cliffs_delta(b, a)  # Default vs Small
        d2, nx2, ny2 = cliffs_delta(c, b)  # Large vs Default

        print(f"[Post-hoc Wilcoxon one-sided + Holm] {label}")
        print(f"  Default > Small: raw p={raw[0]:.6g}, Holm p={adj[0]:.6g} | "
              f"Cliff's δ={d1:.4g} ({_delta_label(d1)}) [n={nx1}x{ny1}]")
        print(f"  Large > Default: raw p={raw[1]:.6g}, Holm p={adj[1]:.6g} | "
              f"Cliff's δ={d2:.4g} ({_delta_label(d2)}) [n={nx2}x{ny2}]")

    if fr_max.pvalue < 0.05:
        posthoc_with_effects("max|d(t)|", small_max, def_max, large_max)
    else:
        print("max|d(t)|: Friedman not significant; skip post-hoc.")

    if fr_log.pvalue < 0.05:
        posthoc_with_effects("log(max|d|+eps)", small_log, def_log, large_log)
    else:
        print("log(max|d|+eps): Friedman not significant; skip post-hoc.")


# 実行
run_amplitude_tests_friedman_maxabs_with_cliffs_delta()

# --- default-addedケースにおける複数ペアのSeed別比較 ---
def _series_amplitude(x):
    x = np.asarray(x, dtype=float)
    x = x[np.isfinite(x)]
    if len(x) == 0:
        return np.nan
    return float(np.nanmax(x) - np.nanmin(x))


def _series_std(x):
    x = np.asarray(x, dtype=float)
    x = x[np.isfinite(x)]
    if len(x) == 0:
        return np.nan
    if len(x) == 1:
        return 0.0
    return float(np.nanstd(x, ddof=1))


def _series_cv(x, eps=1e-12):
    x = np.asarray(x, dtype=float)
    x = x[np.isfinite(x)]
    if len(x) == 0:
        return np.nan
    mu = float(np.nanmean(np.abs(x)))
    if not np.isfinite(mu) or mu <= eps:
        return np.nan
    sigma = float(np.nanstd(x, ddof=1)) if len(x) > 1 else 0.0
    return sigma / mu


def _series_logdiff_std(x):
    x = np.asarray(x, dtype=float)
    x = x[np.isfinite(x)]
    if len(x) < 2:
        return np.nan
    # Quantity series should be non-negative; clip for numerical safety.
    lx = np.log1p(np.clip(x, a_min=0.0, a_max=None))
    d = np.diff(lx)
    if len(d) == 0:
        return np.nan
    if len(d) == 1:
        return 0.0
    return float(np.nanstd(d, ddof=1))


def _safe_ratio(num, den, eps=1e-12):
    if not np.isfinite(num) or not np.isfinite(den):
        return np.nan
    if abs(den) <= eps:
        return np.nan
    return float(num / den)


def _extract_agent_columns(columns, prefix):
    pattern = re.compile(rf"^{re.escape(prefix)}(\d+)$")
    mapping = {}
    for col in columns:
        m = pattern.match(col)
        if m:
            mapping[int(m.group(1))] = col
    return mapping


def _run_wilcoxon_paired(x, y, alternative="greater"):
    x = np.asarray(x, dtype=float)
    y = np.asarray(y, dtype=float)
    mask = np.isfinite(x) & np.isfinite(y)
    x = x[mask]
    y = y[mask]
    n = len(x)
    if n == 0:
        return np.nan, 0
    if not _has_scipy:
        return np.nan, n
    if np.allclose(x, y):
        return 1.0, n
    try:
        res = wilcoxon(x, y, alternative=alternative, zero_method="zsplit")
        return float(res.pvalue), n
    except Exception:
        return np.nan, n


def _load_seeded_pair_data(case_name, basename1, basename2, prefix1, prefix2, start_time=50):
    files1 = _collect_seed_files(case_name, basename1)
    files2 = _collect_seed_files(case_name, basename2)
    common_seeds = sorted(set(files1.keys()) & set(files2.keys()))
    if not common_seeds:
        print(f"Warning: no common seeds for {case_name}: {basename1} vs {basename2}")
        return None

    seed_series_1 = {}
    seed_series_2 = {}
    seed_diff = {}
    metric_rows = []

    for sd in common_seeds:
        df1 = pd.read_csv(files1[sd])
        df2 = pd.read_csv(files2[sd])
        if "Time" not in df1.columns or "Time" not in df2.columns:
            print(f"Warning: Time column missing for seed {sd} ({basename1} vs {basename2})")
            continue

        df1["Time"] = pd.to_numeric(df1["Time"], errors="coerce")
        df2["Time"] = pd.to_numeric(df2["Time"], errors="coerce")
        df1 = df1[df1["Time"] >= start_time]
        df2 = df2[df2["Time"] >= start_time]

        col_map1 = _extract_agent_columns(df1.columns, prefix1)
        col_map2 = _extract_agent_columns(df2.columns, prefix2)
        common_agents = sorted(set(col_map1.keys()) & set(col_map2.keys()))
        if not common_agents:
            print(f"Warning: no common agents for seed {sd} ({basename1} vs {basename2})")
            continue

        cols1 = [col_map1[a] for a in common_agents]
        cols2 = [col_map2[a] for a in common_agents]
        merged = df1[["Time"] + cols1].merge(df2[["Time"] + cols2], on="Time", how="inner")
        if merged.empty:
            print(f"Warning: no common Time rows for seed {sd} ({basename1} vs {basename2})")
            continue

        arr1 = merged[cols1].apply(pd.to_numeric, errors="coerce").to_numpy(dtype=float)
        arr2 = merged[cols2].apply(pd.to_numeric, errors="coerce").to_numpy(dtype=float)

        mean1 = np.nanmean(arr1, axis=1)
        mean2 = np.nanmean(arr2, axis=1)
        diff = mean1 - mean2
        time = merged["Time"].to_numpy(dtype=float)

        seed_series_1[sd] = pd.Series(mean1, index=time)
        seed_series_2[sd] = pd.Series(mean2, index=time)
        seed_diff[sd] = pd.Series(diff, index=time)

        amp1 = _series_amplitude(mean1)
        amp2 = _series_amplitude(mean2)
        std1 = _series_std(mean1)
        std2 = _series_std(mean2)
        cv1 = _series_cv(mean1)
        cv2 = _series_cv(mean2)
        ldstd1 = _series_logdiff_std(mean1)
        ldstd2 = _series_logdiff_std(mean2)
        metric_rows.append({
            "seed": sd,
            "amp1": amp1,
            "amp2": amp2,
            "amp_diff": amp1 - amp2,
            "amp_ratio": _safe_ratio(amp1, amp2),
            "std1": std1,
            "std2": std2,
            "std_diff": std1 - std2,
            "std_ratio": _safe_ratio(std1, std2),
            "cv1": cv1,
            "cv2": cv2,
            "cv_diff": cv1 - cv2,
            "cv_ratio": _safe_ratio(cv1, cv2),
            "logdiff_std1": ldstd1,
            "logdiff_std2": ldstd2,
            "logdiff_std_diff": ldstd1 - ldstd2,
            "logdiff_std_ratio": _safe_ratio(ldstd1, ldstd2),
            "mean_abs_diff": float(np.nanmean(np.abs(diff))),
            "max_abs_diff": _amplitude_maxabs(diff),
        })

    if not seed_diff:
        print(f"Warning: no valid seed data for {case_name}: {basename1} vs {basename2}")
        return None

    time_sets = [set(s.index.tolist()) for s in seed_diff.values()]
    common_time = sorted(set.intersection(*time_sets))
    if not common_time:
        print(f"Warning: no common Time grid across seeds ({basename1} vs {basename2})")
        return None

    used_seeds = sorted(seed_diff.keys())
    common_time = np.asarray(common_time, dtype=float)

    matrix1 = np.vstack([seed_series_1[s].reindex(common_time).to_numpy(dtype=float) for s in used_seeds])
    matrix2 = np.vstack([seed_series_2[s].reindex(common_time).to_numpy(dtype=float) for s in used_seeds])
    diff_matrix = np.vstack([seed_diff[s].reindex(common_time).to_numpy(dtype=float) for s in used_seeds])

    metrics_df = pd.DataFrame(metric_rows).set_index("seed")
    metrics_df = metrics_df.loc[used_seeds]

    return {
        "time": common_time,
        "seeds": used_seeds,
        "matrix1": matrix1,
        "matrix2": matrix2,
        "diff_matrix": diff_matrix,
        "metrics": metrics_df,
    }


def plot_comparison_pairs(start_time=50):
    """default-addedケースで stylized facts の方向付き仮説をSeed別に検証する。"""
    comparison_pairs = [
        {
            "name": "Production_vs_Sales",
            "basename1": "production",
            "basename2": "sales",
            "prefix1": "Production_",
            "prefix2": "Sales_",
            "label1": "Production",
            "label2": "Sales",
            "color1": "red",
            "color2": "blue",
            "diff_color": "purple",
            "title": "Production vs Sales",
            "hypothesis": "Production is more variable than Sales",
        },
        {
            "name": "Purchases_vs_InputTotal",
            "basename1": "purchases",
            "basename2": "input",
            "prefix1": "Purchase_",
            "prefix2": "Input_",
            "label1": "Purchase (Delivery)",
            "label2": "Input Usage",
            "color1": "red",
            "color2": "blue",
            "diff_color": "purple",
            "title": "Purchases (Delivery) vs Input Usage",
            "hypothesis": "Input delivery is more variable than Input usage",
        },
        {
            "name": "Material_vs_Stock",
            "basename1": "material",
            "basename2": "stock",
            "prefix1": "Material_",
            "prefix2": "Stock_",
            "label1": "Material",
            "label2": "Stock",
            "color1": "red",
            "color2": "blue",
            "diff_color": "purple",
            "title": "Material vs Stock",
            "hypothesis": "Input inventories are more variable than Output inventories",
        },
    ]

    outdir = os.path.join(fig_dir, "seed_comparison", "default-added")
    os.makedirs(outdir, exist_ok=True)

    for pair in comparison_pairs:
        result = _load_seeded_pair_data(
            "default-added",
            pair["basename1"],
            pair["basename2"],
            pair["prefix1"],
            pair["prefix2"],
            start_time=start_time,
        )
        if result is None:
            continue

        time = result["time"]
        seeds = result["seeds"]
        matrix1 = result["matrix1"]
        matrix2 = result["matrix2"]
        diff_matrix = result["diff_matrix"]
        metrics = result["metrics"]

        n_seed = len(seeds)
        mean1 = np.nanmean(matrix1, axis=0)
        mean2 = np.nanmean(matrix2, axis=0)
        mean_diff = np.nanmean(diff_matrix, axis=0)

        sd1 = np.nanstd(matrix1, axis=0, ddof=1) if n_seed > 1 else np.zeros_like(mean1)
        sd2 = np.nanstd(matrix2, axis=0, ddof=1) if n_seed > 1 else np.zeros_like(mean2)
        sd_diff = np.nanstd(diff_matrix, axis=0, ddof=1) if n_seed > 1 else np.zeros_like(mean_diff)

        # Seed横断のエンベロープ図（系列1・系列2・差分）
        fig, axes = plt.subplots(2, 1, figsize=(12, 9), sharex=True)
        ax_top, ax_bottom = axes

        ax_top.plot(time, mean1, color=pair["color1"], linewidth=2, label=f'{pair["label1"]} mean')
        ax_top.fill_between(time, mean1 - sd1, mean1 + sd1, color=pair["color1"], alpha=0.2,
                            label=f'{pair["label1"]} ±SD')
        ax_top.plot(time, mean2, color=pair["color2"], linewidth=2, label=f'{pair["label2"]} mean')
        ax_top.fill_between(time, mean2 - sd2, mean2 + sd2, color=pair["color2"], alpha=0.2,
                            label=f'{pair["label2"]} ±SD')
        ax_top.set_title(f'{pair["title"]} (default-added, t >= {start_time})\nMean ± SD across seeds (n={n_seed})')
        ax_top.set_ylabel("Volume (mean across agents)")
        ax_top.grid(True, alpha=0.3)
        ax_top.legend(loc="upper left")

        ax_bottom.plot(time, mean_diff, color=pair["diff_color"], linewidth=2,
                       label=f'{pair["label1"]} - {pair["label2"]} mean')
        ax_bottom.fill_between(time, mean_diff - sd_diff, mean_diff + sd_diff,
                               color=pair["diff_color"], alpha=0.2, label="Difference ±SD")
        ax_bottom.axhline(y=0, color="black", linestyle="--", alpha=0.5)
        ax_bottom.set_xlabel("Time")
        ax_bottom.set_ylabel("Difference")
        ax_bottom.grid(True, alpha=0.3)
        ax_bottom.legend(loc="upper left")

        plt.tight_layout()
        plt.savefig(os.path.join(outdir, f'{pair["name"]}_seed_envelope.png'), dpi=300)
        plt.close(fig)

        # Seedごとの統計指標を保存（0-1正規化は使わない）
        metrics.to_csv(os.path.join(outdir, f'{pair["name"]}_seed_metrics.csv'))

        # 方向付き仮説の対応検定（label1 > label2）
        metric_specs = [
            ("amp", "Amplitude (max-min)"),
            ("std", "Std over time"),
            ("cv", "CV = std / |mean|"),
            ("logdiff_std", "Std of diff(log1p(x))"),
        ]
        stats_rows = []
        raw_pvals = []
        for key, label in metric_specs:
            c1 = f"{key}1"
            c2 = f"{key}2"
            cr = f"{key}_ratio"
            pval, n_pair = _run_wilcoxon_paired(metrics[c1], metrics[c2], alternative="greater")
            raw_pvals.append(pval)

            ratio = pd.to_numeric(metrics[cr], errors="coerce")
            ratio = ratio[np.isfinite(ratio.to_numpy(dtype=float))]
            med_ratio = float(ratio.median()) if len(ratio) > 0 else np.nan
            share_gt1 = float((ratio > 1.0).mean()) if len(ratio) > 0 else np.nan

            dval, nx, ny = cliffs_delta(metrics[c1], metrics[c2])
            stats_rows.append(
                {
                    "metric": key,
                    "metric_label": label,
                    "alternative": f'{pair["label1"]} > {pair["label2"]}',
                    "wilcoxon_pvalue_one_sided": pval,
                    "n_paired": n_pair,
                    "median_ratio_1_over_2": med_ratio,
                    "share_ratio_gt_1": share_gt1,
                    "cliffs_delta": dval,
                    "cliffs_delta_label": _delta_label(dval),
                    "n_x": nx,
                    "n_y": ny,
                }
            )

        adj_pvals = _holm_adjust(raw_pvals) if len(raw_pvals) > 0 else np.array([])
        for i in range(len(stats_rows)):
            stats_rows[i]["holm_adjusted_pvalue"] = float(adj_pvals[i]) if i < len(adj_pvals) else np.nan

        print(f"[Seeded comparison] {pair['name']} (n_seed={n_seed})")
        print(f"  Hypothesis: {pair['hypothesis']}")
        for row in stats_rows:
            print(
                f"  {row['metric_label']}: p(one-sided)={row['wilcoxon_pvalue_one_sided']:.6g}, "
                f"Holm={row['holm_adjusted_pvalue']:.6g}, "
                f"median ratio={row['median_ratio_1_over_2']:.4g}, "
                f"share(ratio>1)={row['share_ratio_gt_1']:.3f}"
            )

        # 検定結果の保存
        stats_df = pd.DataFrame(stats_rows)
        stats_df.to_csv(os.path.join(outdir, f'{pair["name"]}_seed_stats.csv'), index=False)

        # Seedごとのボックスプロット（レベル比較）
        fig, axes = plt.subplots(2, 2, figsize=(12, 9))
        axes = axes.flatten()
        plot_cfg = [
            ("amp", "Amplitude (max-min)"),
            ("std", "Std over Time"),
            ("cv", "CV = std / |mean|"),
            ("logdiff_std", "Std of diff(log1p(x))"),
        ]
        for i, (key, title) in enumerate(plot_cfg):
            ax = axes[i]
            ax.boxplot(
                [metrics[f"{key}1"], metrics[f"{key}2"], metrics[f"{key}_diff"]],
                tick_labels=[pair["label1"], pair["label2"], f'{pair["label1"]}-{pair["label2"]}'],
                showfliers=True,
            )
            ax.set_title(title)
            ax.axhline(y=0, color="black", linestyle="--", alpha=0.4)
            ax.grid(True, axis="y", alpha=0.3)

        plt.suptitle(
            f'{pair["title"]} Seed-wise Variability Metrics (t >= {start_time}, n={n_seed})\n'
            f'Hypothesis: {pair["label1"]} > {pair["label2"]}'
        )
        plt.tight_layout(rect=[0, 0, 1, 0.93])
        plt.savefig(os.path.join(outdir, f'{pair["name"]}_seed_boxplot.png'), dpi=300)
        plt.close(fig)

        # 比率ボックスプロット（1を基準）
        fig, axes = plt.subplots(2, 2, figsize=(12, 9))
        axes = axes.flatten()
        for i, (key, title) in enumerate(plot_cfg):
            ax = axes[i]
            ratio = pd.to_numeric(metrics[f"{key}_ratio"], errors="coerce")
            ratio = ratio[np.isfinite(ratio.to_numpy(dtype=float))]
            ax.boxplot([ratio], tick_labels=[f'{pair["label1"]}/{pair["label2"]}'], showfliers=True)
            ax.axhline(y=1.0, color="black", linestyle="--", alpha=0.5)
            ax.set_title(f"{title} Ratio")
            ax.grid(True, axis="y", alpha=0.3)

        plt.suptitle(
            f'{pair["title"]} Ratio Boxplots (t >= {start_time}, n={n_seed})\n'
            f'Baseline: ratio = 1'
        )
        plt.tight_layout(rect=[0, 0, 1, 0.93])
        plt.savefig(os.path.join(outdir, f'{pair["name"]}_seed_ratio_boxplot.png'), dpi=300)
        plt.close(fig)

        # 旧名互換（参照される可能性があるため残す）
        fig, axes = plt.subplots(1, 2, figsize=(12, 5))
        axes[0].boxplot(
            [metrics["amp1"], metrics["amp2"], metrics["amp_diff"]],
            tick_labels=[pair["label1"], pair["label2"], f'{pair["label1"]}-{pair["label2"]}'],
            showfliers=True,
        )
        axes[0].set_title("Amplitude (max - min)")
        axes[0].set_ylabel("Amplitude")
        axes[0].axhline(y=0, color="black", linestyle="--", alpha=0.4)
        axes[0].grid(True, axis="y", alpha=0.3)

        axes[1].boxplot(
            [metrics["std1"], metrics["std2"], metrics["std_diff"]],
            tick_labels=[pair["label1"], pair["label2"], f'{pair["label1"]}-{pair["label2"]}'],
            showfliers=True,
        )
        axes[1].set_title("Std over Time")
        axes[1].set_ylabel("Std")
        axes[1].axhline(y=0, color="black", linestyle="--", alpha=0.4)
        axes[1].grid(True, axis="y", alpha=0.3)

        plt.suptitle(
            f'{pair["title"]} Seed-wise Variation Metrics (legacy view)\n'
            f'(t >= {start_time}, n={n_seed})'
        )
        plt.tight_layout(rect=[0, 0, 1, 0.94])
        plt.savefig(os.path.join(outdir, f'{pair["name"]}_seed_boxplot_legacy.png'), dpi=300)
        plt.close(fig)

        print(f"Created seeded comparison outputs for {pair['name']}")


# 複数ペアの比較グラフ（Seed別）を作成
plot_comparison_pairs()

# --- 変動率の比較グラフ ---
def plot_variation_rate_graphs():
    """default-addedケースにおける複数のペアの変動率比較グラフを作成（標準偏差と変動幅表示付き）"""
    
    # 比較ペアの定義
    comparison_pairs = [
        {
            'name': 'Production_vs_Sales_variation',
            'file1': 'production.csv',
            'file2': 'sales.csv',
            'prefix1': 'Production_',
            'prefix2': 'Sales_',
            'label1': 'Production',
            'label2': 'Sales',
            'color1': 'red',
            'color2': 'blue',
            'title': 'Production vs Sales Variation Rate Comparison'
        },
        {
            'name': 'Purchases_vs_InputTotal_variation',
            'file1': 'purchases.csv',
            'file2': 'input.csv',
            'prefix1': 'Purchase_',
            'prefix2': 'Input_',
            'label1': 'Purchase (Delivery)',
            'label2': 'Input Usage',
            'color1': 'red',
            'color2': 'blue',
            'title': 'Purchases (Delivery) vs Input Usage Variation Rate Comparison'
        },
        {
            'name': 'Material_vs_Stock_variation',
            'file1': 'material.csv',
            'file2': 'stock.csv',
            'prefix1': 'Material_',
            'prefix2': 'Stock_',
            'label1': 'Material',
            'label2': 'Stock',
            'color1': 'red',
            'color2': 'blue',
            'title': 'Material vs Stock Variation Rate Comparison'
        }
    ]
    
    for pair in comparison_pairs:
        # データファイルのパス
        file1_path = os.path.join(csv_dir, 'default-added', pair['file1'])
        file2_path = os.path.join(csv_dir, 'default-added', pair['file2'])
        
        # ファイルの存在確認
        if not (os.path.exists(file1_path) and os.path.exists(file2_path)):
            print(f"Warning: {file1_path} or {file2_path} not found for {pair['name']}")
            continue
        
        # データの読み込み
        df1 = pd.read_csv(file1_path)
        df2 = pd.read_csv(file2_path)
        
        # 50期以降のデータのみを使用
        df1 = df1[df1['Time'] >= 50]
        df2 = df2[df2['Time'] >= 50]
        
        time = df1['Time']
        
        # 列名の抽出
        series1_names = [col for col in df1.columns[1:] if col.startswith(pair['prefix1'])]
        series2_names = [col for col in df2.columns[1:] if col.startswith(pair['prefix2'])]
        
        # エージェント番号を抽出して対応関係を作成
        agent1_numbers = [int(name.split('_')[1]) for name in series1_names]
        agent2_numbers = [int(name.split('_')[1]) for name in series2_names]
        
        # 共通のエージェント番号を見つける
        common_agents = sorted(set(agent1_numbers) & set(agent2_numbers))
        
        if not common_agents:
            print(f"Warning: No matching agent numbers found for {pair['name']}")
            continue
        
        num_series = len(common_agents)
        
        # グラフのレイアウト設定
        ncols = 3
        nrows = (num_series + ncols - 1) // ncols
        fig, axes = plt.subplots(nrows, ncols, figsize=(18, 12))
        axes = axes.flatten()
        
        # 各エージェントについて変動率を比較
        for idx, agent_num in enumerate(common_agents):
            if idx >= len(axes):
                break
                
            # 対応する列名を取得
            col1 = f"{pair['prefix1']}{agent_num}"
            col2 = f"{pair['prefix2']}{agent_num}"
            
            # データを取得
            data1 = df1[col1]
            data2 = df2[col2]
            
            # 変動率を計算（前期間比の変化率）
            variation_rate1 = data1.pct_change() * 100  # パーセント表示
            variation_rate2 = data2.pct_change() * 100  # パーセント表示
            
            # 両方の変動率をプロット
            axes[idx].plot(time[1:], variation_rate1[1:], color=pair['color1'], linewidth=2, label=pair['label1'], alpha=0.8)
            axes[idx].plot(time[1:], variation_rate2[1:], color=pair['color2'], linewidth=2, label=pair['label2'], alpha=0.8)
            
            # 統計情報を計算
            # data1の統計
            mean1 = variation_rate1[1:].mean()
            std1 = variation_rate1[1:].std()
            max1 = variation_rate1[1:].max()
            min1 = variation_rate1[1:].min()
            
            # data2の統計
            mean2 = variation_rate2[1:].mean()
            std2 = variation_rate2[1:].std()
            max2 = variation_rate2[1:].max()
            min2 = variation_rate2[1:].min()
            
            # 変動幅を可視化
            # data1の変動幅
            axes[idx].axhline(y=max1, color=pair['color1'], linestyle=':', alpha=0.7, linewidth=1)
            axes[idx].axhline(y=min1, color=pair['color1'], linestyle=':', alpha=0.7, linewidth=1)
            axes[idx].axhspan(min1, max1, alpha=0.1, color=pair['color1'])
            
            # data2の変動幅
            axes[idx].axhline(y=max2, color=pair['color2'], linestyle=':', alpha=0.7, linewidth=1)
            axes[idx].axhline(y=min2, color=pair['color2'], linestyle=':', alpha=0.7, linewidth=1)
            axes[idx].axhspan(min2, max2, alpha=0.1, color=pair['color2'])
            
            # タイトルに標準偏差情報を追加
            title = f'Agent {agent_num}\n{pair["label1"]} Std: {std1:.2f}%, {pair["label2"]} Std: {std2:.2f}%'
            axes[idx].set_title(title, fontsize=10, fontweight='bold')
            
            # グリッドと凡例を設定
            axes[idx].grid(True, alpha=0.3)
            axes[idx].legend(loc='upper left', fontsize=8)
            axes[idx].set_xlabel('Time')
            axes[idx].set_ylabel('Variation Rate (%)')
            
            # ゼロラインを追加
            axes[idx].axhline(y=0, color='black', linestyle='-', alpha=0.3)
        
        # 未使用のサブプロットを非表示
        for idx in range(num_series, nrows * ncols):
            axes[idx].axis('off')
        
        plt.suptitle(f'{pair["title"]} in Default-Added Case\n({pair["label1"]}: {pair["color1"]}, {pair["label2"]}: {pair["color2"]}, Shaded: Variation Range)', 
                     fontsize=16, fontweight='bold')
        plt.tight_layout(rect=[0, 0, 1, 0.96])
        
        # 保存先ディレクトリの作成と保存
        outdir = os.path.join(fig_dir, 'default-added')
        os.makedirs(outdir, exist_ok=True)
        plt.savefig(os.path.join(outdir, f'{pair["name"]}_comparison.png'), 
                    dpi=300, bbox_inches='tight')
        plt.close(fig)
        
        print(f"Created {pair['name']} variation rate comparison graph for default-added case")

# 変動率の時系列グラフを作成
plot_variation_rate_graphs()

print("All graphs have been created successfully!") 
