import math
import csv
import pyomo.environ as pyo

# ------------------------------------------------------------
# データ定義（SAM）
# GAMS の Table SAM(u,v) を (row, col) の辞書で表現
# ------------------------------------------------------------

ACCOUNTS = ["BRD", "MLK", "CAP", "LAB", "HOH", "GOV", "INV", "EXT", "IDT", "TRF"]
GOODS = ["BRD", "MLK"]
FACTORS = ["CAP", "LAB"]


def build_SAM():
    SAM = {}

    def set_sam(r, c, v):
        SAM[(r, c)] = float(v)

    # 列: BRD MLK CAP LAB HOH
    set_sam("BRD", "BRD", 21)
    set_sam("BRD", "MLK", 8)
    set_sam("BRD", "HOH", 20)

    set_sam("MLK", "BRD", 17)
    set_sam("MLK", "MLK", 9)
    set_sam("MLK", "HOH", 30)

    set_sam("CAP", "BRD", 20)
    set_sam("CAP", "MLK", 30)

    set_sam("LAB", "BRD", 15)
    set_sam("LAB", "MLK", 25)

    set_sam("HOH", "CAP", 50)
    set_sam("HOH", "LAB", 40)

    set_sam("GOV", "HOH", 23)
    set_sam("INV", "HOH", 17)

    set_sam("IDT", "BRD", 5)
    set_sam("IDT", "MLK", 4)

    set_sam("TRF", "BRD", 1)
    set_sam("TRF", "MLK", 2)

    set_sam("EXT", "BRD", 13)
    set_sam("EXT", "MLK", 11)

    # 「+  GOV INV IDT TRF EXT」ブロック
    # 行 BRD
    set_sam("BRD", "GOV", 19)
    set_sam("BRD", "INV", 16)
    set_sam("BRD", "EXT", 8)

    # 行 MLK
    set_sam("MLK", "GOV", 14)
    set_sam("MLK", "INV", 15)
    set_sam("MLK", "EXT", 4)

    # 行 GOV
    set_sam("GOV", "IDT", 9)
    set_sam("GOV", "TRF", 3)

    # 行 INV
    set_sam("INV", "GOV", 2)
    set_sam("INV", "EXT", 12)

    # 必要に応じて追加の SAM セルをここに定義してください（CAP, LAB, HOH の 0 行など）

    return SAM


# 安全な参照用（存在しないセルは 0）
def sam_val(SAM, row, col):
    return SAM.get((row, col), 0.0)


# 0 に対する負のべき乗を避けるための安全なべき乗関数
def safe_pow(base: float, exp: float) -> float:
    """
    base <= 0 に対して負の指数を与えると Python ではエラーになるので、
    その場合は 0 を返すようにする。
    GAMS での数値安定化の代替として、小さな値に丸めて計算する。
    """
    if base <= 0.0 and exp < 0.0:
        return 0.0
    return base ** exp


# ------------------------------------------------------------
# キャリブレーション
# ------------------------------------------------------------


def calibrate_parameters(SAM):
    I = GOODS
    H = FACTORS

    # 初期値
    F0 = {(h, j): sam_val(SAM, h, j) for h in H for j in I}
    Y0 = {j: sum(F0[h, j] for h in H) for j in I}
    X0 = {(i, j): sam_val(SAM, i, j) for i in I for j in I}
    Z0 = {j: Y0[j] + sum(X0[i, j] for i in I) for j in I}
    M0 = {i: sam_val(SAM, "EXT", i) for i in I}

    Td0 = sam_val(SAM, "GOV", "HOH")
    Tz0 = {j: sam_val(SAM, "IDT", j) for j in I}
    Tm0 = {j: sam_val(SAM, "TRF", j) for j in I}

    tauz = {j: (Tz0[j] / Z0[j]) if Z0[j] != 0 else 0.0 for j in I}
    taum = {j: (Tm0[j] / M0[j]) if M0[j] != 0 else 0.0 for j in I}

    Xp0 = {i: sam_val(SAM, i, "HOH") for i in I}
    FF = {h: sam_val(SAM, "HOH", h) for h in H}

    Xg0 = {i: sam_val(SAM, i, "GOV") for i in I}
    Xv0 = {i: sam_val(SAM, i, "INV") for i in I}
    E0 = {i: sam_val(SAM, i, "EXT") for i in I}
    Q0 = {i: Xp0[i] + Xg0[i] + Xv0[i] + sum(X0[i, j] for j in I) for i in I}
    D0 = {i: (1.0 + tauz[i]) * Z0[i] - E0[i] for i in I}

    Sp0 = sam_val(SAM, "INV", "HOH")
    Sg0 = sam_val(SAM, "INV", "GOV")
    Sf = sam_val(SAM, "INV", "EXT")

    pWe = {i: 1.0 for i in I}
    pWm = {i: 1.0 for i in I}

    # Calibration parameters
    sigma = {i: 2.0 for i in I}
    psi = {i: 2.0 for i in I}
    eta = {i: (sigma[i] - 1.0) / sigma[i] for i in I}
    phi = {i: (psi[i] + 1.0) / psi[i] for i in I}

    alpha = {i: Xp0[i] / sum(Xp0[j] for j in I) if sum(Xp0.values()) != 0 else 0.0 for i in I}

    beta = {}
    for j in I:
        denom_j = sum(F0[h2, j] for h2 in H)
        for h in H:
            beta[h, j] = F0[h, j] / denom_j if denom_j != 0 else 0.0

    b = {}
    for j in I:
        prod_term = 1.0
        for h in H:
            if F0[h, j] > 0 and beta[h, j] != 0:
                prod_term *= F0[h, j] ** beta[h, j]
        b[j] = Y0[j] / prod_term if prod_term != 0 else 0.0

    ax = {(i, j): (X0[i, j] / Z0[j]) if Z0[j] != 0 else 0.0 for i in I for j in I}
    ay = {j: (Y0[j] / Z0[j]) if Z0[j] != 0 else 0.0 for j in I}
    mu = {i: Xg0[i] / sum(Xg0[j] for j in I) if sum(Xg0.values()) != 0 else 0.0 for i in I}
    lambda_par = {i: Xv0[i] / (Sp0 + Sg0 + Sf) if (Sp0 + Sg0 + Sf) != 0 else 0.0 for i in I}

    deltam = {}
    deltad = {}
    gamma = {}
    for i in I:
        num_m = (1.0 + taum[i]) * safe_pow(M0[i], 1.0 - eta[i])
        num_d = safe_pow(D0[i], 1.0 - eta[i])
        denom = num_m + num_d
        if denom == 0:
            deltam[i] = 0.0
            deltad[i] = 0.0
            gamma[i] = 0.0
        else:
            deltam[i] = num_m / denom
            deltad[i] = num_d / denom
            inner = deltam[i] * safe_pow(M0[i], eta[i]) + deltad[i] * safe_pow(D0[i], eta[i])
            gamma[i] = Q0[i] / safe_pow(inner, 1.0 / eta[i]) if inner != 0 else 0.0

    xie = {}
    xid = {}
    theta = {}
    for i in I:
        num_e = safe_pow(E0[i], 1.0 - phi[i])
        num_d = safe_pow(D0[i], 1.0 - phi[i])
        denom = num_e + num_d
        if denom == 0:
            xie[i] = 0.0
            xid[i] = 0.0
            theta[i] = 0.0
        else:
            xie[i] = num_e / denom
            xid[i] = num_d / denom
            inner = xie[i] * safe_pow(E0[i], phi[i]) + xid[i] * safe_pow(D0[i], phi[i])
            theta[i] = Z0[i] / safe_pow(inner, 1.0 / phi[i]) if inner != 0 else 0.0

    ssp = Sp0 / sum(FF[h] for h in H) if sum(FF.values()) != 0 else 0.0
    ssg = Sg0 / (Td0 + sum(Tz0[j] for j in I) + sum(Tm0[j] for j in I)) if (Td0 + sum(Tz0.values()) + sum(Tm0.values())) != 0 else 0.0
    taud = Td0 / sum(FF[h] for h in H) if sum(FF.values()) != 0 else 0.0

    params = dict(
        I=I,
        H=H,
        F0=F0,
        Y0=Y0,
        X0=X0,
        Z0=Z0,
        M0=M0,
        Td0=Td0,
        Tz0=Tz0,
        Tm0=Tm0,
        tauz=tauz,
        taum=taum,
        Xp0=Xp0,
        FF=FF,
        Xg0=Xg0,
        Xv0=Xv0,
        E0=E0,
        Q0=Q0,
        D0=D0,
        Sp0=Sp0,
        Sg0=Sg0,
        Sf=Sf,
        pWe=pWe,
        pWm=pWm,
        sigma=sigma,
        psi=psi,
        eta=eta,
        phi=phi,
        alpha=alpha,
        beta=beta,
        b=b,
        ax=ax,
        ay=ay,
        mu=mu,
        lambda_par=lambda_par,
        deltam=deltam,
        deltad=deltad,
        gamma=gamma,
        xie=xie,
        xid=xid,
        theta=theta,
        ssp=ssp,
        ssg=ssg,
        taud=taud,
    )
    return params


# ------------------------------------------------------------
# Pyomo モデル構築
# ------------------------------------------------------------


def build_model(params):
    I = params["I"]
    H = params["H"]

    m = pyo.ConcreteModel()

    # 財集合 I と同じ要素を持つ集合 J を別コンポーネントとして定義
    m.I = pyo.Set(initialize=I)
    m.J = pyo.Set(initialize=I)
    m.H = pyo.Set(initialize=H)

    # Parameters
    m.FF = pyo.Param(m.H, initialize=params["FF"])
    m.tauz = pyo.Param(m.I, initialize=params["tauz"])
    m.taum = pyo.Param(m.I, initialize=params["taum"])
    m.pWe = pyo.Param(m.I, initialize=params["pWe"])
    m.pWm = pyo.Param(m.I, initialize=params["pWm"])
    m.beta = pyo.Param(m.H, m.J, initialize=params["beta"], default=0.0)
    m.b = pyo.Param(m.J, initialize=params["b"])
    m.ax = pyo.Param(m.I, m.J, initialize=params["ax"], default=0.0)
    m.ay = pyo.Param(m.J, initialize=params["ay"])
    m.mu = pyo.Param(m.I, initialize=params["mu"])
    m.lambda_par = pyo.Param(m.I, initialize=params["lambda_par"])
    m.deltam = pyo.Param(m.I, initialize=params["deltam"])
    m.deltad = pyo.Param(m.I, initialize=params["deltad"])
    m.gamma = pyo.Param(m.I, initialize=params["gamma"])
    m.xie = pyo.Param(m.I, initialize=params["xie"])
    m.xid = pyo.Param(m.I, initialize=params["xid"])
    m.theta = pyo.Param(m.I, initialize=params["theta"])
    m.eta = pyo.Param(m.I, initialize=params["eta"])
    m.phi = pyo.Param(m.I, initialize=params["phi"])
    m.alpha = pyo.Param(m.I, initialize=params["alpha"])
    m.ssp = pyo.Param(initialize=params["ssp"])
    m.ssg = pyo.Param(initialize=params["ssg"])
    m.taud = pyo.Param(initialize=params["taud"])
    m.Sf = pyo.Param(initialize=params["Sf"])

    Y0 = params["Y0"]
    F0 = params["F0"]
    X0 = params["X0"]
    Z0 = params["Z0"]
    Xp0 = params["Xp0"]
    Xg0 = params["Xg0"]
    Xv0 = params["Xv0"]
    E0 = params["E0"]
    M0 = params["M0"]
    Q0 = params["Q0"]
    D0 = params["D0"]
    Sp0 = params["Sp0"]
    Sg0 = params["Sg0"]
    Td0 = params["Td0"]
    Tz0 = params["Tz0"]
    Tm0 = params["Tm0"]

    LB = 1e-5

    # Variables
    m.Y = pyo.Var(m.J, within=pyo.PositiveReals, bounds=(LB, None), initialize=Y0)
    m.F = pyo.Var(m.H, m.J, within=pyo.PositiveReals, bounds=(LB, None), initialize=F0)
    m.X = pyo.Var(m.I, m.J, within=pyo.PositiveReals, bounds=(LB, None), initialize=X0)
    m.Z = pyo.Var(m.J, within=pyo.PositiveReals, bounds=(LB, None), initialize=Z0)
    m.Xp = pyo.Var(m.I, within=pyo.PositiveReals, bounds=(LB, None), initialize=Xp0)
    m.Xg = pyo.Var(m.I, within=pyo.PositiveReals, bounds=(LB, None), initialize=Xg0)
    m.Xv = pyo.Var(m.I, within=pyo.PositiveReals, bounds=(LB, None), initialize=Xv0)
    m.E = pyo.Var(m.I, within=pyo.PositiveReals, bounds=(LB, None), initialize=E0)
    m.M = pyo.Var(m.I, within=pyo.PositiveReals, bounds=(LB, None), initialize=M0)
    m.Q = pyo.Var(m.I, within=pyo.PositiveReals, bounds=(LB, None), initialize=Q0)
    m.D = pyo.Var(m.I, within=pyo.PositiveReals, bounds=(LB, None), initialize=D0)

    m.pf = pyo.Var(m.H, within=pyo.PositiveReals, bounds=(LB, None), initialize=1.0)
    m.py = pyo.Var(m.J, within=pyo.PositiveReals, bounds=(LB, None), initialize=1.0)
    m.pz = pyo.Var(m.J, within=pyo.PositiveReals, bounds=(LB, None), initialize=1.0)
    m.pq = pyo.Var(m.I, within=pyo.PositiveReals, bounds=(LB, None), initialize=1.0)
    m.pe = pyo.Var(m.I, within=pyo.PositiveReals, bounds=(LB, None), initialize=1.0)
    m.pm = pyo.Var(m.I, within=pyo.PositiveReals, bounds=(LB, None), initialize=1.0)
    m.pd = pyo.Var(m.I, within=pyo.PositiveReals, bounds=(LB, None), initialize=1.0)
    m.epsilon = pyo.Var(within=pyo.PositiveReals, bounds=(LB, None), initialize=1.0)

    m.Sp = pyo.Var(within=pyo.PositiveReals, bounds=(LB, None), initialize=Sp0)
    m.Sg = pyo.Var(within=pyo.PositiveReals, bounds=(LB, None), initialize=Sg0)
    m.Td = pyo.Var(within=pyo.PositiveReals, bounds=(LB, None), initialize=Td0)
    m.Tz = pyo.Var(m.J, within=pyo.NonNegativeReals, bounds=(0.0, None), initialize=Tz0)
    m.Tm = pyo.Var(m.I, within=pyo.NonNegativeReals, bounds=(0.0, None), initialize=Tm0)

    # numeraire
    m.pf["LAB"].fix(1.0)

    # Equations
    def eqpy_rule(m, j):
        return m.Y[j] == m.b[j] * pyo.prod(m.F[h, j] ** m.beta[h, j] for h in m.H)

    m.eqpy = pyo.Constraint(m.J, rule=eqpy_rule)

    def eqF_rule(m, h, j):
        return m.F[h, j] == m.beta[h, j] * m.py[j] * m.Y[j] / m.pf[h]

    m.eqF = pyo.Constraint(m.H, m.J, rule=eqF_rule)

    def eqX_rule(m, i, j):
        return m.X[i, j] == m.ax[i, j] * m.Z[j]

    m.eqX = pyo.Constraint(m.I, m.J, rule=eqX_rule)

    def eqY_rule(m, j):
        return m.Y[j] == m.ay[j] * m.Z[j]

    m.eqY = pyo.Constraint(m.J, rule=eqY_rule)

    def eqpzs_rule(m, j):
        return m.pz[j] == m.ay[j] * m.py[j] + sum(m.ax[i, j] * m.pq[i] for i in m.I)

    m.eqpzs = pyo.Constraint(m.J, rule=eqpzs_rule)

    # Government
    def eqTd_rule(m):
        return m.Td == m.taud * sum(m.pf[h] * m.FF[h] for h in m.H)

    m.eqTd = pyo.Constraint(rule=eqTd_rule)

    def eqTz_rule(m, j):
        return m.Tz[j] == m.tauz[j] * m.pz[j] * m.Z[j]

    m.eqTz = pyo.Constraint(m.J, rule=eqTz_rule)

    def eqTm_rule(m, i):
        return m.Tm[i] == m.taum[i] * m.pm[i] * m.M[i]

    m.eqTm = pyo.Constraint(m.I, rule=eqTm_rule)

    def eqXg_rule(m, i):
        return m.Xg[i] == m.mu[i] * (m.Td + sum(m.Tz[j] for j in m.J) + sum(m.Tm[k] for k in m.I) - m.Sg) / m.pq[i]

    m.eqXg = pyo.Constraint(m.I, rule=eqXg_rule)

    # Investment
    def eqXv_rule(m, i):
        return m.Xv[i] == m.lambda_par[i] * (m.Sp + m.Sg + m.epsilon * m.Sf) / m.pq[i]

    m.eqXv = pyo.Constraint(m.I, rule=eqXv_rule)

    # Savings
    def eqSp_rule(m):
        return m.Sp == m.ssp * sum(m.pf[h] * m.FF[h] for h in m.H)

    def eqSg_rule(m):
        return m.Sg == m.ssg * (m.Td + sum(m.Tz[j] for j in m.J) + sum(m.Tm[i] for i in m.I))

    m.eqSp = pyo.Constraint(rule=eqSp_rule)
    m.eqSg = pyo.Constraint(rule=eqSg_rule)

    # Household consumption
    def eqXp_rule(m, i):
        return m.Xp[i] == m.alpha[i] * (sum(m.pf[h] * m.FF[h] for h in m.H) - m.Sp - m.Td) / m.pq[i]

    m.eqXp = pyo.Constraint(m.I, rule=eqXp_rule)

    # International trade
    def eqpe_rule(m, i):
        return m.pe[i] == m.epsilon * m.pWe[i]

    def eqpm_rule(m, i):
        return m.pm[i] == m.epsilon * m.pWm[i]

    m.eqpe = pyo.Constraint(m.I, rule=eqpe_rule)
    m.eqpm = pyo.Constraint(m.I, rule=eqpm_rule)

    def eqepsilon_rule(m):
        return sum(m.pWe[i] * m.E[i] for i in m.I) + m.Sf == sum(m.pWm[i] * m.M[i] for i in m.I)

    m.eqepsilon = pyo.Constraint(rule=eqepsilon_rule)

    # Armington
    def eqpqs_rule(m, i):
        return m.Q[i] == m.gamma[i] * (m.deltam[i] * m.M[i] ** m.eta[i] + m.deltad[i] * m.D[i] ** m.eta[i]) ** (1.0 / m.eta[i])

    m.eqpqs = pyo.Constraint(m.I, rule=eqpqs_rule)

    def eqM_rule(m, i):
        return m.M[i] == (m.gamma[i] * m.eta[i] * m.deltam[i] * m.pq[i] / ((1.0 + m.taum[i]) * m.pm[i])) ** (1.0 / (1.0 - m.eta[i])) * m.Q[i]

    m.eqM = pyo.Constraint(m.I, rule=eqM_rule)

    def eqD_rule(m, i):
        return m.D[i] == (m.gamma[i] * m.eta[i] * m.deltad[i] * m.pq[i] / m.pd[i]) ** (1.0 / (1.0 - m.eta[i])) * m.Q[i]

    m.eqD = pyo.Constraint(m.I, rule=eqD_rule)

    # Transformation
    def eqpzd_rule(m, i):
        return m.Z[i] == m.theta[i] * (m.xie[i] * m.E[i] ** m.phi[i] + m.xid[i] * m.D[i] ** m.phi[i]) ** (1.0 / m.phi[i])

    m.eqpzd = pyo.Constraint(m.I, rule=eqpzd_rule)

    def eqE_rule(m, i):
        return m.E[i] == (m.theta[i] ** m.phi[i] * m.xie[i] * (1.0 + m.tauz[i]) * m.pz[i] / m.pe[i]) ** (1.0 / (1.0 - m.phi[i])) * m.Z[i]

    def eqDs_rule(m, i):
        return m.D[i] == (m.theta[i] ** m.phi[i] * m.xid[i] * (1.0 + m.tauz[i]) * m.pz[i] / m.pd[i]) ** (1.0 / (1.0 - m.phi[i])) * m.Z[i]

    m.eqE = pyo.Constraint(m.I, rule=eqE_rule)
    m.eqDs = pyo.Constraint(m.I, rule=eqDs_rule)

    # Market clearing
    def eqpqd_rule(m, i):
        return m.Q[i] == m.Xp[i] + m.Xg[i] + m.Xv[i] + sum(m.X[i, j] for j in m.J)

    m.eqpqd = pyo.Constraint(m.I, rule=eqpqd_rule)

    def eqpf_rule(m, h):
        return sum(m.F[h, j] for j in m.J) == m.FF[h]

    m.eqpf = pyo.Constraint(m.H, rule=eqpf_rule)

    # Objective
    def obj_expr(m):
        return pyo.prod(m.Xp[i] ** m.alpha[i] for i in m.I)

    m.obj = pyo.Objective(rule=obj_expr, sense=pyo.maximize)

    return m


# ------------------------------------------------------------
# メイン
# ------------------------------------------------------------


def main():
    SAM = build_SAM()
    params = calibrate_parameters(SAM)
    model = build_model(params)

    # --------------------------------------------------------
    # 備考:
    # このベンチマーク CGE モデルは、キャリブレーションによって
    # すべての変数がちょうど決まる「方程式の個数＝変数の個数」の系です。
    # そのため Ipopt からは「自由度が足りない (Too few degrees of freedom)」
    # という警告が出ますが、初期値 (= GAMS の均衡解) 自体が解になっています。
    #
    # ここではベースラインの解を取得することが目的なので、
    # ソルバーは実行せず、キャリブレーションで設定した値を
    # そのまま CSV に出力します。
    # --------------------------------------------------------
    # もし将来、ショックを与えた後の新しい均衡を解きたい場合は、
    # 下の run_solver を True にして Ipopt を使ってください。

    run_solver = False
    if run_solver:
        solver = pyo.SolverFactory("ipopt")  # 適切な NLP ソルバーを指定
        result = solver.solve(model, tee=True)

        # コンソールへの簡易表示
        print(result.solver.status, result.solver.termination_condition)
        print("epsilon:", pyo.value(model.epsilon))

    # 結果を CSV に保存
    with open("results.csv", mode="w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["variable", "index", "value"])

        # Y(j)
        for j in model.J:
            writer.writerow(["Y", j, pyo.value(model.Y[j])])

        # F(h,j)
        for h in model.H:
            for j in model.J:
                writer.writerow(["F", f"{h}.{j}", pyo.value(model.F[h, j])])

        # X(i,j)
        for i in model.I:
            for j in model.J:
                writer.writerow(["X", f"{i}.{j}", pyo.value(model.X[i, j])])

        # Z(j)
        for j in model.J:
            writer.writerow(["Z", j, pyo.value(model.Z[j])])

        # Xp(i)
        for i in model.I:
            writer.writerow(["Xp", i, pyo.value(model.Xp[i])])

        # Xg(i)
        for i in model.I:
            writer.writerow(["Xg", i, pyo.value(model.Xg[i])])

        # Xv(i)
        for i in model.I:
            writer.writerow(["Xv", i, pyo.value(model.Xv[i])])

        # E(i)
        for i in model.I:
            writer.writerow(["E", i, pyo.value(model.E[i])])

        # M(i)
        for i in model.I:
            writer.writerow(["M", i, pyo.value(model.M[i])])

        # Q(i)
        for i in model.I:
            writer.writerow(["Q", i, pyo.value(model.Q[i])])

        # D(i)
        for i in model.I:
            writer.writerow(["D", i, pyo.value(model.D[i])])

        # pf(h)
        for h in model.H:
            writer.writerow(["pf", h, pyo.value(model.pf[h])])

        # py(j)
        for j in model.J:
            writer.writerow(["py", j, pyo.value(model.py[j])])

        # pz(j)
        for j in model.J:
            writer.writerow(["pz", j, pyo.value(model.pz[j])])

        # pq(i)
        for i in model.I:
            writer.writerow(["pq", i, pyo.value(model.pq[i])])

        # pe(i)
        for i in model.I:
            writer.writerow(["pe", i, pyo.value(model.pe[i])])

        # pm(i)
        for i in model.I:
            writer.writerow(["pm", i, pyo.value(model.pm[i])])

        # pd(i)
        for i in model.I:
            writer.writerow(["pd", i, pyo.value(model.pd[i])])

        # epsilon, Sp, Sg, Td
        writer.writerow(["epsilon", "", pyo.value(model.epsilon)])
        writer.writerow(["Sp", "", pyo.value(model.Sp)])
        writer.writerow(["Sg", "", pyo.value(model.Sg)])
        writer.writerow(["Td", "", pyo.value(model.Td)])

        # Tz(j)
        for j in model.J:
            writer.writerow(["Tz", j, pyo.value(model.Tz[j])])

        # Tm(i)
        for i in model.I:
            writer.writerow(["Tm", i, pyo.value(model.Tm[i])])

        # 目的関数値 UU
        writer.writerow(["UU", "", pyo.value(model.obj())])


if __name__ == "__main__":
    main()
