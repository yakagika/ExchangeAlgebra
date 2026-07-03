# shock_resolve.py — comparative-statics ground truth for the Hosoe Ch.6
# standard CGE (GE plan phase1-cge-reproduction, task 1c).
#
# Re-solves the FULL GAMS square system of stdcge.gms (every equation
# verbatim, all 48 variables; pf("LAB") fixed at 1 as the numeraire and
# eqpf("LAB") dropped by Walras's law -> 47x47) under counterfactual tax
# policies, and writes one results.csv-format fixture per scenario to
# shocks/<name>.csv. The cge-lite-model-test suite compares the CGE-Lite
# (Haskell, reduced 8-coordinate instrument-vector formulation) solution
# against these fixtures — two independent formulations and solvers of the
# same published equation system.
#
# Deliberately dependency-free (math + csv only): a damped Newton with a
# forward-difference Jacobian and partial-pivot Gaussian elimination is
# plenty at 47 unknowns, and stdlib-only keeps the fixture pipeline
# runnable anywhere (the bundled stdcge.py needs pyomo+ipopt for re-solves,
# which this script exists to avoid).
#
# Usage:  uv run python shock_resolve.py   (or plain python3)
#
# Self-check: the baseline scenario (calibrated tax rates, no shock) must
# reproduce GAMS/results.csv to 1e-7 or the script exits nonzero — the same
# ground-truth anchoring the calibration test uses.

import csv
import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))

GOODS = ["BRD", "MLK"]
FACTORS = ["CAP", "LAB"]
ACCOUNTS = ["BRD", "MLK", "CAP", "LAB", "IDT", "TRF", "HOH", "GOV", "INV", "EXT"]

# ------------------------------------------------------------------
# SAM (GAMS Table SAM(u,v)) — identical to stdcge.gms / Calibration.hs
# ------------------------------------------------------------------

SAM = {
    ("BRD", "BRD"): 21, ("BRD", "MLK"): 8, ("BRD", "HOH"): 20,
    ("BRD", "GOV"): 19, ("BRD", "INV"): 16, ("BRD", "EXT"): 8,
    ("MLK", "BRD"): 17, ("MLK", "MLK"): 9, ("MLK", "HOH"): 30,
    ("MLK", "GOV"): 14, ("MLK", "INV"): 15, ("MLK", "EXT"): 4,
    ("CAP", "BRD"): 20, ("CAP", "MLK"): 30,
    ("LAB", "BRD"): 15, ("LAB", "MLK"): 25,
    ("IDT", "BRD"): 5, ("IDT", "MLK"): 4,
    ("TRF", "BRD"): 1, ("TRF", "MLK"): 2,
    ("HOH", "CAP"): 50, ("HOH", "LAB"): 40,
    ("GOV", "IDT"): 9, ("GOV", "TRF"): 3, ("GOV", "HOH"): 23,
    ("INV", "HOH"): 17, ("INV", "GOV"): 2, ("INV", "EXT"): 12,
    ("EXT", "BRD"): 13, ("EXT", "MLK"): 11,
}


def sam(u, v):
    return float(SAM.get((u, v), 0.0))


# ------------------------------------------------------------------
# Calibration (the GAMS "Loading the initial values" + "Calibration"
# blocks, statement by statement — mirrors Calibration.hs)
# ------------------------------------------------------------------

def calibrate():
    p = {}
    p["Td0"] = sam("GOV", "HOH")
    p["Tz0"] = {j: sam("IDT", j) for j in GOODS}
    p["Tm0"] = {j: sam("TRF", j) for j in GOODS}
    p["F0"] = {(h, j): sam(h, j) for h in FACTORS for j in GOODS}
    p["Y0"] = {j: sum(p["F0"][(h, j)] for h in FACTORS) for j in GOODS}
    p["X0"] = {(i, j): sam(i, j) for i in GOODS for j in GOODS}
    p["Z0"] = {j: p["Y0"][j] + sum(p["X0"][(i, j)] for i in GOODS) for j in GOODS}
    p["M0"] = {i: sam("EXT", i) for i in GOODS}
    p["tauz"] = {j: p["Tz0"][j] / p["Z0"][j] for j in GOODS}
    p["taum"] = {j: p["Tm0"][j] / p["M0"][j] for j in GOODS}
    p["Xp0"] = {i: sam(i, "HOH") for i in GOODS}
    p["FF"] = {h: sam("HOH", h) for h in FACTORS}
    p["Xg0"] = {i: sam(i, "GOV") for i in GOODS}
    p["Xv0"] = {i: sam(i, "INV") for i in GOODS}
    p["E0"] = {i: sam(i, "EXT") for i in GOODS}
    p["Q0"] = {i: p["Xp0"][i] + p["Xg0"][i] + p["Xv0"][i]
                  + sum(p["X0"][(i, j)] for j in GOODS) for i in GOODS}
    p["D0"] = {i: (1 + p["tauz"][i]) * p["Z0"][i] - p["E0"][i] for i in GOODS}
    p["Sp0"] = sam("INV", "HOH")
    p["Sg0"] = sam("INV", "GOV")
    p["Sf"] = sam("INV", "EXT")
    p["pWe"] = {i: 1.0 for i in GOODS}
    p["pWm"] = {i: 1.0 for i in GOODS}

    p["sigma"] = {i: 2.0 for i in GOODS}
    p["psi"] = {i: 2.0 for i in GOODS}
    p["eta"] = {i: (p["sigma"][i] - 1) / p["sigma"][i] for i in GOODS}
    p["phi"] = {i: (p["psi"][i] + 1) / p["psi"][i] for i in GOODS}

    p["alpha"] = {i: p["Xp0"][i] / sum(p["Xp0"][j] for j in GOODS) for i in GOODS}
    p["beta"] = {(h, j): p["F0"][(h, j)] / sum(p["F0"][(k, j)] for k in FACTORS)
                 for h in FACTORS for j in GOODS}
    p["b"] = {j: p["Y0"][j] / math.prod(p["F0"][(h, j)] ** p["beta"][(h, j)]
                                        for h in FACTORS) for j in GOODS}
    p["ax"] = {(i, j): p["X0"][(i, j)] / p["Z0"][j] for i in GOODS for j in GOODS}
    p["ay"] = {j: p["Y0"][j] / p["Z0"][j] for j in GOODS}
    p["mu"] = {i: p["Xg0"][i] / sum(p["Xg0"][j] for j in GOODS) for i in GOODS}
    p["lambda"] = {i: p["Xv0"][i] / (p["Sp0"] + p["Sg0"] + p["Sf"]) for i in GOODS}
    p["deltam"] = {}
    p["deltad"] = {}
    for i in GOODS:
        num = (1 + p["taum"][i]) * p["M0"][i] ** (1 - p["eta"][i])
        den = num + p["D0"][i] ** (1 - p["eta"][i])
        p["deltam"][i] = num / den
        p["deltad"][i] = (p["D0"][i] ** (1 - p["eta"][i])) / den
    p["gamma"] = {i: p["Q0"][i]
                  / (p["deltam"][i] * p["M0"][i] ** p["eta"][i]
                     + p["deltad"][i] * p["D0"][i] ** p["eta"][i]) ** (1 / p["eta"][i])
                  for i in GOODS}
    p["xie"] = {}
    p["xid"] = {}
    for i in GOODS:
        se = p["E0"][i] ** (1 - p["phi"][i])
        sd = p["D0"][i] ** (1 - p["phi"][i])
        p["xie"][i] = se / (se + sd)
        p["xid"][i] = sd / (se + sd)
    p["theta"] = {i: p["Z0"][i]
                  / (p["xie"][i] * p["E0"][i] ** p["phi"][i]
                     + p["xid"][i] * p["D0"][i] ** p["phi"][i]) ** (1 / p["phi"][i])
                  for i in GOODS}
    p["ssp"] = p["Sp0"] / sum(p["FF"][h] for h in FACTORS)
    p["ssg"] = p["Sg0"] / (p["Td0"] + sum(p["Tz0"][j] for j in GOODS)
                           + sum(p["Tm0"][j] for j in GOODS))
    p["taud"] = p["Td0"] / sum(p["FF"][h] for h in FACTORS)
    return p


# ------------------------------------------------------------------
# The full square system (every stdcge.gms equation, verbatim)
# ------------------------------------------------------------------

def variable_names():
    names = []
    names += [f"Y.{j}" for j in GOODS]
    names += [f"F.{h}.{j}" for h in FACTORS for j in GOODS]
    names += [f"X.{i}.{j}" for i in GOODS for j in GOODS]
    names += [f"Z.{j}" for j in GOODS]
    names += [f"Xp.{i}" for i in GOODS]
    names += [f"Xg.{i}" for i in GOODS]
    names += [f"Xv.{i}" for i in GOODS]
    names += [f"E.{i}" for i in GOODS]
    names += [f"M.{i}" for i in GOODS]
    names += [f"Q.{i}" for i in GOODS]
    names += [f"D.{i}" for i in GOODS]
    names += ["pf.CAP"]  # pf.LAB = 1 (numeraire, GAMS pf.fx)
    names += [f"py.{j}" for j in GOODS]
    names += [f"pz.{j}" for j in GOODS]
    names += [f"pq.{i}" for i in GOODS]
    names += [f"pe.{i}" for i in GOODS]
    names += [f"pm.{i}" for i in GOODS]
    names += [f"pd.{i}" for i in GOODS]
    names += ["epsilon", "Sp", "Sg", "Td"]
    names += [f"Tz.{j}" for j in GOODS]
    names += [f"Tm.{i}" for i in GOODS]
    return names


def benchmark_point(p):
    x = {}
    for j in GOODS:
        x[f"Y.{j}"] = p["Y0"][j]
        x[f"Z.{j}"] = p["Z0"][j]
        x[f"py.{j}"] = 1.0
        x[f"pz.{j}"] = 1.0
        x[f"Tz.{j}"] = p["Tz0"][j]
    for h in FACTORS:
        for j in GOODS:
            x[f"F.{h}.{j}"] = p["F0"][(h, j)]
    for i in GOODS:
        for j in GOODS:
            x[f"X.{i}.{j}"] = p["X0"][(i, j)]
        x[f"Xp.{i}"] = p["Xp0"][i]
        x[f"Xg.{i}"] = p["Xg0"][i]
        x[f"Xv.{i}"] = p["Xv0"][i]
        x[f"E.{i}"] = p["E0"][i]
        x[f"M.{i}"] = p["M0"][i]
        x[f"Q.{i}"] = p["Q0"][i]
        x[f"D.{i}"] = p["D0"][i]
        x[f"pq.{i}"] = 1.0
        x[f"pe.{i}"] = 1.0
        x[f"pm.{i}"] = 1.0
        x[f"pd.{i}"] = 1.0
        x[f"Tm.{i}"] = p["Tm0"][i]
    x["pf.CAP"] = 1.0
    x["epsilon"] = 1.0
    x["Sp"] = p["Sp0"]
    x["Sg"] = p["Sg0"]
    x["Td"] = p["Td0"]
    return x


def spow(base, exp):
    # GAMS-style lower bound (x.lo = 1e-5): keeps a wayward Newton probe
    # from feeding a negative base to a fractional power.
    return max(base, 1e-9) ** exp


def residuals(x, p, tauz, taum):
    """Every stdcge.gms equation, same names/order; eqpf('LAB') dropped
    (Walras) and pf('LAB') = 1 (numeraire). tauz/taum are the POLICY rates
    (the calibrated parameters in p stay at benchmark values)."""
    pf = {"CAP": x["pf.CAP"], "LAB": 1.0}
    eps = x["epsilon"]
    r = []

    # eqpy(j):  Y(j)  = b(j)*prod(h, F(h,j)**beta(h,j))
    for j in GOODS:
        r.append(x[f"Y.{j}"]
                 - p["b"][j] * math.prod(spow(x[f"F.{h}.{j}"], p["beta"][(h, j)])
                                         for h in FACTORS))
    # eqF(h,j): F(h,j) = beta(h,j)*py(j)*Y(j)/pf(h)
    for h in FACTORS:
        for j in GOODS:
            r.append(x[f"F.{h}.{j}"]
                     - p["beta"][(h, j)] * x[f"py.{j}"] * x[f"Y.{j}"] / pf[h])
    # eqX(i,j): X(i,j) = ax(i,j)*Z(j)
    for i in GOODS:
        for j in GOODS:
            r.append(x[f"X.{i}.{j}"] - p["ax"][(i, j)] * x[f"Z.{j}"])
    # eqY(j):   Y(j)   = ay(j)*Z(j)
    for j in GOODS:
        r.append(x[f"Y.{j}"] - p["ay"][j] * x[f"Z.{j}"])
    # eqpzs(j): pz(j)  = ay(j)*py(j) + sum(i, ax(i,j)*pq(i))
    for j in GOODS:
        r.append(x[f"pz.{j}"]
                 - (p["ay"][j] * x[f"py.{j}"]
                    + sum(p["ax"][(i, j)] * x[f"pq.{i}"] for i in GOODS)))
    # eqTd:     Td     = taud*sum(h, pf(h)*FF(h))
    r.append(x["Td"] - p["taud"] * sum(pf[h] * p["FF"][h] for h in FACTORS))
    # eqTz(j):  Tz(j)  = tauz(j)*pz(j)*Z(j)
    for j in GOODS:
        r.append(x[f"Tz.{j}"] - tauz[j] * x[f"pz.{j}"] * x[f"Z.{j}"])
    # eqTm(i):  Tm(i)  = taum(i)*pm(i)*M(i)
    for i in GOODS:
        r.append(x[f"Tm.{i}"] - taum[i] * x[f"pm.{i}"] * x[f"M.{i}"])
    # eqXg(i):  Xg(i)  = mu(i)*(Td + sum Tz + sum Tm - Sg)/pq(i)
    total_tax = x["Td"] + sum(x[f"Tz.{j}"] for j in GOODS) + sum(x[f"Tm.{i}"] for i in GOODS)
    for i in GOODS:
        r.append(x[f"Xg.{i}"] - p["mu"][i] * (total_tax - x["Sg"]) / x[f"pq.{i}"])
    # eqXv(i):  Xv(i)  = lambda(i)*(Sp + Sg + epsilon*Sf)/pq(i)
    for i in GOODS:
        r.append(x[f"Xv.{i}"]
                 - p["lambda"][i] * (x["Sp"] + x["Sg"] + eps * p["Sf"]) / x[f"pq.{i}"])
    # eqSp:     Sp     = ssp*sum(h, pf(h)*FF(h))
    r.append(x["Sp"] - p["ssp"] * sum(pf[h] * p["FF"][h] for h in FACTORS))
    # eqSg:     Sg     = ssg*(Td + sum Tz + sum Tm)
    r.append(x["Sg"] - p["ssg"] * total_tax)
    # eqXp(i):  Xp(i)  = alpha(i)*(sum(h, pf(h)*FF(h)) - Sp - Td)/pq(i)
    income = sum(pf[h] * p["FF"][h] for h in FACTORS)
    for i in GOODS:
        r.append(x[f"Xp.{i}"]
                 - p["alpha"][i] * (income - x["Sp"] - x["Td"]) / x[f"pq.{i}"])
    # eqpe(i):  pe(i)  = epsilon*pWe(i)
    for i in GOODS:
        r.append(x[f"pe.{i}"] - eps * p["pWe"][i])
    # eqpm(i):  pm(i)  = epsilon*pWm(i)
    for i in GOODS:
        r.append(x[f"pm.{i}"] - eps * p["pWm"][i])
    # eqepsilon: sum(i, pWe(i)*E(i)) + Sf = sum(i, pWm(i)*M(i))
    r.append(sum(p["pWe"][i] * x[f"E.{i}"] for i in GOODS) + p["Sf"]
             - sum(p["pWm"][i] * x[f"M.{i}"] for i in GOODS))
    # eqpqs(i): Q(i)   = gamma(i)*(deltam*M**eta + deltad*D**eta)**(1/eta)
    for i in GOODS:
        r.append(x[f"Q.{i}"]
                 - p["gamma"][i] * spow(p["deltam"][i] * spow(x[f"M.{i}"], p["eta"][i])
                                        + p["deltad"][i] * spow(x[f"D.{i}"], p["eta"][i]),
                                        1 / p["eta"][i]))
    # eqM(i):   M(i)   = (gamma**eta*deltam*pq/((1+taum)*pm))**(1/(1-eta))*Q(i)
    for i in GOODS:
        r.append(x[f"M.{i}"]
                 - spow(p["gamma"][i] ** p["eta"][i] * p["deltam"][i] * x[f"pq.{i}"]
                        / ((1 + taum[i]) * x[f"pm.{i}"]),
                        1 / (1 - p["eta"][i])) * x[f"Q.{i}"])
    # eqD(i):   D(i)   = (gamma**eta*deltad*pq/pd)**(1/(1-eta))*Q(i)
    for i in GOODS:
        r.append(x[f"D.{i}"]
                 - spow(p["gamma"][i] ** p["eta"][i] * p["deltad"][i] * x[f"pq.{i}"]
                        / x[f"pd.{i}"],
                        1 / (1 - p["eta"][i])) * x[f"Q.{i}"])
    # eqpzd(i): Z(i)   = theta(i)*(xie*E**phi + xid*D**phi)**(1/phi)
    for i in GOODS:
        r.append(x[f"Z.{i}"]
                 - p["theta"][i] * spow(p["xie"][i] * spow(x[f"E.{i}"], p["phi"][i])
                                        + p["xid"][i] * spow(x[f"D.{i}"], p["phi"][i]),
                                        1 / p["phi"][i]))
    # eqE(i):   E(i)   = (theta**phi*xie*(1+tauz)*pz/pe)**(1/(1-phi))*Z(i)
    for i in GOODS:
        r.append(x[f"E.{i}"]
                 - spow(p["theta"][i] ** p["phi"][i] * p["xie"][i]
                        * (1 + tauz[i]) * x[f"pz.{i}"] / x[f"pe.{i}"],
                        1 / (1 - p["phi"][i])) * x[f"Z.{i}"])
    # eqDs(i):  D(i)   = (theta**phi*xid*(1+tauz)*pz/pd)**(1/(1-phi))*Z(i)
    for i in GOODS:
        r.append(x[f"D.{i}"]
                 - spow(p["theta"][i] ** p["phi"][i] * p["xid"][i]
                        * (1 + tauz[i]) * x[f"pz.{i}"] / x[f"pd.{i}"],
                        1 / (1 - p["phi"][i])) * x[f"Z.{i}"])
    # eqpqd(i): Q(i)   = Xp(i) + Xg(i) + Xv(i) + sum(j, X(i,j))
    for i in GOODS:
        r.append(x[f"Q.{i}"]
                 - (x[f"Xp.{i}"] + x[f"Xg.{i}"] + x[f"Xv.{i}"]
                    + sum(x[f"X.{i}.{j}"] for j in GOODS)))
    # eqpf(h):  sum(j, F(h,j)) = FF(h)   — CAP only (LAB dropped, Walras)
    r.append(sum(x[f"F.CAP.{j}"] for j in GOODS) - p["FF"]["CAP"])

    return r


# ------------------------------------------------------------------
# Damped Newton with forward-difference Jacobian (47x47, stdlib only)
# ------------------------------------------------------------------

def norm2(v):
    return math.sqrt(sum(t * t for t in v))


def lin_solve(a, b):
    """Gaussian elimination with partial pivoting; a is modified."""
    n = len(b)
    m = [row[:] + [b[k]] for k, row in enumerate(a)]
    for col in range(n):
        piv = max(range(col, n), key=lambda r: abs(m[r][col]))
        if abs(m[piv][col]) < 1e-14:
            raise RuntimeError("singular Jacobian")
        m[col], m[piv] = m[piv], m[col]
        for r in range(col + 1, n):
            f = m[r][col] / m[col][col]
            if f != 0.0:
                for c in range(col, n + 1):
                    m[r][c] -= f * m[col][c]
    sol = [0.0] * n
    for r in range(n - 1, -1, -1):
        s = m[r][n] - sum(m[r][c] * sol[c] for c in range(r + 1, n))
        sol[r] = s / m[r][r]
    return sol


def newton(p, tauz, taum, x0, tol=1e-11, max_iter=100):
    names = variable_names()
    x = dict(x0)

    def rvec(xd):
        return residuals(xd, p, tauz, taum)

    r = rvec(x)
    for it in range(max_iter):
        rn = norm2(r)
        if rn < tol:
            return x, rn, it
        # forward-difference Jacobian, column per variable
        jac_t = []
        for nm in names:
            h = 1.49e-8 * max(1.0, abs(x[nm]))
            xh = dict(x)
            xh[nm] += h
            rh = rvec(xh)
            jac_t.append([(a - b) / h for a, b in zip(rh, r)])
        jac = [[jac_t[c][row] for c in range(len(names))] for row in range(len(r))]
        dx = lin_solve(jac, [-t for t in r])
        # backtracking on the residual norm
        t = 1.0
        while t >= 2.0e-4:
            xt = dict(x)
            for k, nm in enumerate(names):
                xt[nm] = x[nm] + t * dx[k]
            rt = rvec(xt)
            if norm2(rt) <= (1 - 1e-4 * t) * rn:
                x, r = xt, rt
                break
            t /= 2
        else:
            raise RuntimeError("line search failed at ||r|| = %g" % rn)
    raise RuntimeError("no convergence: ||r|| = %g" % norm2(r))


# ------------------------------------------------------------------
# Output (results.csv format) + scenarios
# ------------------------------------------------------------------

def write_csv(path, x, p):
    uu = math.prod(x[f"Xp.{i}"] ** p["alpha"][i] for i in GOODS)
    rows = [("variable", "index", "value")]
    rows += [("Y", j, x[f"Y.{j}"]) for j in GOODS]
    rows += [("F", f"{h}.{j}", x[f"F.{h}.{j}"]) for h in FACTORS for j in GOODS]
    rows += [("X", f"{i}.{j}", x[f"X.{i}.{j}"]) for i in GOODS for j in GOODS]
    rows += [("Z", j, x[f"Z.{j}"]) for j in GOODS]
    rows += [("Xp", i, x[f"Xp.{i}"]) for i in GOODS]
    rows += [("Xg", i, x[f"Xg.{i}"]) for i in GOODS]
    rows += [("Xv", i, x[f"Xv.{i}"]) for i in GOODS]
    rows += [("E", i, x[f"E.{i}"]) for i in GOODS]
    rows += [("M", i, x[f"M.{i}"]) for i in GOODS]
    rows += [("Q", i, x[f"Q.{i}"]) for i in GOODS]
    rows += [("D", i, x[f"D.{i}"]) for i in GOODS]
    rows += [("pf", h, {"CAP": x["pf.CAP"], "LAB": 1.0}[h]) for h in FACTORS]
    rows += [("py", j, x[f"py.{j}"]) for j in GOODS]
    rows += [("pz", j, x[f"pz.{j}"]) for j in GOODS]
    rows += [("pq", i, x[f"pq.{i}"]) for i in GOODS]
    rows += [("pe", i, x[f"pe.{i}"]) for i in GOODS]
    rows += [("pm", i, x[f"pm.{i}"]) for i in GOODS]
    rows += [("pd", i, x[f"pd.{i}"]) for i in GOODS]
    rows += [("epsilon", "", x["epsilon"]),
             ("Sp", "", x["Sp"]), ("Sg", "", x["Sg"]), ("Td", "", x["Td"])]
    rows += [("Tz", j, x[f"Tz.{j}"]) for j in GOODS]
    rows += [("Tm", i, x[f"Tm.{i}"]) for i in GOODS]
    rows += [("UU", "", uu)]
    with open(path, "w", newline="") as f:
        w = csv.writer(f)
        for row in rows:
            w.writerow(row)
    return uu


def check_baseline(p, x):
    """The unshocked solve must reproduce GAMS/results.csv (<= 1e-7)."""
    ref_path = os.path.join(HERE, "results.csv")
    worst = 0.0
    with open(ref_path) as f:
        for var, ix, val in list(csv.reader(f))[1:]:
            val = float(val)
            if var == "UU":
                got = math.prod(x[f"Xp.{i}"] ** p["alpha"][i] for i in GOODS)
            elif var == "pf":
                got = {"CAP": x["pf.CAP"], "LAB": 1.0}[ix]
            elif ix == "":
                got = x[var]
            else:
                got = x[f"{var}.{ix}"]
            worst = max(worst, abs(got - val))
    if worst > 1e-7:
        print(f"FAIL baseline self-check: max |diff| vs results.csv = {worst}")
        sys.exit(1)
    print(f"baseline self-check vs results.csv OK (max |diff| = {worst:.2e})")


def main():
    p = calibrate()
    x0 = benchmark_point(p)
    outdir = os.path.join(HERE, "shocks")
    os.makedirs(outdir, exist_ok=True)

    scenarios = [
        # (name, tauz override fn, taum override fn)
        ("baseline", dict(p["tauz"]), dict(p["taum"])),
        # S1 — import-tariff abolition (the textbook's canonical experiment):
        ("s1_tariff_abolition", dict(p["tauz"]), {i: 0.0 for i in GOODS}),
        # S2 — production-tax hike: tauz * 1.5 on both goods:
        ("s2_tauz_x1.5", {i: 1.5 * p["tauz"][i] for i in GOODS}, dict(p["taum"])),
    ]

    for name, tauz, taum in scenarios:
        x, rn, iters = newton(p, tauz, taum, x0)
        if name == "baseline":
            check_baseline(p, x)
            continue  # results.csv already is the baseline fixture
        path = os.path.join(outdir, name + ".csv")
        uu = write_csv(path, x, p)
        print(f"{name}: converged ||r|| = {rn:.2e} in {iters} Newton steps, "
              f"UU = {uu!r} -> {os.path.relpath(path, HERE)}")


if __name__ == "__main__":
    main()
