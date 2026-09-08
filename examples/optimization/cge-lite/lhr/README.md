# LHR calibration fixtures

These CSV files are normalized fixtures for the IFPRI Lofgren, Harris &
Robinson standard CGE model (Lofgren, Harris, and Robinson 2002, "A Standard
Computable General Equilibrium (CGE) Model in GAMS", Microcomputers in Policy
Research 5, IFPRI; model file Version 1.00).

They come from the public Swazilan, test, and Zimbabwe datasets distributed
with that model.  After scaling Zimbabwe's published SAM, the source program
deliberately zeros its `LANDSH,AAGOT` cell, creating an account imbalance;
the GE-side transcription mirrors the distributed `SAMBAL.INC`
cross-entropy projection before emitting the normalized fixture.  The
original GAMS and data files are not vendored in this repository;
the GE-side ground-truth solver, a literal Python transcription of `mod100`,
emits these reduced fixtures instead.  Acknowledgment for the source data and
model belongs to IFPRI and the LHR authors.  EA independently checks balance
and all downstream calibration/solve results; the projection's KKT conditions
are checked on the GE side rather than reimplemented here.

For each dataset, `*-inputs.csv` is the calibration input boundary
(normalized SAM, sets, elasticities, TAXPAR rules, and template flags), and
`*-calib.csv` is the full calibration output used as the Haskell sentinel-test
expectation.

The `zimbabwe-{TARCUT1,FSAVINCR,PWMINCR}-solution.csv` files are
comparative-static solution fixtures emitted by the GE-side Python oracle's
`lhr_resolve.py --sim` mode.  They reproduce the three `sim100.gms` experiments
that retain the default closure: a 50% tariff cut, a 10% increase in foreign
saving, and a 10% increase in world import prices.  `TARCUT2` and `DEVAL` are
excluded because they switch closure rather than holding the default closure
fixed.
