# LHR calibration fixtures

These CSV files are normalized fixtures for the IFPRI Lofgren, Harris &
Robinson standard CGE model (Lofgren, Harris, and Robinson 2002, "A Standard
Computable General Equilibrium (CGE) Model in GAMS", Microcomputers in Policy
Research 5, IFPRI; model file Version 1.00).

They come from the public Swazilan and test datasets distributed with that
model.  The original GAMS and data files are not vendored in this repository;
the GE-side ground-truth solver, a literal Python transcription of `mod100`,
emits these reduced fixtures instead.  Acknowledgment for the source data and
model belongs to IFPRI and the LHR authors.

For each dataset, `*-inputs.csv` is the calibration input boundary
(normalized SAM, sets, elasticities, TAXPAR rules, and template flags), and
`*-calib.csv` is the full calibration output used as the Haskell sentinel-test
expectation.
