# ILP placement solver: direct ROI sparse-matrix build (2026-05-21)

## Status: COMPLETE — shipping in v2.0.0

## Summary

Replaced the per-cluster `ompr::MIPModel()` → `ompr::add_constraint()` build path
inside `.solve.name.placement.ilp` (in `R/annotations.R`) with a direct
`slam::simple_triplet_matrix()` + `ROI::L_constraint()` + `ROI::ROI_solve()`
build path. Same GLPK solver underneath, so the LP itself is mathematically
identical. The only thing that changed is the speed at which the LP is
*constructed*.

## Measured impact (HeLa IGV fixture, 6 canonical regions, median of 3 reps)

| Region   | ILP-only legacy → direct | Total cold-plot legacy → direct |
|----------|--------------------------|----------------------------------|
| LMO4     | 3.60s → 1.27s  (−64.7%)  | 10.30s →  8.35s  (−18.9%)        |
| GADD45A  | 0.25s → 0.014s (−94.4%)  |  6.56s →  5.55s  (−15.4%)        |
| ADAR     | 0.89s → 0.030s (−96.6%)  |  7.69s →  7.41s  (−3.6%)         |
| TAF1D    | 23.97s → 18.50s (−22.8%) | 33.13s → 26.81s  (−19.1%)        |
| EIF4A2   | 0.89s → 0.028s (−96.9%)  |  8.27s →  7.29s  (−15.5%)        |
| NOP56    | 1.61s → 0.045s (−97.2%)  |  9.12s →  6.78s  (−27.7%)        |

Cold-plot residual is dominated by bigwig fetch + drawing, not ILP.

TAF1D shows the smallest relative ILP win (still −23%) because its 12-gene
clusters have ~144 binary variables with O(N²) conflict-pair constraints —
the *GLPK B&B* itself takes ~9s per cluster. Sub-stage timing confirms:

```
ilp_conflicts    0.011s     (pair-building loop)
ilp_build_direct 0.002s     (sparse-matrix + ROI::OP)
ilp_solve_glpk   18.474s    (GLPK B&B over 4 such clusters)
```

The direct build is essentially free (2 ms per cluster). The TAF1D residual
is genuine combinatorial work in the solver, not anything we can vectorize on
the R side without changing the model.

## Correctness gate

`tests/test_ilp_verify_backends.R` solves every cluster with BOTH backends
and asserts byte-for-byte equality of (a) objective value and (b) chosen
variables. Run via `Rscript tests/test_ilp_verify_backends.R`. Last run:

```
Total clusters verified: 61
Objective mismatches  : 0
Tied tie-break diffs  : 0
[PASS] both backends agree byte-for-byte on objectives AND chosen variables.
```

`tests/test_ilp_benchmark.R` is the regression harness: it snapshots
placements per region into `tests/baseline_placements.rds` and asserts
`identical(current, baseline)` per region on every subsequent run. Stage
and sub-stage timings go to `tests/baseline_timings.csv`.

## Rollback path

```r
options(seqNdisplayR.use_legacy_ilp = TRUE)
```

The legacy ompr branch is preserved inline in `.solve.name.placement.ilp` as
a closure `.solve_legacy()`. With the option set, every cluster runs through
the old path verbatim — useful if a regression is ever traced to the LP
solver path. Default is FALSE (direct path).

## Verify mode (future maintenance)

```r
options(seqNdisplayR.verify_ilp = TRUE)
```

Runs BOTH backends on every cluster, compares objectives + chosen variables,
emits `message("[verify-ilp] ...")` on mismatch. Counts accumulate into the
stage-timer env if also set. Doubles solve time, so don't leave on
permanently.

## What was tried and rejected

- `control = list(presolve = TRUE)` on `ROI_solve` (GLPK LP presolver) —
  causes 10/61 objective-value mismatches on the same test set. Not safe.
  Suspect presolve reformulates the LP in a way ROI's status accounting
  doesn't track. Reverted; not shipping.
- Vectorizing Stages B/C/D (`.find.hosts`, `.pair.enclosed`, `.c2.pack.bars`)
  — these now run in 2–16 ms total even on TAF1D. Below the noise floor; not
  worth touching for v2.0.0.

## Steps NOT taken (for v2.1+)

- TAF1D ILP residual is ~75% of its cold-plot time and ~95% of its ILP time
  is in GLPK B&B. Reducing it requires either model reformulation (high
  risk, can change placements) or a different solver (CBC, HiGHS) — out of
  scope for v2.0.0.
- The conflict-pair construction loop (~10 ms on TAF1D) could be vectorized
  with `GenomicRanges::findOverlaps` if needed; deferred — it's a rounding
  error vs the GLPK solve.

## Files affected

- `R/annotations.R` — `.solve.name.placement.ilp` rewritten with backend
  dispatch + closures `.solve_legacy()` / `.solve_direct()`; sub-stage
  `.tic`/`.toc` hooks for `ilp_conflicts`, `ilp_build_direct`,
  `ilp_solve_glpk`.
- `tests/test_ilp_benchmark.R` — new regression + timing harness, 6 canonical
  regions × 3 cold reps each.
- `tests/test_ilp_verify_backends.R` — new side-by-side correctness harness.
- `tests/baseline_placements.rds` — gold-standard placements (regenerated
  against direct path; identical to legacy).
- `tests/baseline_timings.csv` — new timing floor for future regression
  detection.
- `tests/bench_pdfs/<region>.pdf` — visual snapshot per region (rep 1 of
  each region's run).
- `R/annotations.R.bak-pre-step-ILP` — backup before the rewrite.
- `R/annotations.R.bak-pre-bench` — backup before adding stage-timer hooks.
