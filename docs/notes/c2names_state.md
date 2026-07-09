# collapsed2 name placement — implementation state (2026-04-17)

## Status: COMPLETE

All steps implemented, debug logging removed, regression tests pass.

## What's implemented (current `R/annotations.R`)

**Step 1 — Refactor into pure stages A–E** ✓

**Step 2 — Scoring fixes** ✓
- Above-pref: `3 - 2*(rows_above-1)`
- Distance penalty: `4×row_extra² + 0.1×x_dist_norm²`

**Nested enclosure fix** ✓
- Stage B: direct-children-only `enclosed_map`
- Stage C: outermost-first processing; nested hosts use `host.base = parent_offset + 1`

**Block-shift** ✓
- Stage C: when host can't go inline, shift whole enclosed block down 1 row

**Always-pair** ✓
- Stage C: consecutive enclosed genes unconditionally paired L/R (removed old
  `.try.pair = .g1.L.ok && .g2.R.ok` feasibility guard)

**Cross-level pairing post-processing** ✓
- Stage C: after per-host assignments, genes from different nesting depths at the
  same `pair_row_offset` are re-sorted by genomic start and assigned L/R in order.
  Fixes cases like CEP295 solo child + SCARNA9 solo child both at offset 2.

**Inline-left feasibility check** ✓
- Stage C post-processing: before assigning `forced_side="left"`, checks
  `xl = gene.start - ng - nw >= pr_start - nw×0.10`. If not feasible, leaves
  the gene unconstrained (NULL) — avoids "edge-pinned to plot left boundary" artifact.

**Faux row above** ✓
- Candidates at row 0; `c2_inline_name_above_rows=1` if used; height +1; y.limits extended
- Fixed MED17/RP11-606E8.2 label placement on `chr11:+:93700000:93740000`

**`forced.side` on all candidate types** ✓
- Restricts below/above/row-0 candidates too, not only inline

**Name-width factor reduced to 1.0** ✓
- Was `nchar × font_size × std_letter_width × bp_per_cm × 1.3`
- Now `nchar × font_size × std_letter_width × bp_per_cm`
- The 1.3 factor was too conservative; inline-left placements were failing bounds
  checks unnecessarily

**Step 4 — Debug logging removed** ✓
- C11orf54 candidate dump removed
- ILP-PICK dump removed

## Deferred

- **Step 3: Label-stealing penalty** — motivating case resolved by faux row instead

## Pending unrelated issue

- Coordinate notation: `chr11:93700000–93740000` (en-dash) crashes. Only colon notation works.

## Backups in `R_old/`
- `annotations_fallback_2026-04-08.R` — stable fallback before redesign
- `annotations_steps1-2_2026-04-15.R` — Steps 1+2 validated
- `annotations_cross-level-pair_2026-04-17.R` — state before cross-level + factor fixes

## Test panels (all validated)
- TAF1D feature (9 enclosed snoRNAs + 4 CEP295-enclosed genes including nested SCARNA9)
- LMO4, HELLS, NOP56 (regression)
- `chr11:+:93700000:93740000` gencode v38 (MED17/RP11-606E8.2 faux-row fix)

Design doc (authoritative): [c2names_placement_design.md](../c2names_placement_design.md)
