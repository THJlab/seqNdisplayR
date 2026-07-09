# seqNdisplayR — Remaining Tasks

**Created:** 2026-04-21  
**Last updated:** 2026-04-27  
**Status:** Inline name placement effectively complete; cleanup and polish remaining

---

## High Priority — Functional

### 1. Remove debug logging from annotations.R
Debug messages added during expanded name placement development should be removed before release. Current `[exp-*]` tags (post-Phase-0 refactor):
- `[exp-debug]` — packing group dump (~line 1534)
- `[exp-sep]` — gene-block separator insertion (~line 1570)
- `[exp-obsmap]` — obstacle map dump (~line 1653)
- `[exp-grp]` — feature group contents (~lines 1728, 1733)
- `[exp-tuck]` — Phase 0 pre-placement tuck (~line 1941)
- `[exp-P1-obs]` — Phase 1 obstacle check at chosen position (~line 2032)
- `[exp-P1]` — Phase 1 placement summary (~line 2045)
- `[exp-phase2]` — host adaptive decision (~line 2142)
- `[exp-P2]` — Phase 2 candidate summary and final placement (~lines 2363, 2379, 2381)
- `[exp-cleanup]`, `[exp-cleanup-dbg]` — B3/B4 cleanup sweep (~lines 2687, 2725, 2750, 2770)

The `[exp-cleanup-interior]` and `[exp-cleanup-inline]` tags from the old post-placement Step 1b/1c are gone (those blocks were removed in the Phase 0 refactor).

These are useful during development but will clutter user output.

### 2. Verify collapsed2 regression
After all expanded-mode changes — including the 2026-04-27 Phase 0 refactor, `.score.candidate()` signature change (added `.gene.n.trn` and `.expanded.mode` params with safe defaults), and gene-block separator insertion — run the standard collapsed2 test panels:
- TAF1D, NOP56, LMO4, HELLS, chr11:+:93700000:93740000
- All should be identical to pre-change output. The expanded-mode pref bump is gated by `.expanded.mode = TRUE`; the c2 caller at line ~3000 passes neither new arg, so it inherits both defaults (`1L`, `FALSE`) and behaviour is preserved.
- Gene separators are inserted only into `[['packing']]` (expanded). c2 uses `[['packing2']]` and is untouched.

### 3. Deploy shinyApp on a server
Set up a hosted instance of the seqNdisplayR Shiny app so it is publicly accessible without requiring local R installation. Options include Shiny Server (open source or Pro), shinyapps.io, or a containerized deployment (Docker + ShinyProxy).

### 4. Verify pending Shiny features
- IGV export button — not yet tested with actual export
- IGV2Session import via Shiny — not yet tested with real IGV XML file

---

## Medium Priority — Code Unification (Plan Steps 6–7)

### 4. Step 6: Unify output slots and drawing/height code
Merge the separate expanded and collapsed2 slot names into shared names. Currently:
- `inline_name_placements` (expanded) / `c2_inline_name_placements` (collapsed2)
- `inline_name_extra_rows` / `c2_inline_name_extra_rows`
- `inline_name_above_rows` / `c2_inline_name_above_rows`

Unify to single slot names and merge the drawing blocks in plotting.R (lines ~1106–1144) and height-calc branches in layout.R (lines ~818–827).

**Files:** annotations.R, plotting.R, layout.R  
**Risk:** Medium — previous attempt caused severe regression (2026-04-20). Must be done incrementally with backups of ALL files.

### 5. Step 7 (optional): Merge expanded and collapsed2 main loops
After Step 6, the two independent loops in `ComputeInlineNamePlacements` could become a single loop with mode-specific branches. Structural cleanup only — no behaviour change.

### 5b. Validate gene-block separator behaviour on broader test set (2026-04-27)
Phase 0 inserts visual separator rows between adjacent packing rows whose large-gene (n.trn ≥ 2) sets are disjoint. Validated on the standard test battery and the NXF2/MATR3 cases that motivated it. Watch for edge cases:
- Annotations with many adjacent gene transitions could push panel height up significantly.
- Configurations where a single-transcript "small" gene visually separates two large genes — current rule treats this as no-separation, which may or may not match user expectation.
- Plotting/layout interaction: `length(.packing)` is used for height calculation; verify that the empty rows render as visual gaps without artefacts.

See [notes/expanded_phase0_tuck.md](notes/expanded_phase0_tuck.md) for the full implementation.

---

## Low Priority — Polish and Technical Debt

### 6. Optimize pair-overlap organization in `.pair.enclosed()`
Current fix (2026-04-21): when two consecutive enclosed features overlap genomically, the first goes solo instead of pairing. This greedy approach doesn't search for a non-overlapping partner further in the list. A smarter approach could find optimal non-overlapping L/R pairs (e.g., interval-graph matching), potentially saving rows when multiple overlapping features are interspersed with non-overlapping ones.

**Concrete example — EIF4A2 host (gencode38/plus):**

Enclosed features sorted by start position:
1. MIR1248 (chr3:186,788,128–186,788,222)
2. SNORA81 (chr3:186,791,492–186,791,625)
3. SNORA63.v3 (chr3:186,793,987–186,794,118)
4. SNORA63 (chr3:186,795,685–186,795,816)

Current greedy pairing (left-to-right consecutive):
- Try pair MIR1248 + SNORA81 → **overlap check passes** → paired on row 1 (L/R)
- Try pair SNORA63.v3 + SNORA63 → **overlap check passes** → paired on row 2 (L/R)
- Result: **2 rows** ✓ (this case already works well)

Hypothetical worst case (if features were reordered or additional overlapping features existed):
- If consecutive features overlap, the first goes solo → wastes a row
- A non-greedy algorithm could look ahead to find the best non-overlapping partner, potentially saving rows when overlapping and non-overlapping features are interleaved

### 7. Name width calibration
Estimated name width (`nchar × font_size × std_letter_width × bp_per_cm`) may underestimate actual rendered width for some fonts/sizes. Could cause false-pass on overlap checks. Consider using `strwidth()` for exact measurements or increasing padding factor.

### 8. Known technical debt (from cleanup state doc)
- `%outside%` from IRanges not properly imported (needs `@importFrom` or NAMESPACE entry)
- `LoadIGVSession()` — legacy stub kept for backward compat; replaced by `IGV2Session()`
- `bundle.rds` encoding warning on load (UTF-16 BOM in serialized data)
- Large functions still monolithic: `seqNdisplay()` (~600 lines), `UnpackSamples()` (~450 lines), `OrganizePanelsDimensions()` (~484 lines) — assessed as inherently complex, extraction not beneficial

---

## Completed (session of 2026-04-21)

- [x] Fix `reduce` → `range` for collapsed annotation building (no more `#DUPNAME#` splitting)
- [x] Remove `.exclude.genes = .g` from Phase 1 scoring (prevents names inside own gene block)
- [x] Fix obstacle map to use collapsed2 ranges (full gene span, not truncated exon extents)
- [x] Add `inline_name_above_rows` support for expanded mode (height calc + y-limits + drawing)
- [x] Fix Phase 2 distance measurement: from nearest gene edge, not from display row
- [x] Match collapsed2 scoring for above-row candidates (no artificial penalty, pref=3 < below pref=4)
- [x] Pre-compute host-enclosed detection before Phase 1 (exclude enclosed blocks from host's obstacle map)
- [x] Restrict COM bonus to gene rows only (centering for below/above); controlled by `center_of_mass` parameter
- [x] Thread `center_of_mass` parameter through to `ComputeInlineNamePlacements`
- [x] Prevent pairing of overlapping enclosed features in `.pair.enclosed()`

## Completed (sessions of 2026-04-22 to 2026-04-27)

Adjusted-score cleanup and post-placement tuck (later folded into Phase 0):

- [x] B1–B4 adjusted-score cleanup sweep (bar slide + name re-place when adj_score < 7)
- [x] Adaptive COMPACT/SPREAD host-enclosed sizing (N(host)+M vs N(total))
- [x] SPREAD base-row computation accounts for nested large-gene hosts (e.g. SNHG4 inside MATR3.v1)
- [x] Step 1 inline snap → snap inline names to nearest transcript end on the same row
- [x] Step 1b/Step 1c (post-placement encompassment tuck, bottom-up + 4-corner) — **superseded by Phase 0**
- [x] Cut-intron occupancy: per-transcript `intron.from.start/.end` flags extend visual occupancy to plot border so cut introns count as occupied space
- [x] `incl_feature_names = FALSE` correctly skips the entire expanded placement loop (matches the c2 block)
- [x] Console expanded by default in Shiny app (`tags$details(open = NA, ...)`)
- [x] Phase 0 pre-placement encompassment tuck — see [notes/expanded_phase0_tuck.md](notes/expanded_phase0_tuck.md)
- [x] `.score.candidate()` centered-below/above pref bump (n.trn ≥ 2, expanded mode, truly-centered candidate only)
- [x] Phase 1 / Phase 2 / B3 / B4 skip `.fixed.placements` (tucked genes)
- [x] Gene-block separator rows in expanded packing (disjoint large-gene sets only; preserves contiguous genes spanning the boundary)
