# Expanded mode — Phase 0 pre-placement tuck and gene-block separators (2026-04-27)

## Status: implemented and validated on standard test battery

Pre-placement encompassment tuck (formerly post-placement Step 1b/1c) is now the
primary placement strategy for multi-transcript genes in expanded/squished mode.
Works in conjunction with a centered-below/above pref bump and a gene-block
separator-row insertion.

## What changed

### A. New scoring branch (expanded mode, n.trn ≥ 2)

`.score.candidate()` (R/annotations.R lines 763–839) takes two new parameters:

- `.gene.n.trn` (default `1L`) — the gene's transcript count
- `.expanded.mode` (default `FALSE`) — whether the caller is in expanded mode

When **both** flags are set AND the candidate is **truly centered**
(`adj == 0.5` AND `x` within one letter-gap of the gene block centre):

- Centered-below by 1 row: `pref = 10` (was 4)
- Centered-above by 1 row: `pref = 9` (was 2)

Edge-aligned (`adj=0/1`) and COM-shifted `adj=0.5` candidates revert to original
pref values, so the truly-centered candidate clearly wins. Below-by-2+ /
above-by-2+ unchanged.

The bump is gated to expanded callers (1744, 1792, 2061, 2215, 2376, 2481). The
collapsed2 caller at line 3000 leaves both args at default — c2 mode is
**not affected**.

### B. Phase 0 — pre-placement encompassment tuck (new)

Runs **before** Phase 1 / Phase 2 / B-cleanup, after per-feature-group setup.
For each gene with `n.trn ≥ 2` AND ≥ 2 packing rows, sorted by row count
(largest first):

1. Run the 4-corner encompassment scan (bottom-up + top-down × left + right).
2. If a tunnel is found, place the name centered (`adj=0.5`) at:
   - **Vertical row** = `1 + ceil(N/2) - (N mod 2)`-th row in scan order
     (odd N → middle; even N → one deeper)
   - **Horizontal x** = centre of the deepest tunnel
3. Add to `.feat.placements`, `.global.placed.rects`, `.fixed.placements`.

The fit threshold uses `nw + 2 × letter_width` (1-letter pad each side); the
placed rect uses the actual `nw`. Emits `[exp-tuck]` log line.

Tucked names act as fixed obstacles for Phase 1, Phase 2, and B-cleanup —
freeing the gene's flanks (left/right of the block) for adjacent genes' names.

### C. Encompassment helpers moved out of cleanup guard

`.encompass.scan`, `.com.target.corner`, `.pick.best.runs` now live before
Phase 1 (used by Phase 0). Behaviour unchanged.

Tunnel rule (re-stated for record):
- **+row1**: pick widest free interval extending from the LEFT or RIGHT block
  border that fits the (padded) name. Run the scan twice (one per side) and
  take the longer; tie → corner alignment with free-space COM.
- **+rowK (K≥2)**: tunnel = intersection of row's border-anchored free interval
  with the previous tunnel. A row whose interval is wider than the previous
  tunnel doesn't break the scan — it just doesn't narrow further.
- **Stop**: first row where no qualifying interval exists.
- **Visual extent for occupancy**: per-transcript `[start, end]` is used, with
  the start/end extended to `pr.start` / `pr.end` if the transcript has a
  cut-intron flag (`intron.from.start` / `intron.from.end`) — so cut introns
  drawn as continuation lines count as occupied.

### D. Phase 1, Phase 2, and B-cleanup skip fixed genes

- Phase 1 large-gene loop: `if (.g %in% .fixed.placements) next`
- Phase 2 small-gene info build: same skip
- B3 / B4 filters: `&& !(.g %in% .fixed.placements)` added

### E. Step 1b and Step 1c removed

Both post-placement encompassment blocks (formerly ~283 lines) deleted —
subsumed by Phase 0. The `[exp-cleanup-interior]` and `[exp-cleanup-inline]`
log tags are gone; only `[exp-tuck]` remains.

### F. Gene-block separator rows

Right after each annotation's packing is read (and before the obstacle map
is built), the packing is post-processed: a separator row (`integer(0)`) is
inserted between adjacent rows whose **large-gene** sets are **disjoint**
(both non-empty, no shared gene). The separator is excluded from gene
first/last row computations (no transcripts → no contribution to `.grow`),
so the obstacle map naturally treats it as empty.

- NXF2B / RP11-353J17.5 (different genes, fully overlapping x): separator
- MATR3.v1 / SNHG4+MATR3.v2 (different gene sets, overlapping): separator
- SNHG4+MATR3.v2 / MATR3.v2 (MATR3.v2 shared): no separator (would split MATR3.v2)
- SNU13 / CTA-216E10.11 (CTA is small, n.trn=1): no separator

Emits `[exp-sep]` log line.

## Why pre-placement?

When the tuck ran post-placement (Step 1b/1c), Phase 1/2 had already committed
to placing names *outside* the gene block. The outside-block rects occupied
the gene's flanks for the rest of the pipeline. By the time the name was
tucked, the freed flank space was no longer useful to anyone else.

Running it pre-placement means tucked names are obstacles for the rest of
the pipeline; the gene's flanks are immediately available for adjacent gene
names. The result is denser, cleaner layouts — particularly helpful for
configurations like NXF1 (RP11-727F15.13/12, RNU6-118P, TMEM223 around
NXF1) and dense small-gene clusters around large host genes.

## Verification

Standard regression battery (all passed):
NOP56, NOP58, SNU13, FBL, TAF1D, ZFAS1, GAS5, ADAR, HELLS, NR4A2, SYNGAP1,
ATP2A2, GAPDH, GADD45A, ATF4, EIF2A, EIF4A2, EIF4A3, NXF1, NXF2, NXF3,
XPO1, XPOT, XPO5, IFNAR1, IFNAR2, MATR3.

Specific cases:
- TAF6L, SYNGAP1, RPS11P6, EIF4A2, NXF2, NXF2B, SNU13 — successfully tucked.
- USP34, STX5, TMEM223 — correctly NOT tucked (cut-intron flags reflect
  the visual continuation lines as occupied space).
- MEI1 — centered-below correctly chosen (truly-centred candidate wins
  over COM-shifted).
- ATF4 — empty `.tuck.cands` path validated (no error from `-sapply()`).

## Files

- `R/annotations.R` — only file modified.
- Backups in `R_old/`:
  - `annotations_before-phase0_2026-04-27.R` — pre-refactor baseline
  - `annotations_step1bc-intersection-rule_2026-04-27.R` — last
    post-placement-cleanup version
  - `annotations_before-gene-separators_2026-04-27.R` — before separator rows

## Known limitations / future work

- **Plot height**: separator rows increase the panel height. Layout/height
  computation in `layout.R` uses `length(.packing)` so it propagates
  naturally, but extreme cases (many separators in a single annotation)
  could push panels off-screen. Not observed in tests; flag if it appears.
- **`inline_name_cleanup = FALSE`** disables Step 1 + B1–B4 only. Phase 0
  always runs. Use this to compare baseline placements against the cleanup
  sweep without losing the tuck.
- **Pref bump scope**: gated to the truly-centered candidate (within one
  letter-gap of `gene_center`). If `.generate.candidates()` ever stops
  generating that candidate (e.g. for very narrow gene blocks where the
  centred name extends past the plot edges), no candidate gets the bump
  and Phase 1/2 falls back to inline placement.
