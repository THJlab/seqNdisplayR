# collapsed2 Name Placement — Design Document

_Last updated: 2026-04-17_

---

## Current state — COMPLETE

All planned steps implemented and validated as of 2026-04-17.
Backup of validated Steps 1+2 state: `R_old/annotations_steps1-2_2026-04-15.R`.
Backup before cross-level pairing fix: `R_old/annotations_cross-level-pair_2026-04-17.R`.

Fixes implemented (all in current `R/annotations.R`):

- **Nested enclosure** (Stage B/C): `enclosed_map` is now direct-children-only; Stage C
  processes hosts outermost-first; nested hosts use `host.base = parent_offset + 1`.
- **Block-shift** (Stage C): when a host's name cannot go inline, the entire enclosed
  block shifts down by 1 row (start pairing from offset 1), leaving +1-below free for
  the host name.
- **Always-pair** (Stage C): consecutive enclosed genes are unconditionally paired L/R;
  the old inline-feasibility guard (`.try.pair = .g1.L.ok && .g2.R.ok`) was removed.
- **Cross-level pairing post-processing** (Stage C): after per-host assignments, genes
  from different nesting depths that land at the same `pair_row_offset` are re-sorted by
  genomic start and assigned L/R in order, fixing cases like a solo child of CEP295 and
  a solo child of SCARNA9 both ending up at the same visual row.
- **Inline-left feasibility check** (Stage C post-processing): before assigning
  `forced_side="left"`, checks whether `xl = gene.start - ng - nw >= pr_start - nw×0.10`.
  If not, the gene is left unconstrained (NULL) so the ILP can find the best available
  placement without the "edge-pinned to plot left boundary" artifact.
- **Faux row above** (Stage F/height accounting): candidates generated at row 0
  (one above row 1). If used, `c2_inline_name_above_rows = 1`, height +1,
  `y.limits` in `plotting.R` extended.
- **`forced.side` on all candidate types**: restricts below/above/row-0 candidates too,
  not only inline.
- **Name-width factor reduced to 1.0**: `nw = nchar × font_size × std_letter_width × bp_per_cm`
  (was `× 1.3`). The 30% overhead was too conservative — visual rendering showed a good
  margin, and the inflated estimate caused inline-left placements to fail the bounds check
  unnecessarily.
- **Debug logging removed** (Step 4): C11orf54 candidate dump and ILP-PICK dump removed.

Step 3 (label-stealing penalty) remains deferred — the motivating case was resolved by
the faux-row-above fix instead.

---

## Problem Statement

### Axes and rows

The plotted region spans a genomic interval [plot_start, plot_end] on the x-axis.
The y-axis is divided into **N rows** (N is determined by the solver, minimised).
Each row can display one or more non-overlapping gene bars.

### Hard constraints

1. **Bar–bar**: two bars on the same row must have at least one visible pixel of
   whitespace between them.

2. **Name–bar (other gene)**: a name must not overlap any bar belonging to a
   *different* gene on the same row.  Penalty weight: 1,000,000 × overlap_bp.

3. **Name–name**: names on the same row must not overlap each other.
   Penalty weight: 1 × overlap_bp.

4. **In-bounds**: the name must fit within [plot_start, plot_end] (10% tolerance
   at each edge).  Out-of-bounds candidates are disqualified (score = −∞).

5. **Name–own-bar**: soft penalty (1e6 × overlap_bp via self_ov term).

### Goal

Use the minimum number of rows such that all constraints above are satisfied,
while maximising the total name-placement quality score.

---

## Redesign rationale  (2026-04-10)

The incremental additions (Step 0, Phase 1, Phase 2 patches) created a pipeline
with cascading side-effects between stages: Step 0 overwrites packing2, Step 4c
mutates display rows, Phase 1 rebuilds packing2_display, Phase 2 scores against
a stale obstacle map, post-ILP moves bars again.  Each fix exposed a new
coupling.  Phase 1 never actually improved any layout — the topology was already
fixed before it ran.

### Problems with the previous approach

1. **Bar packing runs too early.** `OrganizeOverlappingLoci` runs before
   `bp_per_cm` is known, so it uses `gap = -1` (touching bars).  Step 0 patched
   this retroactively by re-running `OrganizeOverlappingIVs` inside
   `ComputeInlineNamePlacements`, but this creates two packing passes for
   collapsed2 — the initial one (wrong gap) and the corrected one.

2. **Co-enclosed pairing is coupled to packing2.** Step 4c only pairs genes
   that are "ALL on the same original display row" — so the pairing result
   depends on the initial packing, which depends on the gap, which depends on
   bp_per_cm not being available yet.  This is circular.

3. **Phase 1 bar-row MIP was impotent.** It only considered bar-row moves for
   free (non-frozen) genes within a packing2 group, but the cost-neutral
   result showed the topology was already locked by the initial packing.
   The MIP overhead produced no visual improvement.

4. **Side-effects between stages.** Step 4c mutates `display.row` and
   `packing2_display`.  Phase 1 mutates them again.  The obstacle map is rebuilt
   three times.  Post-ILP moves bars a fourth time.  Each mutation is a potential
   source of bugs and makes reasoning about the pipeline difficult.

### Design principles for the redesign

1. **One function, clear stages, no side-effects between stages.**  Each stage
   receives a data structure and returns a new one.  No stage mutates annot_info
   in place until the final commit.

2. **Single-pass bar packing with correct gap.**  Bar packing for collapsed2
   must happen after `bp_per_cm` is known, with the pixel gap from the start.
   No retroactive fix.

3. **Pairing is purely genomic.**  Co-enclosed genes are identified and paired
   by genomic containment, not by their packing2 row.  The pairing decision
   feeds into the bar packing, not the other way around.

4. **No Phase 1 bar-row MIP.**  The ILP (Phase 2) already handles name–bar and
   name–name conflicts globally.  If bar rows need adjusting, the ILP can add
   extra rows for names below/above — this is equivalent and simpler.  If future
   evidence shows that bar-row moves help, a Phase 1 MIP can be reintroduced
   with a clean interface (takes gene_info + pairing, returns row_assignments).

5. **Label-stealing penalty included from the start.**  Designed into the
   scoring formula, not patched in later.

---

## Redesigned pipeline

```
OrganizeOverlappingLoci       — packing for expanded-mode (unchanged)
                               — packing2 for collapsed2: gap=-1 (legacy, used
                                 only as initial seed; will be overridden)
    |
    v
ComputeInlineNamePlacements
    |
    +-- Stage A: Collect gene info (start, end, nw, ng, com, feat.name)
    +-- Stage B: Identify host-enclosed structure (purely genomic)
    +-- Stage C: Pair co-enclosed genes + assign forced.side (purely genomic)
    +-- Stage D: Single-pass bar packing with pixel gap
    +-- Stage E: Build obstacle map from Stage D bar rows
    +-- Stage F: Generate candidates (10 per gene, collapsed2 set)
    +-- Stage G: Score candidates (pref + dist_penalty + steal_penalty + overlaps)
    +-- Stage H: ILP name placement (GLPK, per cluster)
    +-- Stage I: Post-ILP bar co-location (single-gene groups only)
    +-- Commit: write placements + packing2_display into annot_info
```

Each stage is described below.

---

## Row numbering: group-local but visually global

**Clarified 2026-04-14.**  Packing groups (`packing2_display`) use group-local
row numbers (1, 2, 3...), but all groups draw into the **same y-space** in
`plotting.R` (line 933: y from `.pack.line`, all groups in the same plot frame).
Row 1 in group A and row 1 in group B occupy the same visual y-band — this is
safe because packing groups are defined by genomic overlap, so their bars at the
same row number never collide in x.

The ILP, obstacle map, and label-stealing penalty all operate **globally** across
groups: the obstacle map pools all groups by `display.row` key, the ILP clusters
by candidate x-span overlap regardless of group, and the label-stealing loop
iterates all genes.  Therefore **all features in the plotted region are already
organized simultaneously** — no design change needed to achieve this.

Stage D preserves this property: its re-packed row numbers are still group-local,
but they map to the same global visual positions because different groups don't
overlap in x.

---

## Stage A — Collect gene info

For each collapsed2 gene, collect:
- `start`, `end`: genomic range (from collapsed2 GRanges)
- `com`: center-of-mass from expanded transcripts (weighted exon midpoints),
  falls back to `(start + end) / 2`
- `nw`: name width in bp = `nchar(display_name) × font_size × std_letter_width × bp_per_cm`
- `ng`: inline name gap = `font_size × std_letter_width × bp_per_cm` (1 char width)
- `feat.name`: the packing2 group key this gene belongs to

**Input:** `annot_info`, `bp_per_cm`
**Output:** `gene_info` (named list, keyed by gene name)

No side-effects.  This is identical to the current Step 1.

---

## Stage B — Identify host-enclosed structure

For each gene, find all **host genes**: other genes whose collapsed2 bar fully
contains this gene's genomic range (`host.start ≤ gene.start` AND
`host.end ≥ gene.end`).

**Input:** `gene_info`
**Output:** `host_map` (gene → character vector of host gene names),
           `enclosed_map` (host → character vector of enclosed gene names)

Purely genomic — no dependency on packing2 rows.

---

## Stage C — Pair co-enclosed genes

For each host gene with ≥ 2 enclosed genes:
1. Sort enclosed genes left-to-right by `start`.
2. Assign in consecutive pairs:
   - Pair feasibility check: left gene's left-inline and right gene's
     right-inline must both fit within [plot_start, plot_end] (10% tolerance).
   - Feasible pair: both genes get a `forced.side` ("left" / "right").
   - Infeasible: the gene gets its own row-slot with whichever side fits
     (prefer "left" for consistency).
3. Assign pair-row offsets: pair 1 → offset 0, pair 2 → offset 1, etc.
   These are *relative* to whatever row the host gene ends up on.

**Input:** `gene_info`, `enclosed_map`, `plot_start`, `plot_end`
**Output:** `forced_side` (gene → "left"/"right"/NULL),
           `pair_row_offsets` (gene → integer offset, for enclosed genes only)

No dependency on packing2.  No mutation of display rows — the offsets are
consumed by Stage D.

---

## Stage D — Single-pass bar packing with pixel gap

This is the **only** bar packing step for collapsed2.  It runs **after**
Stages A–C so that:
- `bp_per_cm` is known → correct pixel gap can be computed.
- Pairing decisions are final → enclosed genes can be packed considering their
  pair offsets.

### Algorithm

For each packing2 group (multi-gene collapsed2 groups only):

1. Compute `gap_bp = ceiling(bp_per_cm / 28.35)` (1 pixel at 72 dpi).
2. Run `OrganizeOverlappingIVs(subset_annotation, gap = gap_bp)`.
3. Reorder rows: widest gene on row 1 (same as `OrganizeOverlappingLoci`).
4. For enclosed genes with `pair_row_offsets` from Stage C: adjust their
   `display.row` by adding the pair offset relative to their initial packing
   row.  If this creates a new row, extend the packing.

**Input:** `annot_info[['packing2']]`, `annot_info[['collapsed2']]`,
          `gene_info`, `pair_row_offsets`, `bp_per_cm`
**Output:** `bar_rows` (gene → display row), `packing2_display` (group → list of row vectors)

Singleton groups (1 gene) are trivially row 1.

---

## Stage E — Build obstacle map

From `bar_rows`, build:
- `global_row_bars`: row (as string key) → list of `(start, end, gene)` entries.
- `host_exclusions`: gene → character vector of host gene names (from Stage B).

**Input:** `gene_info`, `bar_rows`, `host_map`
**Output:** `global_row_bars`, `host_exclusions`

Same structure as the current Step 4 + 4b, but built once from Stage D output,
not rebuilt multiple times.

---

## Stage F — Generate candidates

For each collapsed2 gene, generate exactly 10 candidates (unchanged from current):

| # | Type | x anchor | adj |
|---|------|----------|-----|
| 1 | inline-left | `bar.start - ng` | 1 |
| 2 | inline-right | `bar.end + ng` | 0 |
| 3 | center +1 below | `(bar.start+bar.end)/2` | 0.5 |
| 4–6 | left-aligned +1/+2/+3 | `bar.start` | 0 |
| 7–9 | right-aligned +1/+2/+3 | `bar.end` | 1 |
| 10 | center −1 above | `(bar.start+bar.end)/2` | 0.5 |

Edge-case fallback: if a candidate extends past the plot region (10% tolerance),
replace with an edge-pinned substitute on the same row.

`forced.side` from Stage C suppresses inline-right (if "left") or inline-left
(if "right").

**Input:** `gene_info`, `bar_rows`, `forced_side`, `plot_start`, `plot_end`
**Output:** `candidates` (gene → list of candidate structs)

---

## Stage G — Score candidates

For each candidate, compute:

### Position preference (unchanged)

| Score | Placement |
|-------|-----------|
| 8 | Inline right or left on bar row |
| 4 | Below, +1 row |
| 3 | Above, −1 row |
| 2 | Below, +2 rows |
| 1 | Above, −2 rows |
| 0 | Below, +3 rows |
| … | Subtract 2 per additional row |

### Overlap penalties (unchanged)

- `bar_overlap`: name over any other gene's bar on the same row.  Weight: 1e6.
- `name_overlap`: name over any other placed name on the same row.  Weight: 1.
- `self_overlap`: name over own bar.  Weight: 1e6.

Host bars are excluded from `bar_overlap` on the gene's own display row
(from `host_exclusions`).

### Distance penalty (collapsed2 below/above only)

```r
name_center_x  = text.xl + nw / 2
bar_center     = (bar.start + bar.end) / 2
row_height_bp  = 0.8 × bp_per_cm
x_dist_norm    = |name_center_x - bar_center| / row_height_bp
row_extra      = row_dist - 1          # 0 for +1, 1 for +2, 2 for +3
dist_penalty   = 4 × row_extra² + 0.1 × x_dist_norm²
```

Name-width-independent.  Center-of-mass is NOT used in collapsed2 mode.

### Label-stealing penalty (collapsed2 below/above only) — NEW

#### The problem

When a name sits closer to the wrong gene's bar than to its own, the viewer
associates the label with the wrong feature.  The existing `bar_overlap`
penalty (1e6) prevents names from physically covering another bar, but it
does **not** prevent a name from hovering *just below* (or just above) the
wrong bar in empty space.

**Motivating example (chr11:93700000–93740000, gencode v38):**

The panel has 5 genes in one packing2 group.  With the fallback code the
ILP can place MED17's name centered one row below its bar (row 2, +1) — but
MED17's bar is on row 1 while RP11-606E8.2's bar is also on row 1, directly
to the right.  MED17's centered name on row 2 is geographically closer to
RP11-606E8.2's bar center than to MED17's own bar center.  A reader scanning
the plot sees "MED17" and looks at the nearest bar above — which is RP11's.
The label is technically correct but visually misleading.

#### How the penalty works

For each **below/above** candidate (not inline), compare the Euclidean distance
(in row-height-normalised units) from the candidate name position to:
1. The gene's **own** bar center (`d_own`).
2. Every **other** gene's bar center (`d_other`).

If any other bar is closer than the gene's own bar (`d_other < d_own`), the
candidate receives a penalty proportional to the gap.

```r
# All distances measured in row-height units (1 unit = 0.8 cm of plot height)
row_height_bp  = 0.8 × bp_per_cm
name_center_x  = text.xl + nw / 2
bar_center     = (gene.start + gene.end) / 2

# Distance to own bar
x_dist_norm    = |name_center_x - bar_center| / row_height_bp
d_own           = sqrt(row_dist² + x_dist_norm²)

# For each other gene h:
steal_penalty = 0
for each other gene h:
    h_bar_center   = (h.start + h.end) / 2
    h_x_dist_norm  = |name_center_x - h_bar_center| / row_height_bp
    h_row_dist     = |candidate_row - h.display_row|
    d_other        = sqrt(h_row_dist² + h_x_dist_norm²)
    if d_other < d_own:
        steal_penalty += (d_own - d_other) × STEAL_WEIGHT
```

#### Weight rationale

`STEAL_WEIGHT = 2.0`.  This means:
- A candidate where the closest other bar is 1.0 row-height-unit closer than
  the gene's own bar receives penalty = 2.0.  The below-+1 pref is 4, so
  the candidate still wins over +2-below (pref 2) unless the stealing is
  egregious.
- A candidate where two other bars are each 0.5 units closer receives
  penalty = 2 × 0.5 × 2.0 = 2.0.  Same effect.
- For a center-aligned +1 candidate directly under its own bar (x_dist=0,
  row_dist=1), `d_own = 1.0`.  Another bar on the same row as the candidate
  with bar center at the same x would have `d_other = 0`.  Penalty = 2.0 ×
  (1.0 − 0.0) = 2.0.  The candidate's final score drops from 4 − 0 = 4 to
  4 − 2 = 2, making it equivalent to a clean +2-below.  This pushes the name
  to a left-aligned or right-aligned candidate that is farther from the
  other bar.

The weight may need tuning after visual testing.  It should be large enough to
penalise obvious label-stealing but not so large that distant bars on other
rows dominate the score.

#### Edge cases and scope

| Situation | Handled? | Notes |
|-----------|----------|-------|
| Inline candidates | **Excluded** | Inline names are visually connected to their bar by adjacency. No confusion possible. |
| Genes on distant rows | **Naturally handled** | `d_other` for a gene 3+ rows away is large; it will almost never be < `d_own`. No cutoff needed. |
| Genes in other packing2 groups | **Included** | The penalty loops over all genes in `gene_info`, not just the same group. Groups share display row numbering. |
| Host bars containing this gene | **Included** | A name below a tiny bar could be closer to the host bar. The penalty correctly discourages this. |
| Self-bar | **Excluded** | The loop skips `h == gene`. |
| Performance | **Acceptable** | The loop is O(N_genes) per candidate, O(N_genes × N_candidates) per annotation. Typical panels have < 50 genes. |

#### What label-stealing does NOT replace

The `bar_overlap` penalty (1e6 per bp) is the hard constraint — a name must
not physically cover another gene's bar.  Label-stealing is a soft penalty
that discourages *proximity* even when there is no physical overlap.  Both
are needed:
- `bar_overlap` = "the name is ON TOP of the wrong bar" (hard)
- `steal_penalty` = "the name is NEAR the wrong bar" (soft)

NOT applied to inline candidates (they are visually tethered to their own bar).

### Final ILP score

```
effective_score = pref                        # no COM bonus in collapsed2
adj_penalty     = bar_overlap × 1e6
              + name_overlap
              + dist_penalty
              + steal_penalty
              + self_overlap × 1e6
score_k         = effective_score - adj_penalty
```

**Input:** `candidates`, `gene_info`, `bar_rows`, `global_row_bars`,
          `host_exclusions`, `bp_per_cm`
**Output:** `scored_candidates` (gene → list of candidates with `.score`, `.valid`,
           `.xmin`, `.xmax`, `.ymin`, `.ymax` fields)

---

## Stage H — ILP name placement

Unchanged core.  Per-cluster GLPK ILP:

1. Cluster genes by candidate x-span overlap.
2. Per cluster, flatten (gene, candidate) → 1-D index.
3. Binary `xn[n]`: choose candidate n.  Binary `yr[r]`: row r is active.
4. Constraints:
   - Each gene: exactly one candidate chosen.
   - Invalid candidates: forced to 0.
   - Row activation: `xn[n] ≤ yr[r]`.
   - No overlap: conflicting candidate pairs mutually exclusive.
5. Objective: maximize `Σ xn[n] × score[n] - λ × Σ yr[r]`.
   λ = 0.01 / R (tiebreaker for fewer rows).

**Input:** `scored_candidates`, `gene_info`
**Output:** `ilp_placements` (gene → `{x, adj, row}`)

---

## Stage I — Post-ILP bar co-location

Unchanged from current.  For single-gene packing groups only:

If ILP places name at a different row than the bar, try to move the bar IF:
1. Inline placement is feasible at the target row (right preferred, left fallback).
2. No other gene's placed name overlaps the moved bar.
3. No other gene's bar occupies the same genomic range at target row.

Bar move and inline upgrade are atomic.

**Input:** `ilp_placements`, `gene_info`, `bar_rows`, `packing2_display`,
          `global_row_bars`, `placed_name_ranges`
**Output:** updated `placements`, updated `packing2_display`

---

## Commit

Write final `c2_inline_name_placements`, `packing2_display`, and
`c2_inline_name_extra_rows` into `annot_info`.  This is the **only** place
where `annot_info` is mutated by the collapsed2 pipeline.

---

## Key constants

| Constant | Value | Notes |
|----------|-------|-------|
| Inline pref | 8 | |
| Below +1 pref | 4 | |
| Above −1 pref | 3 | above-by-1 beats +2-below |
| Below +2 pref | 2 | |
| dist_penalty (row_extra=1) | 4 | from `4 × (2-1)²` |
| Label-stealing weight | 2.0 | per (d_own − d_other) |
| inline gap | `font_size × std_letter_width × bp_per_cm` | ~1 char width |
| bar gap | `ceil(bp_per_cm / 28.35)` | 1 pixel at 72 dpi |
| font_size | 7 pt | hardcoded |
| std_letter_width | 0.023 | from ConstantsDefaults |

---

## Deferred / out of scope

1. **Phase 1 bar-row MIP:** dropped from the redesign.  The ILP (Stage H)
   handles name conflicts globally.  If future evidence shows that bar-row
   moves help, a Phase 1 MIP can be reintroduced as a clean stage between
   D and E.

2. **Expanded-mode name placement:** still uses the legacy COM-based pipeline.
   Planned alignment with the collapsed2 approach, deferred until collapsed2
   is stable.

3. **Inline overhang penalty:** was only used in Phase 1 cost model (never
   in Phase 2 scoring).  Dropped with Phase 1.  Can be reintroduced into
   Stage G scoring if needed.

---

## Implementation plan

All line numbers below refer to the **fallback** `R/annotations.R` (= the
current live file, identical to `R_old/annotations_fallback_2026-04-08.R`).

---

### Step 1: Refactor into pure stages (no new features)

**Goal:** Restructure the collapsed2 pipeline so each stage is a clearly
separated block that reads explicit inputs and returns explicit outputs.
No functional changes — the output should be identical to the fallback for
all test panels.  The only structural change is Stage D (single-pass packing
with pixel gap), which may shift some bars by 1 pixel in edge cases.

**Verification:** render the three test panels (chr11:93700000–93740000 panel 1,
TAF1D panel, in-house panel) before and after.  Compare screenshots + ILP-PICK
log lines.  Any change must be explained by the pixel-gap difference.

#### 1a. Stage A — factor out gene-info collection

Current code: lines 1246–1289 (the `for (.c2.feat.name ...)` loop that builds
`.all.c2.gene.info`).

Wrap in a helper function:

```r
.c2.collect.gene.info = function(annot_info, annot_name, c2, c2_names_vec,
                                  feature_font_size, std_letter_width, bp_per_cm) {
  # ... existing loop body ...
  # Returns: named list gene_name -> list(start, end, com, nw, ng, feat.name)
}
```

No logic change.  The function receives everything it needs as arguments and
returns `gene_info`.  The `annot_info` list is read-only.

#### 1b. Stage B — factor out host-exclusion discovery

Current code: lines 1332–1362 (Step 4b).

Wrap in a helper:

```r
.c2.find.hosts = function(gene_info) {
  # For each gene, find other genes whose bar fully contains this gene's bar.
  # Returns: list(
  #   host_map     = gene -> c(host1, host2, ...),
  #   enclosed_map = host -> c(enclosed1, enclosed2, ...)
  # )
}
```

Currently Step 4b only builds `host_map` (`.c2.exclude.from.obs`).  The
`enclosed_map` is built separately inside Step 4c (lines 1374–1390).
In the refactor, **both** maps are returned from Stage B so Stage C
doesn't need to re-derive the containment relationship.

No logic change — the containment test is the same (`h.start <= g.start AND
h.end >= g.end`).

#### 1c. Stage C — factor out co-enclosed pairing

Current code: lines 1364–1445 (Step 4c) + lines 1447–1491 (obstacle-map and
packing2_display rebuild).

Wrap in a helper:

```r
.c2.pair.enclosed = function(gene_info, enclosed_map, pr_start, pr_end) {
  # Sort enclosed genes left-to-right, assign L/R pairs.
  # Inline-feasibility-aware (checks plot bounds).
  # Returns: list(
  #   forced_side      = gene -> "left"/"right"/NULL,
  #   pair_row_offsets = gene -> integer offset (0, 1, 2, ...)
  # )
}
```

Key change from the current code: the pairing result is **not** immediately
written into `gene_info$display.row`.  Instead, `pair_row_offsets` are
returned and consumed by Stage D when computing final bar rows.

The current code (lines 1392–1403) only pairs genes that are "ALL on the same
original display row".  In the refactored version this check is **removed**:
pairing is purely genomic (based on `enclosed_map`).  The row assignment is
Stage D's responsibility.  If enclosed genes end up on different rows after
packing, the pairing still holds — the pair offsets are applied relative to
the base row that Stage D assigns.

The obstacle-map rebuild (lines 1447–1457) and packing2_display rebuild
(lines 1459–1491) that currently follow Step 4c are **removed** from this
stage.  They move into Stage E (which builds the obstacle map once, from
Stage D output) and into Stage D (which builds packing2_display as its
primary output).

#### 1d. Stage D — single-pass bar packing with pixel gap

**This is the only functional change in Step 1.**

Currently, `OrganizeOverlappingLoci` (line 507) runs `OrganizeOverlappingIVs`
with `gap = -1` (default, touching bars allowed) before `bp_per_cm` is known.
The fallback code uses these rows directly.  In the refactor, Stage D replaces
this for collapsed2 groups:

```r
.c2.pack.bars = function(annot_info, annot_name, gene_info, pair_row_offsets,
                          c2, c2_names_vec, bp_per_cm) {
  gap_bp = ceiling(bp_per_cm / 28.35)  # 1 pixel at 72 dpi
  # For each multi-gene packing2 group:
  #   1. Re-run OrganizeOverlappingIVs(subset, gap = gap_bp)
  #   2. Reorder rows: widest gene on row 1
  #   3. Read off display.row per gene
  #   4. Apply pair_row_offsets for enclosed genes
  #      (if offset pushes a gene past the current max row, extend the packing)
  #   5. Build packing2_display from the final row assignments
  # For singleton groups: display.row = 1, packing2_display = list(list(1))
  # Returns: list(
  #   bar_rows         = gene -> integer display row,
  #   packing2_display = group -> list(row1_indices, row2_indices, ...)
  # )
}
```

**Why near-identical output:** the pixel gap `ceiling(bp_per_cm / 28.35)` is
typically ~100–300 bp at standard zoom.  Most collapsed2 bars are thousands of
bp apart or clearly overlapping.  Only bars that were *exactly* touching (0 bp
gap) under the old `gap = -1` will be pushed to separate rows.  This is the
correct behaviour — touching bars are indistinguishable visually.

**Pair offset application:** if Stage C assigned pair offsets (e.g. snoRNAs
inside TAF1D: pair 1 offset=0, pair 2 offset=1, etc.), these offsets are
added to the base display row from the packing.  This replaces the current
approach where Step 4c directly mutates `gene_info$display.row`.

#### 1e. Stage E — build obstacle map (once)

Current code: lines 1319–1330 (Step 4) + rebuild at lines 1447–1457.

In the refactor, built **once** from Stage D output:

```r
.c2.build.obstacle.map = function(gene_info, bar_rows, host_map) {
  # Returns: list(
  #   global_row_bars  = row_key -> list(list(start, end, gene), ...),
  #   host_exclusions  = gene -> c(host1, host2, ...)
  # )
}
```

`host_exclusions` is just `host_map` from Stage B, carried through for clarity.
`global_row_bars` uses `bar_rows` from Stage D — not from `packing2` and not
rebuilt after mutations.

#### 1f. Stages F–I — wrap existing Steps 5–7 and post-ILP

These are structural wraps only — the code inside is unchanged.

**Stage F** (candidate generation): current lines 1502–1541.
No helper function needed — this is a straightforward loop.  Just ensure it
reads from `bar_rows` (Stage D output) instead of `gene_info$display.row`.

**Stage G** (scoring): current lines 1543–1644.
Same code.  Reads from `global_row_bars` (Stage E) and `host_exclusions`.
The `.score.candidate()` helper (lines 697–762) is unchanged in Step 1.

**Stage H** (ILP): current lines 1650–1805.
Same code.  Reads scored candidates.

**Stage I** (post-ILP bar co-location): current lines 1850–end.
Same code.  Reads ILP placements + obstacle map.

**Safety check** (lines 1817–1839): stays between Stage H and Stage I.

**Commit:** after Stage I, write `c2_inline_name_placements`,
`packing2_display`, `c2_inline_name_extra_rows` into `annot_info`.  This is
the only `annot_info` mutation in the collapsed2 pipeline.

#### 1g. Remove the double obstacle-map rebuild

The current code rebuilds `.c2.global.row.bars` at three places:
1. After Step 4 (line 1319) — initial build.
2. After Step 4c (line 1447) — because pairing changed display rows.
3. Implicit — `.gene.row.bars` is rebuilt per-gene in the scoring loop (1554).

In the refactor, build #1 is replaced by Stage E (once).  Build #2 is
removed (pairing no longer mutates display rows).  Build #3 stays — it's
the per-gene host-exclusion filtering, not a rebuild.

---

### Step 2: Scoring fixes (above-pref + distance penalty)

**Goal:** Two isolated changes to `.score.candidate()` and the collapsed2
scoring block.  No structural changes.

#### 2a. Above-pref: 1 → 3

Current code (line 745):
```r
.pref = 1 - 2 * (.rows.above - 1)  # above: lowest priority
```

Change to:
```r
.pref = 3 - 2 * (.rows.above - 1)  # above-by-1 beats +2-below
```

**Effect:** an above-by-1 candidate gets pref=3 (was 1).  It now beats a
+2-below candidate (pref=2) but still loses to +1-below (pref=4).  This
is correct: placing a name above its bar is better than skipping a row below.

Note: this change is inside `.score.candidate()` which is shared by both
expanded-mode and collapsed2.  The expanded-mode pipeline feeds different
row parameters, so verify no regression there.

#### 2b. Distance penalty formula

Current code (lines 1588–1589):
```r
.d              = sqrt(.row.dist^2 + .x.dist.norm^2)
.dist.penalty   = max(0, .d - 1)^2 * .gi$nw * 24
```

Change to:
```r
.row.extra      = .row.dist - 1       # 0 for +1, 1 for +2, 2 for +3
.dist.penalty   = 4 * .row.extra^2 + 0.1 * .x.dist.norm^2
```

**Effect:** the penalty is now independent of name width (`nw`).  Under the
old formula, genes with long names (e.g. ENSG10010135528.1) had enormous
distance penalties that made them immovable anchors.  The new formula
treats all genes equally:

| Candidate | row_extra | x_dist_norm=0 | x_dist_norm=1 |
|-----------|-----------|---------------|---------------|
| +1 below, centered | 0 | 0.0 | 0.1 |
| +2 below, centered | 1 | 4.0 | 4.1 |
| +3 below, centered | 2 | 16.0 | 16.1 |
| +1 below, edge-aligned | 0 | 0.0–0.1 | — |

**Verification:** render the three test panels.  Compare name positions.
Expect: some names that were previously stuck at +2/+3 may move to +1 or
above because the penalty no longer scales with name width.

---

### Step 3: Label-stealing penalty

**Status: deferred.** The motivating case (MED17/RP11-606E8.2) was resolved by the
faux-row-above mechanism instead. A full Euclidean label-stealing penalty was implemented
on 2026-04-15 but caused cascading ILP disruptions (other gene names displaced) and was
reverted. If future panels reveal clear label-stealing that the faux row does not address,
revisit this step. The algorithm design in the "Label-stealing penalty" section above
remains valid as a starting point.

Key lesson from the failed attempt: the penalty must not fire for inline candidates
(they are tethered to their bar by the gap constraint), and must be weak enough that
it does not override the ILP's global name–name overlap resolution.

---

### Step 4: Remove debug logging

**Goal:** Clean up all debug `message()` calls that were added during
development.  These are:

1. **C11orf54 candidate dump** (lines 1610–1641): the `if (identical(.g, "C11orf54"))`
   block inside the scoring loop.  Remove entirely.

2. **ILP-PICK dump** (lines 1795–1803): the `message(sprintf("[c2names DEBUG ILP-PICK]...`
   inside the ILP solution readback.  Remove entirely.

3. **Step 4c pairing log** (lines 1436–1440): the `message("[c2names]   paired ...`
   line.  **Keep** — this is useful operational logging, not debug noise.

4. **packing2_display rebuild log** (lines 1486–1489): the
   `message("[c2names]   packing2_display rebuilt ...` line.  **Keep** for now;
   can be removed later if too noisy.

5. **Step 3 warning** (lines 1308–1311): the out-of-range index warning.
   **Keep** — this catches data integrity issues.

**Verification:** render all three test panels.  Log output should be clean
operational messages only, no DEBUG lines.

---

### Implementation order summary

| Step | Changes | Status |
|------|---------|--------|
| 1 | Structural refactor + Stage D pixel gap | **Done** |
| 2 | Scoring: above-pref + dist penalty | **Done** |
| Nested enclosure | Stage B direct-children-only + Stage C outermost-first + block-shift | **Done** |
| Faux row above | Row-0 candidates + height accounting + y-limits extension | **Done** |
| forced.side on below/above | Candidate generation respects forced.side for all candidate types | **Done** |
| 3 | Label-stealing penalty | **Deferred** (motivating case resolved by faux row) |
| 4 | Debug log cleanup | **Pending** |

---

## Change log

### 2026-04-15

**Nested enclosure fix (Stage B/C).** `enclosed_map` changed to direct-children-only:
each gene maps to its smallest containing host only.  Stage C now processes hosts
outermost-first (largest span first); nested hosts receive `host.base = parent_offset + 1`
so their children stack below them correctly.

**Block-shift logic (Stage C).** Replaced solo-first with block-shift: when a host's
name cannot go inline (blocked by plot bounds or a neighbouring bar), the entire enclosed
block shifts down by one row (start pairing from offset 1), leaving +1-below free.

**Faux row above (Stage F + height accounting + plotting.R).** Candidates are now
generated at row 0 (one above row 1) for all genes whose bar is on row 1.  If any
name lands at row 0, `c2_inline_name_above_rows = 1` is stored, annotation height
increases by 1, and `y.limits` in `plotting.R` extend to include the row.  Unused
if no name lands there — no visual change for other loci.

**`forced.side` on below/above candidates.** Previously `forced.side` only blocked
inline-R/L candidates.  Now it also filters below, above, and row-0 candidates:
`"left"` → only `xr=gene.end` candidates; `"right"` → only `xl=gene.start` candidates.
Fixes co-enclosed genes (e.g. ENSG pair under SCARNA9) from generating names into each
other's territory.

**Label-stealing penalty attempted and reverted.** A Euclidean steal penalty was
implemented but caused cascading ILP disruptions. Reverted. Deferred to Step 3 if
needed in future.

**Backup:** `R_old/annotations_steps1-2_2026-04-15.R` (Steps 1+2, validated).
Current code includes all the additional fixes listed above on top of that backup.

### 2026-04-14

**Row numbering clarification.**  Investigated how packing2 group-local row
numbers map to visual positions in `plotting.R`.  Confirmed that all groups
share the same y-space (same plot frame, same `.pack.line` → y formula).
The obstacle map, ILP, and label-stealing penalty already operate globally
across groups.  No design change needed — added a "Row numbering" section
to the spec for clarity.

### 2026-04-10

**Reverted to fallback.** Code reverted to `R_old/annotations_fallback_2026-04-08.R`.
Pre-revert code backed up at `R_old/annotations_pre_revert_2026-04-10.R`.

**Redesign spec written.** Pipeline redesigned as pure stages A–I with no
inter-stage side-effects.  Phase 1 bar-row MIP dropped.  Label-stealing penalty
designed into Stage G from the start.  Single-pass bar packing with pixel gap
in Stage D (no retroactive Step 0 fix).  Co-enclosed pairing decoupled from
packing2 row assignment.

### 2026-04-09

**Step 0 — pixel-gap bar repacking** (reverted 2026-04-10):
Re-ran `OrganizeOverlappingIVs` with `gap = ceil(bp_per_cm / 28.35)` for each
collapsed2 group inside `ComputeInlineNamePlacements`, overwriting `packing2`
and `packing2_display`.

**Phase 1 — name-aware bar-row MIP** (reverted 2026-04-10):
Per-group binary MIP via ompr/glpk.  Never produced an improvement over
baseline cost.  Dropped in redesign.

**Above pref bumped 1 → 3** in `.score.candidate()` (reverted 2026-04-10).

### 2026-04-08 (round 3)

**Inline overhang penalty added** (then reverted from Phase 2, kept in Phase 1
cost model): tiny-bar / huge-name genes have their inline cost docked by
`min(6, overhang_rows)` in Phase 1's cost approximation.

**Distance penalty formula changed** from `(d-1)² × nw × 24` to
`4 × row_extra² + 0.1 × x_dist_norm²` — independent of name width, preventing
wide-named genes from acting as immovable anchors.

**Fallback backup:** `R_old/annotations_fallback_2026-04-08.R` created.

### 2026-04-08 (round 2)

**Strict collapsed2 candidate set** — `.generate.candidates(.collapsed2=TRUE)`:
10 fixed candidate types (inline-L, inline-R, center+1, L/R-aligned +1/+2/+3,
center-1) plus edge-pinned fallbacks.  COM-shifted candidates removed for
collapsed2.  `.forced.side` now consumed.

**Plotting fix:** min-bar-width expansion lifted out of `if (bin_size > 1)` so
single-position features render at any zoom.

**Inline pref bumped 5 → 8.**

**Step 4c pairing made inline-feasibility-aware.**

### 2026-04-07

**Post-ILP bar co-location** — atomic bar-move + inline-upgrade.
**Y_RNA.v464 fix** — moved bar to name row when inline feasible.
