# Post-placement cleanup pass — idea (2026-04-21)

## Status: SUPERSEDED — implemented as Phase 0 pre-placement tuck (2026-04-27)

The original idea (post-placement small-shift cleanup) was prototyped through
several iterations as Step 1b / Step 1c (post-placement encompassment tuck),
then redesigned as a **pre-placement** Phase 0 step. See
[expanded_phase0_tuck.md](expanded_phase0_tuck.md) for the implementation.

The motivating NXF1 example is now handled correctly by the combination of:
1. Phase 0 encompassment tuck (multi-transcript names placed inside their
   block where space allows)
2. Centered-below preference for n.trn ≥ 2 genes that can't be tucked
3. B-cleanup adjusted-score sweep for remaining sub-7 placements

## Original idea (kept for record)

After all Phase 1 + Phase 2 placements are done, iterate from the lowest-scoring
feature/name combinations upward and check if small adjustments improve the layout.

### Concrete example (NXF1 plot, gencode38/minus)

1. RP11-727F15.13 name is two rows below its feature bar — moving the feature bar down one row would close the gap
2. RP11-727F15.12 name can be centered if both feature bar and name each move down one row
3. This frees up space for TMEM223 to become centered under its transcript group

### What replaced it

- **Bar-slide cleanup (B3)** — handles RP11-727F15.13/12 cases by sliding the
  bar to where the name is, then re-scoring as inline.
- **Phase 0 tuck** — handles TMEM223 case by tucking the name inside the
  block before any other placement, freeing the flank for adjacent names.
