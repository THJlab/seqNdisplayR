# Per-row `bigwig_directory` support — pending deeper review (2026-04-27)

The "one bigwig_directory per dataset" constraint was relaxed in v2.0.0 — `session$bigwig_dirs` is now a nested list mirroring `session$bigwigs` (per-replicate directories) instead of a flat per-dataset character vector.

## State as of 2026-04-27

- Re-applied after a revert (the original implementation entangled with another optimization). Current files: `R/io.R`, `R/data_loading.R`, `R/seqNdisplay.R`. Reference copies: `R_old/io_new.R`, `R_old/data_loading_new.R`, `R_old/seqNdisplay_new.R`. Pre-edit `.bak` siblings exist in `R/`.
- Surface verification PASSED: parse-check, fast tests T01–T09 (incl. new T09 mixed-directory round-trip), dummy plot of LMO4 on `sNdR_sample_example_simple.xlsx`.
- One bug fix during review: `seqNdisplay.R:433` `rapply` callback used scalar `if (grepl(...))` — replaced with `ifelse(...)` because leaves are now character vectors.

## Status

User flagged 2026-04-27: "Looks good at first glance. Will need to look closer into it at some point." Treat this area as not-yet-validated until that deeper review happens — be skeptical of and explicitly flag any code path that touches:

- `UnpackSamples()` with non-trivial `which_samples` / `which_reps` (replicate subsetting walked in lockstep with the new `.dirs` traversal)
- `Session2Df()` round-trip where dataset-level structure has unequal-length leaves between `bigwigs` and `bigwig_dirs`
- `LegacyBigwigDirsToNested()` for v1.x sessions supplied directly via the `seqNdisplayRSession()` constructor
- Shiny app (not exercised in this session — only static parse via T60)
- Real bigwig fetching with mixed-directory datasets (T09 only verifies session structure + `Session2Df` round-trip, not actual data load)
- Interaction with `batchCorrect = TRUE`, `whichReps`, `whichSamples` parameters
- Any code path that previously relied on `bigwig_dirs` being indexable by dataset name as a flat vector — there may be unexposed callers outside the changed files

When deep-testing happens and issues are found/closed, update or remove this note.
