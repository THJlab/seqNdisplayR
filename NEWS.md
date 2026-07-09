# seqNdisplayR 2.0.0 (2026-05-27)

First major release since `1.1.2` (2025-10-09).  Headline items: a much
faster c2 annotation-name placement solver, parallel bigwig fetch, a
substantially redesigned Shiny interface, IGV import/export improvements,
and the deployment artefacts (`Dockerfile` + `docs/deployment.md`) to host
the app for collaborators.

## Major new features

- **c2 annotation-name placement: ~33× faster on dense regions.** The ILP
  solver builds its LP via `slam::simple_triplet_matrix` + `ROI::ROI_solve`
  directly rather than going through `ompr`'s constraint-by-constraint
  DSL.  Same GLPK underneath, byte-for-byte identical placements verified
  across 61 ILP clusters / 6 canonical regions; see
  `docs/notes/ilp_optimization_step2_5.md`.  Toggle
  `options(seqNdisplayR.use_legacy_ilp = TRUE)` to fall back to the old
  build path if a future regression is suspected.
- **Parallel bigwig fetch.** `future` / `future.apply` are used to fan out
  the per-track loads across a persistent worker pool (cross-platform,
  Windows-compatible).  Gracefully falls back to sequential when
  `future` / `future.apply` aren't installed (they are in Suggests, not
  Imports).  Disable via
  `options(seqNdisplayR.disable_parallel_bw = TRUE)`.
- **Shiny: new "Plot Segments & Strands" tab.**  Drag-reorderable table
  for the plotting segment order with per-row add / remove (spacers of
  all three types, datasets, header, scale, annotations).  Removing a
  dataset from the table also unchecks it in the Data Overview tree.
  The strand-display checkboxes (`bothstrands`, `intermingled`, the
  intermingled colour picker, `neg_as_neg`, `reverse_strand`) live in
  this tab too, with the dataset-removal flow.
- **Shiny: IGV genome modal.**  Export-to-IGV now prompts for the target
  genome via a small modal (defaults to `hg38`, common identifiers
  listed); the chosen genome string is written into the exported XML.
  Previously hard-coded to `hg38`.
- **IGV session import: partial-xlsx fallback with sentinels.**  When
  `IGV2Session()` can't infer a value (synthetic `solo_N` / `group_N`
  dataset names, missing batches, missing subgroups), it writes
  `<FILL_ME: ...>` sentinel strings into the generated xlsx and the
  Shiny app auto-runs `CheckSampleFile()` to surface them as warnings.
  Walkthrough in `inst/docs/igv_import.md`.
- **`CheckSampleFile()`: password-protected URL hint.**  Unreachable
  `https://` paths now hint at the embedded-credentials syntax
  (`https://user:password@host/path`) used by labs that put bigwigs
  behind HTTP basic auth.
- **In-band annotation move-together in intermingled mode.**  When both
  strands are intermingled, dragging `annotations` in the segment-order
  table moves the `+` and `-` annotation blocks together with the
  `thickline-spacer` separator preserved between them.  See
  `docs/notes/ilp_optimization_step2_5.md`'s sibling notes.
- **Deployment artefacts.**  Ships a `Dockerfile` (rocker-based, builds
  the Shiny app with GLPK + Bioconductor + the package source) and a
  `docs/deployment.md` walking three deployment paths: lab-internal
  Shiny Server, Docker + ShinyProxy, and (blocked) shinyapps.io.

## Bug fixes

- **Unstranded bigwig directory lookup on single-`-`-strand plots.**
  Previously `bigwigs[['-']]` was patched from `bigwigs[['+']]` for
  unstranded datasets (so the filename lookup worked) but
  `bigwig_dirs[['-']]` was not, so the resolved path collapsed to just
  the filename without its directory prefix and triggered
  "non-existing file" warnings (e.g. ChIP-seq RNAPII on a `-`-strand
  locus like TAF1D).  Now both are mirrored.
- **Defensive plotting guards.**  `PlotSegment` / `PlotSpacer` /
  `PlotAnnotation` now skip cleanly when a segment in the iteration
  loop isn't present in the unified `windows_height` (which happens
  whenever the trailing-spacer cleanup drops an auto-inserted spacer
  the segment order still references).  Previously crashed with
  `"argument is of length zero"` or `"graphical parameter 'fig' has
  the wrong length"`.  Diagnostic messages are gated behind
  `options(seqNdisplayR.debug = TRUE)`.
- **`OrganizePanelsDimensions`: skip removed datasets.**  When a user
  removes a dataset from the segment-order table, the panel-dimension
  code no longer fails with `"argument of length 0"` from
  `2:NULL` / `rep(..., NULL)`.
- **`rlist` no longer required.**  The Shiny app's
  `GetShinyTrackColors` reactive now uses the package's internal
  `ListFlatten()` helper rather than `rlist::list.flatten()`.  Friends
  installing without `rlist` no longer hit
  `"there is no package called 'rlist'"` at Draw-Plot time.
- **Plot Segment Order drag-reorder: actually re-sorts the rows.**
  Initial implementation had `ordering = FALSE` in the DT options which
  let RowReorder update the order column but blocked the visible
  re-sort; switching to a forced re-init (via a hidden alternating
  column tied to a render-epoch counter) makes the rows follow the
  drag.
- **`future` "built under R version X.Y.Z" warning** silenced in the
  Shiny plot paths.
- **IGV round-trip fixes.**  Regex escape bug in `Session2IGV()` /
  `IGV2Session()` (literal `(){}` in a character class), strand suffix
  leakage from track names into subgroup_1 on re-import, and a number
  of structural fixes verified by `tests/test_igv_roundtrip.R`.
- **Windows: remote bigwig plots now work at full speed.**  Requires
  `bwimport (>= 0.2.3)` -- earlier versions unconditionally converted
  `/` to `\` on Windows in the C wrapper, mangling every `http://` URL
  into `http:\\...`. libBigWig then couldn't parse it as a URL, every
  remote track failed silently, and some hosts caused a ~50 s hang per
  file (fopen retrying an invalid Windows path with the URL host
  treated as a UNC share). See `bwimport` NEWS 0.2.3 for the underlying
  fix. On a fresh Windows install of seqNdisplayR 2.0.0 the shipped
  LMO4 example plot now renders in seconds, matching macOS/Linux.
- **libBigWig `[bwHdrRead]` / `[bwOpen]` chatter silenced.** When a
  remote bigwig fetch's first attempt fails (transient curl hiccup,
  header retry, alt-chrom fallback), libBigWig used to print a pair of
  `fprintf(stderr, ...)` lines per attempt into the R console. These
  are cosmetic — the retry logic in `.fetch_bw_raw()` and the batched
  fetcher handle the actual failures — but they cluttered the output
  on every first plot in a session. `.onLoad()` now sets
  `BWIMPORT_QUIET=1` (respected by `bwimport >= 0.2.1`), which suppresses
  the C-level prints. Set `Sys.setenv(BWIMPORT_QUIET = "")` before
  loading the package to see the original diagnostics.

## UI and labelling

- **Tab rename**: "Plot Segment Order" → "Plot Segments & Strands"
  (broader scope now that strand-display checkboxes live here too).
- **"Display a dummy plot" → "Display a test plot"** in the Plot
  Layout tab.
- **"Color Display of Data from Negative Strand"** moved from Plot
  Segments & Strands to Track Colors (only shown when intermingled is
  enabled, since it doesn't apply otherwise).

## Breaking changes for v1.x users

- **System requirement: GLPK ≥ 4.57**
  (`libglpk-dev` on Debian/Ubuntu, `brew install glpk` on macOS,
  bundled with Rtools on Windows).  Needed by `ROI.plugin.glpk` /
  `ompr.roi` for the annotation-name ILP.
- **`bwimport` is GitHub-only.**  `remotes::install_github(...)` (or
  the install-from-local approach in the README) pulls it in
  automatically via the `Remotes:` field; plain
  `install.packages(seqNdisplayR_2.0.0.tar.gz, repos = NULL)` will
  not, and will error on the missing dependency.
- **New CRAN dependencies in `Imports`**: `DT`, `ompr`, `ompr.roi`,
  `ROI.plugin.glpk`, `slam` — installed automatically with the
  package.  The Shiny stack (`shiny`, `shinyjs`, `shinyTree`,
  `shinyBS`, `shinybusy`, `spsComps`, `colourpicker`, `DT`) stays in
  `Imports` as in v1.x, since the Shiny app is the main usage path.
- **`future`, `future.apply`** are in `Suggests` and only used when
  available; install them for parallel bigwig fetch.

## Known limitations / deferred to a follow-up

- The defensive plotting guards silently skip mismatched segments by
  default.  Set `options(seqNdisplayR.debug = TRUE)` if a plot looks
  wrong and you want to see which segment got skipped.
- Tree ↔ segment-table sync is one-way (table → tree).  Manually
  editing samples in the Data Overview tree doesn't reshape the
  segment table.
- A public hosted Shiny instance is still a TODO; `Dockerfile` +
  `docs/deployment.md` ship the recipe.

## Documentation

- New `docs/deployment.md` (Shiny Server / ShinyProxy / shinyapps.io
  trade-offs).
- New `docs/notes/ilp_optimization_step2_5.md` covering the LP backend
  rewrite, byte-equality verification, and rollback toggle.
- New `inst/docs/igv_import.md` walking the IGV → xlsx workflow.
- Carried-forward vignettes from v1.x under `vignettes/` (renamed from
  the v1.x `vignette/` typo).
