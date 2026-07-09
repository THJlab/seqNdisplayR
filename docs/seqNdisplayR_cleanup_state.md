# seqNdisplayR Cleanup Project — State Document

**Last updated:** 2026-04-17
**Conversation:** Phase 1–5 complete, Phase 6 (code optimization) complete, Phase 7 (annotation packing) in progress
**New project path:** `/Users/au103725/Dropbox/Lab_stuff/Bioinformatics/R_packages/seqNdisplayR_v2.0.0`  
**Pre-optimization backup:** `R_old/` (frozen copies of R/ files before Phase 6)

---

## 1. Project Goal

Major cleanup of the seqNdisplayR R package:
1. Better file structure (split monolithic source into logical modules)
2. Reduce external dependencies (make more robust)
3. Structured developer troubleshooting tools
4. Better error/warning messaging
5. Preserve ALL existing functionality (iterative, verified approach)
6. Code optimization (reduce repetition, break up large functions)

---

## 2. What Exists Today

### 2.1 The Main Package: seqNdisplayR

- **Location:** GitHub at `THJlab/seqNdisplayR` (plan: also mirror at `slaish/seqNdisplayR`)
- **Structure:** 12 source files, ~97 functions, modular layout (see §3.1)
- **Main entry point:** `seqNdisplay()` — the master plotting function (~600 lines)
- **Session system:** `seqNdisplayRSession` objects with S3 `plot()` and `print()` methods
- **I/O:** `LoadExcel()`, `Session2xlsx()`, `IGV2Session()`, `Session2IGV()`, `Session2Df()`
- **Shiny app:** `inst/shiny/seqNdisplayR_app.R` (~2,575 lines, single file)
- **Options table:** `inst/extdata/variable_defaults_and_help.xlsx` — drives the Shiny UI
- **Example templates:** Various `.xlsx` files in `inst/extdata/`

### 2.2 The bwimport Package

- **Location:** GitHub at `slaish/bwimport`
- **Purpose:** Fast BigWig file import via C++ (libBigWig), works on Windows (downloads URLs to temp files since libBigWig is compiled with -DNOCURL on Windows)
- **Key files:** `R/bw_import.R` (R wrapper), `src/bw_import.cpp` (C++ via Rcpp), `src/libBigWig.a` (precompiled library)
- **Exports:** `bw_import` (R-level), `bw_import_impl` (C++-level, now exported), `bw_cleanup`
- **Bug fixed in previous conversation:** Shadowed `bw_ready` atomic variable in C++ — local static in `ensure_bw_init()` shadowed the file-scope variable, causing cleanup to not properly reset the init guard. Fixed version delivered.
- **seqNdisplayR dependency:** seqNdisplayR has bwimport in `Imports:` and `import(bwimport)` in NAMESPACE. The old built-in `bw_import` was removed from seqNdisplayR.

### 2.3 Key External Dependencies (current, post-cleanup)

**Bioconductor (heavy, essential):**
- `GenomicRanges`, `IRanges`, `S4Vectors`, `BiocGenerics`, `GenomeInfoDb` — core genomic data structures
- `rtracklayer` — annotation import (`import.bed`)
- `limma` — `removeBatchEffect` for batch correction

**CRAN (essential):**
- `xml2` — IGV session XML reading/writing
- `readxl` — Excel template reading
- `writexl` — Excel template writing

**Removed in Phase 3:**
- `dplyr` → base R (`RbindFill`, `unique`, `cbind`, base subset)
- `rlist` → `ListFlatten()` in utils.R
- `jsonlite` → `deparse()`/`eval(parse(...))`
- `RCurl` → `grepl("^(https?|ftp)://", ...)`

**Shiny-specific (in Suggests):**
- `shiny`, `shinyjs`, `shinyTree`, `shinyBS`, `shinybusy`, `spsComps`, `colourpicker`

### 2.4 Remaining Known Issues / Technical Debt

- `%outside%` from IRanges not properly imported (needs `@importFrom IRanges %outside%` or NAMESPACE entry)
- `LoadIGVSession()` — legacy stub kept for backward compatibility; replaced by `IGV2Session()`
- `bundle.rds` encoding warning on load (UTF-16 BOM in serialized data)
- **Large functions needing breakup:** `seqNdisplay()` (~600 lines), `UnpackSamples()` (~450 lines), `OrganizePanelsDimensions()` (~484 lines), `AlignBasicPlotParameters()` (~355 lines), `PlotAnnotation()` (~381 lines)
- **S4 overhead (~45% of runtime):** GRanges/IRanges dispatch dominates profiling; optimization potential in reducing intermediate GRanges creation and caching annotation subsetting
- **Shiny app known issues:** The app contains many workaround patches for unpredictable behavior. Known problems include PDF save breaking when ".pdf" is appended to the filename, and various edge cases with dynamic UI elements (insertUI/removeUI). Don't assume any patch is unnecessary without testing the specific case it was written for.

---

## 3. Agreed Architecture for New Package

### 3.1 File Structure

```
seqNdisplayR/
├── R/
│   ├── zzz.R              # .onLoad, package startup
│   ├── defaults.R          # ConstantsDefaults, PlotVerticalParameters, DefaultPlotOptions, DefaultAnnotationOptions, DefaultParameters
│   ├── utils.R             # IsColor, ListDepth, DeleteNULLs, IsEmpty, AllEmpty, OrderedSplit, CommonPrefix, PrintOutput, NumberingSpacers, Empty2Null, ListFlatten, RbindFill, sNdR_log, sNdR_clear_log, NewMessages, AddMsg
│   ├── validation.R        # ParName, ScrutinizeExpandAndNameParameter, EvaluateNumericValue
│   ├── colors.R            # Hex2Hsl, AdjustColorPhi, Hsl2Hex, ConvertColor, ChangeColorLightness
│   ├── annotations.R       # ReadInAnnotations, RegionGRanges, OrganizeAnnotatedFeaturesInRegion + all sub-functions
│   ├── data_loading.R      # SortUnlistedSampleNames, UnpackSamples, ImportBigWigRegion, Log2TransformMatrix, BatchCorrectMatrix, BuildTrackList, LoadAndTransformDataForTrack, LoadTracks
│   ├── layout.R            # PlotWidths, panel/segment/height computation (~20 functions)
│   ├── plotting.R          # PreparePlottingInterface, PlotHeader, PlotScale, PlotSpacer, PlotPanels, PlotData, PlotAnnotation, PlotSegment
│   ├── seqNdisplay.R       # seqNdisplay() main function, seqNdisplayRSession, plot.seqNdisplayRSession, print.seqNdisplayRSession
│   ├── io.R                # LoadExcel, Session2Df, Session2xlsx, ParseOption, DeparseOption, Get*/Fill*/Empty* helpers, ExamplesSampleSheetsFolder, run_seqNdisplayR_app
│   └── igv_conversion.R    # Session2IGV, IGV2Session (new, replaces broken LoadIGVSession)
├── R_old/                  # Pre-optimization backup of all R/ files
├── inst/
│   ├── extdata/            # Example Excel templates, variable_defaults_and_help.xlsx
│   └── shiny/
│       └── seqNdisplayR_app.R
├── src/                    # (empty — C++ code lives in bwimport package)
├── man/                    # Generated by roxygen2
├── DESCRIPTION
├── NAMESPACE
└── tests/
    └── test_cases.R        # 27 test cases
```

### 3.2 Dependency Structure Between Files

```
defaults.R          ← no internal deps
utils.R             ← no internal deps
validation.R        ← utils.R (IsColor, PrintOutput)
colors.R            ← no internal deps (uses grDevices only)
annotations.R       ← utils.R, validation.R, defaults.R
data_loading.R      ← utils.R (bwimport package for BigWig import)
layout.R            ← defaults.R, utils.R, validation.R
plotting.R          ← defaults.R, colors.R
seqNdisplay.R       ← ALL other files (uses ParName from validation.R)
io.R                ← defaults.R, utils.R
igv_conversion.R    ← io.R, defaults.R (xml2)
```

---

## 4. Cleanup Phases

### Phase 1: Baseline Test Cases ← COMPLETE
- 23 tests covering all major code paths (now expanded to 27)
- Located at `tests/test_cases.R`
- Fast tests (T01-T08, T40-T42, T60, T70, T71): no remote data
- Medium tests (T03, T10-T14, T22-T24, T30-T31, T72): need remote annotations
- Slow tests (T20-T21): fetch remote bigwig data
- All 27/27 passing

### Phase 2: File Split ← COMPLETE
- 11 source files, 97 functions, 8,620 lines
- 7 obsolete `_obs` functions intentionally excluded
- Zero code changes from original — pure cut-and-paste
- NAMESPACE manually written with all exports and imports
- `Remotes: slaish/bwimport` added to DESCRIPTION for automatic GitHub dependency
- All tests passing

### Phase 3: Reduce Dependencies ← COMPLETE
- **rlist** removed: `list.flatten` → new `ListFlatten()` in utils.R
- **dplyr** removed: `bind_rows` → `RbindFill()` / `do.call(rbind,...)`, `bind_cols` → `cbind()`, `distinct` → `unique()`, `filter` → base subset
- **jsonlite** removed: `toJSON` → `deparse()`, `fromJSON` → `eval(parse(...))`
- **RCurl** removed: `url.exists` → `grepl("^(https?|ftp)://", ...)`
- Two new helper functions added to utils.R: `ListFlatten()`, `RbindFill()`
- All tests passing

### Phase 4: Error/Warning Messaging ← COMPLETE
- Enhanced `PrintOutput()` with internal logging (all messages logged regardless of verbosity)
- Added `sNdR_log()` — developer tool to view all messages from current session
- Added `sNdR_clear_log()` — clear the internal log
- Added `NewMessages()` and `AddMsg()` helpers for consistent message list building
- Package-level log environment `.sNdR_log_env` stores messages across function calls
- Existing `.messages` pattern preserved — zero risk of breaking existing code
- All tests passing

### Phase 5: Shiny App Cleanup ← COMPLETE
- Removed 8 duplicated functions (268 lines): OpenOptionsTable, ParseOption, DeparseOption, ListDepth, IsEmpty, AllEmpty, OrderedSplit, GetColors
- Removed commented-out IGV tabPanel, debug cat() lines, debug buttons
- Added clean header
- Exported ListDepth and GetColors (needed by Shiny app)
- Fixed OpenOptionsTable path: `shiny/` → `extdata/` (matching actual file location)
- 2,867 lines → ~2,575 lines (dead code removed)
- All workaround patches preserved
- **UI improvements:**
  - Locus name/coordinates inputs moved to always-visible wellPanel in mainPanel (visible across all tabs)
  - "Select Template and Locus" tab renamed to "Select Template"
  - Instructions made collapsible (collapsed by default) using HTML details/summary
  - Example sNdR sample files path moved inside collapsible instructions
  - "Export to IGV" download button added to sidebar
  - IGV session XML import wired up via `IGV2Session()` (replaces old `LoadIGVSession` call)
  - File input label updated to "Load sNdR sample file (Excel or IGV session XML)"
  - IGV export handler added to server (uses `Session2IGV()`)
- **Startup cleanup:**
  - `.onLoad` now only sets locale (silent)
  - Welcome message moved to `.onAttach` (only shows on `library()`, not `::` calls)
  - `@import shiny` removed from `run_seqNdisplayR_app` roxygen (prevents shiny loading at namespace load time)
- **New exports:** `IGV2Session`, `Session2IGV`, `ListDepth`, `GetColors`
- All tests passing

### Phase 6: Code Optimization ← IN PROGRESS

#### 6a. Shiny App Startup Fix ← COMPLETE
- **Problem:** `seqNdisplayR::run_seqNdisplayR_app()` printed "Loading required package: shiny" and version warning despite suppression wrappers
- **Root cause:** `shiny::runApp()` sources the app file in its own environment where `suppressMessages()` wrappers around `library()` calls don't reliably catch all startup messages
- **Fix:** Rewrote `run_seqNdisplayR_app()` in io.R to pre-load and attach all 7 Shiny packages with full suppression *before* calling `runApp()`. Confirmation message "seqNdisplayR Shiny app: all libraries loaded." prints after silent loading. App file's library block is now a silent fallback.
- **Bonus:** App locator changed from fragile `.libPaths()` loop to `system.file()`. Missing-package check now covers all 7 Shiny packages with a single clear error.
- **Files modified:** `R/io.R`, `inst/shiny/seqNdisplayR_app.R`
- All tests passing

#### 6b. ParName Refactor — Eliminate ifelse(interface=='R', ...) ← COMPLETE
- **Problem:** 73 instances of `ifelse(interface=='R', 'r_param_name', 'Shiny Display Label')` scattered throughout `seqNdisplay()`, making the parameter validation section nearly unreadable
- **Fix:** New `ParName(par_name, interface)` function in validation.R — a central lookup table mapping 40+ R parameter names to their Shiny display equivalents. Returns R name unchanged in R mode; returns human-readable label in Shiny mode. Unknown names fall through gracefully.
- **Result:** All 73 `ifelse(interface=='R', ...)` calls eliminated from seqNdisplay.R. Part 1 numeric checks restructured into readable spec tables with loops. Adding/renaming a Shiny label is now a single-line edit in `ParName()`.
- **Files modified:** `R/validation.R` (+76 lines), `R/seqNdisplay.R` (+27 lines, net)
- **New tests:** T70 (ParName R mode), T71 (ParName Shiny mode), T72 (dummy plot with interface='shiny')
- All 27/27 tests passing

#### 6c. Break Up Large Functions ← COMPLETE
- **`LoadAndTransformDataForTrack()`** (data_loading.R): 258 → 136 lines. Extracted 4 helpers:
  - `ImportBigWigRegion()` — BigWig import with chr-prefix fallback (was 40-line nested block in a for loop)
  - `Log2TransformMatrix()` — sign-aware log2 with pseudocount adjustment (**eliminated code duplication** — was copy-pasted in both log2 and batch correction blocks)
  - `BatchCorrectMatrix()` — limma batch correction with temporary log2 wrapping
  - `BuildTrackList()` — matrix → named track list with optional mean/zeroing
- **`OrganizePanelsDimensions()`** (layout.R, 484 lines): Assessed — not extracted. The function is a panel layout optimization algorithm with three tightly coupled phases. Each step depends on previous results and shares extensive state. Extraction would create helpers with 15–20 parameters called exactly once, reducing readability. Complexity is inherent, not structural.
- **`AlignBasicPlotParameters()`** (layout.R, 355 lines): Same assessment — inherently complex height alignment logic. Left as-is.
- **`PlotAnnotation()`** (plotting.R, 381 lines): Same assessment — rendering engine with deeply nested coordinate state. Three clear visual subsections (features, names/brackets, shadings) but extraction would require passing ~20 shared variables.
- **`seqNdisplay()`** (seqNdisplay.R, ~600 lines): Already cleaned by ParName refactor. The "organize panels" section (172 lines) is the largest block but requires 30+ inputs and produces 20+ outputs — extraction would not improve readability.
- All 27/27 tests passing

#### 6d. S4 Overhead Optimization ← COMPLETE
- **Goal:** Reduce S4/Bioconductor method dispatch overhead (~45% of runtime per profiling)
- **Strategy:** Cache S4 accessor results (`start()`, `end()`, `width()`, `mcols()`) into plain R vectors at function/loop entry, then use cached vectors for all comparisons and logic. Only write back to GRanges at output stage.
- **Functions optimized in annotations.R (1.4–2.2× speedup):**
  - `AnnotatedFeaturesInRegion()` — cached `.pr.start`/`.pr.end`/`.pr.ranges` (plotted region, used ~20× per annotation), `.sa.mcols`/`.sa.names` (subset annotation metadata), `.ca.mcols` (collapsed annotation metadata for revmap loop), `.fa.starts`/`.fa.ends`/`.fa.mcols` (per-feature coords). Replaced `S4Vectors::subset()` with plain vector indexing. Vectorized on-from-start/end with `pmax`/`pmin`.
  - `CoordsOfFeatName()` — cached `.fg.start`/`.fg.end`/`.fg.width` and `.pr.start`/`.pr.end`. This function is called N_features × N_font_sizes times, so per-call savings multiply.
  - `OrganizeAnnotationText()` — cached `.pr.width`/`.pr.start`/`.pr.end`, `.gr.mcols`. Replaced `S4Vectors::subset()` with vector indexing in center-of-mass loop.
  - `OrganizeAllAnnotationTextsInPlottedRegion()` — cached `.pr.start`/`.pr.end`/`.pr.granges` at entry, `.abngr.starts`/`.abngr.ends`/`.abngr.mcols` in overlap resolution loop.
- **PlotAnnotation() in plotting.R (no measurable speedup):**
  - Applied same caching strategy: `.pr.start`/`.pr.end`/`.pr.width`, `.fa.mcols`/`.fa.starts`/`.fa.ranges` per feature, `.ftg.mcols`/`.fbg.starts`/`.fbg.ends`/`.fbg.mcols` for bracket drawing, `.is.squished` cached per annotation.
  - 20/20 loci produce byte-identical PDFs (md5 differs only by timestamp, file sizes identical).
  - **Speedup: ~1.0×** — negligible. The function's runtime is dominated by graphics device calls (`rect`, `segments`, `lines`, `text`, `polygon`) and IRanges set operations (`setdiff`, `shift`, `overlapsRanges`), not by S4 accessor overhead. The optimization is correct but not impactful. Kept for code consistency with the annotations.R caching pattern.
- **Bug found and fixed (annotations.R):** Initial optimization had an indexing mismatch in the on-from-start/end clipping — a short logical vector (subset length) was used to index a full-length GRanges, causing R to recycle and clip wrong features. Fixed by using full-length logical vectors. Verified identical output on 20 loci: LMO4, HELLS, ADAR, NOP56, ATF4, NOP58, DMD, GAPDH, TP53, MYC, ACTB, BRCA1, EGFR, FMR1, HOXA1, RB1, SNHG12, UBE3A, XIST, CDK2.
- **Files modified:** `R/annotations.R`, `R/plotting.R`
- **Verification:** `tests/compare_annotations.R` (20 loci, data comparison), `tests/compare_plotting.R` (20 loci, PDF rendering comparison)
- All 27/27 tests passing + 20/20 loci data-identical + 20/20 loci render-identical

### Bug Fix: ListFlatten name concatenation (previous session)
- **Bug:** `ListFlatten` was not concatenating hierarchical names with dots
- **Effect:** `UnpackSamples` failed on deeply nested templates (elaborate example)
- **Fix:** Added `prefix` parameter to `ListFlatten` for recursive name building
- **Verification:** `ListFlatten(x)` now produces identical output to `rlist::list.flatten(x, use.names=TRUE, classes="ANY")`

### Performance Profiling Results (elaborate template, LMO4 locus)
Total time: ~6.3 seconds (pre-optimization). Breakdown:
- **S4/Bioconductor dispatch overhead (~45%):** `is`, `standardGeneric`, `callNextMethod`, `updateObject`, `validObject`, `initialize`, `getClassDef` — all GRanges/IRanges class machinery
- **BigWig data fetching (~9%):** `bwimport::bw_import_impl` — irreducible network/IO
- **Actual plotting (~5%):** `plot.xy`, `rect`, `segments`, `text.default`
- **Data transformation (~11%):** `LoadAndTransformDataForTrack`, `mean`, `extractROWS`
- **Annotation processing (~15%):** `ranges`, `elementMetadata`, GRanges subsetting
- **Key insight:** S4 dispatch is the dominant cost in annotation data processing (1.4–2.2× improvement via caching). In rendering code, graphics device calls dominate — S4 caching has negligible effect there.

### Files modified in this session (Phase 6)
- `R/io.R` — `run_seqNdisplayR_app()` rewritten (silent pre-loading, system.file locator)
- `R/validation.R` — new `ParName()` function (40+ parameter name mappings)
- `R/seqNdisplay.R` — all 73 `ifelse(interface=='R', ...)` replaced with `ParName()`, Part 1 checks restructured as spec tables
- `R/data_loading.R` — `LoadAndTransformDataForTrack` broken up: 4 new helpers (ImportBigWigRegion, Log2TransformMatrix, BatchCorrectMatrix, BuildTrackList), eliminated duplicated log2 code
- `R/annotations.R` — S4 accessor caching in AnnotatedFeaturesInRegion, CoordsOfFeatName, OrganizeAnnotationText, OrganizeAllAnnotationTextsInPlottedRegion; gene-level row packing in OrganizeOverlappingLoci; new ComputeInlineNamePlacements function; RelativeAnnotationHeight updated for inline names
- `R/plotting.R` — S4 accessor caching in PlotAnnotation; inline gene name drawing for expanded/squished modes; collapsed/collapsed2 name drawing preserved
- `R/layout.R` — EstimatePlotHeights updated for inline name extra rows
- `inst/shiny/seqNdisplayR_app.R` — library block made silent fallback, cat message removed
- `tests/test_cases.R` — 4 new tests (T60, T70, T71, T72), total now 27
- `tests/compare_annotations.R` — multi-locus S4 optimization verification (20 loci, data)
- `tests/compare_plotting.R` — multi-locus PlotAnnotation verification (20 loci, PDF rendering)
- `tests/compare_packing.R` — multi-locus annotation row packing verification (20 loci, index completeness)

### Files modified in previous session (Phases 2–5 + fixes)
- `R/zzz.R` — .onLoad/.onAttach split
- `R/utils.R` — ListFlatten fix (name concatenation), RbindFill helper, sNdR_log system
- `R/io.R` — dplyr/jsonlite/RCurl removed, OpenOptionsTable path fixed
- `R/data_loading.R` — rlist removed, RCurl removed
- `R/igv_conversion.R` — new file (Session2IGV + IGV2Session)
- `inst/shiny/seqNdisplayR_app.R` — full cleanup, UI improvements
- `NAMESPACE` — updated exports (IGV2Session, Session2IGV, ListDepth, GetColors, sNdR_log, sNdR_clear_log)
- `DESCRIPTION` — dependencies cleaned, Remotes field added

### Pending verification
- IGV export button in Shiny app — not yet tested with actual export
- IGV2Session import via Shiny — not yet tested with real IGV XML file

### Phase 7: Annotation Row Packing ← IN PROGRESS

#### 7a. Gene-Level Row Packing for Expanded Mode ← COMPLETE
- **Problem:** In `expanded` annotation packing mode, when genes overlap (e.g., snoRNAs hosted inside NOP56), all genes are merged into one group but their transcripts are stacked sequentially — each gene gets its own rows even when non-overlapping genes could share rows.
- **Root cause:** `OrganizeOverlappingLoci()` concatenated per-gene packings sequentially when merging overlapping gene groups, giving each gene its own rows regardless of genomic overlap.
- **Fix:** Two-level packing in `OrganizeOverlappingLoci()`:
  1. Level 1 (unchanged): Pack transcripts within each gene using `OrganizeOverlappingIVs`
  2. Level 2 (new): Pack genes against each other by their collapsed2 footprints using `OrganizeOverlappingIVs` at gene level. Non-overlapping genes share row blocks.
  3. Compute `gene_row_offsets` — each gene's starting row position within the merged group
  4. Build merged packing by placing each gene's transcripts at its offset position
- **New data slot:** `annot_info[[annotation]][['gene_row_offsets']]` — named list per merged group, mapping gene names to integer row offsets. Standalone genes get offset 0.
- **Row savings verified on 20 loci:**
  - NOP56: 20 → 15 rows (saved 5) — 6 snoRNAs packed into 1 row at offset 14
  - NOP58: 12 → 9 rows (saved 3) — 4 SNORDs packed at offset 8
  - SNHG12: 20 → 17 rows (saved 3) — SNORAs packed at offset 15
  - HELLS, BRCA1, FMR1, DMD, XIST: 1 row saved each
  - All other loci: unchanged (no hosted genes or genes already non-overlapping)
- **Verification:** `tests/compare_packing.R` — per-group index completeness check on 20 loci, all transcript indices present, collapsed/collapsed2 modes identical
- **Files modified:** `R/annotations.R` (OrganizeOverlappingLoci rewritten)
- All 27/27 tests passing + 20/20 loci verified

#### 7b. Inline Gene Name Placement for Expanded Mode ← IN PROGRESS
- **Goal:** Draw gene names inline with the transcript block (beside, above, or below transcripts) instead of in a separate text section below.
- **Architecture:** Three-stage pipeline:
  1. `ComputeInlineNamePlacements()` — runs between `OrganizeOverlappingLoci` and `ConvertCollapsedFormat`. Pre-computes (x, adj, row) per gene.
  2. `EstimatePlotHeights()` / `RelativeAnnotationHeight()` — read `inline_name_extra_rows` for exact height.
  3. `PlotAnnotation()` — draws from stored placements + applies `small_gene_row_overrides`.
- **Two-phase placement algorithm:**
  - **Phase 1: Large genes (≥3 transcripts)** — fixed transcript positions. Candidate grid scored by:
    - Position preference: +5 right/left-mid, +4 corners, +3 below-centered (−2/extra row), +2 below-left/right (−2/extra row), +1 above (−2/extra row)
    - Center-of-mass proximity bonus: 0 to +3 (name closer to transcript mass center scores higher)
    - Penalty: total overlap in bp (transcripts + placed names). Out of bounds = disqualifying.
    - Selection: lowest penalty first, then highest score.
  - **Phase 2: Small genes (1-2 transcripts)** — movable name+transcript units. Minimum-row-count search:
    - Try fitting ALL small genes on N rows (N=1, then 2, then 3...)
    - For each N: round-robin distribution, alternating name-left/name-right per row
    - Each candidate checked against global transcript map + placed names
    - First N that fits without overlap wins.
- **Global transcript map:**
  - Built upfront with ALL transcripts from all gene groups
  - Each transcript tagged with individual gene name (not group name)
  - Phase 1: checks all transcripts (no exclusion)
  - Phase 2: excludes only the small genes being moved (so e.g. ENSG genes in a CEP295 merged group still see CEP295's transcripts as obstacles)
  - Processing order: groups with large genes first, then small-gene-only groups
- **Data slots:** `inline_name_placements`, `inline_name_extra_rows`, `inline_name_max_row`, `gene_trn_indices`, `small_gene_row_overrides`
- **Bar-overlap hard constraint (2026-03-27):** Bar overlap in Phase 1 scoring changed from a soft penalty (weight 0.3) to a practically-hard constraint (weight 1e6). Any candidate with zero bar overlap will always beat any candidate with bar overlap. Graceful degradation retained: if all candidates overlap a bar (very crowded region), the least-overlapping candidate wins rather than crashing.
- **Phase 2 name-only conflict checking (2026-03-27/28):** Root cause of one-per-row stacking for co-located small genes (snoRNAs inside NOP56/TAF1D): the old footprint (bar + name) was used for conflict detection, and co-located small gene bars always overlap each other. Fixed by replacing footprints with name-only rectangles (`nr.right = [gene.end+gap, gene.end+gap+nw]`, `nr.left = [gene.start-gap-nw, gene.start-gap]`). Now two genes at the same genomic position share a row: one name left, one name right. Expected improvement: NOP56 6 snoRNAs → 3 rows (was 6), TAF1D 8 snoRNAs → 4 rows (was 8). Name-on-bar checks still enforce no text over large-gene bars. Bar-on-bar co-location of small genes is visually acceptable. Added `tryCatch` so Phase 2 errors are surfaced as warnings instead of silently producing one-per-row fallback.
- **Phase 1 extended candidate generation (2026-03-28):** "Below gene" candidates now extend from `gene.last.row+1` through `max(gene.last.row+3, total.packing.rows+1)` (instead of only +3). This covers all hosted snoRNA rows and one row beyond, so the scoring always finds a bar-free below-row for large gene labels like TAF1D. Prevents TAF1D label from landing on a snoRNA-occupied row.
- **Phase 1 null default (2026-03-28):** Default candidate changed from hardcoded `gene.end+gap, gene.mid.row` (could be out-of-bounds) to NULL with explicit fallback after candidate evaluation. Eliminates the case where the out-of-bounds default overrides valid scored candidates.
- **Remaining issues:**
  - **Name width calibration:** The estimated name width in bp may be too small, causing the overlap check to pass even when the rendered text visually overlaps transcripts. Need to verify `nchar × 7 × 0.023 × bp_per_cm × 1.3` against actual rendered widths, possibly increase padding factor or use actual font metrics from `strwidth()`.
  - **TAF1D gencode v38+ panel:** ENSG10010188116.1 and ENSG10010185528.1 names still overlap C11orf54 transcripts. The global map check sees C11orf54's transcripts on the correct rows, but the estimated name width may be smaller than the actual rendered width, so the overlap check passes falsely.
  - **Row sharing across groups:** Different feat.name groups share the same y-coordinate space (all `.pack.line = 1` maps to the same y). The global map correctly represents this, but the calibration issue above defeats the check.
  - **Small gene packing row count:** NOP56 snoRNAs use 3 rows when 2 might be achievable with better distribution strategies (beyond round-robin).
  - **TAF1D SNORA/SNORD cluster:** 9 genes on 5 rows — could potentially be reduced with tighter packing.
- **Files modified:** `R/annotations.R` (ComputeInlineNamePlacements, OrganizeOverlappingLoci, RelativeAnnotationHeight), `R/plotting.R` (PlotAnnotation inline drawing + small gene row overrides), `R/layout.R` (EstimatePlotHeights)
- **Collapsed/collapsed2 modes:** Unchanged — still use the original below-block name section with brackets

#### 7c. Inline Gene Name Placement for Collapsed2 Mode ← COMPLETE (2026-04-17)
- **User context:** User is exclusively testing `collapsed2` packing mode.
- **Redesign (2026-04-08 to 2026-04-17):** The candidate-scoring approach was replaced by a full ILP (Integer Linear Programming) redesign using `ompr`/GLPK. The new implementation is in `ComputeInlineNamePlacements` collapsed2 section, refactored as pure stages A–E.
- **ILP architecture (stages A–E):**
  - **Stage A:** Build `gene_info` — per-gene: bar row, name width (`nw = nchar × font_size × std_letter_width × bp_per_cm`), start/end, gap
  - **Stage B:** Build `enclosed_map` — direct-children-only (each gene assigned to its smallest containing host). Used by Stage C.
  - **Stage C:** For each host (outermost-first by span), assign `forced_side` (left/right) and `pair_row_offsets` to co-enclosed children. Consecutive pairs get L/R. Odd solo child: picks side by feasibility. After per-host pass: cross-level post-processing re-sorts all genes at the same `pair_row_offset` by genomic start and assigns L/R in order (fixes cases like ENSG10010135528.1 and ENSG10010138116.1 both being assigned "right"). Inline-left feasibility check prevents edge-pinned label: if `xl = gene.start - ng - nw < pr_start - nw×0.10`, side is set to NULL (unconstrained) rather than left.
  - **Stage D:** Generate ILP candidates for every gene: inline-right, inline-left (if feasible), faux-row-above (row 0), per-row-below, per-row-above. Scoring: above-pref `3 - 2×(rows_above-1)`, distance penalty `4×row_extra² + 0.1×x_dist_norm²`. `forced_side` restricts all candidate types.
  - **Stage E:** Solve ILP — one candidate per gene, no name–name overlaps; faux-row-above marker sets `c2_inline_name_above_rows=1` and extends y.limits.
- **Key fixes included in final implementation:**
  1. **Nested-enclosure / direct-children-only enclosed_map** — Stage B assigns each gene to its smallest containing host only; Stage C processes hosts outermost-first; nested hosts use `host.base = parent_offset + 1`.
  2. **Block-shift** — when a host gene can't go inline, shift its whole enclosed block down 1 row.
  3. **Always-pair** — consecutive co-enclosed genes unconditionally paired L/R (removed old `.try.pair = .g1.L.ok && .g2.R.ok` guard).
  4. **Cross-level pairing post-processing** — after per-host assignments, genes from different nesting depths at the same `pair_row_offset` are re-sorted by genomic start and assigned L/R in order.
  5. **Inline-left feasibility check** — Stage C: before assigning `forced_side="left"`, checks `xl >= pr_start - nw×0.10`; if not feasible, forced_side=NULL to avoid edge-pinned artifact.
  6. **`forced_side` applied to all candidate types** — not only inline, but also below/above/row-0 candidates.
  7. **Name-width factor 1.0** — `nw = nchar × font_size × std_letter_width × bp_per_cm` (factor was 1.3; reduced to allow correct inline-left placements).
  8. **Faux row above** — candidates at row 0; if used `c2_inline_name_above_rows=1`, height+1, y.limits extended; fixes MED17/RP11-606E8.2 label placement at chr11:+:93700000:93740000.
  9. **Debug logging removed** — C11orf54 candidate dump and ILP-PICK dump deleted (Step 4 complete).
- **Backups in `R_old/`:**
  - `annotations_fallback_2026-04-08.R` — stable fallback before redesign
  - `annotations_steps1-2_2026-04-15.R` — Steps 1+2 validated
  - `annotations_cross-level-pair_2026-04-17.R` — state before cross-level + factor fixes
- **Design doc:** `docs/c2names_placement_design.md` — authoritative, marked COMPLETE as of 2026-04-17.
- **Test panels (all validated):** TAF1D feature (9 enclosed snoRNAs + 4 CEP295-enclosed genes including nested SCARNA9); LMO4, HELLS, NOP56 (regression); chr11:+:93700000:93740000 gencode v38 (MED17/RP11-606E8.2 faux-row fix).
- **Files modified:** `R/annotations.R` (ComputeInlineNamePlacements collapsed2 section, full ILP redesign)

### Planned work (remaining)

#### Recently completed (2026-04-17)
- **7c: Collapsed2 ILP name placement** ← COMPLETE — full redesign with stages A–E, all test panels validated.
- **En-dash coordinate notation fix** ← COMPLETE:
  - `ParseLocus()` added to `R/annotations.R` — normalises unicode dashes, commas, space/dash separators, `:+:`/`:-:` strand notation; returns `c(chrom, strand, start, end)` or NULL.
  - Shiny app UI: two separate fields (`"gene"` + `"coordinates"`) merged into single `textInput("locus_input", ...)` with updated instructive text.
  - `GetFeature` / `GetLocus` reactives updated to use `ParseLocus(input$locus_input)`.
  - `input$gene` direct reference in IGV export block replaced with `GetFeature()`.

#### Pending
- **7c: Full visual regression** — run collapsed2 plots including in-house annotation panel; verify no regressions.
- **7b: Name width for expanded mode** — the 1.3 factor may also be too conservative in expanded mode; check against actual rendered widths via `strwidth()`.
- **Auto-omit feature names for large regions:** If plotted region contains too many features, automatically omit feature names. Add `force_feature_names = TRUE` option.
- **Annotation-aware autocomplete:** Intelligent guessing/autocompletion of locus names in Shiny when annotation is loaded.
- **Stack imbalance warning:** `Warning: stack imbalance in '::', 2 then 4` on Shiny app startup.
- **Enable IGV tests:** Uncomment T50/T51 after manual verification.
- **Push v2.0.0 to GitHub** (THJlab/seqNdisplayR + mirror at slaish/seqNdisplayR).

---

## 5. New Project Setup

**Decision:** Start a fresh R project (not modify the old one).
- Old package stays intact as safety net
- New project pulls in verified code piece by piece
- Both can coexist on the same machine via different library paths or `devtools::load_all()`

**Project name:** `seqNdisplayR` (same package name, new repo/directory)

---

## 6. Key Decisions Made

| Decision | Rationale |
|----------|-----------|
| bwimport as separate package | Keeps C++ compilation isolated; seqNdisplayR stays pure R |
| Remove built-in bw_import from seqNdisplayR | Single source of truth in bwimport package |
| Fresh R project for cleanup | Avoid contamination from old dead code and build artifacts |
| Iterative with test verification | Package is too complex for a big-bang rewrite |
| Keep all `_obs` functions out of new version | They're dead code kept for historical reference only |
| Replace `LoadIGVSession` with `IGV2Session` + `Session2IGV` | Old one was never properly implemented |
| Central `ParName()` lookup for interface labels | Eliminates 73 scattered `ifelse` calls; single edit point for label changes |
| Pre-load Shiny packages in `run_seqNdisplayR_app` | `runApp()` sources app in its own env where suppression wrappers fail |
| `R_old/` for pre-optimization backup | Frozen reference of pre-Phase-6 code |
| Two-level gene packing for expanded mode | Additive change: keeps per-gene transcript packing, adds gene-level row offsets. Lower risk than rewrite. |
| Pre-compute name placements before height calc | Names placed in ComputeInlineNamePlacements(), heights use exact row count. No guessing — placement and height use same data. |
| Inline names for expanded/squished, below-block for collapsed | Different modes have different visual needs; expanded benefits from proximity, collapsed from the established bracket+name layout. |

---

## 7. Files Already Created (all verified, all tests passing)

1. **`defaults.R`** — fully implemented
2. **`utils.R`** — fully implemented (incl. ListFlatten, RbindFill, sNdR_log system)
3. **`validation.R`** — fully implemented (ParName, ScrutinizeExpandAndNameParameter, EvaluateNumericValue)
4. **`colors.R`** — fully implemented
5. **`annotations.R`** — fully implemented
6. **`data_loading.R`** — fully implemented
7. **`layout.R`** — fully implemented (~20 functions)
8. **`plotting.R`** — fully implemented (~13 functions)
9. **`seqNdisplay.R`** — fully implemented, ParName refactored
10. **`io.R`** — fully implemented, run_seqNdisplayR_app rewritten
11. **`igv_conversion.R`** — fully implemented (Session2IGV + IGV2Session)
12. **`seqNdisplayR_app.R`** — fully cleaned, silent library loading
13. **`bw_import.cpp`** — fixed version (shadow bug + Windows path handling) — in bwimport package
14. **`bw_import.R`** — fixed version (shortPathName + better error messages) — in bwimport package
15. **`test_cases.R`** — 27 tests (23 original + T60, T70, T71, T72)
16. **`NAMESPACE`** — manually written, all exports and imports verified
17. **`DESCRIPTION`** — v2.0.0, dependencies cleaned

---

## 8. Example Data for Testing

The package examples use remote bigwig files hosted at:
```
http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/
```

Key test locus: **LMO4** (chr1, hg38)

Example Excel templates are in the package's `inst/extdata/` folder:
- `sNdR_sample_example_simple.xlsx`
- `sNdR_sample_example_elaborate.xlsx`
- `minimal_example_excel_template.xlsx`
- `minimal_example_excel_template_w_annotation.xlsx`
- `empty_excel.xlsx`
- `variable_defaults_and_help.xlsx`

---

## 9. Contact / Authors

- **SLA** — primary author of seqNdisplayR core plotting engine
- **MS** — co-author, session/Excel/IGV infrastructure, Shiny app co-author
- **JR** — Shiny app co-author

---

*This document should be uploaded at the start of any new conversation about this project.*
