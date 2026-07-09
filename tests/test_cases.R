# =============================================================================
# seqNdisplayR Phase 1 Test Suite
# =============================================================================
#
# PURPOSE: Verify all major code paths before and after each cleanup step.
#
# USAGE:
#   source("tests/test_cases.R")
#   run_all_tests()             # run everything
#   run_test("T01")             # run a single test
#   run_tests(c("T01", "T02"))  # run selected tests
#
# Each test returns TRUE (pass) or FALSE (fail) and prints a status line.
# The "elaborate" template loads remote data — allow ~30-60 seconds per
# plotting test depending on connection speed. Use the "simple" template
# or dummy_plot=TRUE tests for quick iterations.
#
# =============================================================================

library(seqNdisplayR)

# ---- paths ------------------------------------------------------------------
.test_env <- new.env(parent = emptyenv())
.test_env$extdata   <- seqNdisplayR::ExamplesSampleSheetsFolder()
.test_env$xl_simple <- paste0(.test_env$extdata, "sNdR_sample_example_simple.xlsx")
.test_env$xl_elab   <- paste0(.test_env$extdata, "sNdR_sample_example_elaborate.xlsx")
.test_env$xl_minimal <- paste0(.test_env$extdata, "minimal_example_excel_template.xlsx")
.test_env$xl_minimal_annot <- paste0(.test_env$extdata, "minimal_example_excel_template_w_annotation.xlsx")
.test_env$tmpdir    <- file.path(tempdir(), "sNdR_tests")
.test_env$results   <- list()

if (!dir.exists(.test_env$tmpdir)) dir.create(.test_env$tmpdir, recursive = TRUE)


# ---- test runner ------------------------------------------------------------

run_test <- function(test_id) {
  test <- .test_registry[[test_id]]
  if (is.null(test)) {
    cat(sprintf("  [???] %s — unknown test ID\n", test_id))
    return(invisible(FALSE))
  }
  cat(sprintf("  Running %s: %s ... ", test_id, test$desc))
  t0 <- Sys.time()
  result <- tryCatch({
    test$fn()
    TRUE
  }, error = function(e) {
    cat(sprintf("\n         ERROR: %s\n", conditionMessage(e)))
    FALSE
  })
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  .test_env$results[[test_id]] <- result
  cat(sprintf("%s (%s sec)\n", ifelse(result, "PASS", "FAIL"), elapsed))
  invisible(result)
}

run_tests <- function(test_ids) {
  cat("=== seqNdisplayR Test Suite ===\n\n")
  for (id in test_ids) run_test(id)
  .print_summary()
}

run_all_tests <- function() {
  run_tests(names(.test_registry))
}

run_fast_tests <- function() {
  fast <- names(.test_registry)[sapply(.test_registry, function(t) isTRUE(t$fast))]
  cat("=== Fast tests only (no remote data) ===\n\n")
  for (id in fast) run_test(id)
  .print_summary()
}

.print_summary <- function() {
  n_pass <- sum(unlist(.test_env$results) == TRUE)
  n_fail <- sum(unlist(.test_env$results) == FALSE)
  n_total <- length(.test_env$results)
  cat(sprintf("\n=== %d / %d passed", n_pass, n_total))
  if (n_fail > 0) {
    failed <- names(.test_env$results)[!unlist(.test_env$results)]
    cat(sprintf(" | FAILED: %s", paste(failed, collapse = ", ")))
  }
  cat(" ===\n")
}


# ---- test registry ----------------------------------------------------------
.test_registry <- list()

# helper to register tests
.reg <- function(id, desc, fn, fast = FALSE) {
  .test_registry[[id]] <<- list(desc = desc, fn = fn, fast = fast)
}


# =============================================================================
# T01-T05: Excel Loading and Session Structure
# =============================================================================

.reg("T01", "LoadExcel simple (no annotations)", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = FALSE)
  stopifnot(inherits(sess, "seqNdisplayRSession"))
  stopifnot(all(c("samples", "colors", "bigwig_dirs", "bigwigs", "parameters") %in% names(sess)))
  stopifnot(length(names(sess$samples)) == 4)  # 3-seq, TT-seq, RNA-seq, ChIP-seq
}, fast = TRUE)

.reg("T02", "LoadExcel elaborate (no annotations)", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_elab, load_annotations = FALSE)
  stopifnot(inherits(sess, "seqNdisplayRSession"))
  stopifnot(length(names(sess$samples)) == 4)
  # elaborate has deeper nesting (subgroup_1, _2, _3)
  stopifnot(is.list(sess$samples[["3-seq"]]))
}, fast = TRUE)

.reg("T03", "LoadExcel with annotations", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  stopifnot(!is.null(sess$annots))
  stopifnot(length(sess$annots) == 3)  # gencode v38, in-house, ChIP-peaks
  stopifnot(all(sapply(sess$annots, function(a) inherits(a, "GRanges"))))
}, fast = FALSE)

.reg("T04", "LoadExcel minimal template", function() {
  if (!file.exists(.test_env$xl_minimal)) return()  # skip if not present
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_minimal, load_annotations = FALSE)
  stopifnot(inherits(sess, "seqNdisplayRSession"))
}, fast = TRUE)

.reg("T05", "print.seqNdisplayRSession works", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = FALSE)
  out <- capture.output(print(sess))
  stopifnot(length(out) > 0)
}, fast = TRUE)


# =============================================================================
# T06-T08: Session Round-Trip (Excel write → read)
# =============================================================================

.reg("T06", "Session2xlsx and reload", function() {
  sess1 <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = FALSE)
  outfile <- file.path(.test_env$tmpdir, "roundtrip_test.xlsx")
  seqNdisplayR::Session2xlsx(sess1, outfile)
  stopifnot(file.exists(outfile))
  sess2 <- seqNdisplayR::LoadExcel(outfile, load_annotations = FALSE)
  stopifnot(identical(names(sess1$samples), names(sess2$samples)))
  stopifnot(identical(names(sess1$parameters), names(sess2$parameters)))
}, fast = TRUE)

.reg("T07", "Session2Df produces valid data.frame", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = FALSE)
  df <- seqNdisplayR::Session2Df(sess$samples, sess$colors, sess$bigwigs, sess$bigwig_dirs, sess$parameters)
  stopifnot(is.data.frame(df))
  stopifnot(all(c("color", "bigwig_directory", "bigwig_file", "strand", "dataset", "subgroup_1") %in% colnames(df)))
  stopifnot(nrow(df) > 0)
}, fast = TRUE)

.reg("T08", "Session2Df elaborate preserves subgroups", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_elab, load_annotations = FALSE)
  df <- seqNdisplayR::Session2Df(sess$samples, sess$colors, sess$bigwigs, sess$bigwig_dirs, sess$parameters)
  stopifnot("subgroup_2" %in% colnames(df))
}, fast = TRUE)

.reg("T09", "Per-row bigwig_directory survives round-trip", function() {
  # Two replicates of the same sample, in different directories
  df <- data.frame(
    color            = c('#111111', '#111111'),
    bigwig_directory = c('http://dirA/', 'http://dirB/'),
    bigwig_file      = c('sampleA_rep1.bw', 'sampleA_rep2.bw'),
    strand           = c('plus', 'plus'),
    batch            = c(NA, NA),
    dataset          = c('DS1', 'DS1'),
    subgroup_1       = c('grpX', 'grpX'),
    stringsAsFactors = FALSE
  )
  sess <- seqNdisplayR::seqNdisplayRSession(df = df)
  # bigwig_dirs should now be a nested list mirroring bigwigs
  stopifnot(is.list(sess$bigwig_dirs))
  stopifnot(all(c('+', '-') %in% names(sess$bigwig_dirs)))
  # Leaf should be a character vector of length 2 with both directories
  leaf <- sess$bigwig_dirs[['+']][['DS1']][['grpX']]
  stopifnot(is.character(leaf))
  stopifnot(length(leaf) == 2)
  stopifnot(identical(sort(leaf), c('http://dirA/', 'http://dirB/')))
  # Round-trip via Session2Df preserves per-row directories
  df2 <- seqNdisplayR::Session2Df(sess$samples, sess$colors, sess$bigwigs, sess$bigwig_dirs, sess$parameters)
  stopifnot(nrow(df2) == 2)
  stopifnot(identical(sort(df2$bigwig_directory), c('http://dirA/', 'http://dirB/')))
}, fast = TRUE)


# =============================================================================
# T10-T14: Dummy Plotting (no data fetching, tests layout engine)
# =============================================================================

.reg("T10", "Dummy plot — both strands intermingled", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  out <- capture.output(plot(sess, feature = "LMO4", dummy_plot = TRUE,
                             both_strands = TRUE, strands_intermingled = TRUE,
                             verbosity = "off"))
}, fast = FALSE)  # needs annotations loaded from remote

.reg("T11", "Dummy plot — both strands NOT intermingled", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  out <- capture.output(plot(sess, feature = "LMO4", dummy_plot = TRUE,
                             both_strands = TRUE, strands_intermingled = FALSE,
                             verbosity = "off"))
}, fast = FALSE)

.reg("T12", "Dummy plot — single strand only", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  out <- capture.output(plot(sess, feature = "LMO4", dummy_plot = TRUE,
                             both_strands = FALSE,
                             verbosity = "off"))
}, fast = FALSE)

.reg("T13", "Dummy plot — locus by coordinates", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  out <- capture.output(plot(sess, locus = c("chr1", "+", "87325400", "87351991"),
                             dummy_plot = TRUE, verbosity = "off"))
}, fast = FALSE)

.reg("T14", "Dummy plot — elaborate template", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_elab, load_annotations = TRUE)
  out <- capture.output(plot(sess, feature = "LMO4", dummy_plot = TRUE,
                             verbosity = "off"))
}, fast = FALSE)


# =============================================================================
# T20-T24: Real Plotting (fetches remote data — slow)
# =============================================================================

.reg("T20", "Real plot — simple template, on-screen", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  out <- capture.output(plot(sess, feature = "LMO4", verbosity = "normal"))
  # If we got here without error, the plot was created
  dev.off()
}, fast = FALSE)

.reg("T21", "Real plot — PDF output", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  pdf_file <- file.path(.test_env$tmpdir, "test_plot")
  out <- capture.output(plot(sess, feature = "LMO4",
                             pdf = TRUE, pdf_name = "test_plot",
                             pdf_dir = .test_env$tmpdir,
                             verbosity = "normal"))
  # Check a PDF was created
  pdfs <- list.files(.test_env$tmpdir, pattern = "test_plot.*", full.names = TRUE)
  stopifnot(length(pdfs) >= 1)
}, fast = FALSE)

.reg("T22", "Real plot — annotation packing modes", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  for (packing in c("collapsed", "collapsed2", "squished", "expanded")) {
    out <- capture.output(plot(sess, feature = "LMO4", dummy_plot = TRUE,
                               annotation_packing = packing,
                               verbosity = "off"))
  }
}, fast = FALSE)

.reg("T23", "Real plot — various display options", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  out <- capture.output(plot(sess, feature = "LMO4", dummy_plot = TRUE,
                             alternating_background = FALSE,
                             incl_track_scales = FALSE,
                             include_genomic_scale = FALSE,
                             suppress_header = TRUE,
                             horizontal_spacers = FALSE,
                             verbosity = "off"))
}, fast = FALSE)

.reg("T24", "Real plot — detailed verbosity", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  out <- capture.output(plot(sess, feature = "LMO4", dummy_plot = TRUE,
                             verbosity = "detailed"))
  # Detailed output should include dimension info
  stopifnot(any(grepl("Plot Dimensions", out)))
}, fast = FALSE)


# =============================================================================
# T30-T32: Annotation Functions
# =============================================================================

.reg("T30", "ReadInAnnotations loads remote bed files", function() {
  annot_files <- c(
    'in-house' = 'http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/annotations/HeLa_major_isoform_hg38_gc34.bed'
  )
  annots <- seqNdisplayR::ReadInAnnotations(annot_files, verbosity = 0)
  stopifnot(is.list(annots))
  stopifnot(inherits(annots[["in-house"]], "GRanges"))
  stopifnot(length(annots[["in-house"]]) > 0)
}, fast = FALSE)

.reg("T31", "Feature lookup in annotations", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  # LMO4 should be findable in the annotations
  # This is tested indirectly through plotting, but we verify the session has the annotation
  stopifnot("in-house" %in% names(sess$annots))
  gr <- sess$annots[["in-house"]]
  stopifnot("LMO4" %in% S4Vectors::mcols(gr)$name)
}, fast = FALSE)


# =============================================================================
# T40-T42: Defaults and Utility Functions
# =============================================================================

.reg("T40", "DefaultPlotOptions returns complete list", function() {
  opts <- seqNdisplayR:::DefaultPlotOptions()
  stopifnot(is.list(opts))
  # Check a selection of expected options
  expected <- c("both_strands", "strands_intermingled", "track_width_cm",
                "bin_size", "alternating_background", "annot_panel_color")
  stopifnot(all(expected %in% names(opts)))
}, fast = TRUE)

.reg("T41", "DefaultParameters returns complete list", function() {
  pars <- seqNdisplayR:::DefaultParameters()
  stopifnot(is.list(pars))
  expected <- c("calcMean", "batchCorrect", "log2transform", "group_autoscale")
  stopifnot(all(expected %in% names(pars)))
}, fast = TRUE)

.reg("T42", "ExamplesSampleSheetsFolder and ListExamplesSampleSheets", function() {
  folder <- seqNdisplayR::ExamplesSampleSheetsFolder()
  stopifnot(dir.exists(folder))
  files <- seqNdisplayR::ListExamplesSampleSheets()
  stopifnot(length(files) > 0)
  stopifnot("sNdR_sample_example_simple.xlsx" %in% files)
}, fast = TRUE)


# =============================================================================
# T50-T51: IGV Conversion (new functions)
# =============================================================================

# These tests will only work once the new igv_conversion.R is integrated.
# Uncomment when ready.

# .reg("T50", "Session2IGV writes valid XML", function() {
#   sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = FALSE)
#   outfile <- file.path(.test_env$tmpdir, "test_igv_session.xml")
#   Session2IGV(sess, outfile, genome = "hg38", locus = "LMO4")
#   stopifnot(file.exists(outfile))
#   # Verify it's valid XML
#   doc <- xml2::read_xml(outfile)
#   stopifnot(xml2::xml_name(doc) == "Session")
# }, fast = TRUE)

# .reg("T51", "IGV round-trip (Session → IGV → Session)", function() {
#   sess1 <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = FALSE)
#   igv_file <- file.path(.test_env$tmpdir, "roundtrip_igv.xml")
#   Session2IGV(sess1, igv_file, genome = "hg38")
#   sess2 <- IGV2Session(igv_file)
#   stopifnot(inherits(sess2, "seqNdisplayRSession"))
#   # Dataset count should match
#   stopifnot(length(names(sess2$samples)) >= length(names(sess1$samples)))
# }, fast = TRUE)


# =============================================================================
# T60: Shiny App Loads Without Error
# =============================================================================

.reg("T60", "Shiny app source parses without error", function() {
  app_path <- system.file("shiny", "seqNdisplayR_app.R", package = "seqNdisplayR")
  stopifnot(file.exists(app_path))
  # Just parse it, don't run it
  parse(app_path)
}, fast = TRUE)


# =============================================================================
# T70-T72: ParName and Interface Validation
# =============================================================================

.reg("T70", "ParName returns R name in R mode", function() {
  # In R mode, ParName should return the parameter name unchanged
  stopifnot(seqNdisplayR:::ParName('track_width_cm', 'R') == 'track_width_cm')
  stopifnot(seqNdisplayR:::ParName('bgr_alpha', 'R') == 'bgr_alpha')
  stopifnot(seqNdisplayR:::ParName('annotation_packing', 'R') == 'annotation_packing')
  # Unknown names should also pass through unchanged
  stopifnot(seqNdisplayR:::ParName('nonexistent_param', 'R') == 'nonexistent_param')
}, fast = TRUE)

.reg("T71", "ParName returns Shiny labels in shiny mode", function() {
  # In shiny mode, ParName should return the human-readable label
  stopifnot(seqNdisplayR:::ParName('track_width_cm', 'shiny') == 'Tracks Width')
  stopifnot(seqNdisplayR:::ParName('bgr_alpha', 'shiny') == 'Background  Opacity')
  stopifnot(seqNdisplayR:::ParName('annotation_packing', 'shiny') == 'Annotation Packing')
  stopifnot(seqNdisplayR:::ParName('bin_size', 'shiny') == 'Bin Size')
  stopifnot(seqNdisplayR:::ParName('plotting_segment_order', 'shiny') == 'Plotting Segment Order')
  # Unknown names should fall through as-is
  stopifnot(seqNdisplayR:::ParName('nonexistent_param', 'shiny') == 'nonexistent_param')
}, fast = TRUE)

.reg("T72", "Dummy plot with interface='shiny' succeeds", function() {
  sess <- seqNdisplayR::LoadExcel(.test_env$xl_simple, load_annotations = TRUE)
  out <- capture.output(plot(sess, feature = "LMO4", dummy_plot = TRUE,
                             interface = "shiny", verbosity = "off"))
}, fast = FALSE)  # needs remote annotations


# =============================================================================
# Summary of test categories
# =============================================================================
#
# FAST tests (no remote data needed):
#   T01, T02, T04, T05, T06, T07, T08, T09, T40, T41, T42, T60, T70, T71
#
# MEDIUM tests (need remote annotations only):
#   T03, T10-T14, T22-T24, T30, T31, T72
#
# SLOW tests (fetch remote bigwig data):
#   T20, T21
#
# Run fast tests during development:   run_fast_tests()
# Run all tests before committing:     run_all_tests()
# =============================================================================

cat("Test suite loaded. Use run_all_tests(), run_fast_tests(), or run_test('T01')\n")
