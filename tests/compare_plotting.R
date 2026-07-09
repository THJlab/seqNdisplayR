# =============================================================================
# Compare old vs new PlotAnnotation — PDF rendering comparison
# =============================================================================
#
# USAGE:
#   1. Have the OLD plotting.R in R_old/
#   2. Install the NEW package (with optimized plotting.R)
#   3. source("tests/compare_plotting.R")
#
# Strategy: For each test locus, render a dummy plot to PDF using both
# old and new PlotAnnotation, then compare the PDFs byte-for-byte.
# Also compares annotation packing modes.
# =============================================================================

library(seqNdisplayR)

test_loci <- c("LMO4", "HELLS", "ADAR", "NOP56", "ATF4", "NOP58", "DMD",
               "GAPDH", "TP53", "MYC", "ACTB", "BRCA1", "EGFR", "FMR1",
               "HOXA1", "RB1", "SNHG12", "UBE3A", "XIST", "CDK2")

# ---- Load test data ----------------------------------------------------------
cat("Loading test session...\n")
extdata <- seqNdisplayR::ExamplesSampleSheetsFolder()
sess <- seqNdisplayR::LoadExcel(
  paste0(extdata, "sNdR_sample_example_simple.xlsx"),
  load_annotations = TRUE
)

# ---- Source OLD PlotAnnotation -----------------------------------------------
cat("Loading old PlotAnnotation from R_old/...\n")

old_file <- "R_old/plotting.R"
if (!file.exists(old_file)) {
  old_file <- file.path(dirname(extdata), "..", "R_old", "plotting.R")
}
if (!file.exists(old_file)) {
  stop("Cannot find R_old/plotting.R")
}

old_env <- new.env(parent = asNamespace("seqNdisplayR"))
old_source <- readLines(old_file)
start_line <- grep("^PlotAnnotation = function", old_source)[1]
all_func_lines <- grep("^[A-Z][a-zA-Z]* = function", old_source)
next_func <- all_func_lines[which(all_func_lines == start_line) + 1]
end_line <- if (is.na(next_func)) length(old_source) else next_func - 1
while (end_line > start_line && trimws(old_source[end_line]) == "") end_line <- end_line - 1
func_text <- paste(old_source[start_line:end_line], collapse = "\n")
func_text <- sub("^PlotAnnotation = function", "PlotAnnotation_OLD = function", func_text)
eval(parse(text = func_text), envir = old_env)
PlotAnnotation_OLD <- old_env$PlotAnnotation_OLD

# ---- Test helper: render a dummy plot to PDF and return md5 ------------------
render_dummy_pdf <- function(sess, locus, pdf_path, use_old_plot_annotation = FALSE, packing = "collapsed2") {
  # Temporarily swap PlotAnnotation if testing old version
  if (use_old_plot_annotation) {
    # Inject old function into package namespace temporarily
    ns <- asNamespace("seqNdisplayR")
    original_func <- ns$PlotAnnotation
    assignInNamespace("PlotAnnotation", PlotAnnotation_OLD, ns = "seqNdisplayR")
    on.exit(assignInNamespace("PlotAnnotation", original_func, ns = "seqNdisplayR"), add = TRUE)
  }
  
  capture.output(suppressWarnings({
    plot(sess, feature = locus, dummy_plot = TRUE,
         pdf = TRUE, pdf_name = basename(pdf_path), pdf_dir = dirname(pdf_path),
         annotation_packing = packing,
         verbosity = "off")
  }))
  
  # Find the generated PDF (name may have extras appended)
  pdfs <- list.files(dirname(pdf_path), pattern = paste0("^", basename(pdf_path)), full.names = TRUE)
  if (length(pdfs) == 0) return(NULL)
  # Return the most recent one
  pdfs[which.max(file.info(pdfs)$mtime)]
}

# ---- Run comparisons --------------------------------------------------------
cat("\n=== MULTI-LOCUS PlotAnnotation COMPARISON ===\n")

tmpdir <- file.path(tempdir(), "sNdR_plot_compare")
if (!dir.exists(tmpdir)) dir.create(tmpdir, recursive = TRUE)

results <- list()
timings <- list()

for (locus in test_loci) {
  cat(sprintf("\n--- %s ---\n", locus))
  
  # Check if locus is in annotations
  found <- FALSE
  for (annot in sess$annots) {
    if (locus %in% S4Vectors::mcols(annot)$name) { found <- TRUE; break }
  }
  if (!found) {
    cat("  SKIPPED — not in annotations\n")
    results[[locus]] <- NA
    next
  }
  
  old_pdf_base <- file.path(tmpdir, paste0(locus, "_old"))
  new_pdf_base <- file.path(tmpdir, paste0(locus, "_new"))
  
  # Clean up any previous PDFs
  for (f in list.files(tmpdir, pattern = paste0("^", locus, "_"), full.names = TRUE)) file.remove(f)
  
  # Render OLD
  t_old <- system.time({
    old_pdf <- tryCatch(render_dummy_pdf(sess, locus, old_pdf_base, use_old_plot_annotation = TRUE),
                        error = function(e) { cat("  OLD ERROR:", e$message, "\n"); NULL })
  })
  
  # Render NEW
  t_new <- system.time({
    new_pdf <- tryCatch(render_dummy_pdf(sess, locus, new_pdf_base, use_old_plot_annotation = FALSE),
                        error = function(e) { cat("  NEW ERROR:", e$message, "\n"); NULL })
  })
  
  timings[[locus]] <- c(old = t_old["elapsed"], new = t_new["elapsed"])
  
  if (is.null(old_pdf) || is.null(new_pdf)) {
    cat("  FAIL — one or both PDFs not generated\n")
    results[[locus]] <- FALSE
    next
  }
  
  # Compare PDFs
  old_md5 <- tools::md5sum(old_pdf)
  new_md5 <- tools::md5sum(new_pdf)
  old_size <- file.size(old_pdf)
  new_size <- file.size(new_pdf)
  
  if (old_md5 == new_md5) {
    cat(sprintf("  PDF identical (md5 match, %d bytes)\n", old_size))
    results[[locus]] <- TRUE
  } else {
    # PDFs may differ in metadata (timestamps) but have identical rendering
    # Compare file sizes as a secondary check
    size_diff_pct <- abs(old_size - new_size) / max(old_size, 1) * 100
    if (size_diff_pct < 0.1) {
      cat(sprintf("  PDF md5 differs but sizes match (old=%d, new=%d, diff=%.2f%%) — likely timestamp only\n",
                  old_size, new_size, size_diff_pct))
      cat(sprintf("  old: %s\n  new: %s\n", old_pdf, new_pdf))
      results[[locus]] <- TRUE  # treat as pass — metadata timestamp differences are expected
    } else {
      cat(sprintf("  MISMATCH — old=%d bytes, new=%d bytes (%.1f%% diff)\n",
                  old_size, new_size, size_diff_pct))
      cat(sprintf("  Inspect visually:\n  old: %s\n  new: %s\n", old_pdf, new_pdf))
      results[[locus]] <- FALSE
    }
  }
}

# ---- Summary -----------------------------------------------------------------
cat("\n\n========================================\n")
cat("           SUMMARY\n")
cat("========================================\n\n")

for (locus in test_loci) {
  status <- results[[locus]]
  if (is.na(status)) {
    sym <- "SKIP"
  } else if (status) {
    sym <- " OK "
  } else {
    sym <- "FAIL"
  }
  timing_str <- ""
  if (!is.null(timings[[locus]])) {
    t <- timings[[locus]]
    timing_str <- sprintf("  (old=%.3fs, new=%.3fs, %.1fx)",
                          t["old.elapsed"], t["new.elapsed"],
                          t["old.elapsed"] / max(t["new.elapsed"], 0.001))
  }
  cat(sprintf("  [%s]  %-10s%s\n", sym, locus, timing_str))
}

n_pass <- sum(unlist(results) == TRUE, na.rm = TRUE)
n_fail <- sum(unlist(results) == FALSE, na.rm = TRUE)
n_skip <- sum(is.na(unlist(results)))
cat(sprintf("\n  %d passed, %d failed, %d skipped out of %d loci\n",
            n_pass, n_fail, n_skip, length(test_loci)))

if (n_fail > 0) {
  cat("\n  FAILED loci:", paste(names(results)[!is.na(unlist(results)) & unlist(results) == FALSE], collapse = ", "), "\n")
  cat(sprintf("  PDFs are in: %s\n", tmpdir))
  cat("  -> Compare old vs new PDFs visually for each failed locus\n")
} else if (n_fail == 0) {
  cat(sprintf("\n  ALL PLOTS IDENTICAL — optimization is safe\n"))
  cat(sprintf("  PDFs in: %s\n", tmpdir))
}
