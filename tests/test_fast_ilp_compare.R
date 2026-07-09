# =============================================================================
# Compare fast-ILP path (single-gene clusters bypass the LP solver) against
# the full ILP path, plotting a set of representative regions.
#
# For each region, we run the full plot twice:
#   1. With fast path enabled (default after the Item 4 edit).
#   2. With fast path disabled via options(seqNdisplayR.disable_fast_ilp = TRUE).
# We extract the inline-name placements from the resulting annot_info
# structures and assert they are IDENTICAL.
#
# If you see any DIFF lines, the fast path is producing different placements
# than the ILP -- inspect those before trusting the optimization.
# =============================================================================

suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".")
  else library(seqNdisplayR)
})

try(graphics.off(), silent = TRUE)
clear_bigwig_cache()
clear_annotation_cache()

fix <- file.path("tests", "IGV_test.xml")
stopifnot(file.exists(fix))

cat("Loading session ...\n")
sess <- suppressMessages(
  IGV2Session(fix, group_by = "common_prefix", load_annotations = TRUE)
)

# Helper: get annot_info$<annot>$c2_inline_name_placements after a plot.
# We use the annotation cache as a side-channel: after plot() runs, the cache
# holds the OrganizeAnnotatedFeaturesInRegion output for the region.
.extract_placements_from_annot_cache <- function() {
  keys <- ls(seqNdisplayR:::.annot_cache, all.names = TRUE)
  if (length(keys) == 0L) return(NULL)
  # Most recent (only one expected; we clear before each plot)
  ann_info <- get(keys[1L], envir = seqNdisplayR:::.annot_cache)
  lapply(ann_info, function(ai) ai$c2_inline_name_placements)
}

.compare <- function(fast, slow, label) {
  ok <- identical(fast, slow)
  if (ok) {
    cat("  [OK]   ", label, ": placements identical\n")
  } else {
    cat("  [DIFF] ", label, ": placements differ - inspect manually\n")
    common <- intersect(names(fast), names(slow))
    for (annot in common) {
      f <- fast[[annot]]; s <- slow[[annot]]
      if (!identical(f, s)) {
        diff_genes <- setdiff(union(names(f), names(s)), character(0))
        for (g in diff_genes) {
          if (!identical(f[[g]], s[[g]])) {
            cat("    annot=", annot, " gene=", g, "\n", sep = "")
            cat("      fast: "); str(f[[g]])
            cat("      slow: "); str(s[[g]])
          }
        }
      }
    }
  }
  invisible(ok)
}

# Regions to test: from sparse (mostly singletons, fast path fires often)
# to dense (NOP56 area; fast path rarely fires).
regions <- list(
  LMO4_simple   = c("chr1",  "+", "87325400", "87351991"),
  NOP56_dense   = c("chr11", "+", "93700000", "93740000"),
  TAF1D_small   = c("chr11", "+", "93448000", "93474000"),
  GADD45A       = c("chr1",  "+", "67689000", "67700000")
)

all_ok <- TRUE
for (rname in names(regions)) {
  cat("\n--- Region: ", rname, " ---\n", sep = "")
  locus <- regions[[rname]]

  # Fast path
  clear_annotation_cache()
  options(seqNdisplayR.disable_fast_ilp = FALSE)
  t_fast <- system.time({
    invisible(plot(sess, locus = locus, interface = "shiny", verbosity = "off",
                   pdf = TRUE, pdf_name = paste0("cmp_fast_", rname),
                   pdf_dir = tempdir()))
  })
  fast_pl <- .extract_placements_from_annot_cache()

  # Slow path (force ILP)
  clear_annotation_cache()
  options(seqNdisplayR.disable_fast_ilp = TRUE)
  t_slow <- system.time({
    invisible(plot(sess, locus = locus, interface = "shiny", verbosity = "off",
                   pdf = TRUE, pdf_name = paste0("cmp_slow_", rname),
                   pdf_dir = tempdir()))
  })
  slow_pl <- .extract_placements_from_annot_cache()

  cat(sprintf("  timing: fast %.2fs   slow %.2fs   speedup %.2fx\n",
              t_fast["elapsed"], t_slow["elapsed"],
              t_slow["elapsed"] / max(t_fast["elapsed"], 0.001)))

  if (!.compare(fast_pl, slow_pl, rname)) all_ok <- FALSE

  options(seqNdisplayR.disable_fast_ilp = FALSE)
}

cat("\n=========================================\n")
if (all_ok) {
  cat("OVERALL: all regions produced IDENTICAL placements between fast and slow paths.\n")
} else {
  cat("OVERALL: at least one region differed - DO NOT TRUST the fast path until investigated.\n")
}
cat("=========================================\n")
