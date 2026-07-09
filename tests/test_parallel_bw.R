# =============================================================================
# Measure the speedup from batched/parallel bigwig fetch.
# For a fair comparison we clear ALL caches before each run, and toggle the
# parallel feature via options(seqNdisplayR.disable_parallel_bw = ...).
# =============================================================================

suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".")
  else library(seqNdisplayR)
})

try(graphics.off(), silent = TRUE)
fix <- file.path("tests", "IGV_test.xml")
stopifnot(file.exists(fix))

cat("Available physical cores:",
    tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA), "\n")
cat("Workers seqNdisplayR will use for fetch:",
    seqNdisplayR:::.pick_n_bw_workers(28L), " (capped, 28 tracks)\n\n")

cat("Loading session (with annotations) ...\n")
sess <- suppressMessages(
  IGV2Session(fix, group_by = "common_prefix", load_annotations = TRUE)
)

locus <- c("chr11", "+", "93700000", "93740000")   # NOP56 area
cat("Locus:", paste(locus, collapse = ":"), "\n\n")

.cold_plot <- function(label, parallel) {
  clear_bigwig_cache()
  clear_annotation_cache()
  options(seqNdisplayR.disable_parallel_bw = !parallel)
  cat("--- ", label, " ---\n", sep = "")
  t <- system.time({
    invisible(plot(sess, locus = locus, interface = "shiny", verbosity = "off",
                   pdf = TRUE, pdf_name = paste0("par_", label),
                   pdf_dir = tempdir()))
  })
  cat(sprintf("  elapsed %.2fs  (user %.2fs  system %.2fs)\n\n",
              t["elapsed"], t["user.self"], t["sys.self"]))
  invisible(t)
}

# Run BOTH paths twice each to amortize JIT compilation cost in the first run.
# Order: parallel first, sequential second, then repeat to compare second runs.
cat("===== First-call runs (include R JIT compile cost) =====\n")
t1_par <- .cold_plot("PARALLEL_1st", TRUE)
t1_seq <- .cold_plot("SEQUENTIAL_1st", FALSE)

cat("===== Second-call runs (JIT already warm) =====\n")
t2_par <- .cold_plot("PARALLEL_2nd", TRUE)
t2_seq <- .cold_plot("SEQUENTIAL_2nd", FALSE)

cat("\n========== SUMMARY ==========\n")
cat(sprintf("Sequential cold plot (2nd run): %.2fs\n", t2_seq["elapsed"]))
cat(sprintf("Parallel cold plot   (2nd run): %.2fs\n", t2_par["elapsed"]))
cat(sprintf("Speedup: %.2fx\n",
            as.numeric(t2_seq["elapsed"]) / max(as.numeric(t2_par["elapsed"]), 0.001)))
cat(sprintf("Bigwigs cached: %d   Annotations cached: %d\n",
            bigwig_cache_size(), annotation_cache_size()))
