# =============================================================================
# Side-by-side verification: ompr legacy vs direct ROI sparse-matrix LP.
#
# For every ILP cluster in every canonical region, solve with BOTH backends
# and assert:
#   1. objval bit-equal (to numeric tolerance)
#   2. chosen xn vector identical
#
# Tied-objective tie-break may legitimately differ (GLPK internal) -- those
# get logged via message() but counted as warnings, not errors. Any objval
# mismatch is an ERROR.
# =============================================================================

suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".")
  else library(seqNdisplayR)
})

try(graphics.off(), silent = TRUE)

fix <- file.path("tests", "IGV_test.xml")
stopifnot(file.exists(fix))

cat("Loading HeLa session ...\n")
sess <- suppressMessages(
  IGV2Session(fix, group_by = "common_prefix", load_annotations = TRUE)
)

regions <- list(
  LMO4    = "LMO4",
  GADD45A = "GADD45A",
  ADAR    = "ADAR",
  TAF1D   = "TAF1D",
  EIF4A2  = "EIF4A2",
  NOP56   = "NOP56"
)

grand_total <- list(n = 0L, obj_mm = 0L, tie_diff = 0L)

for (rname in names(regions)) {
  cat("\n=== ", rname, " ===\n", sep = "")
  clear_bigwig_cache()
  clear_annotation_cache()

  env <- new.env(parent = emptyenv())
  old_opts <- options(
    seqNdisplayR.verify_ilp  = TRUE,
    seqNdisplayR.stage_timer = env
  )

  t0 <- Sys.time()
  invisible(suppressMessages(
    plot(sess, feature = regions[[rname]], interface = "shiny",
         verbosity = "off", pdf = TRUE, pdf_name = paste0("verify_", rname),
         pdf_dir = tempdir())
  ))
  el <- as.numeric(Sys.time() - t0, units = "secs")

  vc <- if (exists("verify_counts", envir = env)) get("verify_counts", envir = env)
        else list(n = 0L, obj_mm = 0L, tie_diff = 0L)

  cat(sprintf("  elapsed %.2fs   clusters verified: %d   objval mismatches: %d   tied tie-break diffs: %d\n",
              el, vc$n, vc$obj_mm, vc$tie_diff))
  grand_total$n        <- grand_total$n        + vc$n
  grand_total$obj_mm   <- grand_total$obj_mm   + vc$obj_mm
  grand_total$tie_diff <- grand_total$tie_diff + vc$tie_diff

  options(old_opts)
}

cat("\n========================================\n")
cat(sprintf("Total clusters verified: %d\n", grand_total$n))
cat(sprintf("Objective mismatches  : %d\n", grand_total$obj_mm))
cat(sprintf("Tied tie-break diffs  : %d\n", grand_total$tie_diff))
cat("========================================\n")

if (grand_total$obj_mm > 0L) {
  cat("[FAIL] direct path does not match legacy objective values -- DO NOT ship.\n")
  quit(status = 1)
} else if (grand_total$tie_diff > 0L) {
  cat("[WARN] objectives match but tied-objective tie-breaks differ on ", grand_total$tie_diff,
      " of ", grand_total$n, " clusters.\n  Visually diff the affected regions before shipping.\n", sep = "")
} else {
  cat("[PASS] both backends agree byte-for-byte on objectives AND chosen variables.\n")
}
