# =============================================================================
# Profile a representative cold plot operation.
# Captures function-level timing via Rprof and prints a text summary you can
# paste back. The two halves we care about:
#   1. data-loading + ILP work (single profile output)
#   2. just the ILP (we run plot twice to separate cold/warm)
# =============================================================================

suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".")
  else library(seqNdisplayR)
})

# Reset device + caches so the cold run is genuinely cold
try(graphics.off(), silent = TRUE)
clear_bigwig_cache()
clear_annotation_cache()

# Load a session with annotations - HeLa is the cleanest fixture
fix <- file.path("tests", "IGV_test.xml")
stopifnot(file.exists(fix))
cat("Loading session via IGV2Session ...\n")
sess <- suppressMessages(
  IGV2Session(fix, group_by = "common_prefix", load_annotations = TRUE)
)

# Pick a region with annotations so the ILP path is exercised
locus <- c("chr11", "+", "93700000", "93740000")   # NOP56 area
cat("\nLocus to plot:", paste(locus, collapse = ":"), "\n")

# ----- Cold profile ---------------------------------------------------------
prof_cold <- tempfile(fileext = ".out")
cat("\n===== COLD plot (caches empty) - profiling =====\n")
Rprof(prof_cold, interval = 0.02, line.profiling = FALSE)
t_cold <- system.time({
  invisible(plot(sess, locus = locus, interface = "shiny", verbosity = "off"))
})
Rprof(NULL)

cat("\n--- Cold plot wall time ---\n")
print(t_cold)

cat("\n--- Cold plot: top 25 functions by self.pct (most time spent IN them) ---\n")
sm_cold <- summaryRprof(prof_cold)
sb_cold <- sm_cold$by.self
print(head(sb_cold[order(-sb_cold$self.pct), ], 25))

cat("\n--- Cold plot: top 25 functions by total.pct (includes nested calls) ---\n")
print(head(sb_cold[order(-sb_cold$total.pct), ], 25))

# ----- Warm profile ---------------------------------------------------------
prof_warm <- tempfile(fileext = ".out")
cat("\n===== WARM plot (caches populated) - profiling =====\n")
Rprof(prof_warm, interval = 0.02, line.profiling = FALSE)
t_warm <- system.time({
  invisible(plot(sess, locus = locus, interface = "shiny", verbosity = "off"))
})
Rprof(NULL)

cat("\n--- Warm plot wall time ---\n")
print(t_warm)

cat("\n--- Warm plot: top 25 by self.pct ---\n")
sm_warm <- summaryRprof(prof_warm)
sb_warm <- sm_warm$by.self
print(head(sb_warm[order(-sb_warm$self.pct), ], 25))

cat("\nDone. Cache sizes now:\n")
cat("  bigwig:", bigwig_cache_size(), "\n")
cat("  annotation:", annotation_cache_size(), "\n")
