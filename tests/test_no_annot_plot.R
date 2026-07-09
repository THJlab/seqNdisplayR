# =============================================================================
# Reproduce the 'invalid times argument' error from the no-annotation
# coordinate-plot case. Loads SF_seq.xml (no annotations) and calls plot()
# with locus only; prints a traceback if it errors.
# =============================================================================

suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".")
  else library(seqNdisplayR)
})

# Reset any leftover graphics state from earlier failed runs (e.g. a stale
# par(bg = "black") from a plot() that aborted mid-way).
try(graphics.off(), silent = TRUE)
par(bg = "white", fg = "black")

fix <- file.path("tests", "IGV_sessions", "SF_seq.xml")
stopifnot(file.exists(fix))

cat("Loading SF_seq.xml via IGV2Session ...\n")
sess <- suppressMessages(
  IGV2Session(fix, group_by = "common_prefix", load_annotations = FALSE)
)
cat("  -> ", length(sess$samples), " dataset(s);  annots: ",
    if (is.null(sess$annots)) "NULL" else length(sess$annots), "\n", sep = "")

locus <- c("chr20", "+", "2652632", "2658393")
cat("Attempting plot with locus = ", paste(locus, collapse = ":"), "\n", sep = "")

options(error = function() {
  cat("\n--- traceback ---\n")
  traceback(max.lines = 30)
})

# Try the plot. We don't actually need the bigwigs to load to hit a setup-time
# error like "invalid 'times' argument" — it likely fails before bigwig fetch.
plot(sess, locus = locus, interface = "shiny", verbosity = "detailed")

cat("\nDone (or got past the error).\n")
