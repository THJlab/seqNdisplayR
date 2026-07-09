# =============================================================================
# Comprehensive parallel-vs-sequential bigwig fetch timing comparison.
# - 4 sessions with annotations (HeLa IGV + 3 Excel templates)
# - 1 session without annotations (SF_seq IGV)
# - 5 genes for the annotated sessions: LMO4, NOP56, ADAR, TAF1D, EIF4A2
# - 1 coordinate region for SF_seq
# - For each (session, region) cold-plot twice: once parallel, once sequential.
#   The bigwig cache is cleared between runs to ensure both are cold.
# =============================================================================

suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".")
  else library(seqNdisplayR)
})

try(graphics.off(), silent = TRUE)

cat("Physical cores:",
    tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA), "\n")

# ----- Sessions to test ------------------------------------------------------
sessions_to_load <- list(
  HeLa_IGV        = file.path("tests", "IGV_test.xml"),
  excel_minimal   = system.file("extdata", "minimal_example_excel_template_w_annotation.xlsx",
                                package = "seqNdisplayR"),
  excel_simple    = system.file("extdata", "sNdR_sample_example_simple.xlsx",
                                package = "seqNdisplayR"),
  excel_elaborate = system.file("extdata", "sNdR_sample_example_elaborate.xlsx",
                                package = "seqNdisplayR"),
  SF_seq          = file.path("tests", "IGV_sessions", "SF_seq.xml")
)

cat("\nLoading sessions (with annotations) ...\n")
sessions <- list()
for (lab in names(sessions_to_load)) {
  p <- sessions_to_load[[lab]]
  if (!file.exists(p) || nchar(p) == 0L) {
    cat("  ", lab, ": missing -> skipping\n"); next
  }
  cat("  ", lab, " ... ", sep = "")
  t <- system.time({
    sess <- if (grepl("\\.xml$", p)) {
      suppressMessages(IGV2Session(p, group_by = "common_prefix",
                                   load_annotations = TRUE))
    } else {
      suppressMessages(LoadExcel(p, load_annotations = TRUE))
    }
  })
  sessions[[lab]] <- sess
  cat(sprintf("loaded in %.1fs (%d datasets, %d annots)\n",
              t["elapsed"], length(sess$samples),
              if (is.null(sess$annotation_files)) 0L else length(sess$annotation_files)))
}

# ----- Regions per session ---------------------------------------------------
genes        <- c("LMO4", "NOP56", "ADAR", "TAF1D", "EIF4A2")
chr20_region <- c("chr20", "+", "2652632", "2658393")

regions_by_file <- list(
  HeLa_IGV        = setNames(as.list(genes), genes),
  excel_minimal   = setNames(as.list(genes), genes),
  excel_simple    = setNames(as.list(genes), genes),
  excel_elaborate = setNames(as.list(genes), genes),
  SF_seq          = list(chr20_2652_2658 = chr20_region)
)

# ----- Helper ---------------------------------------------------------------
.cold_plot_once <- function(sess, region, parallel) {
  clear_bigwig_cache()
  clear_annotation_cache()
  options(seqNdisplayR.disable_parallel_bw = !parallel)
  t <- tryCatch(
    system.time({
      if (is.character(region) && length(region) == 1L) {
        invisible(plot(sess, feature = region, interface = "shiny",
                       verbosity = "off", pdf = TRUE,
                       pdf_name = "bench", pdf_dir = tempdir()))
      } else {
        invisible(plot(sess, locus = region, interface = "shiny",
                       verbosity = "off", pdf = TRUE,
                       pdf_name = "bench", pdf_dir = tempdir()))
      }
    }),
    error = function(e) {
      cat("    [ERROR] ", conditionMessage(e), "\n"); structure(c(NA, NA, NA),
        names = c("user.self", "sys.self", "elapsed"))
    }
  )
  as.numeric(t["elapsed"])
}

# ----- Benchmark loop --------------------------------------------------------
results <- list()
for (sess_name in names(sessions)) {
  sess <- sessions[[sess_name]]
  for (region_name in names(regions_by_file[[sess_name]])) {
    region <- regions_by_file[[sess_name]][[region_name]]
    cat("\n--- ", sess_name, " / ", region_name, " ---\n", sep = "")
    t_par <- .cold_plot_once(sess, region, parallel = TRUE)
    t_seq <- .cold_plot_once(sess, region, parallel = FALSE)
    cat(sprintf("  parallel %.2fs   sequential %.2fs   speedup %.2fx\n",
                t_par, t_seq, t_seq / max(t_par, 0.001)))
    results[[length(results) + 1L]] <- data.frame(
      session  = sess_name, region = region_name,
      parallel_s = round(t_par, 2),
      sequential_s = round(t_seq, 2),
      speedup_x = round(t_seq / max(t_par, 0.001), 2)
    )
  }
}

cat("\n========== SUMMARY ==========\n")
df <- do.call(rbind, results)
print(df, row.names = FALSE)

ok <- !is.na(df$parallel_s) & !is.na(df$sequential_s)
cat(sprintf("\nTotal parallel time:   %.1fs\n", sum(df$parallel_s[ok])))
cat(sprintf("Total sequential time: %.1fs\n", sum(df$sequential_s[ok])))
cat(sprintf("Overall speedup:       %.2fx\n",
            sum(df$sequential_s[ok]) / max(sum(df$parallel_s[ok]), 0.001)))
