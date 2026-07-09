# =============================================================================
# ILP placement benchmark harness (Item 4 Phase 1).
#
# Purpose:
#   - Cold-plot 6 canonical regions (sparse -> dense) on the HeLa IGV fixture.
#   - Decompose cold-plot wall time into Stage B (.find.hosts), Stage C
#     (.pair.enclosed), Stage D (.c2.pack.bars), and the ILP solve total.
#   - Capture per-cluster size + time distribution from inside the ILP solver.
#   - Capture the actual `c2_inline_name_placements` per region from the
#     annotation cache as the correctness ground-truth.
#
# Modes:
#   Rscript tests/test_ilp_benchmark.R                  # compare vs baseline
#   Rscript tests/test_ilp_benchmark.R --update-baseline # (re)write baseline
#
# Outputs:
#   tests/baseline_timings.csv       (region, stage, median_s, n_reps)
#   tests/baseline_placements.rds    (list keyed by region)
#   tests/bench_pdfs/<region>.pdf    (one PDF per region for visual diff)
#
# Gate:
#   In compare mode, placement-identity is the binding correctness gate:
#   per-region `identical(new, baseline)`. Any FALSE prints the first few
#   differing genes and the script exits non-zero.
# =============================================================================

suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".")
  else library(seqNdisplayR)
})

try(graphics.off(), silent = TRUE)

`%||%` <- function(a, b) if (is.null(a)) b else a

args <- commandArgs(trailingOnly = TRUE)
update_baseline <- any(args == "--update-baseline")

bench_dir       <- file.path("tests")
pdf_dir         <- file.path(bench_dir, "bench_pdfs")
timings_path    <- file.path(bench_dir, "baseline_timings.csv")
placements_path <- file.path(bench_dir, "baseline_placements.rds")
dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)

fix <- file.path("tests", "IGV_test.xml")
stopifnot(file.exists(fix))

cat("Loading HeLa session (with annotations) ...\n")
sess <- suppressMessages(
  IGV2Session(fix, group_by = "common_prefix", load_annotations = TRUE)
)

# ---- 6 canonical regions, sparse -> dense ---------------------------------
# Mix of `feature=` (resolved via loaded annotations) and `locus=` so the
# harness still runs if a feature lookup happens to be ambiguous.
regions <- list(
  LMO4    = list(kind = "feature", val = "LMO4"),    # sparse, 1 gene
  GADD45A = list(kind = "feature", val = "GADD45A"), # small
  ADAR    = list(kind = "feature", val = "ADAR"),    # moderate
  TAF1D   = list(kind = "feature", val = "TAF1D"),   # small w/ isoforms
  EIF4A2  = list(kind = "feature", val = "EIF4A2"),  # moderate
  NOP56   = list(kind = "feature", val = "NOP56")    # dense host-enclosed
)

N_REPS <- 3L

# ---- Per-region run: returns list(stages, clusters, placements) ----------
.run_one <- function(region_name, region_spec, capture_pdf = FALSE) {
  clear_bigwig_cache()
  clear_annotation_cache()
  try(graphics.off(), silent = TRUE)

  timer_env <- new.env(parent = emptyenv())
  old_opt <- options(seqNdisplayR.stage_timer = timer_env)
  on.exit(options(old_opt), add = TRUE)

  pdf_args <- if (capture_pdf) {
    list(pdf = TRUE, pdf_name = paste0("bench_", region_name), pdf_dir = pdf_dir)
  } else {
    list(pdf = TRUE, pdf_name = "bench_tmp", pdf_dir = tempdir())
  }

  t0 <- Sys.time()
  err <- NULL
  res <- tryCatch({
    if (region_spec$kind == "feature") {
      do.call(plot, c(list(sess, feature = region_spec$val,
                           interface = "shiny", verbosity = "off"),
                      pdf_args))
    } else {
      do.call(plot, c(list(sess, locus = region_spec$val,
                           interface = "shiny", verbosity = "off"),
                      pdf_args))
    }
    NULL
  }, error = function(e) e)
  total_s <- as.numeric(Sys.time() - t0, units = "secs")
  if (inherits(res, "error")) err <- conditionMessage(res)

  totals   <- if (exists("totals",   envir = timer_env)) get("totals",   envir = timer_env) else list()
  counts   <- if (exists("counts",   envir = timer_env)) get("counts",   envir = timer_env) else list()
  clusters <- if (exists("clusters", envir = timer_env)) get("clusters", envir = timer_env) else list()

  # Pull placements from annotation cache (set by the just-completed plot).
  placements <- tryCatch({
    keys <- ls(seqNdisplayR:::.annot_cache, all.names = TRUE)
    if (length(keys) == 0L) NULL
    else {
      ann_info <- get(keys[1L], envir = seqNdisplayR:::.annot_cache)
      lapply(ann_info, function(ai) ai$c2_inline_name_placements)
    }
  }, error = function(e) NULL)

  list(
    region       = region_name,
    total_s      = total_s,
    stage_totals = totals,
    stage_counts = counts,
    clusters     = clusters,
    placements   = placements,
    error        = err
  )
}

# ---- Run N_REPS cold reps per region --------------------------------------
all_runs <- list()
for (rname in names(regions)) {
  cat("\n=== Region: ", rname, " ===\n", sep = "")
  reps <- list()
  for (i in seq_len(N_REPS)) {
    cap_pdf <- (i == 1L)  # save PDF on first rep only
    cat(sprintf("  rep %d/%d ... ", i, N_REPS))
    r <- .run_one(rname, regions[[rname]], capture_pdf = cap_pdf)
    if (!is.null(r$error)) {
      cat("[ERROR] ", r$error, "\n", sep = "")
    } else {
      cat(sprintf("total %.2fs  (B %.2f, C %.2f, D %.2f, ILP %.2f, clusters %d)\n",
                  r$total_s,
                  as.numeric(r$stage_totals[["stage_B"]]   %||% 0),
                  as.numeric(r$stage_totals[["stage_C"]]   %||% 0),
                  as.numeric(r$stage_totals[["stage_D"]]   %||% 0),
                  as.numeric(r$stage_totals[["ilp_total"]] %||% 0),
                  length(r$clusters)))
    }
    reps[[i]] <- r
  }
  all_runs[[rname]] <- reps
}

# ---- Aggregate: median time per stage per region --------------------------
stages_to_report <- c("total", "stage_B", "stage_C", "stage_D", "ilp_total",
                      "ilp_conflicts", "ilp_build_direct", "ilp_solve_glpk")
agg_rows <- list()
for (rname in names(all_runs)) {
  reps <- all_runs[[rname]]
  good <- Filter(function(r) is.null(r$error), reps)
  if (length(good) == 0L) {
    for (st in stages_to_report) {
      agg_rows[[length(agg_rows) + 1L]] <- data.frame(
        region = rname, stage = st, median_s = NA_real_, n_reps = 0L
      )
    }
    next
  }
  for (st in stages_to_report) {
    vals <- vapply(good, function(r) {
      if (st == "total") as.numeric(r$total_s)
      else as.numeric(r$stage_totals[[st]] %||% 0)
    }, numeric(1L))
    agg_rows[[length(agg_rows) + 1L]] <- data.frame(
      region = rname, stage = st, median_s = round(median(vals), 3),
      n_reps = length(good)
    )
  }
}
timings_df <- do.call(rbind, agg_rows)

# ---- Cluster size distribution (across all regions, all reps) -------------
all_clusters <- do.call(rbind, lapply(names(all_runs), function(rname) {
  reps <- all_runs[[rname]]
  do.call(rbind, lapply(seq_along(reps), function(i) {
    cl <- reps[[i]]$clusters
    if (length(cl) == 0L) return(NULL)
    data.frame(
      region  = rname,
      rep     = i,
      N_genes = vapply(cl, function(x) as.integer(x$N_genes), integer(1L)),
      N_flat  = vapply(cl, function(x) as.integer(x$N_flat),  integer(1L)),
      time_s  = vapply(cl, function(x) as.numeric(x$time_s),  numeric(1L))
    )
  }))
}))

# ---- Placements: use rep 1 per region ------------------------------------
current_placements <- lapply(all_runs, function(reps) reps[[1L]]$placements)

# ---- Print summary --------------------------------------------------------
cat("\n========== STAGE TIMING SUMMARY (median across reps) ==========\n")
print(timings_df, row.names = FALSE)

cat("\n========== CLUSTER SIZE / TIME DISTRIBUTION ==========\n")
if (!is.null(all_clusters) && nrow(all_clusters) > 0L) {
  cat(sprintf("Total clusters observed: %d (across %d reps x %d regions)\n",
              nrow(all_clusters), N_REPS, length(regions)))
  cat(sprintf("Cluster size (N_genes): min=%d  median=%.0f  p95=%d  max=%d\n",
              min(all_clusters$N_genes), median(all_clusters$N_genes),
              as.integer(quantile(all_clusters$N_genes, 0.95)),
              max(all_clusters$N_genes)))
  cat(sprintf("Cluster size (N_flat) : min=%d  median=%.0f  p95=%d  max=%d\n",
              min(all_clusters$N_flat), median(all_clusters$N_flat),
              as.integer(quantile(all_clusters$N_flat, 0.95)),
              max(all_clusters$N_flat)))
  cat(sprintf("Cluster time (s)      : median=%.4f  p95=%.4f  max=%.4f  sum=%.3f\n",
              median(all_clusters$time_s), quantile(all_clusters$time_s, 0.95),
              max(all_clusters$time_s), sum(all_clusters$time_s)))
  # Top 5 worst clusters
  ord <- order(all_clusters$time_s, decreasing = TRUE)
  cat("\nTop 5 slowest clusters:\n")
  print(head(all_clusters[ord, ], 5), row.names = FALSE)
} else {
  cat("(no clusters captured -- did .record_cluster hooks fire?)\n")
}

# ---- Update or compare mode ----------------------------------------------
if (update_baseline) {
  cat("\n>>> Writing baselines (--update-baseline) ...\n")
  write.csv(timings_df, timings_path, row.names = FALSE)
  saveRDS(current_placements, placements_path)
  cat("  wrote ", timings_path, "\n", sep = "")
  cat("  wrote ", placements_path, "\n", sep = "")
  cat("  PDFs (rep 1) in ", pdf_dir, "\n", sep = "")
} else {
  cat("\n========== COMPARE vs BASELINE ==========\n")
  if (!file.exists(placements_path)) {
    cat("  baseline_placements.rds missing -- run with --update-baseline first.\n")
    quit(status = 0)
  }
  base_pl <- readRDS(placements_path)
  base_tm <- if (file.exists(timings_path)) read.csv(timings_path, stringsAsFactors = FALSE) else NULL

  # Placement identity
  all_ok <- TRUE
  for (rname in names(regions)) {
    cur <- current_placements[[rname]]
    bas <- base_pl[[rname]]
    if (identical(cur, bas)) {
      cat(sprintf("  [OK]   %s : placements identical\n", rname))
    } else {
      all_ok <- FALSE
      cat(sprintf("  [DIFF] %s : placements differ -- inspect manually\n", rname))
      common <- intersect(names(cur), names(bas))
      diff_count <- 0L
      for (annot in common) {
        a <- cur[[annot]]; b <- bas[[annot]]
        if (!identical(a, b)) {
          genes <- union(names(a), names(b))
          for (g in genes) {
            if (!identical(a[[g]], b[[g]])) {
              diff_count <- diff_count + 1L
              if (diff_count <= 5L) {
                cat(sprintf("    annot=%s gene=%s\n", annot, g))
                cat("      current : "); str(a[[g]])
                cat("      baseline: "); str(b[[g]])
              }
            }
          }
        }
      }
      if (diff_count > 5L) cat(sprintf("    ... and %d more differing genes\n", diff_count - 5L))
    }
  }

  # Timing delta
  if (!is.null(base_tm)) {
    cat("\nTiming delta vs baseline:\n")
    merged <- merge(base_tm[, c("region", "stage", "median_s")],
                    timings_df[, c("region", "stage", "median_s")],
                    by = c("region", "stage"), suffixes = c("_base", "_new"))
    merged$delta_s   <- round(merged$median_s_new - merged$median_s_base, 3)
    merged$delta_pct <- ifelse(merged$median_s_base > 0,
                               round(100 * merged$delta_s / merged$median_s_base, 1),
                               NA_real_)
    # Reorder rows to group by region (sparse->dense)
    merged$region <- factor(merged$region, levels = names(regions))
    merged$stage  <- factor(merged$stage,  levels = stages_to_report)
    merged <- merged[order(merged$region, merged$stage), ]
    print(merged, row.names = FALSE)
  }

  if (!all_ok) {
    cat("\n[FAIL] placements differ from baseline -- DO NOT trust the change.\n")
    quit(status = 1)
  } else {
    cat("\n[PASS] all regions produced identical placements vs baseline.\n")
  }
}
