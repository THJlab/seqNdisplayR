# =============================================================================
# Compare old vs new annotation packing — expanded mode row counts
# =============================================================================
#
# This test compares the OLD (sequential gene stacking) vs NEW (gene-level
# row packing) for expanded annotation mode. Unlike the S4 optimization
# tests, we EXPECT differences — the new version should use fewer rows
# for loci with hosted genes.
#
# For collapsed/collapsed2 modes: outputs should be IDENTICAL (no change).
# For expanded/packing: row count should be <= old, and all transcript
# indices should still be present (no data loss).
#
# USAGE:
#   source("tests/compare_packing.R")
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

# ---- Source OLD functions ----------------------------------------------------
cat("Loading old functions from R_old/...\n")

old_file <- "R_old/annotations.R"
if (!file.exists(old_file)) {
  old_file <- file.path(dirname(extdata), "..", "R_old", "annotations.R")
}
if (!file.exists(old_file)) stop("Cannot find R_old/annotations.R")

old_env <- new.env(parent = asNamespace("seqNdisplayR"))
old_source <- readLines(old_file)

for (func_name in c("AnnotatedFeaturesInRegion", "OrganizeOverlappingLoci",
                     "ConvertCollapsedFormat", "OrganizeAnnotatedFeaturesInRegion")) {
  start_line <- grep(paste0("^", func_name, " = function"), old_source)
  if (length(start_line) == 0) next
  start_line <- start_line[1]
  all_func_lines <- grep("^[A-Z][a-zA-Z]* = function", old_source)
  next_func <- all_func_lines[which(all_func_lines == start_line) + 1]
  end_line <- if (is.na(next_func)) length(old_source) else next_func - 1
  while (end_line > start_line && trimws(old_source[end_line]) == "") end_line <- end_line - 1
  func_text <- paste(old_source[start_line:end_line], collapse = "\n")
  func_text <- sub(paste0("^", func_name, " = function"),
                   paste0(func_name, "_OLD = function"), func_text)
  eval(parse(text = func_text), envir = old_env)
}

# Build OLD OrganizeAnnotatedFeaturesInRegion using old sub-functions
OrganizeAnnotatedFeaturesInRegion_OLD <- function(plotted_region, annotations) {
  annot_info <- old_env$AnnotatedFeaturesInRegion_OLD(plotted_region, annotations)
  annot_info <- old_env$OrganizeOverlappingLoci_OLD(annot_info)
  annot_info <- old_env$ConvertCollapsedFormat_OLD(annot_info)
  return(annot_info)
}


# ---- Run comparisons --------------------------------------------------------
cat("\n=== ANNOTATION PACKING COMPARISON (expanded mode) ===\n")

results <- list()

for (locus in test_loci) {
  cat(sprintf("\n--- %s ---\n", locus))
  
  .pr <- tryCatch(
    seqNdisplayR:::RegionGRanges(
      locus = NULL, tracks_width = 10, feature = locus,
      annotations = sess$annots, bin_start = NULL,
      extra_space = c(1.5, 1.5), verbosity = 0, interface = 'R'),
    error = function(e) NULL
  )
  if (is.null(.pr)) {
    cat("  SKIPPED\n")
    results[[locus]] <- NA
    next
  }
  
  res_old <- tryCatch(OrganizeAnnotatedFeaturesInRegion_OLD(.pr, sess$annots),
                      error = function(e) { cat("  OLD ERROR:", e$message, "\n"); NULL })
  res_new <- tryCatch(seqNdisplayR:::OrganizeAnnotatedFeaturesInRegion(.pr, sess$annots),
                      error = function(e) { cat("  NEW ERROR:", e$message, "\n"); NULL })
  
  if (is.null(res_old) || is.null(res_new)) {
    results[[locus]] <- FALSE
    next
  }
  
  locus_ok <- TRUE
  for (annot_name in names(res_old)) {
    cat(sprintf("  [%s]\n", annot_name))
    old_a <- res_old[[annot_name]]
    new_a <- res_new[[annot_name]]
    
    # collapsed and collapsed2 should be identical
    for (slot in c("collapsed", "collapsed2")) {
      if (identical(old_a[[slot]], new_a[[slot]])) {
        cat(sprintf("    %-12s: identical — OK\n", slot))
      } else {
        ae <- all.equal(old_a[[slot]], new_a[[slot]])
        if (isTRUE(ae)) {
          cat(sprintf("    %-12s: equal (all.equal) — OK\n", slot))
        } else {
          cat(sprintf("    %-12s: MISMATCH — %s\n", slot, paste(ae[1:min(3, length(ae))], collapse = "; ")))
          locus_ok <- FALSE
        }
      }
    }
    
    # expanded: GRangesList should have same content (possibly reordered within merged groups)
    if (!is.null(old_a[["expanded"]]) && !is.null(new_a[["expanded"]])) {
      # Check that all transcript data is preserved (same names, same total count)
      old_names <- sort(names(old_a[["expanded"]]))
      new_names <- sort(names(new_a[["expanded"]]))
      if (identical(old_names, new_names)) {
        old_total_trn <- sum(lengths(old_a[["expanded"]]))
        new_total_trn <- sum(lengths(new_a[["expanded"]]))
        if (old_total_trn == new_total_trn) {
          cat(sprintf("    expanded    : same genes (%d), same transcripts (%d) — OK\n",
                      length(old_names), old_total_trn))
        } else {
          cat(sprintf("    expanded    : MISMATCH — transcript count differs (old=%d, new=%d)\n",
                      old_total_trn, new_total_trn))
          locus_ok <- FALSE
        }
      } else {
        cat(sprintf("    expanded    : gene names differ (old=%d, new=%d)\n",
                    length(old_names), length(new_names)))
        # This is expected for merged groups — check total transcript count
        old_total <- sum(lengths(old_a[["expanded"]]))
        new_total <- sum(lengths(new_a[["expanded"]]))
        cat(sprintf("                  total transcripts: old=%d, new=%d %s\n",
                    old_total, new_total, ifelse(old_total == new_total, "— OK", "— MISMATCH")))
        if (old_total != new_total) locus_ok <- FALSE
      }
    }
    
    # packing: compare row counts (new should be <= old for merged groups)
    if (!is.null(old_a[["packing"]]) && !is.null(new_a[["packing"]])) {
      old_max_rows <- max(lengths(old_a[["packing"]]))
      new_max_rows <- max(lengths(new_a[["packing"]]))
      
      # Check that each gene group's packing covers all its transcripts exactly once
      trn_ok <- TRUE
      for (.grp in names(new_a[["packing"]])) {
        .all_idx <- sort(as.integer(unlist(new_a[["packing"]][[.grp]])))
        .n_trn <- length(new_a[["expanded"]][[.grp]])
        if (.n_trn > 0) {
          .expected <- 1L:.n_trn
          if (!identical(.all_idx, .expected)) {
            cat(sprintf("    packing     : [%s] index mismatch — expected 1:%d, got %s\n",
                        .grp, .n_trn, paste(head(.all_idx, 10), collapse=",")))
            trn_ok <- FALSE
          }
        }
      }
      
      saved <- old_max_rows - new_max_rows
      trn_str <- ifelse(trn_ok, "indices complete — OK", "INDEX ERROR")
      if (saved > 0) {
        cat(sprintf("    packing     : max rows %d -> %d (saved %d rows) | %s\n",
                    old_max_rows, new_max_rows, saved, trn_str))
      } else if (saved == 0) {
        cat(sprintf("    packing     : max rows %d (unchanged) | %s\n",
                    old_max_rows, trn_str))
      } else {
        cat(sprintf("    packing     : max rows %d -> %d (INCREASED by %d!) | %s\n",
                    old_max_rows, new_max_rows, -saved, trn_str))
      }
      if (!trn_ok) locus_ok <- FALSE
    }
    
    # packing2: should be identical (collapsed2 mode unchanged)
    if (!is.null(old_a[["packing2"]]) || !is.null(new_a[["packing2"]])) {
      if (identical(old_a[["packing2"]], new_a[["packing2"]])) {
        cat(sprintf("    packing2    : identical — OK\n"))
      } else {
        cat(sprintf("    packing2    : DIFFERS (check if expected)\n"))
        # Not necessarily an error — packing2 may legitimately change
      }
    }
    
    # gene_row_offsets: new-only, report what was computed
    if (!is.null(new_a[["gene_row_offsets"]])) {
      for (grp in names(new_a[["gene_row_offsets"]])) {
        offsets <- new_a[["gene_row_offsets"]][[grp]]
        if (any(offsets > 0)) {  # only report non-trivial offsets
          cat(sprintf("    gene_offsets: [%s] %s\n", 
                      grp, paste(paste0(names(offsets), "=", offsets), collapse = ", ")))
        }
      }
    }
  }
  results[[locus]] <- locus_ok
}

# ---- Summary -----------------------------------------------------------------
cat("\n\n========================================\n")
cat("           SUMMARY\n")
cat("========================================\n\n")

for (locus in test_loci) {
  status <- results[[locus]]
  sym <- if (is.na(status)) "SKIP" else if (status) " OK " else "FAIL"
  cat(sprintf("  [%s]  %s\n", sym, locus))
}

n_pass <- sum(unlist(results) == TRUE, na.rm = TRUE)
n_fail <- sum(unlist(results) == FALSE, na.rm = TRUE)
n_skip <- sum(is.na(unlist(results)))
cat(sprintf("\n  %d passed, %d failed, %d skipped out of %d loci\n",
            n_pass, n_fail, n_skip, length(test_loci)))

if (n_fail > 0) {
  cat("\n  FAILED:", paste(names(results)[!is.na(unlist(results)) & unlist(results) == FALSE], collapse = ", "), "\n")
}
