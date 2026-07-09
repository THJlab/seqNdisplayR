# =============================================================================
# Compare old vs new AnnotatedFeaturesInRegion — Multi-locus
# =============================================================================
#
# USAGE:
#   1. Have the OLD annotations.R in R_old/
#   2. Install the NEW package (with optimized annotations.R)
#   3. source("compare_annotations.R")
#
# Tests: LMO4, HELLS, ADAR, NOP56, ATF4, NOP58, DMD, GAPDH, TP53, MYC
# =============================================================================

library(seqNdisplayR)

# ---- Test loci ---------------------------------------------------------------
test_loci <- test_loci <- c("LMO4", "HELLS", "ADAR", "NOP56", "ATF4", "NOP58", "DMD",
                            "GAPDH", "TP53", "MYC", "ACTB", "BRCA1", "EGFR", "FMR1",
                            "HOXA1", "RB1", "SNHG12", "UBE3A", "XIST", "CDK2")

# ---- Load test data ----------------------------------------------------------
cat("Loading test session...\n")
extdata <- seqNdisplayR::ExamplesSampleSheetsFolder()
sess <- seqNdisplayR::LoadExcel(
  paste0(extdata, "sNdR_sample_example_simple.xlsx"),
  load_annotations = TRUE
)

# ---- Source the OLD function -------------------------------------------------
cat("Loading old function from R_old/...\n")

old_file <- "R_old/annotations.R"
if (!file.exists(old_file)) {
  # Try package-relative path
  old_file <- file.path(dirname(extdata), "..", "R_old", "annotations.R")
}
if (!file.exists(old_file)) {
  stop("Cannot find R_old/annotations.R\nLooked in: ", old_file,
       "\nMake sure the pre-optimization annotations.R is in R_old/")
}

old_env <- new.env(parent = asNamespace("seqNdisplayR"))
old_source <- readLines(old_file)

# Extract and rename each function we need to compare
for (func_name in c("AnnotatedFeaturesInRegion", "CoordsOfFeatName",
                     "OrganizeAnnotationText", "OrganizeAllAnnotationTextsInPlottedRegion")) {
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

AnnotatedFeaturesInRegion_OLD <- old_env$AnnotatedFeaturesInRegion_OLD


# ---- Deep comparison helper --------------------------------------------------
compare_slots <- function(old_val, new_val, slot_name, locus, verbose = TRUE) {
  if (is.null(old_val) && is.null(new_val)) {
    if (verbose) cat(sprintf("    %-12s: both NULL — OK\n", slot_name))
    return(TRUE)
  }
  if (is.null(old_val) || is.null(new_val)) {
    if (verbose) cat(sprintf("    %-12s: MISMATCH — one is NULL (old=%s, new=%s)\n",
                              slot_name, is.null(old_val), is.null(new_val)))
    return(FALSE)
  }

  # For GRanges / GRangesList, compare piece by piece for better diagnostics
  if (inherits(old_val, "GRanges") || inherits(old_val, "GRangesList")) {
    # Try identical first
    if (identical(old_val, new_val)) {
      if (verbose) cat(sprintf("    %-12s: identical — OK\n", slot_name))
      return(TRUE)
    }
    # Detailed comparison
    problems <- c()
    if (length(old_val) != length(new_val)) {
      problems <- c(problems, sprintf("length differs: old=%d, new=%d", length(old_val), length(new_val)))
    } else {
      if (inherits(old_val, "GRanges")) {
        # Compare ranges
        if (!identical(IRanges::ranges(old_val), IRanges::ranges(new_val))) {
          old_s <- IRanges::start(old_val); new_s <- IRanges::start(new_val)
          old_e <- IRanges::end(old_val); new_e <- IRanges::end(new_val)
          diff_s <- which(old_s != new_s)
          diff_e <- which(old_e != new_e)
          if (length(diff_s) > 0) problems <- c(problems, sprintf("start differs at indices %s (old=%s, new=%s)",
            paste(diff_s, collapse=","), paste(old_s[diff_s], collapse=","), paste(new_s[diff_s], collapse=",")))
          if (length(diff_e) > 0) problems <- c(problems, sprintf("end differs at indices %s (old=%s, new=%s)",
            paste(diff_e, collapse=","), paste(old_e[diff_e], collapse=","), paste(new_e[diff_e], collapse=",")))
        }
        # Compare mcols
        old_mc <- as.data.frame(S4Vectors::mcols(old_val))
        new_mc <- as.data.frame(S4Vectors::mcols(new_val))
        if (!identical(colnames(old_mc), colnames(new_mc))) {
          problems <- c(problems, sprintf("mcols columns differ: old=%s, new=%s",
            paste(colnames(old_mc), collapse=","), paste(colnames(new_mc), collapse=",")))
        } else {
          for (col in colnames(old_mc)) {
            if (!identical(old_mc[[col]], new_mc[[col]])) {
              ae <- all.equal(old_mc[[col]], new_mc[[col]])
              if (!isTRUE(ae)) {
                problems <- c(problems, sprintf("mcols$%s differs: %s", col, paste(ae, collapse="; ")))
              }
            }
          }
        }
      } else {
        # GRangesList — compare element by element
        for (i in seq_along(old_val)) {
          if (!identical(old_val[[i]], new_val[[i]])) {
            ae <- all.equal(old_val[[i]], new_val[[i]])
            if (!isTRUE(ae)) {
              nm <- if (!is.null(names(old_val))) names(old_val)[i] else i
              problems <- c(problems, sprintf("element '%s': %s", nm, paste(ae, collapse="; ")))
            }
          }
        }
      }
    }
    if (length(problems) == 0) {
      if (verbose) cat(sprintf("    %-12s: equal (all.equal) — OK\n", slot_name))
      return(TRUE)
    } else {
      if (verbose) {
        cat(sprintf("    %-12s: MISMATCH\n", slot_name))
        for (p in problems) cat(sprintf("      -> %s\n", p))
      }
      return(FALSE)
    }
  }

  # For lists (packing), compare directly
  if (identical(old_val, new_val)) {
    if (verbose) cat(sprintf("    %-12s: identical — OK\n", slot_name))
    return(TRUE)
  }
  ae <- all.equal(old_val, new_val)
  if (isTRUE(ae)) {
    if (verbose) cat(sprintf("    %-12s: equal (all.equal) — OK\n", slot_name))
    return(TRUE)
  }
  if (verbose) {
    cat(sprintf("    %-12s: MISMATCH\n", slot_name))
    cat(sprintf("      -> %s\n", paste(ae, collapse = "; ")))
  }
  return(FALSE)
}


# ---- Run comparisons per locus -----------------------------------------------
cat("\n=== MULTI-LOCUS COMPARISON ===\n")

results <- list()
timings <- list()

for (locus in test_loci) {
  cat(sprintf("\n--- %s ---\n", locus))

  # Build plotted_region
  .pr <- tryCatch(
    seqNdisplayR:::RegionGRanges(
      locus = NULL, tracks_width = 10, feature = locus,
      annotations = sess$annots, bin_start = NULL,
      extra_space = c(1.5, 1.5), verbosity = 0, interface = 'R'
    ),
    error = function(e) NULL
  )

  if (is.null(.pr)) {
    cat("  SKIPPED — feature not found in annotations\n")
    results[[locus]] <- NA
    next
  }

  # Run OLD
  t_old <- system.time({
    res_old <- tryCatch(AnnotatedFeaturesInRegion_OLD(.pr, sess$annots),
                        error = function(e) { cat("  OLD ERROR:", e$message, "\n"); NULL })
  })

  # Run NEW
  t_new <- system.time({
    res_new <- tryCatch(seqNdisplayR:::AnnotatedFeaturesInRegion(.pr, sess$annots),
                        error = function(e) { cat("  NEW ERROR:", e$message, "\n"); NULL })
  })

  timings[[locus]] <- c(old = t_old["elapsed"], new = t_new["elapsed"])

  if (is.null(res_old) || is.null(res_new)) {
    results[[locus]] <- FALSE
    next
  }

  # Compare all annotation beds
  locus_ok <- TRUE
  for (annot_name in union(names(res_old), names(res_new))) {
    cat(sprintf("  [%s]\n", annot_name))
    old_a <- res_old[[annot_name]]
    new_a <- res_new[[annot_name]]

    if (is.null(old_a) && is.null(new_a)) {
      cat("    both NULL — OK\n")
      next
    }
    if (is.null(old_a) || is.null(new_a)) {
      cat("    MISMATCH — one annotation is NULL\n")
      locus_ok <- FALSE
      next
    }

    for (slot in c("collapsed", "collapsed2", "expanded", "packing", "packing2")) {
      if (!is.null(old_a[[slot]]) || !is.null(new_a[[slot]])) {
        ok <- compare_slots(old_a[[slot]], new_a[[slot]], slot, locus)
        if (!ok) locus_ok <- FALSE
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
  cat("\n  FAILED loci:", paste(names(results)[unlist(results) == FALSE], collapse = ", "), "\n")
  cat("  -> Investigate the MISMATCH details above\n")
} else if (n_fail == 0 && n_skip == 0) {
  cat("\n  ALL OUTPUTS IDENTICAL — optimization is safe\n")
}
