# =============================================================================
# IGV import / export validation script
# -----------------------------------------------------------------------------
# Exercises IGV2Session() and Session2IGV() against the existing fixture and a
# few synthesised variants. Prints structural summaries; nothing here writes to
# package source. Run interactively with devtools::load_all() set on the
# package root.
#
# Variants exercised:
#   1. Full multi-dataset fixture with autoscaleGroup attributes
#       -> group_by = "autoscalegroups", "common_prefix", "directory", "none"
#   2. Same fixture with autoscaleGroup attributes stripped
#       -> group_by = "autoscalegroups" should fall back to common_prefix
#   3. Single-dataset slice (3-seq only)
#       -> exercise the smallest happy path
#   4. Round-trip: IGV -> Session -> IGV -> Session (structural equality)
# =============================================================================

suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".")
  } else {
    library(seqNdisplayR)
  }
  library(xml2)
})

# ----- locate fixtures -------------------------------------------------------
fixtures <- c(
  HeLa     = file.path("tests", "IGV_test.xml"),
  SF_seq   = file.path("tests", "IGV_sessions", "SF_seq.xml"),
  Z4_ARS2  = file.path("tests", "IGV_sessions",
                       "Z4_ARS2_RTF1_Z8_TT_zumer_THJ_SEP.xml")
)
fixtures <- fixtures[file.exists(fixtures)]
stopifnot(length(fixtures) >= 1)

tmp_dir <- tempfile("igv_roundtrip_")
dir.create(tmp_dir)
cat("Temp dir for synthesised XMLs / round-trip output:\n  ", tmp_dir, "\n\n")

# ----- helper: structural summary of a seqNdisplayRSession ------------------
# A session stores samples as a nested list: dataset -> (vector of sample names)
# or dataset -> subgroup -> ... -> (vector). Bigwigs are split by strand at
# the top level: sess$bigwigs[['+']] / sess$bigwigs[['-']], each mirroring the
# same nested shape as samples but only for that strand.
count_leaves <- function(x) {
  if (is.character(x)) length(x)
  else if (is.list(x)) sum(vapply(x, count_leaves, integer(1)))
  else 0L
}
summarize_session <- function(sess, label) {
  cat("---", label, "---\n")
  ds_names <- names(sess$samples)
  n_plus  <- count_leaves(sess$bigwigs[["+"]])
  n_minus <- count_leaves(sess$bigwigs[["-"]])
  cat("  Total samples:        ", n_plus + n_minus, "\n")
  cat("  Plus / minus split:   ", n_plus, " / ", n_minus, "\n", sep = "")
  cat("  Datasets:             ", length(ds_names), "\n")
  for (ds in ds_names) {
    n_p <- count_leaves(sess$bigwigs[["+"]][[ds]])
    n_m <- count_leaves(sess$bigwigs[["-"]][[ds]])
    ga  <- isTRUE(sess$parameters[[ds]]$group_autoscale)
    cat(sprintf("    %-30s  n=%d  +/-=%d/%d  ga=%s\n",
                substr(ds, 1, 30), n_p + n_m, n_p, n_m, ga))
  }
  cat("  Annotations:          ",
      if (is.null(sess$annotation_files)) 0 else length(sess$annotation_files),
      "\n")
  if (!is.null(sess$annotation_files)) {
    cat("    names:              ", paste(names(sess$annotation_files), collapse = ", "), "\n")
  }
  cat("\n")
  invisible(sess)
}

# ----- helper: synthesise a stripped-autoscaleGroup variant -----------------
strip_autoscalegroup <- function(in_xml, out_xml) {
  doc <- xml2::read_xml(in_xml)
  trks <- xml2::xml_find_all(doc, "//Track[@autoscaleGroup]")
  for (t in trks) xml2::xml_set_attr(t, "autoscaleGroup", NULL)
  xml2::write_xml(doc, out_xml)
  invisible(out_xml)
}

# ----- helper: synthesise a single-dataset slice ----------------------------
slice_single_dataset <- function(in_xml, out_xml, keep_substring) {
  doc <- xml2::read_xml(in_xml)
  res_panel <- xml2::xml_find_first(doc, "//Resources")
  for (r in xml2::xml_find_all(res_panel, "Resource")) {
    p <- xml2::xml_attr(r, "path")
    if (!grepl(keep_substring, p, fixed = TRUE) &&
        !grepl("\\.bed$", p, ignore.case = TRUE)) {
      xml2::xml_remove(r)
    }
  }
  data_panel <- xml2::xml_find_first(doc, "//Panel[@name='DataPanel']")
  for (t in xml2::xml_find_all(data_panel, "Track")) {
    id <- xml2::xml_attr(t, "id")
    if (!grepl(keep_substring, id, fixed = TRUE)) xml2::xml_remove(t)
  }
  xml2::write_xml(doc, out_xml)
  invisible(out_xml)
}

# =============================================================================
# Per-fixture battery
# =============================================================================
run_battery <- function(fixture_label, fix_full,
                        single_slice_substring = NULL,
                        roundtrip_locus = "All") {
  cat("\n#############################################\n")
  cat("##   FIXTURE: ", fixture_label, "  (", fix_full, ")\n", sep = "")
  cat("#############################################\n")

  ## Variant 1 - all four group_by strategies
  cat("\n----- V1: all group_by strategies -----\n\n")
  for (gb in c("autoscalegroups", "common_prefix", "directory", "none")) {
    cat(">>> IGV2Session(group_by = '", gb, "')\n", sep = "")
    s <- IGV2Session(fix_full, group_by = gb, load_annotations = FALSE)
    summarize_session(s, paste0(fixture_label, " / ", gb))
  }

  ## Variant 2 - autoscaleGroup stripped (only meaningful when the fixture has it)
  ag_present <- any(grepl("autoscaleGroup",
                          readLines(fix_full, warn = FALSE)))
  if (ag_present) {
    cat("\n----- V2: autoscaleGroup attributes stripped -----\n\n")
    no_ga_xml <- file.path(tmp_dir, paste0(fixture_label, "_no_autoscale.xml"))
    strip_autoscalegroup(fix_full, no_ga_xml)
    cat(">>> IGV2Session(group_by = 'autoscalegroups')  -- expect fallback message\n")
    s_fb <- IGV2Session(no_ga_xml, group_by = "autoscalegroups",
                        load_annotations = FALSE)
    summarize_session(s_fb, paste0(fixture_label, " / stripped -> common_prefix"))
  }

  ## Variant 3 - single-dataset slice (if a sub-string was supplied)
  if (!is.null(single_slice_substring)) {
    cat("\n----- V3: single-dataset slice (keep='",
        single_slice_substring, "') -----\n\n", sep = "")
    single_xml <- file.path(tmp_dir, paste0(fixture_label, "_single.xml"))
    slice_single_dataset(fix_full, single_xml,
                         keep_substring = single_slice_substring)
    cat(">>> IGV2Session(group_by = 'autoscalegroups')\n")
    s_one <- IGV2Session(single_xml, group_by = "autoscalegroups",
                         load_annotations = FALSE)
    summarize_session(s_one, paste0(fixture_label, " / single-dataset slice"))
  }

  ## Variant 4 - round-trip
  cat("\n----- V4: round-trip IGV -> Session -> IGV -> Session -----\n\n")
  cat(">>> IGV2Session(fixture)\n")
  s1 <- IGV2Session(fix_full, group_by = "autoscalegroups",
                    load_annotations = FALSE)
  summarize_session(s1, paste0(fixture_label, " / step 1: IGV -> Session"))

  cat("--- str(s1$samples, max.level=2) ---\n")
  str(s1$samples, max.level = 2)
  cat("--- str(s1$bigwigs, max.level=2) ---\n")
  str(s1$bigwigs, max.level = 2)
  cat("\n")

  cat(">>> Session2IGV(s1)\n")
  xml_rt <- file.path(tmp_dir, paste0(fixture_label, "_roundtrip.xml"))
  s2 <- NULL
  tryCatch({
    Session2IGV(s1, output = xml_rt, genome = "hg38", locus = roundtrip_locus)
    cat(">>> IGV2Session(roundtripped xml)\n")
    s2 <- IGV2Session(xml_rt, group_by = "autoscalegroups",
                      load_annotations = FALSE)
    summarize_session(s2, paste0(fixture_label, " / step 2: IGV -> Session"))
  }, error = function(e) {
    cat("!! Session2IGV failed:", conditionMessage(e), "\n\n")
  })

  if (!is.null(s2)) {
    cat("\n--- Structural comparison s1 vs s2 ---\n")
    check <- function(lab, a, b) {
      status <- if (identical(a, b)) "OK " else "DIFF"
      cat(sprintf("  [%s] %-32s  s1=%s  s2=%s\n",
                  status, lab,
                  paste(a, collapse = ","), paste(b, collapse = ",")))
    }
    check("n_plus",  count_leaves(s1$bigwigs[["+"]]), count_leaves(s2$bigwigs[["+"]]))
    check("n_minus", count_leaves(s1$bigwigs[["-"]]), count_leaves(s2$bigwigs[["-"]]))
    check("n_datasets",   length(names(s1$samples)), length(names(s2$samples)))
    check("n_annotations",
          if (is.null(s1$annotation_files)) 0L else length(s1$annotation_files),
          if (is.null(s2$annotation_files)) 0L else length(s2$annotation_files))
    check("samples per dataset (sorted)",
          unname(sort(vapply(s1$samples, count_leaves, integer(1)))),
          unname(sort(vapply(s2$samples, count_leaves, integer(1)))))
  } else {
    cat("\n--- Structural comparison skipped (round-trip failed) ---\n")
  }
  invisible(NULL)
}

# fixture-specific knobs
single_slice <- list(
  HeLa    = "HeLa_3pseq",         # 3-seq dataset only
  SF_seq  = "FNBP4",              # FNBP4 tracks only
  Z4_ARS2 = "ARS2_DMSO"           # ARS2_DMSO tracks only
)
locus_for <- list(HeLa = "LMO4", SF_seq = "All", Z4_ARS2 = "All")

for (lab in names(fixtures)) {
  run_battery(
    fixture_label = lab,
    fix_full      = fixtures[[lab]],
    single_slice_substring = single_slice[[lab]],
    roundtrip_locus = locus_for[[lab]]
  )
}

cat("\nFixture files written under: ", tmp_dir, "\n")
cat("Done.\n")
