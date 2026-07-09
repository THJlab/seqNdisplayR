#' ============================================================================
#' IGV Session Conversion Functions for seqNdisplayR
#'
#' Bidirectional conversion between seqNdisplayR sessions and IGV session XML.
#'
#'   Session2IGV()  -  seqNdisplayR session  -->  IGV session XML
#'   IGV2Session()  -  IGV session XML       -->  seqNdisplayR session
#'
#' Requires: xml2
#'
#' These functions belong in the seqNdisplayR package R/ directory and depend
#' on existing package functions: Session2Df, Session2xlsx, LoadExcel,
#' seqNdisplayRSession, DefaultParameters, DefaultPlotOptions,
#' DefaultAnnotationOptions.
#' ============================================================================


# ---- internal helpers -------------------------------------------------------

#' @keywords internal
.hex2igv <- function(hex) {
  if (is.null(hex) || is.na(hex) || hex == "") return("0,0,178")
  tryCatch({
    v <- as.integer(grDevices::col2rgb(hex))
    paste(v, collapse = ",")
  }, error = function(e) "0,0,178")
}

#' @keywords internal
.igv2hex <- function(rgb_str) {
  if (is.null(rgb_str) || is.na(rgb_str) || rgb_str == "") return("#000000")
  tryCatch({
    v <- as.integer(strsplit(rgb_str, ",")[[1]]) / 255
    grDevices::rgb(v[1], v[2], v[3])
  }, error = function(e) "#000000")
}

#' Shared prefix of a character vector (stops at last whole-word or separator)
#'
#' Returns the longest character-by-character shared prefix that ends at a
#' separator boundary (`_ - . / whitespace`) or end-of-string. This avoids
#' mid-token "matches" like `PRPF4` of `PRPF4B` / `PRPF40A`: those two names
#' diverge mid-token (`B` vs `0`), so no valid shared prefix exists and we
#' return `""` --  letting the caller fall back to per-track grouping.
#'
#' @keywords internal
.shared_prefix <- function(strings) {
  if (length(strings) <= 1) return("")
  s1 <- strings[1]
  n <- nchar(s1)
  prefix_end <- 0L
  for (i in seq_len(n)) {
    ch <- substring(s1, i, i)
    if (!all(sapply(strings[-1], function(s) {
      nchar(s) >= i && substring(s, i, i) == ch
    }))) {
      break
    }
    prefix_end <- i
  }
  if (prefix_end == 0L) return("")
  pfx <- substring(s1, 1, prefix_end)

  # Hyphen at the END of the character class so it's interpreted literally
  # in both TRE and PCRE. Separators we recognise: whitespace, _, ., /, -.
  sep_class <- "[[:space:]_./-]"
  nonsep_class <- "[^[:space:]_./-]"
  # Boundary-aligned if either (a) the prefix itself ends at a separator,
  # or (b) every string's next char is a separator or end-of-string.
  ends_with_sep <- grepl(paste0(sep_class, "$"), pfx, perl = TRUE)
  next_chars <- vapply(strings, function(s) {
    if (nchar(s) > prefix_end) substring(s, prefix_end + 1, prefix_end + 1) else ""
  }, character(1))
  if (ends_with_sep ||
      all(next_chars == "" | grepl(sep_class, next_chars, perl = TRUE))) {
    return(pfx)
  }
  # Otherwise walk back to the last separator inside pfx (yielding a prefix
  # that lands cleanly on a token boundary).
  cut <- regexpr(paste0(sep_class, nonsep_class, "*$"), pfx, perl = TRUE)
  if (cut[1] > 0L) return(substring(pfx, 1, cut[1]))
  return("")
}

#' Trim trailing whitespace, underscores, dashes from a prefix
#' @keywords internal
.clean_prefix <- function(s) {
  gsub("[\\s_\\-./]+$", "", s, perl = TRUE)
}

#' Sentinel marker for cells the IGV heuristic could not unambiguously infer.
#' Wrapped values are detectable via \code{.SENTINEL_PATTERN} and surfaced to
#' the user via the Session2xlsx README sheet and CheckSampleFile WARN lines.
#' @keywords internal
.SENTINEL_PATTERN <- "^<FILL_ME:"

#' @keywords internal
.fill_me <- function(reason) paste0("<FILL_ME: ", reason, ">")

#' Test whether a string is a fill-me sentinel.
#' @keywords internal
.is_fill_me <- function(x) {
  !is.na(x) & grepl(.SENTINEL_PATTERN, x)
}


# ============================================================================
# Session2IGV : seqNdisplayR session --> IGV session XML
# ============================================================================

#' Export seqNdisplayR Session to IGV Session XML
#'
#' @description Converts a seqNdisplayR session object (or Excel template
#'   file) into an IGV desktop session XML that can be opened with
#'   File > Open Session in IGV.
#'
#' @param session A \code{seqNdisplayRSession} object **or** a file path
#'   to a seqNdisplayR Excel template (.xlsx).
#' @param output Character. Path for the output XML file.
#'   Default \code{"igv_session.xml"}.
#' @param genome Character. IGV genome identifier, e.g. \code{"hg38"},
#'   \code{"hg19"}, \code{"mm10"}. Default \code{"hg38"}.
#' @param locus Character. Initial locus to display. Can be a gene name
#'   (\code{"LMO4"}), coordinates (\code{"chr1:1000000-2000000"}), or
#'   \code{"All"} for whole-genome view. Default \code{"All"}.
#' @param track_height Integer. Height of each data track in pixels.
#'   Default 40.
#' @param include_annotations Logical. Include annotation bed files from
#'   the session as IGV FeatureTracks? Default \code{TRUE}.
#'
#' @return Invisibly returns the output file path.
#'
#' @details
#' The concept mapping:
#' \describe{
#'   \item{bigwig files}{Each becomes an IGV \code{<Resource>} and
#'     \code{<Track>}.}
#'   \item{colors}{Hex colors are converted to IGV's \code{"R,G,B"} format.}
#'   \item{dataset names}{Prepended to sample/subgroup names to form the
#'     IGV track display name.}
#'   \item{group_autoscale}{If TRUE for a dataset, all its tracks share an
#'     \code{autoscaleGroup} ID in IGV. Plus and minus strand bigwigs for
#'     the same sample share the same group.}
#'   \item{annotations}{Bed files become \code{FeatureTrack} entries in a
#'     separate panel.}
#' }
#'
#' @import xml2
#' @export
#'
#' @examples
#' \dontrun{
#' session <- LoadExcel("my_experiment.xlsx", load_annotations = FALSE)
#' Session2IGV(session, "for_collaborator.xml", genome = "hg38", locus = "LMO4")
#' }
#'
Session2IGV <- function(session,
                        output = "igv_session.xml",
                        genome = "hg38",
                        locus  = "All",
                        track_height = 40,
                        include_annotations = TRUE) {

  ## ---- resolve input --------------------------------------------------------
  if (is.character(session) && length(session) == 1L && file.exists(session)) {
    message("Loading seqNdisplayR Excel: ", session)
    session <- LoadExcel(session, load_annotations = FALSE)
  }
  if (!inherits(session, "seqNdisplayRSession")) {
    stop("'session' must be a seqNdisplayRSession object or a path to an Excel template.")
  }

  ## ---- flatten to one-row-per-bigwig ----------------------------------------
  df <- Session2Df(
    session$samples, session$colors, session$bigwigs,
    session$bigwig_dirs, session$parameters,
    strand_regex = c('+' = 'plus', '-' = 'minus')
  )

  ## full URL / path for each bigwig
  df$full_path <- ifelse(
    grepl("^(https?|ftp)://", df$bigwig_directory, ignore.case = TRUE),
    paste0(df$bigwig_directory, df$bigwig_file),
    paste0(df$bigwig_directory, df$bigwig_file)
  )

  ## build a human-readable display name from dataset + subgroups
  sg_cols <- sort(grep("^subgroup_", names(df), value = TRUE))
  df$display_name <- apply(df, 1, function(row) {
    parts <- row["dataset"]
    for (col in sg_cols) {
      val <- row[[col]]
      if (!is.na(val) && val != "" && val != row["dataset"]) {
        parts <- c(parts, val)
      }
    }
    paste(parts, collapse = " ")
  })

  ## strand label for display
  df$display_name <- ifelse(
    df$strand == "minus",
    paste0(df$display_name, " (-)"),
    paste0(df$display_name, " (+)")
  )

  ## ---- autoscale groups -----------------------------------------------------
  ## Each dataset gets a unique integer ID. Plus and minus bigwigs within the
  ## same dataset share the group, which is how IGV group-autoscale works.
  datasets <- unique(df$dataset)
  grp_ids  <- structure(seq_along(datasets), names = datasets)
  df$autoscale_grp <- grp_ids[df$dataset]

  ## which datasets actually want group autoscale?
  ga_flags <- sapply(session$parameters, function(p) {
    v <- p$group_autoscale
    if (is.null(v) || is.na(v)) TRUE else as.logical(v)
  })

  ## ---- build XML ------------------------------------------------------------
  doc <- xml2::xml_new_root("Session",
    genome          = genome,
    hasGeneTrack    = "true",
    hasSequenceTrack = "true",
    locus           = locus,
    version         = "8"
  )

  ## -- Resources --
  res_node <- xml2::xml_add_child(doc, "Resources")
  for (p in unique(df$full_path)) {
    xml2::xml_add_child(res_node, "Resource", path = p)
  }
  if (include_annotations && !is.null(session$annotation_files)) {
    for (nm in names(session$annotation_files)) {
      xml2::xml_add_child(res_node, "Resource",
                          path = session$annotation_files[[nm]], name = nm)
    }
  }

  ## -- Data panel (bigwig tracks) --
  panel <- xml2::xml_add_child(doc, "Panel",
    name   = "DataPanel",
    height = as.character(nrow(df) * track_height + 100)
  )

  for (i in seq_len(nrow(df))) {
    ds <- df$dataset[i]
    use_ga <- isTRUE(ga_flags[ds])

    tr <- xml2::xml_add_child(panel, "Track",
      clazz          = "org.broad.igv.track.DataSourceTrack",
      id             = df$full_path[i],
      name           = df$display_name[i],
      color          = .hex2igv(df$color[i]),
      altColor       = "0,0,178",
      autoScale      = ifelse(use_ga, "false", "true"),
      displayMode    = "COLLAPSED",
      height         = as.character(track_height),
      renderer       = "BAR_CHART",
      visible        = "true",
      windowFunction = "mean",
      normalize      = "false"
    )
    if (use_ga) {
      xml2::xml_set_attr(tr, "autoscaleGroup", as.character(df$autoscale_grp[i]))
    }
    xml2::xml_add_child(tr, "DataRange",
      baseline = "0.0", drawBaseline = "true", flipAxis = "false",
      maximum  = "50.0", minimum = "0.0", type = "LINEAR"
    )
  }

  ## -- Annotation panel --
  if (include_annotations && !is.null(session$annotation_files)) {
    apanel <- xml2::xml_add_child(doc, "Panel",
      name   = "FeaturePanel",
      height = as.character(length(session$annotation_files) * 60 + 50)
    )
    for (nm in names(session$annotation_files)) {
      xml2::xml_add_child(apanel, "Track",
        clazz       = "org.broad.igv.track.FeatureTrack",
        id          = session$annotation_files[[nm]],
        name        = nm,
        color       = "0,0,178",
        displayMode = "SQUISHED",
        height      = "35",
        renderer    = "BASIC_FEATURE",
        visible     = "true"
      )
    }
  }

  ## -- Hidden attributes (standard IGV boilerplate) --
  ha <- xml2::xml_add_child(doc, "HiddenAttributes")
  for (a in c("DATA FILE", "DATA TYPE", "NAME")) {
    xml2::xml_add_child(ha, "Attribute", name = a)
  }

  xml2::write_xml(doc, output)
  message("IGV session written to: ", output)
  message("  ", nrow(df), " data tracks across ", length(datasets), " dataset(s)")
  if (include_annotations && !is.null(session$annotation_files)) {
    message("  ", length(session$annotation_files), " annotation track(s)")
  }
  invisible(output)
}


# ============================================================================
# IGV2Session : IGV session XML --> seqNdisplayR session
# ============================================================================

#' Import IGV Session XML to seqNdisplayR
#'
#' @description Reads an IGV desktop session XML file and converts it to a
#'   seqNdisplayR session object, optionally writing an Excel template.
#'
#' @param igv_file Character. Path to an IGV session XML file.
#' @param output_xlsx Character or NULL. If a file path is provided, the
#'   session is also saved as a seqNdisplayR Excel template. Default NULL.
#' @param strand_regex Named character vector for identifying plus/minus
#'   strand from bigwig file names.
#'   Default \code{c('+' = 'plus', '-' = 'minus')}.
#' @param load_annotations Logical. Try to load annotation bed files as
#'   GRanges? Default FALSE (much faster; set TRUE when ready to plot).
#' @param group_by Character. Strategy for assigning IGV tracks to
#'   seqNdisplayR datasets, ordered from most-automatic to most-manual:
#'   \describe{
#'     \item{\code{"common_prefix"}}{(default) Greedy boundary-aligned prefix
#'       clustering on track names. Works well for any session with
#'       consistent naming conventions; no IGV-side setup required.}
#'     \item{\code{"autoscalegroups"}}{Tracks sharing an IGV
#'       \code{autoscaleGroup} become one dataset. Best when the IGV session
#'       was annotated with biological autoscaleGroups (one per sample, with
#'       + and - on sister groups). Falls back to \code{"common_prefix"} if
#'       no autoscaleGroup attributes exist.}
#'     \item{\code{"directory"}}{Tracks from the same parent directory become
#'       one dataset.}
#'     \item{\code{"none"}}{Every track becomes its own dataset
#'       (safest but most verbose).}
#'   }
#'
#' @return A \code{seqNdisplayRSession} object.
#'
#' @details
#' \strong{This direction is inherently heuristic.} IGV sessions are flat
#' (every track is independent) whereas seqNdisplayR uses a nested
#' hierarchy (dataset > subgroups > samples > replicates). The function
#' does its best to reconstruct the tree, but the result should be reviewed
#' and possibly fine-tuned --  either by editing the returned session in R
#' or by saving to Excel with \code{output_xlsx} and adjusting there.
#'
#' After import it is often useful to inspect the result:
#' \preformatted{
#'   session <- IGV2Session("file.xml")
#'   print(session)                # overview of parsed structure
#'   Session2xlsx(session, "check.xlsx")  # edit in Excel if needed
#' }
#'
#' @import xml2
#' @export
#'
#' @examples
#' \dontrun{
#' # Quick round-trip test
#' session <- IGV2Session("experiment.xml")
#' print(session)
#' plot(session, feature = "LMO4")
#'
#' # Save as editable Excel
#' IGV2Session("experiment.xml", output_xlsx = "experiment_sNdR.xlsx")
#' }
#'
IGV2Session <- function(igv_file,
                        output_xlsx = NULL,
                        strand_regex = c('+' = 'plus', '-' = 'minus'),
                        load_annotations = FALSE,
                        group_by = c("common_prefix", "autoscalegroups",
                                     "directory", "none")) {

  group_by <- match.arg(group_by)

  if (!file.exists(igv_file)) stop("File not found: ", igv_file)
  igv <- xml2::read_xml(igv_file)

  ## ---- collect all <Track> elements ----------------------------------------
  all_tracks <- xml2::xml_find_all(igv, "//Track")
  if (length(all_tracks) == 0) {
    ## some session files use <Resource> only --  try those
    all_resources <- xml2::xml_find_all(igv, "//Resource")
    if (length(all_resources) == 0) stop("No tracks or resources found in session file.")
  }

  ## separate bigwig data tracks from annotation/feature tracks
  clazz <- xml2::xml_attr(all_tracks, "clazz")
  ids   <- xml2::xml_attr(all_tracks, "id")

  is_data <- (clazz == "org.broad.igv.track.DataSourceTrack") %in% TRUE |
             grepl("\\.(bw|bigWig|bigwig)$", ids, ignore.case = TRUE)
  is_feat <- (clazz == "org.broad.igv.track.FeatureTrack") %in% TRUE |
             grepl("\\.(bed|bed\\.gz|gtf|gff)$", ids, ignore.case = TRUE)
  ## exclude anything that is both (bed tracks shouldn't be data tracks)
  is_data <- is_data & !is_feat

  data_tracks <- all_tracks[is_data]
  feat_tracks <- all_tracks[is_feat]

  if (length(data_tracks) == 0) {
    ## last resort: treat all resources with .bw extension as tracks
    all_res <- xml2::xml_find_all(igv, "//Resource")
    res_paths <- xml2::xml_attr(all_res, "path")
    bw_idx <- grep("\\.(bw|bigWig)$", res_paths, ignore.case = TRUE)
    if (length(bw_idx) == 0) stop("No bigWig tracks found in IGV session.")
    ## build minimal track info from resources alone
    ti <- data.frame(
      id    = res_paths[bw_idx],
      name  = xml2::xml_attr(all_res, "name")[bw_idx],
      color = NA_character_,
      autoscaleGroup = NA_character_,
      stringsAsFactors = FALSE
    )
  } else {
    ti <- data.frame(
      id    = xml2::xml_attr(data_tracks, "id"),
      name  = xml2::xml_attr(data_tracks, "name"),
      color = xml2::xml_attr(data_tracks, "color"),
      autoscaleGroup = xml2::xml_attr(data_tracks, "autoscaleGroup"),
      stringsAsFactors = FALSE
    )
  }

  ## fill NA names
  ti$name[is.na(ti$name) | ti$name == ""] <- basename(ti$id[is.na(ti$name) | ti$name == ""])
  ## strip trailing strand label " (+)" / " (-)" --  Session2IGV writes these into
  ## the display name, but strand is tracked separately via the filename regex.
  ## Leaving them in the name leaks the strand into subgroup_1 after prefix
  ## extraction and breaks the samples/bigwigs shape downstream.
  ti$name <- sub("\\s*\\(\\+\\)$|\\s*\\(-\\)$", "", ti$name)
  ## seqNdisplayR uses "." as a hierarchy separator in sample names, so any
  ## "." in the name (typical for raw bigwig filenames like "foo.bw" or
  ## "GSE_X.subset_Mm39.bw") would collide with the dataset.subgroup split in
  ## LoadTracks. Strip the .bw / .bigwig extension and replace any remaining
  ## dots with underscores.
  ti$name <- sub("\\.(bw|bigwig|bigWig)$", "", ti$name, ignore.case = TRUE)
  ti$name <- gsub("\\.", "_", ti$name)
  ## convert colors
  ti$hex_color <- sapply(ti$color, .igv2hex, USE.NAMES = FALSE)

  ## ---- strand assignment ----------------------------------------------------
  ti$strand <- "plus"
  if (!is.null(strand_regex)) {
    ti$strand[grepl(strand_regex["-"], ti$id)] <- "minus"
  }

  ## ---- grouping into datasets -----------------------------------------------
  ti$dataset    <- ti$name
  ti$subgroup_1 <- ti$name

  has_autoscale <- any(!is.na(ti$autoscaleGroup))

  if (group_by == "autoscalegroups" && !has_autoscale) {
    message("No autoscaleGroup attributes found in IGV session; falling back to 'common_prefix'.")
    group_by <- "common_prefix"
  }

  if (group_by == "autoscalegroups") {
    ## Best-practice IGV sessions keep + and - strand tracks in SEPARATE
    ## autoscaleGroups (so they aren't autoscaled together --  they have
    ## independent value ranges). Bridge sister groups here so that each
    ## biological sample is processed as one merged group during prefix
    ## extraction: each minus track adopts the autoscaleGroup of its plus
    ## partner (matched by filename via strand_regex). This is a no-op when
    ## + and - already share a group.
    if (!is.null(strand_regex)) {
      for (i in which(ti$strand == "minus")) {
        plus_id <- sub(strand_regex["-"], strand_regex["+"], ti$id[i], fixed = TRUE)
        plus_row <- which(ti$id == plus_id)
        if (length(plus_row) == 1L && !is.na(ti$autoscaleGroup[plus_row])) {
          if (is.na(ti$autoscaleGroup[i]) ||
              ti$autoscaleGroup[i] != ti$autoscaleGroup[plus_row]) {
            ti$autoscaleGroup[i] <- ti$autoscaleGroup[plus_row]
          }
        }
      }
    }
    ## tracks without a group get their own singleton group
    ti$autoscaleGroup[is.na(ti$autoscaleGroup)] <- paste0("solo_", seq_len(sum(is.na(ti$autoscaleGroup))))
    for (grp in unique(ti$autoscaleGroup)) {
      rows <- which(ti$autoscaleGroup == grp)
      pfx  <- .clean_prefix(.shared_prefix(ti$name[rows]))
      if (nchar(pfx) > 1) {
        ti$dataset[rows] <- pfx
        ti$subgroup_1[rows] <- sub("^[\\s_\\-./]+", "", trimws(substring(ti$name[rows], nchar(pfx) + 1)), perl = TRUE)
      } else {
        ## Synthetic dataset name --  kept Shiny-safe here; Session2xlsx()
        ## will rewrite group_N / group_solo_N to <FILL_ME:...> when the
        ## user downloads the partial xlsx.
        ti$dataset[rows] <- paste0("group_", grp)
        ti$subgroup_1[rows] <- ti$name[rows]
      }
    }

  } else if (group_by == "common_prefix") {
    ## iterative greedy prefix grouping
    remaining <- seq_len(nrow(ti))
    grp_n <- 0
    while (length(remaining) > 0) {
      i <- remaining[1]
      pfx <- ti$name[i]
      ## find all remaining names that share a progressively shorter prefix
      members <- i
      for (j in remaining[-1]) {
        cpfx <- .clean_prefix(.shared_prefix(c(pfx, ti$name[j])))
        if (nchar(cpfx) > 2) {
          members <- c(members, j)
          pfx <- cpfx
        }
      }
      pfx <- .clean_prefix(.shared_prefix(ti$name[members]))
      grp_n <- grp_n + 1
      if (nchar(pfx) > 1) {
        ti$dataset[members] <- pfx
        ti$subgroup_1[members] <- sub("^[\\s_\\-./]+", "", trimws(substring(ti$name[members], nchar(pfx) + 1)), perl = TRUE)
      } else {
        ti$dataset[members] <- paste0("group_", grp_n)
        ti$subgroup_1[members] <- ti$name[members]
      }
      remaining <- setdiff(remaining, members)
    }

  } else if (group_by == "directory") {
    dirs <- dirname(ti$id)
    dirs[dirs == "."] <- "dataset"
    ti$dataset <- basename(dirs)
    ti$subgroup_1 <- ti$name

  } else {
    ## "none" --  each track is its own dataset
    ti$dataset <- ti$name
    ti$subgroup_1 <- "_"
  }

  ## empty subgroup_1 -> use name
  ti$subgroup_1[ti$subgroup_1 == "" | is.na(ti$subgroup_1)] <-
    ti$name[ti$subgroup_1 == "" | is.na(ti$subgroup_1)]

  ## ---- pair plus/minus strands for same sample ------------------------------
  if (!is.null(strand_regex)) {
    for (i in which(ti$strand == "minus")) {
      plus_id <- sub(strand_regex["-"], strand_regex["+"], ti$id[i], fixed = TRUE)
      plus_row <- which(ti$id == plus_id)
      if (length(plus_row) == 1) {
        ## inherit dataset and subgroup from the plus track
        ti$dataset[i]    <- ti$dataset[plus_row]
        ti$subgroup_1[i] <- ti$subgroup_1[plus_row]
        ## inherit color if the minus track has none
        if (is.na(ti$color[i]) || ti$color[i] == "") {
          ti$hex_color[i] <- ti$hex_color[plus_row]
        }
      }
    }
  }

  ## ---- second-level grouping: collapse replicate-like rows ------------------
  ## After dataset + subgroup_1 are assigned, look for clusters of rows within
  ## a dataset whose subgroup_1 values share a boundary-aligned prefix. e.g.
  ## c("nodTAG_rep1_plus", "nodTAG_rep2_plus", "nodTAG_rep3_plus",
  ##   "4HdTAG_rep1_plus", "4HdTAG_rep2_plus", "4HdTAG_rep3_plus")
  ## clusters into nodTAG (3 reps) and 4HdTAG (3 reps). The cluster prefix
  ## becomes the new subgroup_1; rows that previously had distinct subgroup_1
  ## values now share one, and the seqNdisplayRSession constructor treats
  ## same-subgroup rows as replicates of one sample.
  if (group_by != "none") {
    for (ds in unique(ti$dataset)) {
      ds_rows <- which(ti$dataset == ds)
      if (length(ds_rows) < 2L) next
      sgs <- ti$subgroup_1[ds_rows]
      remaining <- seq_along(sgs)
      while (length(remaining) > 0) {
        i <- remaining[1]
        pfx <- sgs[i]
        members <- i
        for (j in remaining[-1]) {
          cpfx <- .clean_prefix(.shared_prefix(c(pfx, sgs[j])))
          if (nchar(cpfx) > 1L) {
            members <- c(members, j)
            pfx <- cpfx
          }
        }
        # Verify the final prefix is shared by ALL claimed members
        pfx <- .clean_prefix(.shared_prefix(sgs[members]))
        if (length(members) > 1L && nchar(pfx) > 1L) {
          ti$subgroup_1[ds_rows[members]] <- pfx
          ## Unify colours within the cluster: replicates of the same sample
          ## should share one display colour. Without this, GetColors() ends
          ## up with duplicate-named entries (one per rep) which breaks the
          ## Shiny widget update via `which(names(colors)==name)` returning
          ## a vector when scalar is expected.
          cluster_rows <- ds_rows[members]
          first_color <- ti$hex_color[cluster_rows[1]]
          ti$hex_color[cluster_rows] <- first_color
        }
        remaining <- setdiff(remaining, members)
      }
    }
  }

  ## ---- extract common bigwig directory per dataset --------------------------
  ti$bigwig_directory <- ""
  ti$bigwig_file      <- ti$id
  for (ds in unique(ti$dataset)) {
    rows  <- which(ti$dataset == ds)
    paths <- ti$id[rows]
    cpfx  <- .shared_prefix(paths)
    ## trim to last path separator
    last_sep <- max(gregexpr("[/\\\\]", cpfx)[[1]])
    dir_pfx  <- if (last_sep > 0) substring(cpfx, 1, last_sep) else ""
    ti$bigwig_directory[rows] <- dir_pfx
    if (nchar(dir_pfx) > 0) {
      ti$bigwig_file[rows] <- sub(dir_pfx, "", paths, fixed = TRUE)
    }
  }

  ## ---- build samples data.frame ---------------------------------------------
  samples_df <- data.frame(
    color            = ti$hex_color,
    bigwig_directory = ti$bigwig_directory,
    bigwig_file      = ti$bigwig_file,
    strand           = ti$strand,
    batch            = NA,
    dataset          = ti$dataset,
    subgroup_1       = ti$subgroup_1,
    stringsAsFactors = FALSE
  )

  ## ---- annotations ----------------------------------------------------------
  annots <- NULL
  if (length(feat_tracks) > 0) {
    ann_ids   <- xml2::xml_attr(feat_tracks, "id")
    ann_names <- xml2::xml_attr(feat_tracks, "name")
    ann_names[is.na(ann_names)] <- basename(ann_ids[is.na(ann_names)])
    ## keep only bed-like files (IGV also shows RefSeq etc. which we skip)
    bed_idx <- grep("\\.(bed|bed\\.gz)$", ann_ids, ignore.case = TRUE)
    if (length(bed_idx) > 0) {
      annots <- as.list(ann_ids[bed_idx])
      names(annots) <- ann_names[bed_idx]
    }
  }

  ## also scan <Resource> entries for bed files not in <Track>
  all_res_paths <- xml2::xml_attr(xml2::xml_find_all(igv, "//Resource"), "path")
  bed_res <- grep("\\.(bed|bed\\.gz)$", all_res_paths, ignore.case = TRUE, value = TRUE)
  for (bp in bed_res) {
    nm <- basename(bp)
    if (is.null(annots) || !(bp %in% unlist(annots))) {
      if (is.null(annots)) annots <- list()
      annots[[nm]] <- bp
    }
  }

  ## ---- parameters -----------------------------------------------------------
  datasets <- unique(samples_df$dataset)
  params <- lapply(datasets, function(ds) {
    p <- DefaultParameters()
    ds_rows <- which(ti$dataset == ds)
    ## set group_autoscale if the IGV tracks shared an autoscaleGroup
    if (any(!is.na(ti$autoscaleGroup[ds_rows]))) {
      p$group_autoscale <- TRUE
    }
    p$calcMean <- FALSE    # IGV shows individual tracks
    p
  })
  names(params) <- datasets

  ## ---- options (annotation display) -----------------------------------------
  opts <- DefaultPlotOptions()
  if (!is.null(annots)) {
    anno_opts <- DefaultAnnotationOptions()
    for (opt_name in names(anno_opts)) {
      opts[[opt_name]] <- rep(anno_opts[[opt_name]], length(annots))
      names(opts[[opt_name]]) <- names(annots)
    }
    ## On IGV import, default brackets and locus shading to FALSE; IGV sessions
    ## typically use generic annotation files (gencode, etc.) where these are
    ## visual noise. The user can re-enable per-annotation in the app.
    for (opt_name in c("incl_feature_brackets", "incl_feature_shadings")) {
      opts[[opt_name]] <- structure(rep(FALSE, length(annots)),
                                    names = names(annots))
    }
  }
  opts$replicate_names <- NULL  # IGV tracks aren't replicate-averaged

  ## ---- assemble session object ----------------------------------------------
  sess <- seqNdisplayRSession(
    df               = samples_df,
    parameters       = params,
    annotations      = annots,
    options          = opts,
    load_annotations = load_annotations
  )

  ## ---- optional Excel export ------------------------------------------------
  if (!is.null(output_xlsx)) {
    Session2xlsx(sess, output_xlsx)
    message("seqNdisplayR Excel template written to: ", output_xlsx)
  }

  ## ---- summary --------------------------------------------------------------
  n_plus  <- sum(ti$strand == "plus")
  n_minus <- sum(ti$strand == "minus")
  message(sprintf(
    "Imported %d track(s) (%d plus, %d minus) in %d dataset(s) from: %s",
    nrow(ti), n_plus, n_minus, length(datasets), basename(igv_file)
  ))
  if (!is.null(annots)) {
    message(sprintf("  %d annotation(s): %s", length(annots), paste(names(annots), collapse = ", ")))
  }
  if (group_by != "none") {
    message("  Grouping strategy: '", group_by, "' -- review with print(session) and adjust if needed.")
  }

  return(sess)
}
