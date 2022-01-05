# WORK In PROGRESS, loads an igv session for plotting with the tool


#' Load IGV Session
#'
#' @param igvsession_fname path to igv session xml file
#' @param group_by group tracks info string: 'common_prefix', 'autoscalegroups', 'do_not_group', default = 'autoscalegroups'
#' @param strand_regex for stranded files a regex distinguishing plus and minus strand file names
#' @param load_annotations load annotations as GRanges? default=FALSE.
#'
#' @details This is experimental as IGV and seqNdisplayR have very different
#'   approach. I IGV each track is considered a separate entity wheres seqNdisplayR
#'   shines when it comes to combination of different kind of track groups etc.
#'   This function is therefore a first guess, may be used in combination with
#'   session_to_df, which then allows to specify better the grouping on the
#'   resulting data.frame (see examples).
#'   group_by: autoscalegroups assumes that all samples within one
#'   autoscalegroup are one experiment and will consider these a separate group.
#'   Group name will be guessed from a common prefix present in all track names.
#'   If no common prefix is found, uses grp and an unique index.
#'   group_by: common_prefix assumes that all samples with a shared prefix are
#'   from one sample group. ie if your igv session track names are RNAseq wt;
#'   RNAseq ko1; RNAseq ko2; ChIPSeq a; ChIPseq b etc this will create a group
#'   RNAseq and a group ChIPseq.
#'   If group_by is none of the above will simply not assign datasets. Uses
#'   strand_regex to assign strand information, if strand_regex = NULL, will not
#'   try to assign strand information and treat all tracks as + strand tracks.
#'   This last version is the most robust.
#'   Parameters returned are essentially defaults except that if autoscale
#'   groups are found all samples within one group are considered the same
#'   batch.
#'   UPS: annotations are imported correct, except the default RefSeq/NCBI used
#'   in IGV is not implemented. Better set these by hand afterwards.
#'
#' @return A named list with entries samples, colors, bigwig_dirs, bigwigs,
#'   parameters and annotations
#'
#' @examples
#'
#' igvsession_fname <- system.file('extdata','example_igv_session.xml',package='seqNdisplayR')
#' igvtbl <- load_igvsession( igvsession_fname, group_by = 'autoscalegroups' )
#' igvtbl$samples
#' igvtbl$colors
#' igvtbl$bigwigs
#' igvtbl$bigwig_dirs
#' igvtbl$parameters
#' igvtbl$annotations
#'
#' session_to_df(igvtbl$samples, igvtbl$colors, igvtbl$bigwigs, igvtbl$bigwig_dirs,strand_regex = c('+'='plus', '-'='minus'))
#'
#' @export
load_igvsession <- function( igvsession_fname,
                             group_by = 'autoscalegroups',
                             strand_regex = c('+'= 'plus', '-'= 'minus'),
                             load_annotations = FALSE) {
  igv <- xml2::read_xml(igvsession_fname)
  session <- xml2::xml_find_all(igv, "//Session")
  genome <- xml2::xml_attr(session, "genome")
  tracks <- xml2::xml_find_all(igv, "//Track")
  dataSourceTracks <-
    tracks[which(xml2::xml_attr(tracks, "clazz") == "org.broad.igv.track.DataSourceTrack")]
  featureTracks <-
    tracks[which(xml2::xml_attr(tracks, "clazz") == "org.broad.igv.track.FeatureTrack")]

  annots <- as.list(xml2::xml_attr(featureTracks, "id"))
  names(annots) <- xml2::xml_attr(featureTracks, "attributeKey")

  annots[genome] = paste0('http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/Genomes/', genome, '.refGene.nohosted.bed')

  #annots <- annots[grepl('.bed', annots) | grepl('.gtf', annots) | grepl('.gff', annots)]
  annots <- annots[grepl('.bed$', annots)]
  annots

  annots <- lapply(annots, function(ann) gsub(' ', '%20', ann))

  bw_paths <- xml2::xml_attr(dataSourceTracks, "id")
  tracknames <- xml2::xml_attr(dataSourceTracks, "name")
  autoscalegroups <- xml2::xml_attr(dataSourceTracks, "autoscaleGroup")
  autoscalegroups <- sapply(autoscalegroups, function(x) ifelse(is.na(x), 'NA', x))

  trackcolors <- xml2::xml_attr(dataSourceTracks, "color")
  trackcolors <-
    sapply(trackcolors, function(cl)
      if (is.na(cl)) {
        '#000000' #black
      } else{
        {
          cls <- as.integer(strsplit(cl, ',')[[1]]) / 255
          rgb(cls[1], cls[2], cls[3])
        }
      })

  if ( !is.null(strand_regex) ) {
    track_strands <- ifelse(grepl(strand_regex['-'], bw_paths), 'minus', 'plus')
  } else {
    track_strands <- rep('', length(bw_paths))
  }


  tbl <- data.frame(
    color = as.character(trackcolors),
    name = as.character(tracknames),
    bigwig_directory = '',
    bigwig_file = bw_paths,
    strand = track_strands,
    batch = as.character(autoscalegroups),
    dataset = '',
    subgroup_1 = '',
    stringsAsFactors = F
  )

  if (group_by == 'autoscalegroups') {
    tbl$dataset = autoscalegroups

    for ( grp in unique(autoscalegroups) ) {
      grp_rows <- which(tbl$batch == grp)

      if ( sum(tbl$dataset[grp_rows] != grp) != 0 ) {
        next
      }

      ## extract common bigwig_dir
      ### need to take into account that stranded data,
      #### negative strand may have different autoscale group in IGV,
      #### but in PTSD this is considered by default...
      grp_bws <- tbl$bigwig_file[grp_rows]
      minus_bws_rows <-
        unlist(lapply(grp_bws, function(bw)
          which(
            tbl$bigwig_file == sub(strand_regex['+'], strand_regex['-'], bw, fixed = T)
          )))
      ##could simply be that plus, minus are not in names!!
      minus_bws_rows <-
        minus_bws_rows[!(minus_bws_rows %in% grp_rows)]

      if ( length(minus_bws_rows) > 0 ) {
        grp_rows <- c(grp_rows, minus_bws_rows)
      }

      name_common_prefix <- common_prefix(tbl$name[grp_rows])
      grp_name <- gsub(' $', '', name_common_prefix)

      if ( grp_name %in% tbl$dataset ) {
        i <- 1
        while ( grp_name %in% tbl$dataset ) {
          grp_name = paste0(grp_name, i)
          i <- i + 1
        }
      }

      if ( name_common_prefix != '' ) {
        tbl$dataset[grp_rows] <- grp_name
        tbl$subgroup_1[grp_rows] <-
          sub(name_common_prefix, '', tbl$name[grp_rows], fixed = TRUE)
      } else {
        i <- 1
        grp_name = paste0('grp', i)
        while ( grp_name %in% tbl$dataset ) {
          grp_name = paste0('grp', i)
          i <- i + 1
        }
        tbl$dataset[grp_rows] <- grp_name
        tbl$subgroup_1[grp_rows] <- tbl$name[grp_rows]
      }

      bw_dir <- common_prefix(tbl$bigwig_file[grp_rows])
      tbl$bigwig_directory[grp_rows] <- bw_dir
      if(bw_dir != ''){
        tbl$bigwig_file[grp_rows] <-
          sub(bw_dir, '', tbl$bigwig_file[grp_rows], fixed = TRUE)
      }


      for ( row in grp_rows ) {
        if ( tbl$strand[row] == 'minus' ) {
          bw_minus_subgrp1 <- tbl$subgroup_1[row]
          bw_plus_name <- sub(strand_regex['-'], strand_regex['+'], tbl[row, 'bigwig_file'])
          bw_plus_row <- tbl$bigwig_file == bw_plus_name
          bw_plus_subgrp1 <- tbl$subgroup_1[bw_plus_row]
          subgrp1_pfx <- common_prefix(c(bw_plus_subgrp1, bw_minus_subgrp1))
          subgrp1_pfx <- gsub(' $', '', subgrp1_pfx)
          if ( nchar(subgrp1_pfx) > 0 & !(subgrp1_pfx %in% tbl$subgroup_1[grp_rows]) ) {
            tbl$subgroup_1[row] <- subgrp1_pfx
            tbl$subgroup_1[bw_plus_row] <- subgrp1_pfx
          }
        }
      }

    }

    for ( col in c('subgroup_1', 'dataset') ) {
      tbl <- dplyr::bind_rows(lapply(unique(tbl[[col]]), function(coli) tbl[tbl[[col]]==coli,]))
    }

  } else if (group_by == 'common_prefix') {

    cp <- ''
    i <- 1
    j <- 2
    while (i < nrow(tbl)) {
      cp <- common_prefix(c(tbl$name[i], tbl$name[j]))
      while (!is.na(cp) & cp != '' & j <= nrow(tbl)) {
        cp <- common_prefix(c(tbl$name[i], tbl$name[j]))
        j = j + 1
      }
      if (j > (i + 1)) {
        if (j == (nrow(tbl) + 1)) {
          grp_rows <- i:nrow(tbl)
        } else {
          grp_rows <- i:(j - 2)
        }
        name_common_prefix <- common_prefix(tbl$name[grp_rows])
        if (sum(tbl$dataset[grp_rows] != '') == 0) {
          ## extract common bigwig_dir
          ### need to take into account that stranded data,
          #### negative strand may have different autoscale group in IGV,
          #### but in PTSD this is considered by default...
          grp_bws <- tbl$bigwig_file[grp_rows]
          minus_bws_rows <-
            unlist(lapply(grp_bws, function(bw)
              which(
                tbl$bigwig_file == sub('plus', 'minus', bw, fixed = T)
              )))
          ##could simply be that plus, minus are not in names!!
          minus_bws_rows <-
            minus_bws_rows[!(minus_bws_rows %in% grp_rows)]
          minus_names <- tbl$name[grp_rows]
          cp_minus <- grepl(paste0('^', cp), minus_names)
          if (length(minus_bws_rows) > 0) {
            grp_rows <- c(grp_rows, minus_bws_rows)
          }

          grp_name <- gsub(' $', '', name_common_prefix)

          if (grp_name %in% tbl$dataset) {
            grp_i <- 1
            while (grp_name %in% tbl$dataset) {
              grp_name = paste0(grp_name, grp_i)
              grp_i <- grp_i + 1
            }
          }

          if (name_common_prefix != '') {
            tbl$dataset[grp_rows] <- grp_name
            tbl$subgroup_1[grp_rows] <-
              sub(name_common_prefix, '', tbl$name[grp_rows], fixed = TRUE)
          } else {
            grp_i <- 1
            grp_name = paste0('grp', grp_i)
            while (grp_name %in% tbl$dataset) {
              grp_name = paste0('grp', grp_i)
              grp_i <- grp_i + 1
            }
            tbl$dataset[grp_rows] <- grp_name
            tbl$subgroup_1[grp_rows] <- tbl$name[grp_rows]
          }

          bw_dir <- common_prefix(tbl$bigwig_file[grp_rows])
          tbl$bigwig_directory[grp_rows] <- bw_dir
          tbl$bigwig_file[grp_rows] <-
            sub(bw_dir, '', tbl$bigwig_file[grp_rows], fixed = TRUE)

          for ( row in grp_rows ) {
            if ( tbl$strand[row] == 'minus' ) {
              bw_minus_subgrp1 <- tbl$subgroup_1[row]
              bw_plus_name <- sub(strand_regex['-'], strand_regex['+'], tbl[row, 'bigwig_file'])
              bw_plus_row <- tbl$bigwig_file == bw_plus_name
              bw_plus_subgrp1 <- tbl$subgroup_1[bw_plus_row]
              subgrp1_pfx <- common_prefix(c(bw_plus_subgrp1, bw_minus_subgrp1))
              subgrp1_pfx <- gsub(' $', '', subgrp1_pfx)
              if ( nchar(subgrp1_pfx) > 0 & !(subgrp1_pfx %in% tbl$subgroup_1[grp_rows]) ) {
                tbl$subgroup_1[row] <- subgrp1_pfx
                tbl$subgroup_1[bw_plus_row] <- subgrp1_pfx
              } else {
                tbl$subgroup_1[row] <- bw_plus_subgrp1
              }
            }
          }

        }
      } else {

      }
      if ( i > (j-1) ) {
        i <- j - 1
      } else {
        i <- i + 1
      }
      j <- i + 1
    }
  } else {
    tbl$dataset = tracknames
    tbl$subgroup_1 = '_'
    if ( !is.null(strand_regex) ) {
      for ( row in 1:nrow(tbl) ) {
        if ( tbl$strand[row] == 'minus' ) {
          bw_minus_subgrp1 <- tbl$dataset[row]
          bw_plus_name <- sub(strand_regex['-'], strand_regex['+'], tbl[row, 'bigwig_file'])
          bw_plus_row <- tbl$bigwig_file == bw_plus_name
          bw_plus_subgrp1 <- tbl$dataset[bw_plus_row]
          subgrp1_pfx <- common_prefix(c(bw_plus_subgrp1, bw_minus_subgrp1))
          subgrp1_pfx <- gsub(' $', '', subgrp1_pfx)
          if ( nchar(subgrp1_pfx) > 0 & !(subgrp1_pfx %in% tbl$dataset[-bw_plus_row]) ) {
            tbl$dataset[row] <- subgrp1_pfx
            tbl$dataset[bw_plus_row] <- subgrp1_pfx
          } else {
            tbl$dataset[row] <- bw_plus_subgrp1
          }
          tbl$subgroup_1[row] <- tbl$subgroup_1[bw_plus_row]
        }
      }
    }
  }

  opts <- default_plot_options()
  opts[['replicate_names']] <- NULL #makes more sense imho
  opts <- c(opts, list('replicate_names'=NULL))

  ##add annotation-specific options
  anno_options = default_annotation_options()
  anno_names <- names(annots)
  n_annos <- length(annots)
  for ( opt in names(anno_options) ) {
    opts[[opt]] <- rep(anno_options[[opt]], n_annos)
    names(opts[[opt]]) <- anno_names
  }

  # ??replicate averaging does not make sense in this case, disable
  params <- default_parameters()
  params$calcMean <- FALSE
  params$preMean <- FALSE

  datasets <- unique(tbl$dataset)
  param_list <- lapply(datasets, function(n) {x <- params; x})
  names(param_list) <- datasets

  seqNdisplayRSession(
    df = tbl,
    annotations = annots,
    parameters = param_list,
    options = opts,
    load_annotations = load_annotations
  )

  # default_options = default_annotation_options()
  #
  # annot_plot_options <- lapply(names(default_options), function(opt) {
  #   if ( opt %in% colnames(annotations) ) {
  #     l <- annotations[[opt]]
  #   } else {
  #     l <- rep(default_options[[opt]], nrow(annotations))
  #   }
  #   names(l) <- annotations$Name
  #   l
  # })
  # names(annot_plot_options) <- names(default_options)
  #
  # list('annot' = annot,
  #      'annot_plot_options' = annot_plot_options)

  # return (
  #   structure(
  #     list(
  #       'samples' = get_samples(tbl),
  #       'colors' = get_colors(tbl),
  #       'bigwig_dirs' = get_bigwig_dirs(tbl),
  #       'bigwigs' = get_bigwigs(tbl),
  #       'parameters' = get_parameters(tbl),
  #       'annotations' = annots,
  #       'options' = default_plot_options
  #     ),
  #     class = 'seqNdisplayRSession'
  #   )
  # )


}


#' Common prefix
#'
#' @return A single string which is a common prefix in all strings
common_prefix <- function(vector_of_strings) {
  if ( length(vector_of_strings) == 1 ) {
    ''
  } else {
    str1 <- vector_of_strings[1]

    eq_chars <- sapply(1:nchar(str1), function(i)
      all(
        substring(str1, i, i) == sapply(vector_of_strings, function(n)
          substring(n, i, i))
      ))

    if ( all(eq_chars == TRUE) ) {
      str1
    } else {
      substring(str1, 1, min(which(eq_chars == FALSE)) - 1)
    }
  }
}

# vector_of_strings <- c('ab_test', 'cd_test')
# ## probably never used
# common_suffix <- function(vector_of_strings) {
#   str1 <- vector_of_strings[1]
#   nc <- nchar(str1)
#   eq_chars <- sapply(1:nchar(str1), function(i)
#     all(
#         substring(str1, nc-i+1, nc-i+1) == sapply(vector_of_strings, function(n)
#           substring(n, nchar(n)-i+1, nchar(n)-i+1))
#       )
#   )
#   last_cmn_idx <- min(which(eq_chars == FALSE)) -2
#   substring(str1, nc-last_cmn_idx, nc)
# }

#' Unique suffices
#'
#' @return Vector of unique suffices in a vector of strings
unique_suffices <- function(vector_of_strings) {
  str1 <- vector_of_strings[1]

  eq_chars <- sapply(1:nchar(str1), function(i)
    all(
      substring(str1, i, i) == sapply(vector_of_strings, function(n)
        substring(n, i, i))
    ))

  as.character(sapply(vector_of_strings, function(str)
    substring(str, min(
      which(eq_chars == FALSE)
    ), nchar(str))))
}
