#' seqNdisplayRSession
#'
#' Class as container for seqNdisplayR session information.
#'
#' @param df an optional df, overrides all other options except annotations
#' @param samples samples object as used by seqNdisplayR
#' @param colors colors object as used by seqNdisplayR
#' @param bigwig_dirs bigwig_dirs object as used by seqNdisplayR
#' @param bigwigs bigwigs object as used by seqNdisplayR
#' @param parameters parameters object as used by seqNdisplayR
#' @param annotations annotations object as used by seqNdisplayR
#' @param options named list of other arguments used by seqNdisplay function
#' @param load_annotations load annotations as GRanges? default=FALSE.
#'
#' @details seqNdisplayR session object holding above slots identical to parameters
#'   for seqNdisplay. If df is provided parses information from columns, colors,
#'   bigwig_file, bigwig_directory, dataset and subgroup_1, subgroup_2 etc. See
#'   Excel template sheet in
#'   \code{system.file('extdata','example_excel_template.xls',
#'   package='seqNdisplayR')} for more information. The df here is tidy, ie all
#'   "empty" slots are filled, see \link[seqNdisplayR]{fill_df}. Otherwise see
#'   \code{vignette(package='seqNdisplayR')}. If options are not provided, adds
#'   default options to session object. If load_annotations=TRUE will try to
#'   load annotations using \link[rtracklayer]{import}. UPS: Currently only bed
#'   files are used correctly by
#'   seqNdisplayR.
#'
#' @return A named list with slots samples, colors, bigwig_dirs, bigwigs,
#'   parameters and annotation_files and annots. Annots is either identical to
#'   annotation_files or a named list of loaded GRanges.
#'
#' @export
seqNdisplayRSession <- function(df=NULL, samples=NULL, colors=NULL, bigwig_dirs=NULL, bigwigs=NULL, parameters=NULL, annotations=NULL, options=NULL, load_annotations=F) {

  if ( !missing(df) ) {
    for ( col in c('dataset', colnames(df)[grepl('^subgroup_', colnames(df))]) ){
      if ( any(grepl(';', df[[col]])) ) {
        cat('Note: Semicolons not allowed in dataset and subgroup names, will be exchanged to colon [":"] in ', col, '\n')
        df[[col]] <- sub(';', ':', df[[col]])
      }
    }

    samples = get_samples(df)
    colors = get_colors(df)
    bigwig_dirs = get_bigwig_dirs(df)
    bigwigs = get_bigwigs(df)
    if ( missing(parameters) ) {
      parameters = lapply(names(samples), function(n) {x <- default_parameters(); x})
      names(parameters) <- names(samples)
    }

  }

  if ( missing(options) ) {
    options <- default_plot_options()
    options <- c(options, default_annotation_options())
  }

  if ( load_annotations ) {
    .annots <- lapply(annotations, function(anno) GenomicRanges::GRanges(rtracklayer::import.bed(anno)))
  } else {
    .annots <- annotations
  }

  structure(
    c(list(
      samples = samples,
      colors = colors,
      bigwig_dirs = bigwig_dirs,
      bigwigs = bigwigs,
      parameters = parameters,
      annotation_files = annotations,
      annots = .annots),
      options
    ),
    class='seqNdisplayRSession'
  )

}





#' Plot seqNdisplayR Object
#'
#'
#' @param session object of class seqNdisplayR
#' @param ... arguments passed to seqNdisplay. Should contain at least arg feature or locus.
#'
#' @details see seqNdisplay for details. Session contains samples, colors, bigwigs,
#'   bigwig_dirs, parameters and annotation information.
#'
#' @return Plot or produces pdf with default settings
#'
#' @examples
#' xl_fname <- system.file('extdata', 'example_excel_template.xls', package='seqNdisplayR')
#' session <- load_excel(xl_fname, load_annotations =F)
#' class(session) <- 'seqNdisplayRSession'
#' plot(session, feature='TAF1D')
#'
#' @export
plot.seqNdisplayRSession <- function(session, ...){
  external_args = list(...)

  # use default args except if present in ellipsis (...) (first priority), or in session (2nd priority)
  from_dots <- intersect(names(external_args), names(session))
  session[from_dots] <- external_args[from_dots]

  # add external_args not in args ((ie add feature or locus!))
  only_dots <- setdiff(names(external_args), names(session))
  session <- c(session, external_args[only_dots])

  # add default for all for some reason missing
  default_args = default_plot_options()
  only_default <- setdiff(names(default_args), names(session))
  session <- c(session, default_args[only_default])

  # handle force_scale which is part of parameters but needs to passed differently to plot function
  if ( !('force_scale' %in% names(external_args)) ) {
      force_scales <- lapply(session$parameters, function(para) {
        fc <- parse_option(para$force_scale)
        if (is.null(fc)) {
          c(NA,NA)
        }else if (length(fc) == 1){
          rep(fc, 2)
        }else{
          if ( is.null(fc[[1]]) ) {
            fc[[1]] <- NA
          }
          if ( is.null(fc[[2]]) ) {
            fc[[2]] <- NA
          }
          unlist(fc)
        }
      })
      names(force_scales) <- names(session$parameters)

      session$force_scale <- list(
        '+' = sapply(force_scales, function(x) x[1]),
        '-' = sapply(force_scales, function(x) x[2])
      )
  }

  session$parameters <- lapply(session$parameters, function(x) x[!(names(x)=='force_scale')])


  # samples renamed to dataset for function call

  names(session) <- sub('^samples$', 'datasets', names(session))

  do.call('seqNdisplay', session)

}



## just a pretty overview over a session
#' Print seqNdisplayRSession object
#'
#' Prints an overview over samples, colors and associated bigwigs in a seqNdisplayR Session.
#'
#' @param session seqNdisplayRSession object
#' @param verbose print detailed information? default=FALSE
#' @param ... arguments to glimpse_session
#'
#' @details Convenience function for checking parsing of samples, colors and bigwigs. See glimpse_session for details.
#'
#' @return Print to R session.
#'
#' @examples
#' xl_fname <- system.file('extdata', 'example_excel_template.xls', package='seqNdisplayR')
#' session <- load_excel(xl_fname, load_annotations =F)
#' print(session)
#' print(session, verbose=T)
#'
#' @export
print.seqNdisplayRSession <- function(session, verbose=FALSE, ...) {
  glimpse_session(session$samples,
                  session$colors,
                  session$bigwigs,
                  ...)
  if ( verbose ) {
    cat('\nBigwig Paths:\n')
    print(session$bigwig_dirs)
    cat('\nBigwigs:\n')
    print(session$bigwigs)
    cat('\nParameters:\n')
    print(session$parameters)
    cat('\nAnnotations:\n')
    print(session$annotation_files)
    cat('\nOptions:\n')
    for ( opt in names(default_plot_options()) ) {
      print(session[opt])
    }
    for ( opt in names(default_annotation_options()) ) {
      print(session[opt])
    }
  }

}


## from session objects to data frame
#TODO ups: does not retain batch info, would require taking parameters into account....
## just a pretty overview over a session
#' Session To Dataframe
#'
#' Converts session information to data frame as in Excel import sheet.
#'
#' @param session seqNdisplayRSession object
#' @param ... other arguments to session_to_df
#'
#'
#' @details see \link{session_to_df} for more details.
#'
#' @return A data.frame with columns: color, bigwig_directory, bigwig_file, strand, dataset and optionally subgroup_1, subgroup_2, ...
#'
#' @examples
#'
#' @export
as.data.frame.seqNdisplayRSession <- function(session, ...) {

    session_to_df(session$samples,
                  session$colors,
                  session$bigwigs,
                  session$bigwig_dirs,
                  session$parameters,
                  ...)

}


## save session object to Excel xlsx
#' Save Session To Excel
#'
#'
#' @param session seqNdisplayRSession object
#' @param path Excel file path
#' @param strand_regex named vector c('+': ..., '-': ...) for regex for converting plus strand to minus strand bigwig names.
#'
#' @details Write Excel template as described in \link{load_excel}.
#'
#' @return nothing
#'
#' @examples
#'
#' @export
session_to_xlsx <- function(session, path, ...) {
  samples <- session_to_df(session$samples,
                           session$colors,
                           session$bigwigs,
                           session$bigwig_dirs,
                           session$parameters,
                           strand_regex = c('+'= 'plus', '-'='minus'))

  #clean redundancy for better human readability
  ## optional but makes sense imho
  samples <- empty_df(samples)

  annos = NULL
  anno_display_option_names <- names(default_annotation_options())
  if (!is.null(session$annotation_files)){
    annos <- data.frame('annotation_name' = names(session$annotation_files),
                        'annotation_file' = as.character(session$annotation_files))

    #@anno_display_option_names <- names(default_annotation_options())
    anno_display_options <- session[anno_display_option_names]
    anno_display_options_df <- as.data.frame(lapply(anno_display_options, function(x) x[annos$annotation_name]))
    annos <- dplyr::bind_cols(annos, anno_display_options_df)
  }

  para_df <- data.frame(dataset = names(session$parameters))
  para_df <- dplyr::bind_cols(para_df,
                              dplyr::bind_rows(lapply(session$parameters, function(para) sapply(para, deparse_option) ) ))
  #special handling for whichSamples
  para_df$whichSamples <- sapply(names(session$parameters), function(dataset) {
    whichSamples <- session$parameters[[dataset]]$whichSamples
    if ( !is.null(whichSamples) ) {
      if ( length(whichSamples) == 0 ){
        #exclude entire dataset
        'NA'
      }else{
        #specific samples from dataset or all
        whichSamples_str <- jsonlite::toJSON(whichSamples)
        samples_str <- jsonlite::toJSON(session$samples[[dataset]])
        if( whichSamples_str == samples_str ) {
          #include all
          'NULL'
        } else {
          #specific samples
          whichSamples_str
        }
      }
    } else {
      #include all
      'NULL'
    }
  })

  session_options <- session[!names(session) %in% c('samples', 'colors', 'bigwigs', 'bigwig_dirs', 'parameters', 'annotation_files', 'annots', anno_display_option_names)]

  options <- data.frame('Option' = names(session_options),
                        'Value' = as.character(
                          sapply(session_options, deparse_option, USE.NAMES = FALSE)
                        )
                        )

  #@
  if (!is.null(annos)){
    writexl::write_xlsx(list('SAMPLES' = samples,
                             'DATASET_OPTIONS' = para_df,
                             'ANNOTATIONS' = annos,
                             'GLOBAL_OPTIONS' = options),
                        path)
  }else{
    writexl::write_xlsx(list('SAMPLES' = samples,
                             'DATASET_OPTIONS' = para_df,
                             'GLOBAL_OPTIONS' = options),
                        path)
  }
}




#' Glimpse session
#'
#' Prints an overview over samples, colors and associated bigwigs in a seqNdisplayR Session.
#'
#' @param samples samples object as used by seqNdisplayR
#' @param colors colors object as used by seqNdisplayR
#' @param bigwigs bigwigs object as used by seqNdisplayR
#' @param levels used only for internal recursion, don't change default=1.
#' @param indent_size string used for indent spacing of levels in the outpur
#'
#' @details Convenience function for checking parsing of samples, colors and bigwigs.
#'
#' @return Print to R session.
#'
#' @examples
#'
#' @export
glimpse_session <- function(samples, colors, bigwigs, level=0, indent_size='   ') {
  if ( is.list(samples) ) {
    for ( .sample in names(samples) ) {
      cat(rep(indent_size, level), .sample, '\n')
      glimpse_session(samples[[.sample]],
                      colors[[.sample]],
                      list('+'=bigwigs[['+']][[.sample]], '-'=bigwigs[['-']][[.sample]]),
                      level=level+1)
    }
  } else {
    for ( .sample in samples ) {
      if( .sample == '' ) {
        cat(rep(indent_size, level), .sample, ' color:', colors, '  bigwigs+:', length(bigwigs[['+']][[1]]), '  bigwigs-:', length(bigwigs[['-']][[1]]), '\n')
      } else {
        cat(rep(indent_size, level), .sample, ' color:', colors[[.sample]], '  bigwigs+:', length(bigwigs[['+']][[.sample]]), '  bigwigs-:', length(bigwigs[['-']][[.sample]]), '\n')
      }
    }
  }
}




## from session objects to data frame
## just a pretty overview over a session
#' Session To Dataframe
#'
#' Converts session information to data frame as in Excel import sheet.
#'
#' @param samples samples object as used by seqNdisplayR
#' @param colors colors object as used by seqNdisplayR
#' @param bigwigs bigwigs object as used by seqNdisplayR
#' @param bigwig_dirs bigwig_dirs as used in seqNdisplayR
#' @param parameters list of parameters as used in seqNdisplayR
#' @param strand_regex named vector c('+': ..., '-': ...) for regex for converting plus strand to minus strand bigwig names.
#' @param level do not change, required internally during recursion, defaults=0.
#'
#'
#' @details Converts session information to data frame as specified in Excel
#'   import sheet but using all-filled mode. Batch information is obtained from parameters.
#'
#' @return A data.frame with columns: color, bigwig_directory, bigwig_file, strand, batch, dataset and optionally subgroup_1, subgroup_2, ...
#'
#' @examples
#'
#' @export
session_to_df <-
  function(.samples,
           .colors,
           .bigwigs,
           .bigwig_dirs,
           .parameters,
           strand_regex = c('+'= 'plus', '-'='minus'),
           factorize = FALSE,
           level = 0) {

    if (level == 0) {
      grpname <- 'dataset'
    } else {
      grpname <- paste0('subgroup_', level)
    }

    if ( is.list(.samples) ) {
      inner_df <-
        lapply(names(.samples), function(samplei)
          session_to_df(
            .samples[[samplei]],
            .colors[[samplei]],
            list('+' =
                   .bigwigs[['+']][[samplei]],
                 '-' =
                   .bigwigs[['-']][[samplei]]),
            .bigwig_dirs,
            .parameters,
            level = level + 1
          ))

      for ( i in seq_along(inner_df) ) {
        inner_df[[i]][grpname] <- names(.samples)[i]
      }
      df_out <- dplyr::bind_rows(inner_df)

      if (level == 0) {
        df_out$bigwig_directory <- .bigwig_dirs[df_out$dataset]
        df_out$strand <- ifelse(grepl(strand_regex['-'], df_out$bigwig_file), 'minus', 'plus')
        df_out$batch <- NA
        datasets <- unique(df_out$dataset)
        for ( dataset in datasets ) {
          if ( !is.null(.parameters[[dataset]]$batch) ) {
            df_out$batch[df_out$dataset == dataset & df_out$strand == 'plus'] <- .parameters[[dataset]]$batch
            df_out$batch[df_out$dataset == dataset & df_out$strand == 'minus'] <- .parameters[[dataset]]$batch
          }
        }
        subgroup__names <- colnames(df_out)[grepl('^subgroup_', colnames(df_out))]
        ordered_subgroup__names <- subgroup__names[order(subgroup__names)]
        colnames_order <- c('color', 'bigwig_directory', 'bigwig_file', 'strand', 'batch', 'dataset', ordered_subgroup__names)

        return( df_out[,colnames_order] )

      }else{
        return( df_out )
      }

    } else {
      dplyr::bind_rows(
        lapply(.samples, function(samplei) {
          df <- data.frame(color = .colors[samplei],
                           bigwig_file = .bigwigs[['+']][[samplei]],
                           bigwig_directory = .bigwig_dirs[samplei],
                           grp = samplei,
                           row.names = NULL,
                           stringsAsFactors = FALSE)
          colnames(df)[colnames(df) == 'grp'] <- grpname

          if ( !is.null(.bigwigs[['-']]) & length(.bigwigs[['-']][[samplei]]) > 0)  {
            dfm <- data.frame(color = .colors[samplei],
                              bigwig_file = .bigwigs[['-']][[samplei]],
                              bigwig_directory = .bigwig_dirs[samplei],
                              grp = samplei,
                              row.names = NULL,
                              stringsAsFactors = FALSE)
            colnames(dfm)[colnames(dfm) == 'grp'] <- grpname
            df <- dplyr::bind_rows(df, dfm)
          }

          df
        })
      )
    }
  }


