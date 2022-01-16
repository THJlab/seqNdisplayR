##simplified version, which uses internally a tidy table, function to convert xl sheet to tidy table included!
#library(readxl) ## ups: required only once, to get bigwig_dirs
#library(tidyverse) ## ups: required only once, to get bigwig_dirs
#library(rtracklayer) ## ups: required only once, to get bigwig_dirs



#' Load Excel Template
#'
#' @param fname path to an excel template file
#' @param load_annotations load annotations as GRanges? default=FALSE.
#'
#' @details Load Excel template and parses information to seqNdisplayR session
#'   object. See Excel template sheet in
#'   \code{inst/extdata/example_excel_template.xls} for more information.
#'
#' @return seqNdisplayRSession object, essentially a named list with slots samples,
#'   colors, bigwig_dirs, bigwigs, parameters and annotations.
#'
#' @export
load_excel <- function(xl_fname, load_annotations=FALSE) {

  cat('Parsing Excel Template File\n')
  #@cat('  Samples Table\n')
  samples_df <- NULL
  tryCatch(
    {noout <- capture.output(
      samples_df <- readxl::read_excel(xl_fname, sheet = 'SAMPLES')
    )
    cat('  Samples Table               --> OK\n') #@ cat('    --> OK\n')
    },
    error=function(cond) {
     cat('  Samples Table               --> ERROR: Required sheet "SAMPLES" not found in file\n')
     error('Required sheet "SAMPLES" not found in file\n')
    }
  )
  samples_df = fill_df(samples_df)

  #@cat('  Dataset-specific options\n')

  params_df <- data.frame(dataset = unique(samples_df$dataset))
  tryCatch(
    {
      noout <- capture.output(
        params_df <- readxl::read_excel(xl_fname, sheet = 'DATASET_OPTIONS')
      )
      cat('  Dataset-specific options    --> OK\n')  #@ cat('    --> OK\n')
    },
    error=function(cond) {
      cat('  Dataset-specific options    --> Sheet "DATASET_OPTIONS" was not found in file or empty; using defaults.\n')#@ cat('    --> Sheet DATASET_OPTIONS was not found in file; using defaults.\n')
    }
  )
  params = get_parameters(samples_df, params_df)


  #@cat('  Annotations\n')
  annot_and_options <- list(annot = NULL,
                            annot_plot_options = default_annotation_options())
  tryCatch(
    {
      noout <- capture.output(
        anno_df <- readxl::read_excel(xl_fname, sheet = 'ANNOTATIONS')
      )

      annot_and_options <- get_annotations(anno_df)
      cat('  Annotations                 --> OK\n')  #@ cat('    --> OK\n')
    },
    error=function(cond) {
      cat('  Annotations                 --> Sheet "ANNOTATIONS" was not found in excel file or empty; proceeding without annotations.\n')
    }
  )

  #@cat('  Other plotting options\n')
  options <- default_plot_options()
  tryCatch(
    {
      noout <- capture.output(
        options_df <- readxl::read_excel(xl_fname, sheet = 'GLOBAL_OPTIONS')
      )

      options <- suppressWarnings(get_plot_options(options_df))
      cat('  Other plotting options      --> OK\n') #@ cat('    --> OK\n')
    },
    error=function(cond) {
      cat('  Other plotting options      --> Sheet "GLOBAL_OPTIONS" was not found in excel file or empty; using default options.\n')
    }
  )

  ##add annotation-specific options
  for ( opt in names(annot_and_options$annot_plot_options) ) {
    options[[opt]] <- annot_and_options$annot_plot_options[[opt]]
  }

  seqNdisplayRSession(
      df = samples_df,
      parameters = params,
      annotations = annot_and_options$annot,
      options = options,
      load_annotations = load_annotations
  )

}



#' isEmpty
#' @details: convenience function checks whether all NA or empty string
isempty <- function(x){
  is.na(x) | x == ''
}

#' all Empty
#' @details: convenience function checks whether all NA or empty in a vector of strings
allempty <- function(x){
  sum( sapply(x, isempty) ) == length(x)
}



## NOT USED YET BUT FUNCTIONAL
#' Get next group column
#' @param df data.frame
#' @param col a column index
#'
#' @details Returns ncol+1 if no next column found
#' @examples
#'   df <- data.frame(dataset=c(rep('a',3),rep('b',2)), subgroup_1 = c('a', 'b', 'b', 'a', 'b'), subgroup_2 = c('a', 'a', 'b', '', ''))
#'   next_grpcol(df, 1)
#'   next_grpcol(df, 2)
#'   next_grpcol(df, 3)
#'   df <- data.frame(dataset=c(rep('a',3),rep('b',2)), subgroup_2 = c('a', 'b', 'b', 'a', 'b'), subgroup_1 = c('a', 'a', 'b', '', ''))
#'   next_grpcol(df, 1)
#'   next_grpcol(df, 2)
#'   next_grpcol(df, 3)
#'
next_grpcol <- function(df, col) {
  .colnames <- colnames(df)
  cur_colname <- .colnames[col]
  if ( cur_colname == 'dataset' ) {
    next_colname <- 'subgroup_1'
  } else if ( substring(cur_colname,1,9) == 'subgroup_' ) {
    next_colname <- paste0('subgroup_', as.numeric(substring(cur_colname,10,11))+1)
  } else {
    next_colname <- NA #assuming NA is never observed as colname
  }
  if ( next_colname %in% .colnames ) {
    return (  which(.colnames == next_colname) )
  } else {
    return ( ncol(df)+1 )
  }

}



#extension of base split.data.frame where resulting list is order by order of appearance of f in x
#' Ordered Split
#'
#' @details Split data frame by values in a column but maintain order as order of
#'   appearance. ie different to base R split.data.frame where order of split
#'   objects are ordered as default factors.
ordered_split <- function(x, f, drop=TRUE){
  spl <- split(x, f, drop)
  spl[unique(as.character(f))]
}



#' Annotations from Excel
#'
#' Reads Annotations from Excel template
#'
#' @param annotations Data frame
#'
#' @details annotations is a data.frame with at least 2 columns *annotation_file* and *annotation_name*, both
#'   case-sensitive and the space is required.
#'
#' @return Named list of path, or named list of GRanges. Names used are entries in the *annotation_name* column.
#'
#' @export
get_annotations <- function(annotations) {
  annot=as.list(annotations$annotation_file)
  names(annot) <- annotations$annotation_name

  default_options = default_annotation_options()

  annot_plot_options <- lapply(names(default_options), function(opt) {
    if ( opt %in% colnames(annotations) ) {
      l <- annotations[[opt]]
    } else {
      l <- rep(default_options[[opt]], nrow(annotations))
    }
    names(l) <- annotations$annotation_name
    l
  })
  names(annot_plot_options) <- names(default_options)

  list('annot' = annot,
       'annot_plot_options' = annot_plot_options)

}



#' Parse Option String
#'
#' Parse string into relevant R object class
#'
#' @param option_str string representation of the option
#'
#' @details String can represent a named list, unnamed list, named vector, unnamed vector or single value. If string contains ";" assumes a list; if string contains "," assumes a vector; individual strings are interprated as "NULL" -> NULL; if "TRUE" or "FALSE" --> TRUE/FALSE; if single number --> as.numeric; if single non-number --> as.character;
#'
#' @examples
#' parse_option("1.2,3")
#' parse_option("RNA-seq:1.2,3;TT-seq:2,4")
#'
#' @export
parse_option <- function(option_str) {
  if( is.null(option_str) ){
    NULL
  }else if(grepl(';', option_str)){
    option_list <- strsplit(option_str,';')[[1]]
    option_list_names <- lapply(option_list, function(op) if(grepl(':', op)){sub(':.*', '', op)}else{NULL})
    option_list <- lapply(option_list, function(op) parse_option(sub('.*:', '', op)))
    names(option_list) <- option_list_names
    option_list
  }else if(grepl(',', option_str)){
    sapply(strsplit(option_str,',')[[1]], parse_option, USE.NAMES = FALSE)
  }else if( is.na(option_str) | option_str == '' ){  #same as empty cell in excel sheet
    NULL
  }else if(option_str == 'TRUE' | option_str == 'T'){
    TRUE
  }else if(option_str == 'FALSE' | option_str == 'F'){
    FALSE
  }else if(option_str == 'NULL'){
    NULL
  }else if( !is.na(suppressWarnings(as.numeric(option_str))) ){
    as.numeric(option_str)
  }else{
    option_str
  }
}



#' Deparse Option
#'
#' Parse option into string
#'
#' @param option named list, vector or single element
#'
#' @details String representation of object, compatible with parse_option
#'
#' @examples
#' deparse_option(c(1.2,3))
#' deparse_option(list('RNA-seq' = c(1.2,3), 'TT-seq' = c(2,4)))
#' deparse_option(list('RNA-seq' = c(TRUE,FALSE), 'TT-seq' = c(TRUE,FALSE)))
#'
#' @export
deparse_option <- function(option) {
  if( length(option) > 1 ){
    if ( is.list(option) ) {
      elems <- lapply(option, deparse_option)
      paste(paste(names(elems), elems, sep=':'), collapse=';')
    } else {
      paste(sapply(option, deparse_option), collapse=',')
    }
  } else if( is.null(option) ) {
    "NULL"
  } else if( option == '' ) {
    "NULL"
  } else if( is.character(option) ) {
    option
  } else if( is.numeric(option) ) {
    as.character(option)
  } else if( option == TRUE ){
    "TRUE"
  }else if( option == FALSE ){
    "FALSE"
  }else  {
    option
  }

}



#' Options from Excel
#'
#' Reads Options from Excel template
#'
#' @param options a data.frame
#'
#' @details options is a data.frame with columns named *Option* and *Value*,
#'   both case-sensitive. Several entries are vectors of character, numeric or
#'   boolean. This function tries to parse this correctly.
#'
#' @return Named list of values.
#'
#' @examples
#'   xl_fname <- system.file('extdata', 'example_excel_template.xls', package='seqNdisplayR')
#'   get_plot_options(xl_fname)
#'
#' @export
get_plot_options <- function(options) {
    if(is.null(options)){
      cat(' ! no options found in Excel sheet, setting all to defaults')
      opts <- default_plot_options()
    }else{
      opts <- as.list(options$Value)
      names(opts) <- options$Option

      #handle horizontal panels, as special case; only needed to handle V,H ??
      # if ( sum(grepl('horizontal_panels_list', names(opts))) > 0 ) {
      #   hpls <- opts[grepl('horizontal_panels_list', names(opts))]
      #   opts <- opts[!grepl('horizontal_panels_list', names(opts))]
      #
      #   hpls <- lapply(hpls, function(op) sapply(op, function(o) if(o=='FALSE' | o=='F' | o=='V'){FALSE}else if(o=='TRUE' | o=='T' | o=='H'){TRUE}else{o}, USE.NAMES=FALSE))
      #   names(hpls) <- sub('horizontal_panels_list.', '', names(hpls))
      #   opts$horizontal_panels_list <- hpls
      # }

      opts <- lapply(opts, parse_option)
    }

    opts
  }



#' Fill Empty Rows
#'
#' @param df a data frame
#'
#' @details Empty rows in data frame from loaded XL sheet are filled with values
#'   from columns above in hierarchical fashion such that the right-most columns
#'   are only filled if specified in the dataset defining row of the seqtype.
#'   See Excel template sheet in \code{inst/extdata/example_excel_template.xls}
#'   for more information.
#'
#' @return Data frame
#'
#' @export
fill_df <- function(df) {
  filled_df <- df
  start_col <- which(colnames(filled_df) == 'dataset')
  for ( i in 2:nrow(filled_df) ) {
      col <- start_col
      need_fill = ifelse(col <= ncol(filled_df), isempty(filled_df[[col]][i]) & !isempty(filled_df[[col]][i-1]), FALSE)
      while( col <= ncol(filled_df) & need_fill ){
        filled_df[[col]][i] <- filled_df[[col]][i-1]
        col <- col + 1
        need_fill = ifelse(col <= ncol(filled_df), isempty(filled_df[[col]][i]) & !isempty(filled_df[[col]][i-1]), FALSE)
      }
  }

  filled_df
}



#' Empty Unnecessary Cells in Sample DataFrame
#'
#' @param df a data frame
#'
#' @details Empty rows in data frame from a session data frame such that cells
#'   which have exactly same entry in cell above are left empty. This only
#'   applies to columns named 'bigwig_directory', 'dataset' and all columns with
#'   prefix 'subgroup_'. Output should look similar to the Samples sheet in the
#'   Excel template in \code{inst/extdata/example_excel_template.xls}.
#'
#' @return Data frame
#'
#' @export
empty_df <- function(df) {
  emptied_df <- df
  if ( nrow(df) > 1 ) {
    cols <- c('bigwig_directory', 'dataset', colnames(df)[grepl('^subgroup_', colnames(df))])

    for ( i in 2:nrow(emptied_df) ) {
      print(i)
      #print(emptied_df[i,])
      for ( col in cols ) {
        #print(col)
        print(emptied_df[[col]][[i]])
        print(df[[col]][[i-1]])
        if ( is.na(emptied_df[[col]][[i]]) ) {
          emptied_df[[col]][[i]] <- ''
        } else if ( !is.na(df[[col]][[i - 1]]) & emptied_df[[col]][[i]] == df[[col]][[i-1]] ) {
          emptied_df[[col]][[i]] <- ''
        }
      }
    }
  }

  emptied_df
}



## STEP 2: extract samples list
#' Get Samples
#'
#' @param filled_df a fill data frame (see details)
#'
#' @details Get samples from a data frame containing at a minimum columns
#'   dataset. Columns on the right to dataset may contain subcategories,
#'   called subgroup_1, subgroup_2 etc.
#'
#' @return Named lists or nested lists of named lists
#'
#' @examples
#' df <- data.frame(dataset=c(rep('a',4), rep('b',2)), subgroup_1=c('x','x','y','y', 'x','y'), stringsAsFactors=FALSE)
#' get_samples(df)
#' df <- data.frame(dataset=c(rep('a',4), rep('b',2)), subgroup_1=c('x','x','y','y', 'x','y'), subgroup_2=c('a', 'b', 'a', 'b', NA, NA), stringsAsFactors=FALSE)
#' get_samples(df)
#'
#' @export
get_samples <- function(filled_df){
  start_col <- which(colnames(filled_df) == 'dataset')
  sample_split <- function(df, split_col) {
    if( split_col >= ncol(df) ) {
      return( as.character(unique(df[[ncol(df)]])) )
    } else if ( allempty(df[[split_col+1]]) ) { # | length(unique(df[[split_col+1]])) == 1 unsafe, removed
      return ( as.character(unique(df[[split_col]])) )
    } else {
      return ( lapply(ordered_split(df, df[[split_col]], drop=TRUE), function(dfi) sample_split(dfi, split_col+1)) )
    }
  }

  sample_split(filled_df, start_col)
}



#' Get Colors
#'
#' @param filled_df a fill data frame (see details)
#'
#' @details Get colors from a data frame containing at a minimum columns
#'   color and dataset. Columns on the right to dataset may contain subcategories,
#'   called subgroup_1, subgroup_2 etc. UPS: dataset and subgroup_s must be the right-most columns, see example below.
#'
#' @return Named lists or nested lists of named lists
#'
#' @examples
#' df <- data.frame(color=c(rep('red', 4), rep('green', 2)),dataset=c(rep('a',4), rep('b',2)), subgroup_1=c('x','x','y','y', 'x','y'), subgroup_2=1:6, stringsAsFactors=FALSE)
#' get_colors(df)
#'
#' @export
get_colors <- function(filled_df){
  start_col <- which(colnames(filled_df) == 'dataset')

  color_split <- function(df, split_col) {
    if( split_col >= ncol(df) ) {
      split_col_name <- colnames(df)[ncol(df)]
      #df_dist <- dplyr::distinct_(df, split_col_name, 'color')
      df_dist <- dplyr::distinct(df, .data[[split_col_name]], color)
      colors <- df_dist$color
      names(colors) <- df_dist[[split_col_name]]
      return (colors)
    } else if ( allempty(df[[split_col+1]]) ) { #| length(unique(df[[split_col+1]])) == 1
      split_col_name <- colnames(df)[split_col]
      #df_dist <- dplyr::distinct_(df, split_col_name, 'color')
      df_dist <- dplyr::distinct(df, .data[[split_col_name]], color)
      colors <- df_dist$color
      names(colors) <- df_dist[[split_col_name]]
      return (colors)
    } else {
      return ( lapply(ordered_split(df, df[[split_col]], drop=TRUE), function(dfi) color_split(dfi, split_col+1)) )
    }
  }

  color_split(filled_df, start_col)
}




## STEP 4: extract bigwig dirs (requires dplyr/tidyverse)
#' Get Bigwig Dirs
#'
#' @param filled_df a fill data frame (see details)
#'
#' @details Get bigwig dirs file names from a data frame containing at a minimum columns
#'   bigwig_file and dataset. Note: in seqNdisplayR only one bigwig_dir is allowed per seqtype.
#'
#' @return Named vector
#'
#' @examples
#' df <- data.frame(bigwig_directory=c(rep('http://seqA/', 4), rep('http://seqB/', 2)),dataset=c(rep('a',4), rep('b',2)), subgroup_1=c('x','x','y','y', 'x','y'), subgroup_2=1:6, stringsAsFactors=FALSE)
#' get_bigwig_dirs(df)
#'
#' @export
get_bigwig_dirs <- function(filled_df){
  bigwig_dirs_df <- dplyr::distinct(filled_df, bigwig_directory, dataset)
  bigwig_dirs_df <- dplyr::filter(bigwig_dirs_df, !is.na(bigwig_directory))

  bigwig_dirs <- bigwig_dirs_df$bigwig_directory
  names(bigwig_dirs) <- bigwig_dirs_df$dataset

  bigwig_dirs
}




## STEP 5: extract bigwigs
#' Get Bigwigs
#'
#' @param filled_df a fill data frame (see details)
#'
#' @details Get bigwig file names from a data frame containing at a minimum columns
#'   bigwig_file, strand and dataset. Strand must be 'plus' or 'minus', use 'plus' for unstranded data. Columns on the right to dataset may contain subcategories,
#'   called subgroup_1, subgroup_2 etc. UPS: dataset and subgroup_s must be the right-most columns, see example below.
#'
#' @return Named lists or nested lists of named lists
#'
#' @examples
#' df <- data.frame(bigwig_file=c('a.bw', 'b.bw', 'c.bw', 'd.bw', 'e.bw', 'f.bw'),
#'   strand = rep('plus', 6), dataset=c(rep('a',4), rep('b',2)),
#'   subgroup_1=c('x','x','y','y', 'x','y'),
#'   stringsAsFactors=FALSE)
#' get_bigwigs(df)
#'
#' df <- data.frame(
#'   bigwig_file=c('a_plus.bw', 'a_minus.bw', 'b_plus.bw', 'b_minus.bw', 'c_plus.bw', 'c_minus.bw'),
#'   strand = rep(c('plus', 'minus'), 3),
#'   dataset=c(rep('a',4), rep('b',2)),
#'   subgroup_1=c('x','x','y','y', 'x','y'),
#'   stringsAsFactors=FALSE)
#' get_bigwigs(df)
#'
#' @export
get_bigwigs <- function(filled_df) {
  split_col <- which(colnames(filled_df) == 'dataset')
  bw_split <- function(df, split_col) {
    if( split_col >= ncol(df) ) {
      return ( lapply( ordered_split(df, df[[ncol(df)]], drop=TRUE), function(df) df$bigwig_file) )
    } else if ( allempty(df[[split_col+1]]) ) { #| length(unique(df[[split_col+1]])) == 1
      return ( lapply( ordered_split(df, df[[split_col]], drop=TRUE), function(df) df$bigwig_file) )
    } else {
      return ( lapply(ordered_split(df, df[[split_col]], drop=TRUE), function(dfi) bw_split(dfi, split_col+1)) )
    }
  }

  bw_plus <- bw_split(filled_df[filled_df$strand == 'plus' | is.na(filled_df$strand) | filled_df$strand == '',], split_col)
  if( any(grepl('minus', filled_df$strand)) ) {
    bw_minus <- bw_split(filled_df[filled_df$strand == 'minus' | is.na(filled_df$strand) | filled_df$strand == '',], split_col)
  }else{
    bw_minus <- NULL
  }

  list('+' = bw_plus,
       '-' = bw_minus)
}



## STEP 5: extract batch info for parameters
#' Zero-length object to null
#'
#' @details: Helper used for parsing parameters$<dataset>$whichSamples
empty_to_null <- function(x){
  if ( !is.list(x) ) {
    x
  } else if ( length(x) == 0 ) {
    NULL
  } else {
    lapply(x, empty_to_null)
  }
}



#' Get Parameters
#'
#' @param samples_df a filled data frame with sample information(see details)
#' @param params_df dataframe with Dataset_options

#'
#' @details Prepare seqNdisplayR parameters based on info in param_df. param_df
#'   needs to contain a column called dataset, all column names are considered
#'   names of dataset-specific options. Each row should contain one dataset and
#'   its information. See examples below. Batch information is from a data frame
#'   containing the track info. Needs to contain at a minimum columns strand,
#'   batch and dataset. Strand must be 'plus' or 'minus', use 'plus' for
#'   unstranded data. If batch is all empty, NAs or all identical, will assume
#'   no batch correction, otherwise set parameters to batch correction using
#'   information from the batch column. You can manually change individual
#'   parameters after this (see bottom examples in the man page).
#'
#' @return Named list
#'
#' @examples
#' samples_df <- data.frame(bigwig_file=c('a.bw', 'b.bw', 'c.bw', 'd.bw', 'e.bw', 'f.bw'),
#'   strand = rep('plus', 6),
#'   batch = rep(NA, 6),
#'   dataset=c(rep('a',4), rep('b',2)),
#'   subgroup_1=c('x','x','y','y', 'x','y'), stringsAsFactors=FALSE)
#' params_df <- data.frame(dataset=c('a','b'), calcMean=c('TRUE','FALSE'), log2Transform=c('TRUE','FALSE'), stringsAsFactors=FALSE)
#' get_parameters(samples_df, params_df)
#'
#' df <- data.frame(bigwig_file=c('a.bw', 'b.bw', 'c.bw', 'd.bw', 'e.bw', 'f.bw'),
#'   strand = rep('plus', 6),
#'   batch = rep(c('rep1','rep2'), 3),
#'   dataset=c(rep('a',4), rep('b',2)),
#'   subgroup_1=c('x','x','y','y', 'x','y'), stringsAsFactors=FALSE)
#' params <- get_parameters(df, params_df)
#' params
#'
#' #change specific arguments afterwards
#' params$a$calcMean <- FALSE
#' params$b$log2Transform <- TRUE
#'
#' @export
get_parameters <- function(samples_df, params_df){
  default_params <- default_parameters()
  param_names <- names(default_params)

  #fill in defaults if there are missing columns in params_df
  for ( param_name in param_names[!(param_names %in% colnames(params_df))] ) {
    if ( !is.null(default_params[[param_name]]) ) {
      params_df[[param_name]] <- default_params[[param_name]]
    } else {
      params_df[[param_name]] <- ''
    }
  }

  #clean params_df to only contain useful columns for parameters_list
  params_df <- params_df[,c('dataset', param_names)]

  #create params list used by seqNdisplayR except for batch
  params <- lapply(split(params_df, params_df$dataset), function(xl) lapply(as.list(xl[,colnames(xl)!='dataset']), parse_option))

  # fix special case whichSamples
  dataset_names <- names(params)
  params <- lapply(dataset_names, function(dataset) {
    para <- params[[dataset]]
    whichSamples <- params_df$whichSamples[params_df$dataset == dataset][[1]]
    if ( is.null(whichSamples) | whichSamples == 'NULL' ) {
      #include all
      para['whichSamples'] <- list(NULL)
    } else if ( is.na(whichSamples) | whichSamples == 'NA' ) {
      #exclude all
      para$whichSamples <- NA
    } else {
      #include specific ones
      para$whichSamples <- empty_to_null(jsonlite::fromJSON(whichSamples))
    }
    para
  })
  names(params) <- dataset_names

  # add batch info from filled_df
  df_plus <- samples_df[samples_df$strand != 'minus' | isempty(samples_df$strand),]
  parameter_split <- split(df_plus, df_plus$dataset)
  batches <- lapply(parameter_split, function(x) x$batch)

  for ( dataset in names(params) ) {
    batch <- batches[[dataset]]
    if ( sum(!is.na(batch)) > 0 & !all(batch == batch[1]) ) {
      params[[dataset]]$batch <-  batch
    }
  }

  params
}
