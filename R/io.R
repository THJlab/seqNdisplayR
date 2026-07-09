# I/O --  Excel, IGV, session serialization

#' Load Excel
#'
#' @description Load Excel Template in a format that fits to seqNdisplayR (see documentation or examples for details)
#'
#' @author MS/SLA
#'
#' @param xl_fname excel template file including path
#' @param load_annotations load annotations (paths in ANNOTATIONS sheet in excel template) as GRanges? default=FALSE.
#'
#' @details Load Excel template and parses information to seqNdisplayR session
#'   object. See examples of the format of Excel Templates in
#'   \code{ExamplesSampleSheetsFolder()} for more information.
#'
#' @return seqNdisplayRSession object, essentially a named list with slots samples,
#'   colors, bigwig_dirs, bigwigs, parameters and annotations and optional arguments
#'   to be fed into seqNdisplay function
#' 
#' @importFrom readxl read_excel
#' 
#' @export
#'
#' @examples
#' #' # EXAMPLE 1:
#' xl_fname = system.file('extdata', 'seqNdisplayR_sample_sheet_elaborate2.xlsx', package='seqNdisplayR')
#' session = LoadExcel(xl_fname, load_annotations = TRUE)
#' plot(session, feature='LMO4')
#' 
#' # EXAMPLE 2:
#' example_folder = ExamplesSampleSheetsFolder()
#' xl_fname = paste0(example_folder, 'seqNdisplayR_sample_sheet_elaborate2.xlsx')
#' session = LoadExcel(xl_fname, load_annotations = TRUE)
#' plot(session, feature='LMO4')
#' 
LoadExcel = function(xl_fname, load_annotations=FALSE) {
  if (!file.exists(xl_fname)){
    cat('The provided file does not exist', '\n')
    return()
  }else if ( !(grepl('.xls$', xl_fname) | grepl('.xlsx$', xl_fname))  ) {
    cat('The provided file does not have the right format - lacking extension "xls" or "xlsx"', '\n')
    return()
  }
  
  cat('Parsing sNdR sample file\n')
  
  samples_df = NULL
  tryCatch(
    { noout = capture.output( samples_df <- readxl::read_excel(xl_fname, sheet = 'SAMPLES') ) },
    error=function(cond) {
      cat('  Samples table               --> Required sheet "SAMPLES" not found in file\n')
      cat('  ERROR: ! \n')
    }
  )
  if (is.null(samples_df)){
    return()
  }
  
  if (nrow(samples_df) > 1){
    samples_df = samples_df[!apply(apply(samples_df, 2, is.na), 1, all),,drop=FALSE] 
    samples_df = samples_df[,!apply(apply(samples_df, 2, is.na), 2, all),drop=FALSE] 
    mandatory_columns = c('bigwig_directory', 'bigwig_file', 'strand', 'dataset', 'subgroup_1')
    missing_columns = setdiff(mandatory_columns, colnames(samples_df)) 
    if (length(missing_columns) > 0){ 
      cat(paste0('  Samples table               --> Required ', ifelse(length(missing_columns)==1, 'column (', 'columns ('), paste(missing_columns, collapse=', '), ') missing in sheet "SAMPLES"'), '\n')
      cat('  ERROR: ! \n')
      return()
    } 
    top_row_NAs = is.na(samples_df[1,mandatory_columns])
    if ( any(top_row_NAs) ){
      cat(paste0('  Samples table               --> Required first row ', ifelse(sum(top_row_NAs)==1, 'value (', 'values ('), paste(names(top_row_NAs)[top_row_NAs], collapse=', '), ') missing in sheet "SAMPLES"'), '\n')
      cat('  ERROR: ! \n')
      return()
    }
    cat('  Samples table               --> OK\n') 
    samples_df = FillDf(samples_df)
  }
  datasets = unique(samples_df$dataset) 
  
  params_df = data.frame(dataset = unique(samples_df$dataset))
  tryCatch(
    { noout = capture.output( params_df <- readxl::read_excel(xl_fname, sheet = 'DATASET_OPTIONS') )
      cat('  Dataset-specific options    --> OK\n')  
    },
    error=function(cond) {
      cat('  Dataset-specific options    --> Sheet "DATASET_OPTIONS" was not found in file or empty; using defaults.\n')
    }
  )
  params = GetParameters(samples_df, params_df)
  
  annot_and_options = list(annot = NULL, annot_plot_options = DefaultAnnotationOptions())
  tryCatch(
    { noout = capture.output( anno_df <- readxl::read_excel(xl_fname, sheet = 'ANNOTATIONS') )
      annot_and_options = GetAnnotations(anno_df)
      cat('  Annotations                 --> OK\n')  
    },
    error=function(cond) {
      cat('  Annotations                 --> Sheet "ANNOTATIONS" was not found in excel file or empty; proceeding without annotations.\n')
    }
  )
  
  options = DefaultPlotOptions()
  tryCatch(
    { noout = capture.output( options_df <- readxl::read_excel(xl_fname, sheet = 'GLOBAL_OPTIONS') )
      options = suppressWarnings(GetPlotOptions(options_df))
      cat('  Other plotting options      --> OK\n') 
    },
    error=function(cond) {
      cat('  Other plotting options      --> Sheet "GLOBAL_OPTIONS" was not found in excel file or empty; using default options.\n')
    }
  )
  
  if ( length(setdiff(datasets, names(params)))!=0 ){
    cat('  - there are datasets in "Samples table" that are not in "Dataset-specific options" - using defaults', '\n')
  }
  if ( length(setdiff(names(params), datasets))!=0 ){
    cat('  - there are datasets in "Dataset-specific options" that are not in "Samples table" - ignoring those', '\n')
    params = params[datasets]
  }
  
  ##add annotation-specific options
  for ( opt in names(annot_and_options$annot_plot_options) ) {
    options[[opt]] = annot_and_options$annot_plot_options[[opt]]
  }

  if (!('color' %in% colnames(samples_df))){ 
    cat('  - color(s) are not defined in "Samples table" - using default ("#346C88")', '\n')
    samples_df$color = "#346C88"
  }
  
  if (!('batch' %in% colnames(samples_df))){ 
    samples_df$batch = NA
  }
  standard_sample_cols = c('color', 'bigwig_directory', 'bigwig_file', 'strand', 'batch', 'dataset', 'subgroup_1')
  add_columns = setdiff(colnames(samples_df), standard_sample_cols)
  if (length(add_columns) > 0){
    if (!(all(grepl('subgroup_', add_columns)))){
      cat(paste0('  - there ', ifelse(length(add_columns)==1, 'is a column (', 'are columns ('), paste(add_columns, collapse=', '), ') in sheet "SAMPLES", which will be ignored because they do not fit the standard input'), '\n')
    }
    appr_add_columns = sort(grep('subgroup_', add_columns, value=TRUE))
    standard_sample_cols = c(standard_sample_cols, appr_add_columns)
  }
  samples_df = samples_df[, standard_sample_cols]
  
  seqNdisplayRSession(
    df = samples_df,
    parameters = params,
    annotations = annot_and_options$annot,
    options = options,
    load_annotations = load_annotations
  )
}


#' Load IGV Session
#' 
#' @description Load IGV Session in a format that fits to seqNdisplayR (see documentation or examples for details)
#'
#' @author MS
#'
#' @param igvsession_fname path to igv session xml file
#' @param group_by group tracks info string: 'common_prefix', 'autoscalegroups', 'do_not_group', default = 'autoscalegroups'
#' @param strand_regex for stranded files a regex distinguishing plus and minus strand file names
#' @param load_annotations load annotations as GRanges? default=FALSE
#'
#' @details This is experimental as IGV and seqNdisplayR have very different
#'   approach. In IGV each track is considered a separate entity wheres seqNdisplayR
#'   shines when it comes to combination of different kind of track groups etc.
#'   This function is therefore a first guess, may be used in combination with
#'   Session2Df, which then allows to specify better the grouping on the
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
#' @import xml2

#' 
#' @export
#'
#' @examples
#' igvsession_fname = system.file('extdata','example_igv_session.xml',package='seqNdisplayR')
#' igvtbl = LoadIGVSession( igvsession_fname, group_by = 'autoscalegroups' )
#' igvtbl$samples
#' igvtbl$colors
#' igvtbl$bigwigs
#' igvtbl$bigwig_dirs
#' igvtbl$parameters
#' igvtbl$annotations
#'
#' Session2Df(igvtbl$samples, igvtbl$colors, igvtbl$bigwigs, igvtbl$bigwig_dirs,strand_regex = c('+'='plus', '-'='minus'))
#' 
LoadIGVSession = function( igvsession_fname,
                           group_by = 'autoscalegroups',
                           strand_regex = c('+'= 'plus', '-'= 'minus'),
                           load_annotations = FALSE) {
  igv = xml2::read_xml(igvsession_fname)
  session = xml2::xml_find_all(igv, "//Session")
  genome = xml2::xml_attr(session, "genome")
  tracks = xml2::xml_find_all(igv, "//Track")
  dataSourceTracks =
    tracks[which(xml2::xml_attr(tracks, "clazz") == "org.broad.igv.track.DataSourceTrack")]
  featureTracks =
    tracks[which(xml2::xml_attr(tracks, "clazz") == "org.broad.igv.track.FeatureTrack")]
  
  annots = as.list(xml2::xml_attr(featureTracks, "id"))
  names(annots) = xml2::xml_attr(featureTracks, "attributeKey")
  
  annots[genome] = paste0('http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/Genomes/', genome, '.refGene.nohosted.bed')
  
  #annots = annots[grepl('.bed', annots) | grepl('.gtf', annots) | grepl('.gff', annots)]
  annots = annots[grepl('.bed$', annots)]
  annots
  
  annots = lapply(annots, function(ann) gsub(' ', '%20', ann))
  
  bw_paths = xml2::xml_attr(dataSourceTracks, "id")
  tracknames = xml2::xml_attr(dataSourceTracks, "name")
  autoscalegroups = xml2::xml_attr(dataSourceTracks, "autoscaleGroup")
  autoscalegroups = sapply(autoscalegroups, function(x) ifelse(is.na(x), 'NA', x))
  
  trackcolors = xml2::xml_attr(dataSourceTracks, "color")
  trackcolors =
    sapply(trackcolors, function(cl)
      if (is.na(cl)) {
        '#000000' #black
      } else{
        {
          cls = as.integer(strsplit(cl, ',')[[1]]) / 255
          rgb(cls[1], cls[2], cls[3])
        }
      })
  
  if ( !is.null(strand_regex) ) {
    track_strands = ifelse(grepl(strand_regex['-'], bw_paths), 'minus', 'plus')
  } else {
    track_strands = rep('', length(bw_paths))
  }
  
  
  tbl = data.frame(
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
      grp_rows = which(tbl$batch == grp)
      
      if ( sum(tbl$dataset[grp_rows] != grp) != 0 ) {
        next
      }
      
      ## extract common bigwig_dir
      ### need to take into account that stranded data,
      #### negative strand may have different autoscale group in IGV,
      #### but in PTSD this is considered by default...
      grp_bws = tbl$bigwig_file[grp_rows]
      minus_bws_rows =
        unlist(lapply(grp_bws, function(bw)
          which(
            tbl$bigwig_file == sub(strand_regex['+'], strand_regex['-'], bw, fixed = T)
          )))
      ##could simply be that plus, minus are not in names!!
      minus_bws_rows =
        minus_bws_rows[!(minus_bws_rows %in% grp_rows)]
      
      if ( length(minus_bws_rows) > 0 ) {
        grp_rows = c(grp_rows, minus_bws_rows)
      }
      
      name_common_prefix = CommonPrefix(tbl$name[grp_rows])
      grp_name = gsub(' $', '', name_common_prefix)
      
      if ( grp_name %in% tbl$dataset ) {
        i = 1
        while ( grp_name %in% tbl$dataset ) {
          grp_name = paste0(grp_name, i)
          i = i + 1
        }
      }
      
      if ( name_common_prefix != '' ) {
        tbl$dataset[grp_rows] = grp_name
        tbl$subgroup_1[grp_rows] =
          sub(name_common_prefix, '', tbl$name[grp_rows], fixed = TRUE)
      } else {
        i = 1
        grp_name = paste0('grp', i)
        while ( grp_name %in% tbl$dataset ) {
          grp_name = paste0('grp', i)
          i = i + 1
        }
        tbl$dataset[grp_rows] = grp_name
        tbl$subgroup_1[grp_rows] = tbl$name[grp_rows]
      }
      
      bw_dir = CommonPrefix(tbl$bigwig_file[grp_rows])
      tbl$bigwig_directory[grp_rows] = bw_dir
      if(bw_dir != ''){
        tbl$bigwig_file[grp_rows] =
          sub(bw_dir, '', tbl$bigwig_file[grp_rows], fixed = TRUE)
      }
      
      
      for ( row in grp_rows ) {
        if ( tbl$strand[row] == 'minus' ) {
          bw_minus_subgrp1 = tbl$subgroup_1[row]
          bw_plus_name = sub(strand_regex['-'], strand_regex['+'], tbl[row, 'bigwig_file'])
          bw_plus_row = tbl$bigwig_file == bw_plus_name
          bw_plus_subgrp1 = tbl$subgroup_1[bw_plus_row]
          subgrp1_pfx = CommonPrefix(c(bw_plus_subgrp1, bw_minus_subgrp1))
          subgrp1_pfx = gsub(' $', '', subgrp1_pfx)
          if ( nchar(subgrp1_pfx) > 0 & !(subgrp1_pfx %in% tbl$subgroup_1[grp_rows]) ) {
            tbl$subgroup_1[row] = subgrp1_pfx
            tbl$subgroup_1[bw_plus_row] = subgrp1_pfx
          }
        }
      }
      
    }
    
    for ( col in c('subgroup_1', 'dataset') ) {
      tbl = do.call(rbind, lapply(unique(tbl[[col]]), function(coli) tbl[tbl[[col]]==coli,]))
    }
    
  } else if (group_by == 'common_prefix') {
    
    cp = ''
    i = 1
    j = 2
    while (i < nrow(tbl)) {
      cp = CommonPrefix(c(tbl$name[i], tbl$name[j]))
      while (!is.na(cp) & cp != '' & j <= nrow(tbl)) {
        cp = CommonPrefix(c(tbl$name[i], tbl$name[j]))
        j = j + 1
      }
      if (j > (i + 1)) {
        if (j == (nrow(tbl) + 1)) {
          grp_rows = i:nrow(tbl)
        } else {
          grp_rows = i:(j - 2)
        }
        name_common_prefix = CommonPrefix(tbl$name[grp_rows])
        if (sum(tbl$dataset[grp_rows] != '') == 0) {
          ## extract common bigwig_dir
          ### need to take into account that stranded data,
          #### negative strand may have different autoscale group in IGV,
          #### but in PTSD this is considered by default...
          grp_bws = tbl$bigwig_file[grp_rows]
          minus_bws_rows =
            unlist(lapply(grp_bws, function(bw)
              which(
                tbl$bigwig_file == sub('plus', 'minus', bw, fixed = T)
              )))
          ##could simply be that plus, minus are not in names!!
          minus_bws_rows =
            minus_bws_rows[!(minus_bws_rows %in% grp_rows)]
          minus_names = tbl$name[grp_rows]
          cp_minus = grepl(paste0('^', cp), minus_names)
          if (length(minus_bws_rows) > 0) {
            grp_rows = c(grp_rows, minus_bws_rows)
          }
          
          grp_name = gsub(' $', '', name_common_prefix)
          
          if (grp_name %in% tbl$dataset) {
            grp_i = 1
            while (grp_name %in% tbl$dataset) {
              grp_name = paste0(grp_name, grp_i)
              grp_i = grp_i + 1
            }
          }
          
          if (name_common_prefix != '') {
            tbl$dataset[grp_rows] = grp_name
            tbl$subgroup_1[grp_rows] =
              sub(name_common_prefix, '', tbl$name[grp_rows], fixed = TRUE)
          } else {
            grp_i = 1
            grp_name = paste0('grp', grp_i)
            while (grp_name %in% tbl$dataset) {
              grp_name = paste0('grp', grp_i)
              grp_i = grp_i + 1
            }
            tbl$dataset[grp_rows] = grp_name
            tbl$subgroup_1[grp_rows] = tbl$name[grp_rows]
          }
          
          bw_dir = CommonPrefix(tbl$bigwig_file[grp_rows])
          tbl$bigwig_directory[grp_rows] = bw_dir
          tbl$bigwig_file[grp_rows] =
            sub(bw_dir, '', tbl$bigwig_file[grp_rows], fixed = TRUE)
          
          for ( row in grp_rows ) {
            if ( tbl$strand[row] == 'minus' ) {
              bw_minus_subgrp1 = tbl$subgroup_1[row]
              bw_plus_name = sub(strand_regex['-'], strand_regex['+'], tbl[row, 'bigwig_file'])
              bw_plus_row = tbl$bigwig_file == bw_plus_name
              bw_plus_subgrp1 = tbl$subgroup_1[bw_plus_row]
              subgrp1_pfx = CommonPrefix(c(bw_plus_subgrp1, bw_minus_subgrp1))
              subgrp1_pfx = gsub(' $', '', subgrp1_pfx)
              if ( nchar(subgrp1_pfx) > 0 & !(subgrp1_pfx %in% tbl$subgroup_1[grp_rows]) ) {
                tbl$subgroup_1[row] = subgrp1_pfx
                tbl$subgroup_1[bw_plus_row] = subgrp1_pfx
              } else {
                tbl$subgroup_1[row] = bw_plus_subgrp1
              }
            }
          }
          
        }
      } else {
        
      }
      if ( i > (j-1) ) {
        i = j - 1
      } else {
        i = i + 1
      }
      j = i + 1
    }
  } else {
    tbl$dataset = tracknames
    tbl$subgroup_1 = '_'
    if ( !is.null(strand_regex) ) {
      for ( row in 1:nrow(tbl) ) {
        if ( tbl$strand[row] == 'minus' ) {
          bw_minus_subgrp1 = tbl$dataset[row]
          bw_plus_name = sub(strand_regex['-'], strand_regex['+'], tbl[row, 'bigwig_file'])
          bw_plus_row = tbl$bigwig_file == bw_plus_name
          bw_plus_subgrp1 = tbl$dataset[bw_plus_row]
          subgrp1_pfx = CommonPrefix(c(bw_plus_subgrp1, bw_minus_subgrp1))
          subgrp1_pfx = gsub(' $', '', subgrp1_pfx)
          if ( nchar(subgrp1_pfx) > 0 & !(subgrp1_pfx %in% tbl$dataset[-bw_plus_row]) ) {
            tbl$dataset[row] = subgrp1_pfx
            tbl$dataset[bw_plus_row] = subgrp1_pfx
          } else {
            tbl$dataset[row] = bw_plus_subgrp1
          }
          tbl$subgroup_1[row] = tbl$subgroup_1[bw_plus_row]
        }
      }
    }
  }
  
  opts = DefaultPlotOptions()
  opts[['replicate_names']] = NULL #makes more sense imho
  opts = c(opts, list('replicate_names'=NULL))
  
  ##add annotation-specific options
  anno_options = DefaultAnnotationOptions()
  anno_names = names(annots)
  n_annos = length(annots)
  for ( opt in names(anno_options) ) {
    opts[[opt]] = rep(anno_options[[opt]], n_annos)
    names(opts[[opt]]) = anno_names
  }
  
  # ??replicate averaging does not make sense in this case, disable
  params = DefaultParameters()
  params$calcMean = FALSE
  params$preMean = FALSE
  
  datasets = unique(tbl$dataset)
  param_list = lapply(datasets, function(n) {x = params; x})
  names(param_list) = datasets
  
  seqNdisplayRSession(
    df = tbl,
    annotations = annots,
    parameters = param_list,
    options = opts,
    load_annotations = load_annotations
  )
}


#' Session 2 Df
#'
#' @description 
#' Converts session information to data frame as in Excel import sheet.
#'
#' @author MS
#'
#' @param .samples samples object as used by seqNdisplayR
#' @param .colors colors object as used by seqNdisplayR
#' @param .bigwigs bigwigs object as used by seqNdisplayR
#' @param .bigwig_dirs bigwig_dirs as used in seqNdisplayR
#' @param .parameters list of parameters as used in seqNdisplayR
#' @param strand_regex named vector c('+': ..., '-': ...) for regex for converting plus strand to minus strand bigwig names
#' @param factorize TRUE/FALSE. Default: FALSE
#' @param level do not change, required internally during recursion, defaults=0.
#'
#' @details Converts session information to dataframe as specified in Excel
#'   import sheet but using all-filled mode. Batch information is obtained from parameters.
#'
#' @return A dataframe with columns: color, bigwig_directory, bigwig_file, strand, batch, dataset and optionally subgroup_1, subgroup_2, ...
#'

#' 
#' @export
#'
#' @examples 
#' NULL
#' 
Session2Df = function(.samples, .colors, .bigwigs, .bigwig_dirs, .parameters, strand_regex = c('+'= 'plus', '-'='minus'), factorize = FALSE, level = 0) {
  if (level == 0) {
    grpname = 'dataset'
  } else {
    grpname = paste0('subgroup_', level)
  }
  if ( is.list(.samples) ) {
    inner_df =
      lapply(names(.samples), function(samplei)
        Session2Df(
          .samples[[samplei]],
          .colors[[samplei]],
          list('+' =
                 .bigwigs[['+']][[samplei]],
               '-' =
                 .bigwigs[['-']][[samplei]]),
          list('+' =
                 .bigwig_dirs[['+']][[samplei]],
               '-' =
                 .bigwig_dirs[['-']][[samplei]]),
          .parameters,
          level = level + 1
        ))

    for ( i in seq_along(inner_df) ) {
      inner_df[[i]][grpname] = names(.samples)[i]
    }
    df_out = RbindFill(inner_df)
    if (level == 0) {
      # bigwig_directory is now carried per-row from the leaf (no longer stamped by dataset)
      df_out$strand = NA
      df_out$strand[df_out$bigwig_file %in% unlist(.bigwigs[['+']], use.names = FALSE)] = 'plus'
      df_out$strand[df_out$bigwig_file %in% unlist(.bigwigs[['-']], use.names = FALSE)] = 'minus'
      df_out$batch = NA
      datasets = unique(df_out$dataset)
      for ( dataset in datasets ) {
        if ( !is.null(.parameters[[dataset]]$batch) ) {
          df_out$batch[df_out$dataset == dataset & df_out$strand == 'plus'] = .parameters[[dataset]]$batch
          df_out$batch[df_out$dataset == dataset & df_out$strand == 'minus'] = .parameters[[dataset]]$batch
        }
      }
      subgroup__names = colnames(df_out)[grepl('^subgroup_', colnames(df_out))]
      ordered_subgroup__names = subgroup__names[order(subgroup__names)]
      colnames_order = c('color', 'bigwig_directory', 'bigwig_file', 'strand', 'batch', 'dataset', ordered_subgroup__names)
      return( df_out[,colnames_order] )
    }else{
      return( df_out )
    }
  } else {
    do.call(rbind,
      lapply(.samples, function(samplei) {
        has_plus  <- length(.bigwigs[['+']][[samplei]]) > 0
        has_minus <- !is.null(.bigwigs[['-']]) && length(.bigwigs[['-']][[samplei]]) > 0

        parts <- list()
        if (has_plus) {
          parts$p <- data.frame(color = .colors[samplei],
                                bigwig_file = .bigwigs[['+']][[samplei]],
                                bigwig_directory = .bigwig_dirs[['+']][[samplei]],
                                grp = samplei,
                                row.names = NULL,
                                stringsAsFactors = FALSE)
        }
        if (has_minus) {
          parts$m <- data.frame(color = .colors[samplei],
                                bigwig_file = .bigwigs[['-']][[samplei]],
                                bigwig_directory = .bigwig_dirs[['-']][[samplei]],
                                grp = samplei,
                                row.names = NULL,
                                stringsAsFactors = FALSE)
        }
        if (length(parts) == 0) return(NULL)
        df <- do.call(rbind, parts)
        colnames(df)[colnames(df) == 'grp'] <- grpname
        df
      })
    )
  }
}


#' Session2xlsx
#'
#' @description Save session object to Excel xlsx-file
#'
#' @author MS (minor additions by SLA)
#'
#' @param session seqNdisplayRSession object
#' @param path excel file name including full path
#' @param ... 
#'
#' @return Write Excel template as described in \link{LoadExcel}.
#' 


#' @importFrom writexl write_xlsx
#' 
#' @export
#'
#' @examples 
#' NULL
#' 
Session2xlsx = function(session, path, ...) {
  samples_full = Session2Df(session$samples,
                            session$colors,
                            session$bigwigs,
                            session$bigwig_dirs,
                            session$parameters,
                            strand_regex = c('+'= 'plus', '-'='minus'))

  ## Mark synthetic dataset names (left by IGV2Session when its heuristic
  ## couldn't unambiguously infer a real name) with the FILL_ME sentinel
  ## *only on the way out to xlsx* --  the in-memory session keeps the
  ## Shiny-safe group_N / group_solo_N literals so widget IDs don't break.
  synth_pattern <- "^group_(solo_)?\\d+$"
  synth_rows    <- grepl(synth_pattern, samples_full$dataset)
  if (any(synth_rows)) {
    samples_full$dataset[synth_rows] <- paste0(
      "<FILL_ME: synthetic dataset (",
      samples_full$dataset[synth_rows],
      "); please rename>"
    )
  }

  #clean redundancy for better human readability
  ## optional but makes sense imho
  samples = EmptyDf(samples_full)
  
  annos = NULL
  anno_display_option_names = names(DefaultAnnotationOptions())
  if (!is.null(session$annotation_files)){
    annos = data.frame('annotation_name' = names(session$annotation_files),
                       'annotation_file' = as.character(session$annotation_files))
    anno_display_options = session[anno_display_option_names]
    #ensure consistent sorting
    anno_display_options = lapply(anno_display_options, function(x) sapply(x, DeparseOption))
    anno_display_options_df = as.data.frame(anno_display_options)
    annos = cbind(annos, anno_display_options_df)
  }
  
  para_df = data.frame(dataset = names(session$parameters))
  para_df = cbind(para_df,
                  do.call(rbind, lapply(session$parameters, function(para) sapply(para[names(para) != 'whichSamples'], DeparseOption) ) ))
  #special handling for whichSamples
  para_df$whichSamples = sapply(names(session$parameters), function(dataset) {
    whichSamples = session$parameters[[dataset]]$whichSamples
    if ( !is.null(whichSamples) ) {
      if ( length(whichSamples) == 0 | all(is.na(whichSamples))){
        #exclude entire dataset
        'NA'
      }else{
        #specific samples from dataset or all
        whichSamples_str = deparse(whichSamples)
        samples_str = deparse(session$samples[[dataset]])
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
  
  #@ 2023-09-20 remove batch info from DATASET_OPTIONS
  para_df = para_df[,-which(colnames(para_df)=='batch')]

  ## Apply the same FILL_ME sentinel rewrite to DATASET_OPTIONS so its
  ## dataset column matches the rewritten SAMPLES column.
  synth_rows_para <- grepl(synth_pattern, para_df$dataset)
  if (any(synth_rows_para)) {
    para_df$dataset[synth_rows_para] <- paste0(
      "<FILL_ME: synthetic dataset (",
      para_df$dataset[synth_rows_para],
      "); please rename>"
    )
  }
  
  session_options = session[!names(session) %in% c('samples', 'colors', 'bigwigs', 'bigwig_dirs', 'parameters', 'annotation_files', 'annots', anno_display_option_names)]
  
  #@ 2024-08-16
  if (length(intersect(para_df$dataset, names(session_options[['horizontal_panels_list']]))) > 0){
    session_options[['horizontal_panels_list']] = session_options[['horizontal_panels_list']][intersect(para_df$dataset, names(session_options[['horizontal_panels_list']]))]  
  }else{
    session_options[['horizontal_panels_list']] = NULL
  }
  
  
  options = data.frame('Option' = names(session_options),
                       'Value' = as.character(
                         sapply(session_options, DeparseOption, USE.NAMES = FALSE)
                       )
  )
  options = options[order(names(session_options)),]

  ## Always include an ANNOTATIONS sheet --  even when the source had none
  ## (e.g. IGV sessions that use IGV's built-in gene track). An empty sheet
  ## with the right column headers gives the user a place to add annotations
  ## by hand without recreating the structure.
  if (is.null(annos)) {
    annos <- data.frame(annotation_name = character(0),
                        annotation_file = character(0),
                        stringsAsFactors = FALSE)
  }

  ## Build sheet list. README is prepended when either (a) the SAMPLES data
  ## contains <FILL_ME:...> sentinels left behind by IGV2Session, or (b) the
  ## annotations list is empty (so the user gets explicit guidance on how to
  ## populate the ANNOTATIONS sheet by hand).
  readme <- .BuildReadmeSheet(samples_full, session$annotation_files)

  sheets <- list()
  if (!is.null(readme)) sheets[["README"]] <- readme
  sheets[["SAMPLES"]]         <- samples
  sheets[["DATASET_OPTIONS"]] <- para_df
  sheets[["ANNOTATIONS"]]     <- annos
  sheets[["GLOBAL_OPTIONS"]]  <- options

  writexl::write_xlsx(sheets, path)
}


#' Build README sheet for partial xlsx (internal)
#'
#' @description Internal helper for \code{Session2xlsx()}. Returns a
#' data.frame to be written as the first sheet of the workbook when there
#' is something worth telling the user (sentinel placeholders to fix, or
#' an empty annotation list to populate). Returns NULL when both are fine.
#'
#' @keywords internal
.BuildReadmeSheet <- function(samples_df, annotation_files = NULL) {
  sentinel_pat    <- "^<FILL_ME:"
  sent_rows       <- which(grepl(sentinel_pat, samples_df$dataset))
  has_sentinels   <- length(sent_rows) > 0L
  has_annotations <- !is.null(annotation_files) && length(annotation_files) > 0L

  # Nothing notable -> no README needed.
  if (!has_sentinels && has_annotations) return(NULL)

  rows <- list()

  rows[[length(rows) + 1L]] <- data.frame(
    Step = "Source",
    Description = paste("This Excel file was auto-generated from an IGV session",
                        "via seqNdisplayR::IGV2Session()."),
    stringsAsFactors = FALSE
  )

  if (has_sentinels) {
    rows[[length(rows) + 1L]] <- data.frame(
      Step = c("Sentinels", "How to fix", "Verify"),
      Description = c(
        paste("The heuristic couldn't unambiguously infer all dataset names.",
              "Cells starting with '<FILL_ME:' are placeholders."),
        paste("Replace each '<FILL_ME:...>' cell with a meaningful name",
              "(e.g. the dataset or sample these tracks represent)."),
        paste("After editing, save the file and re-upload it in the Shiny",
              "app, OR click 'Check File' --  CheckSampleFile() will WARN on",
              "any remaining sentinels.")
      ),
      stringsAsFactors = FALSE
    )
  }

  if (!has_annotations) {
    rows[[length(rows) + 1L]] <- data.frame(
      Step = "Annotations",
      Description = paste("The source IGV session has no annotation BED files",
                          "(IGV's built-in gene track is not exported as a Resource).",
                          "The ANNOTATIONS sheet is empty by default. To add",
                          "annotations, populate one row per BED file with at",
                          "least the columns 'annotation_name' and 'annotation_file'",
                          "(local path or URL)."),
      stringsAsFactors = FALSE
    )
  }

  if (has_sentinels) {
    uniq <- unique(samples_df$dataset[sent_rows])
    loc_rows <- lapply(uniq, function(s) {
      idx <- which(samples_df$dataset == s)
      data.frame(
        Step        = paste0("SAMPLES rows ", min(idx), "-", max(idx)),
        Description = paste0("column 'dataset' = ", s),
        stringsAsFactors = FALSE
      )
    })
    rows <- c(rows, loc_rows)
  }

  do.call(rbind, rows)
}


#' Examples Sample Sheets Folder
#'
#' @description Finds the folder with example Excel Templates for seqNdisplayR
#'
#' @author SLA
#'
#' @return finds and returns the folder with example Excel Templates for seqNdisplayR
#'
#' @export
#'
#' @examples
#' example_folder = ExamplesSampleSheetsFolder()
#' list.files(example_folder)
#' fname = list.files(example_folder)[7]
#' xl_fname = paste0(example_folder, fname)
#' session = LoadExcel(xl_fname, load_annotations = T)
#' plot(session, feature='LMO4')
#' 
ExamplesSampleSheetsFolder = function(){
  libpaths = .libPaths()
  for (libpath in libpaths){
    lf = list.files(libpath)
    if (any(grepl('seqNdisplayR', lf))){
      samples_sheets_folders = paste0(libpath, '/seqNdisplayR/extdata/')
    }
  }
  samples_sheets_folders
}


#' List Examples Sample Sheets
#'
#' @description Finds and outputs the example seqNdisplayR Excel Templates that comes with the package
#'
#' @author SLA
#'
#' @return the example seqNdisplayR Excel Templates that comes with the package
#' 
#' @export
#'
#' @examples
#' example_sample_sheets = ListExamplesSampleSheets()
#' example_folder = ExamplesSampleSheetsFolder()
#' xl_fname = paste0(example_folder, example_sample_sheets[1])
#' session = LoadExcel(xl_fname, load_annotations = T)
#' plot(session, feature='LMO4')
#' 
ListExamplesSampleSheets = function(){
  libpaths = .libPaths()
  for (libpath in libpaths){
    lf = list.files(libpath)
    if (any(grepl('seqNdisplayR', lf))){
      samples_sheets_folders = paste0(libpath, '/seqNdisplayR/extdata/')
    }
  }
  list.files(samples_sheets_folders)
}


#' run seq'N'display'R app
#'
#' @description Run the shiny app
#'
#' @author SLA
#'
#' @return placeholder
#' 
#' @export
#'
#' @examples
#' run_seqNdisplayR_app()
#' run_seqNdisplayR_app(launch.browser = TRUE)
#' 
run_seqNdisplayR_app = function(...){
  # Check and pre-load all Shiny-related packages with full message suppression.
  # This must happen here (not inside the app file) because shiny::runApp()
  # sources the app in its own environment where suppressMessages() wrappers
  # around library() calls do not reliably catch all startup messages.
  shiny_pkgs <- c("shiny", "shinyjs", "shinyTree", "shinyBS",
                   "shinybusy", "spsComps", "colourpicker", "DT")
  missing_pkgs <- shiny_pkgs[!sapply(shiny_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("The following packages are required to run the app: ",
         paste(missing_pkgs, collapse = ", "),
         "\nInstall them with install.packages()")
  }
  suppressPackageStartupMessages(suppressMessages(suppressWarnings({
    for (pkg in shiny_pkgs) library(pkg, character.only = TRUE)
  })))
  # Eagerly load future / future.apply (used for parallel bigwig fetch) so
  # the "package 'future' was built under R version X.Y.Z" warning -- shown
  # by R the first time the package is loaded if its build target differs
  # from the running R -- is silenced once at startup rather than firing
  # mid-plot from inside .ensure_future_plan().
  for (pkg in c("future", "future.apply")) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      suppressPackageStartupMessages(suppressMessages(suppressWarnings(
        library(pkg, character.only = TRUE)
      )))
    }
  }
  cat("seqNdisplayR Shiny app: all libraries loaded.\n")
  app <- system.file("shiny", "seqNdisplayR_app.R", package = "seqNdisplayR")
  if (!nzchar(app)) {
    stop("Could not find seqNdisplayR Shiny app. Is the package installed correctly?")
  }
  shiny::runApp(app, ...)
}


#' Parse Option
#'
#' @description Internal function: 
#' Parse string into relevant R object class
#'
#' @keywords internal
#'
#' @author MS
#'
#' @param option_str string representation of the option
#'
#' @details String can represent a named list, unnamed list, named vector, unnamed vector or single value. If string contains ";" assumes a list; if string contains "," assumes a vector; individual strings are interprated as "NULL" -> NULL; if "TRUE" or "FALSE" --> TRUE/FALSE; if single number --> as.numeric; if single non-number --> as.character;
#'
#' @return placeholder
#'
#' @examples
#' ParseOption("1.2,3")
#' ParseOption("RNA-seq:1.2,3;TT-seq:2,4")
#' 
ParseOption = function(option_str) {
  if( is.null(option_str) ){
    NULL
  }else if(grepl(';', option_str)){
    option_list = strsplit(option_str,';')[[1]]
    option_list_names = lapply(option_list, function(op) if(grepl(':', op)){sub(':.*', '', op)}else{NULL})
    option_list = lapply(option_list, function(op) ParseOption(sub('.*:', '', op)))
    names(option_list) = option_list_names
    option_list
  }else if(grepl(',', option_str)){
    sapply(strsplit(option_str,',')[[1]], ParseOption, USE.NAMES = FALSE)
  }else if( is.na(option_str) | option_str == '' ){  #same as empty cell in excel sheet
    NULL
  }else if(option_str == 'TRUE' | option_str == 'T'){
    TRUE
  }else if(option_str == 'FALSE' | option_str == 'F'){
    FALSE
  }else if(option_str == 'NULL'){
    NULL
  }else if( option_str == 'NA' ){ #$ added 230519
    NA
  }else if( !is.na(suppressWarnings(as.numeric(option_str))) ){
    as.numeric(option_str)
  }else{
    option_str
  }
}


#' Deparse Option
#'
#' @description Internal function: 
#' Parse option into string
#'
#' @keywords internal
#'
#' @author MS
#'
#' @param option named list, vector or single element
#'
#' @return String representation of object, compatible with ParseOption
#'
#' @examples
#' DeparseOption(c(1.2,3))
#' DeparseOption(list('RNA-seq' = c(1.2,3), 'TT-seq' = c(2,4)))
#' DeparseOption(list('RNA-seq' = c(TRUE,FALSE), 'TT-seq' = c(TRUE,FALSE)))
#' 
DeparseOption = function(option) {
  if( length(option) > 1 ){
    if ( is.list(option) ) {
      elems = lapply(option, DeparseOption)
      paste(paste(names(elems), elems, sep=':'), collapse=';')
    } else {
      paste(sapply(option, DeparseOption), collapse=',')
    }
  } else if ( is.list(option)) { #@ 2024-08-16 -->
    elems = lapply(option, DeparseOption)
    paste(paste(names(elems), elems, sep=':'), collapse=';')
    #@ 2024-08-16 <--
  } else if( is.null(option) ) {
    "NULL"
  } else if( is.na(option) ) { #@ 2023-09-20 added this; don't know why it was needed all of a sudden - shouldn't interfere with other stuffs
    "NA"
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


#' Get Plot Options
#'
#' @description Internal function: 
#' Reads Global Options from Excel template
#'
#' @keywords internal
#'
#' @author MS
#'
#' @param options Dataframe based on GLOBAL_OPTIONS sheet in Excel template
#' 
#' @details options is a data.frame with columns named *Option* and *Value*,
#'   both case-sensitive. Several entries are vectors of character, numeric or
#'   boolean. This function tries to parse this correctly.
#'   
#' @return Named list of values
#'
#' @examples
#' NULL
#' 
GetPlotOptions = function(options) {
  if(is.null(options)){
    cat(' ! no options found in Excel sheet, setting all to defaults')
    opts = DefaultPlotOptions()
  }else{
    options$Value = as.character(options$Value) #@ 2023-12-15
    opts = as.list(options$Value)
    names(opts) = options$Option
    opts = lapply(opts, ParseOption)
  }
  opts
}


#' Get Annotations
#'
#' @description Internal function: 
#' Reads Annotations (paths and options) from Excel template
#'
#' @keywords internal
#'
#' @author MS (minor additions by SLA)
#'
#' @param annotations Dataframe based on ANNOTATIONS sheet in Excel template
#' 
#' @details annotations is a data.frame with at least 2 columns *annotation_file* and *annotation_name*, both
#'   case-sensitive and the space is required.
#'
#' @return Named list of path, or named list of GRanges. Names used are entries in the *annotation_name* column.
#'
#' @examples
#' NULL
#' 
GetAnnotations = function(annotations) {
  annot=as.list(annotations$annotation_file)
  names(annot) = annotations$annotation_name
  
  default_options = DefaultAnnotationOptions()
  
  annot_plot_options = lapply(names(default_options), function(opt) {
    if ( opt %in% colnames(annotations) ) {
      if (is.list(annotations[[opt]])){
        l = annotations[[opt]]
      }else{ 
        l = sapply(annotations[[opt]], function(x) if(x=='TRUE' | x=='FALSE'){as.logical(x)}else{x})
      } 
    } else {
      l = rep(default_options[[opt]], nrow(annotations))
    }
    names(l) = annotations$annotation_name
    l
  })
  names(annot_plot_options) = names(default_options)
  
  list('annot' = annot,
       'annot_plot_options' = annot_plot_options)
  
}


#' Fill Df
#'
#' @description Internal function: 
#' Fill Empty Rows in individual columns in dataframe
#'
#' @keywords internal
#'
#' @author MS/SLA
#'
#' @param df Dataframe
#'
#' @details Empty rows in dataframe from loaded XL sheet are filled with values
#'   from columns above in hierarchical fashion such that the right-most columns
#'   are only filled if specified in the dataset defining row of the seqtype.
#'   See Excel template sheet in \code{inst/extdata/example_excel_template.xls}
#'   for more information.
#'
#' @return Dataframe
#'
#' @examples
#' NULL
#' 
FillDf = function(df) {
  filled_df = data.frame()
  datasets = df$dataset[!is.na(df$dataset)]
  dateset_start_rows = structure(which(!is.na(df$dataset)), names=datasets)
  if (length(datasets) == 1){
    dateset_end_rows = structure(nrow(df), names=datasets)  
  }else{
    dateset_end_rows = structure(c(dateset_start_rows-1, nrow(df))[2:(length(datasets)+1)], names=datasets) 
  }
  # Cross-dataset bigwig_directory inheritance was removed in v2.0.0 to support
  # per-row directories. Each dataset must now specify its own bigwig_directory
  # (missing values still fill *within* a dataset via the loop below).
  for (dataset in datasets){
    sub_df = df[dateset_start_rows[dataset]:dateset_end_rows[dataset], , drop=FALSE]
    allowed_cols = intersect(colnames(sub_df)[which(!is.na(sub_df[1,]))], c('color', 'bigwig_directory', 'dataset', grep('subgroup_', colnames(df), value=TRUE)))
    for ( i in 2:nrow(sub_df) ) {
      for (col in allowed_cols){
        if ( IsEmpty(sub_df[[col]][i]) & !IsEmpty(sub_df[[col]][i-1]) ){
          sub_df[[col]][i] = sub_df[[col]][i-1]
        }
      }
    }
    filled_df = rbind(filled_df, sub_df)
  }
  # Normalise bigwig_directory: append '/' when the user's Excel entry
  # doesn't already end with one. Covers both local paths and URLs. Empty
  # / NA cells are left untouched. Restored from v1.x's seqNdisplay()-level
  # normalisation so downstream `paste0(dir, filename)` produces a valid
  # path regardless of whether the user typed a trailing slash.
  if ('bigwig_directory' %in% colnames(filled_df)) {
    .dirs <- filled_df$bigwig_directory
    .needs_slash <- !is.na(.dirs) & nzchar(.dirs) & !grepl('/$', .dirs)
    filled_df$bigwig_directory[.needs_slash] <- paste0(.dirs[.needs_slash], '/')
  }
  filled_df
}


#' Empty Df
#'
#' @description Internal function: 
#' Empty Unnecessary Cells in Sample DataFrame
#'
#' @keywords internal
#'
#' @author MS
#'
#' @param df a dataframe
#'
#' @details Empty rows in data frame from a session data frame such that cells
#'   which have exactly same entry in cell above are left empty. This only
#'   applies to columns named 'bigwig_directory', 'dataset' and all columns with
#'   prefix 'subgroup_'. Output should look similar to the Samples sheet in the
#'   Excel template in \code{inst/extdata/example_excel_template.xls}.
#'   
#' @return trimmed dataframe
#'
#' @examples
#' NULL
#' 
EmptyDf = function(df) {
  emptied_df = data.frame()
  cols = c('bigwig_directory', 'dataset', colnames(df)[grepl('^subgroup_', colnames(df))])
  if ( nrow(df) > 1 ) {
    datasets_rle = Rle(df$dataset[!is.na(df$dataset)])
    datasets = runValue(datasets_rle)
    dataset_start_rows = structure(cumsum((c(1, runLength(datasets_rle))))[1:length(datasets)], names=datasets)
    if (length(datasets) == 1){
      dataset_end_rows = structure(nrow(df), names=datasets)  
    }else{
      dataset_end_rows = structure(c(dataset_start_rows-1, nrow(df))[2:(length(datasets)+1)], names=datasets) 
    }
    for (dataset in datasets){
      sub_df = df[dataset_start_rows[dataset]:dataset_end_rows[dataset], , drop=FALSE]
      empty_sub_df = sub_df
      ## Singleton datasets (1 row) have nothing to blank out --  `2:nrow(sub_df)`
      ## with nrow==1 would erroneously become c(2,1) and index out of bounds.
      if ( nrow(sub_df) >= 2L ) {
        for ( i in 2:nrow(sub_df) ) {
          for ( col in cols ) {
            if ( is.na(sub_df[[col]][[i]]) ) {
              empty_sub_df[[col]][[i]] = ''
            } else if ( !is.na(sub_df[[col]][[i - 1]]) & empty_sub_df[[col]][[i]] == sub_df[[col]][[i-1]] ) {
              empty_sub_df[[col]][[i]] = ''
            }
          }
        }
      }
      emptied_df = rbind(emptied_df, empty_sub_df)
    }
  }else{
    emptied_df = df
  }
  emptied_df
}


#' Get Samples
#'
#' @description Internal function: 
#' Get samples from a data frame containing at a minimum columns dataset
#'
#' @keywords internal
#' From chatGPT: the GetSamples function extracts and organizes sample information from the filled_df data frame based on specific column(s) and returns a character vector or a nested list containing the unique samples associated with the corresponding values in the data.
#'
#' @author MS
#'
#' @param filled_df a filled dataframe (see details)
#' 
#' @details Get samples from a data frame containing at a minimum columns
#'   dataset. Columns on the right to dataset may contain subcategories,
#'   called subgroup_1, subgroup_2 etc.
#'   
#' @return Named lists or nested lists of named lists
#'
#' @examples
#' df = data.frame(dataset=c(rep('a',4), rep('b',2)), subgroup_1=c('x','x','y','y', 'x','y'), stringsAsFactors=FALSE)
#' GetSamples(df)
#' df = data.frame(dataset=c(rep('a',4), rep('b',2)), subgroup_1=c('x','x','y','y', 'x','y'), subgroup_2=c('a', 'b', 'a', 'b', NA, NA), stringsAsFactors=FALSE)
#' GetSamples(df)
#' 
GetSamples = function(filled_df){
  start_col = which(colnames(filled_df) == 'dataset')
  sample_split = function(df, split_col) {
    if( split_col >= ncol(df) ) {
      return( as.character(unique(df[[ncol(df)]])) )
    } else if ( AllEmpty(df[[split_col+1]]) ) { 
      return ( as.character(unique(df[[split_col]])) )
    } else {
      return ( lapply(OrderedSplit(df, df[[split_col]], drop=TRUE), function(dfi) sample_split(dfi, split_col+1)) )
    }
  }
  sample_split(filled_df, start_col)
}


#' Get Colors
#'
#' @description Internal function: 
#' Get colors from a data frame containing at a minimum columns color and dataset
#'
#' @keywords internal
#' From chatGPT: the GetColors function extracts and organizes color information from the filled_df data frame based on specific column(s) and returns a vector or a nested list containing the colors associated with the corresponding values in the data.
#'
#' @author MS
#'
#' @param filled_df a filled dataframe (see details)
#' 
#' @details Get colors from a data frame containing at a minimum columns
#'   color and dataset. Columns on the right to dataset may contain subcategories,
#'   called subgroup_1, subgroup_2 etc. UPS: dataset and subgroup_s must be the right-most columns, see example below.
#'
#' @return Named lists or nested lists of named lists
#'

#'
#' @examples
#' df = data.frame(color=c(rep('red', 4), rep('green', 2)),dataset=c(rep('a',4), rep('b',2)), subgroup_1=c('x','x','y','y', 'x','y'), subgroup_2=1:6, stringsAsFactors=FALSE)
#' GetColors(df)
#' 
GetColors = function(filled_df){
  start_col = which(colnames(filled_df) == 'dataset')
  color_split = function(df, split_col) {
    if( split_col >= ncol(df) ) {
      split_col_name = colnames(df)[ncol(df)]
      df_dist = unique(df[, c(split_col_name, 'color'), drop = FALSE])
      colors = df_dist$color
      names(colors) = df_dist[[split_col_name]]
      return (colors)
    } else if ( AllEmpty(df[[split_col+1]]) ) { 
      split_col_name = colnames(df)[split_col]
      df_dist = unique(df[, c(split_col_name, 'color'), drop = FALSE])
      colors = df_dist$color
      names(colors) = df_dist[[split_col_name]]
      return (colors)
    } else {
      return ( lapply(OrderedSplit(df, df[[split_col]], drop=TRUE), function(dfi) color_split(dfi, split_col+1)) )
    }
  }
  color_split(filled_df, start_col)
}


#' Get Bigwig Dirs
#'
#' @description Internal function: 
#' Get bigwig dirs file names from a data frame containing at a minimum columns bigwig_file and dataset
#' From chatGPT: the GetBigwigDirs function extracts the unique combinations of bigwig directories and datasets from the filled_df data frame, filters out rows with missing bigwig directories, and returns a named vector where each dataset is associated with its corresponding bigwig directory.
#'
#' @keywords internal
#'
#' @author MS
#'
#' @param filled_df a filled dataframe (see details)
#'
#' @details Get bigwig directories from a data frame containing at a minimum columns
#'   bigwig_directory, bigwig_file, strand and dataset. Returns a nested list mirroring
#'   the structure of \code{GetBigwigs()}, with a character vector of directories at
#'   each leaf running parallel to the filename vector. Directories may vary per
#'   replicate (one directory per row of the sample sheet).
#'
#' @return Named list of named lists with two top-level entries '+' and '-'
#'
#' @examples
#' df = data.frame(bigwig_directory=c(rep('http://seqA/', 4), rep('http://seqB/', 2)),
#'   bigwig_file=c('a.bw','b.bw','c.bw','d.bw','e.bw','f.bw'),
#'   strand = rep('plus', 6),
#'   dataset=c(rep('a',4), rep('b',2)),
#'   subgroup_1=c('x','x','y','y', 'x','y'),
#'   subgroup_2=1:6, stringsAsFactors=FALSE)
#' GetBigwigDirs(df)
#'
GetBigwigDirs = function(filled_df){
  split_col = which(colnames(filled_df) == 'dataset')
  bd_split = function(df, split_col) {
    if ( split_col >= ncol(df) ) {
      return( lapply( OrderedSplit(df, df[[ncol(df)]], drop=TRUE), function(df) df$bigwig_directory) )
    } else if ( AllEmpty(df[[split_col+1]]) ) {
      return( lapply( OrderedSplit(df, df[[split_col]], drop=TRUE), function(df) df$bigwig_directory) )
    } else {
      return( lapply(OrderedSplit(df, df[[split_col]], drop=TRUE), function(dfi) bd_split(dfi, split_col+1)) )
    }
  }
  bd_plus = bd_split(filled_df[filled_df$strand == 'plus' | is.na(filled_df$strand) | filled_df$strand == '',], split_col)
  if ( any(grepl('minus', filled_df$strand)) ) {
    bd_minus = bd_split(filled_df[filled_df$strand == 'minus' | is.na(filled_df$strand) | filled_df$strand == '',], split_col)
  } else {
    bd_minus = NULL
  }
  list('+' = bd_plus,
       '-' = bd_minus)
}


#' Legacy Bigwig Dirs To Nested
#'
#' @description Internal function:
#' Convert a legacy flat \code{bigwig_dirs} (named character vector, one entry per
#' dataset) into the new nested form mirroring \code{bigwigs}. Each leaf filename
#' vector is replaced by a character vector of the same length filled with the
#' dataset's directory.
#'
#' @keywords internal
#'
#' @author SLA
#'
#' @param bigwig_dirs a named character vector (legacy form)
#' @param bigwigs the nested bigwigs list from the session (used as the shape template)
#'
#' @return a nested list with the same shape as \code{bigwigs}
#'
LegacyBigwigDirsToNested = function(bigwig_dirs, bigwigs){
  broadcast_branch = function(x, dir_value){
    if (is.list(x)){
      lapply(x, broadcast_branch, dir_value = dir_value)
    } else {
      rep(dir_value, length(x))
    }
  }
  out = list('+' = NULL, '-' = NULL)
  for (strand in c('+', '-')){
    if (is.null(bigwigs[[strand]])) next
    out[[strand]] = lapply(names(bigwigs[[strand]]), function(dataset){
      broadcast_branch(bigwigs[[strand]][[dataset]], bigwig_dirs[[dataset]])
    })
    names(out[[strand]]) = names(bigwigs[[strand]])
  }
  out
}


#' Get Bigwigs
#'
#' @description Internal function: 
#' Get bigwig file names from a data frame containing at a minimum columns bigwig_file, strand and dataset
#' From chatGPT: the GetBigwigs function processes the filled_df data frame to extract and organize information based on the values in specific columns, with different extraction strategies depending on the conditions in the data. The extracted results are returned as a list with separate entries for '+' and '-'.
#'
#' @keywords internal
#'
#' @author MS
#'
#' @param filled_df a filled dataframe (see details)
#' 
#' @details Get bigwig file names from a data frame containing at a minimum columns
#'   bigwig_file, strand and dataset. Strand must be 'plus' or 'minus', use 'plus' for unstranded data. Columns on the right to dataset may contain subcategories,
#'   called subgroup_1, subgroup_2 etc. UPS: dataset and subgroup_s must be the right-most columns, see example below.
#'
#' @return Named lists or nested lists of named lists
#'
#' @examples
#' df = data.frame(bigwig_file=c('a.bw', 'b.bw', 'c.bw', 'd.bw', 'e.bw', 'f.bw'),
#'   strand = rep('plus', 6), dataset=c(rep('a',4), rep('b',2)),
#'   subgroup_1=c('x','x','y','y', 'x','y'),
#'   stringsAsFactors=FALSE)
#' GetBigwigs(df)
#'
#' df = data.frame(
#'   bigwig_file=c('a_plus.bw', 'a_minus.bw', 'b_plus.bw', 'b_minus.bw', 'c_plus.bw', 'c_minus.bw'),
#'   strand = rep(c('plus', 'minus'), 3),
#'   dataset=c(rep('a',4), rep('b',2)),
#'   subgroup_1=c('x','x','y','y', 'x','y'),
#'   stringsAsFactors=FALSE)
#' GetBigwigs(df)
#' 
GetBigwigs = function(filled_df) {
  split_col = which(colnames(filled_df) == 'dataset')
  bw_split = function(df, split_col) {
    if( split_col >= ncol(df) ) {
      return ( lapply( OrderedSplit(df, df[[ncol(df)]], drop=TRUE), function(df) df$bigwig_file) )
    } else if ( AllEmpty(df[[split_col+1]]) ) { #| length(unique(df[[split_col+1]])) == 1
      return ( lapply( OrderedSplit(df, df[[split_col]], drop=TRUE), function(df) df$bigwig_file) )
    } else {
      return ( lapply(OrderedSplit(df, df[[split_col]], drop=TRUE), function(dfi) bw_split(dfi, split_col+1)) )
    }
  }
  bw_plus = bw_split(filled_df[filled_df$strand == 'plus' | is.na(filled_df$strand) | filled_df$strand == '',], split_col)
  if( any(grepl('minus', filled_df$strand)) ) {
    bw_minus = bw_split(filled_df[filled_df$strand == 'minus' | is.na(filled_df$strand) | filled_df$strand == '',], split_col)
  }else{
    bw_minus = NULL
  }
  list('+' = bw_plus,
       '-' = bw_minus)
}


#' Get Parameters
#'
#' @description Internal function: 
#' Reads parameters from Excel template 
#'
#' @keywords internal
#'
#' @author MS
#'
#' @param samples_df Dataframe based on SAMPLES sheet in Excel template (see details)
#' @param params_df Dataframe based on DATASET_OPTIONS sheet in Excel template
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
#'   parameters after this (see examples below in the man page).
#'
#' @return Named list
#'

#'
#' @examples
#' samples_df = data.frame(bigwig_file=c('a.bw', 'b.bw', 'c.bw', 'd.bw', 'e.bw', 'f.bw'),
#'   strand = rep('plus', 6),
#'   batch = rep(NA, 6),
#'   dataset=c(rep('a',4), rep('b',2)),
#'   subgroup_1=c('x','x','y','y', 'x','y'), stringsAsFactors=FALSE)
#' params_df = data.frame(dataset=c('a','b'), calcMean=c('TRUE','FALSE'), log2Transform=c('TRUE','FALSE'), stringsAsFactors=FALSE)
#' GetParameters(samples_df, params_df)
#'
#' df = data.frame(bigwig_file=c('a.bw', 'b.bw', 'c.bw', 'd.bw', 'e.bw', 'f.bw'),
#'   strand = rep('plus', 6),
#'   batch = rep(c('rep1','rep2'), 3),
#'   dataset=c(rep('a',4), rep('b',2)),
#'   subgroup_1=c('x','x','y','y', 'x','y'), stringsAsFactors=FALSE)
#' params = GetParameters(df, params_df)
#' params
#'
#' #change specific arguments afterwards
#' params$a$calcMean = FALSE
#' params$b$log2Transform = TRUE
#' 
GetParameters = function(samples_df, params_df){
  default_params = DefaultParameters()
  param_names = names(default_params)
  
  #fill in defaults if there are missing columns in params_df
  for ( param_name in param_names[!(param_names %in% colnames(params_df))] ) {
    if ( !is.null(default_params[[param_name]]) ) {
      params_df[[param_name]] = default_params[[param_name]]
    } else {
      params_df[[param_name]] = ''
    }
  }
  
  #clean params_df to only contain useful columns for parameters_list
  params_df = params_df[,c('dataset', param_names)]
  
  #create params list used by seqNdisplayR except for batch
  params = lapply(split(params_df, params_df$dataset), function(xl) lapply(as.list(xl[,colnames(xl)!='dataset']), ParseOption))
  
  # fix special case whichSamples
  dataset_names = names(params)
  params = lapply(dataset_names, function(dataset) {
    para = params[[dataset]]
    whichSamples = params_df$whichSamples[params_df$dataset == dataset][[1]]
    if ( is.null(whichSamples) | whichSamples == 'NULL' | whichSamples == '') {
      #include all
      para['whichSamples'] = list(NULL)
    } else if ( is.na(whichSamples) | whichSamples == 'NA' ) {
      #exclude all
      para$whichSamples = NA
    } else {
      #include specific ones
      para$whichSamples = Empty2Null(eval(parse(text = whichSamples)))
    }
    para
  })
  names(params) = dataset_names
  
  # add batch info from filled_df
  df_plus = samples_df[samples_df$strand != 'minus' | IsEmpty(samples_df$strand),]
  parameter_split = split(df_plus, df_plus$dataset)
  if ('batch' %in% colnames(df_plus)){
    batches = lapply(parameter_split, function(x) x$batch)
    for ( dataset in names(params) ) {
      batch = batches[[dataset]]
      if ( sum(!is.na(batch)) > 0 & !all(batch == batch[1]) ) {
        params[[dataset]]$batch =  batch
      }
    }
  }
  
  params
}


#' Glimpse Session
#'
#' @description Internal function: 
#' Prints an overview over samples, colors and associated bigwigs in a seqNdisplayR Session.
#'
#' @keywords internal
#'
#' @author MS
#'
#' @param samples samples object as used by seqNdisplayR
#' @param colors colors object as used by seqNdisplayR
#' @param bigwigs bigwigs object as used by seqNdisplayR
#' @param levels used only for internal recursion, don't change default=1.
#' @param indent_size string used for indent spacing of levels in the output
#'
#' @details Convenience function for checking parsing of samples, colors and bigwigs.
#'
#' @return Print to R session
#'
#' @examples
#' NULL
#' 
GlimpseSession = function(samples, colors, bigwigs, level=0, indent_size='   ') {
  if ( is.list(samples) ) {
    for ( .sample in names(samples) ) {
      cat(rep(indent_size, level), .sample, '\n')
      GlimpseSession(samples[[.sample]],
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


#' Open Options Table
#'
#' @description Open the Options table (Excel sheet with info for app)
#'
#' @keywords internal
#'
#' @author SLA
#'
#' @return placeholder
#' 
#' @importFrom readxl read_excel
#' 
#' @examples
#' NULL
#' 
OpenOptionsTable = function(){
  libpaths = .libPaths()
  for (libpath in libpaths){
    lf = list.files(libpath)
    if (any(grepl('seqNdisplayR', lf))){
      options_table = paste0(libpath, '/seqNdisplayR/extdata/variable_defaults_and_help.xlsx')
    }
  }
  readxl::read_excel(options_table, sheet='Shiny_Args')
}


#' Check Sample File
#'
#' @description Diagnostic function that validates an sNdR sample Excel file.
#' Checks sheet structure, column names, value validity, cross-sheet consistency,
#' and optionally tests whether referenced BigWig and annotation files are reachable.
#'
#' @author SLA
#'
#' @param xl_fname path to the sNdR sample Excel file (.xls or .xlsx)
#' @param check_files logical; if TRUE (default), test whether BigWig and
#'   annotation files are reachable (local paths checked with \code{file.exists},
#'   URLs with a small read via \code{url()}). Set to FALSE to skip these
#'   potentially slow checks.
#'
#' @return Invisibly returns a list with:
#'   \describe{
#'     \item{pass}{logical, TRUE if no FAILs were found}
#'     \item{n_pass}{integer count of passed checks}
#'     \item{n_warn}{integer count of warnings}
#'     \item{n_fail}{integer count of failed checks}
#'   }
#'
#' @importFrom readxl read_excel
#'
#' @export
#'
#' @examples
#' xl_fname = system.file('extdata', 'sNdR_sample_example_simple.xlsx', package='seqNdisplayR')
#' result = CheckSampleFile(xl_fname)
#' result = CheckSampleFile(xl_fname, check_files = FALSE)
#'

CheckSampleFile = function(xl_fname, check_files = TRUE) {

  # ---- counters and helpers ----
  n_pass <- 0L
  n_warn <- 0L
  n_fail <- 0L

  PASS <- function(msg) { n_pass <<- n_pass + 1L; cat('  PASS  ', msg, '\n') }
  WARN <- function(msg) { n_warn <<- n_warn + 1L; cat('  WARN  ', msg, '\n') }
  FAIL <- function(msg) { n_fail <<- n_fail + 1L; cat('  FAIL  ', msg, '\n') }
  INFO <- function(msg) { cat('  INFO  ', msg, '\n') }

  .make_result <- function() {
    cat('=====================================\n')
    cat('Summary:', n_pass, 'PASS,', n_warn, 'WARN,', n_fail, 'FAIL\n')
    invisible(list(pass = n_fail == 0L, n_pass = n_pass, n_warn = n_warn, n_fail = n_fail))
  }

  .is_url <- function(path) {
    grepl("^(https?|ftp)://", path, ignore.case = TRUE)
  }

  .check_reachable <- function(path) {
    if (.is_url(path)) {
      old_timeout <- getOption('timeout')
      options(timeout = 10)
      on.exit(options(timeout = old_timeout), add = TRUE)
      tryCatch({
        con <- url(path, open = 'rb')
        on.exit(try(close(con), silent = TRUE), add = TRUE)
        readBin(con, what = 'raw', n = 1L)
        TRUE
      }, error = function(e) FALSE,
         warning = function(w) FALSE)
    } else {
      file.exists(path)
    }
  }

  # Build the "not reachable" message. For URLs, suggest the embedded-
  # credentials form that the user's server expects when the resource is
  # password-protected.
  .unreachable_msg <- function(path) {
    if (.is_url(path)) {
      paste0(path,
             ' - not reachable. Check the URL, or, if the resource is ',
             'password-protected, embed the credentials in the URL ',
             '(e.g. https://user:password@host/path/to/file).')
    } else {
      paste0(path, ' - not found')
    }
  }

  cat('Checking sNdR sample file:', basename(xl_fname), '\n')
  cat('=====================================\n')

  # ---- [1] FILE ----
  cat('\n[1] FILE\n')
  if (!file.exists(xl_fname)) {
    FAIL('File does not exist')
    return(.make_result())
  }
  PASS('File exists')

  is_xml   <- grepl('\\.xml$',  xl_fname, ignore.case = TRUE)
  is_excel <- grepl('\\.xlsx?$|\\.xlsm$', xl_fname, ignore.case = TRUE)
  if (!is_xml & !is_excel) {
    FAIL('File does not have .xls, .xlsx, .xlsm, or .xml extension')
    return(.make_result())
  }
  PASS(paste0('File extension is .', sub('.*\\.', '', xl_fname)))

  ## ====================================================================
  ## XML (IGV session) branch --  different shape than Excel sample files
  ## ====================================================================
  if (is_xml) {
    # ---- [2] XML STRUCTURE ----
    cat('\n[2] XML STRUCTURE\n')
    doc <- tryCatch(xml2::read_xml(xl_fname),
                    error = function(e) {
                      FAIL(paste0('Failed to parse XML: ', conditionMessage(e)))
                      NULL
                    })
    if (is.null(doc)) return(.make_result())
    PASS('XML parses cleanly')

    resources <- xml2::xml_find_all(doc, '//Resource')
    tracks    <- xml2::xml_find_all(doc, '//Track')
    if (length(resources) == 0L) {
      FAIL('No <Resource> elements found')
    } else {
      PASS(paste0(length(resources), ' resource(s) found'))
    }
    if (length(tracks) == 0L) {
      FAIL('No <Track> elements found')
    } else {
      PASS(paste0(length(tracks), ' track element(s) found'))
    }

    # classify resources
    res_paths   <- xml2::xml_attr(resources, 'path')
    bw_paths    <- res_paths[grepl('\\.(bw|bigWig|bigwig)$', res_paths, ignore.case = TRUE)]
    bed_paths   <- res_paths[grepl('\\.(bed|bed\\.gz)$',     res_paths, ignore.case = TRUE)]
    other_paths <- setdiff(res_paths, c(bw_paths, bed_paths))
    INFO(paste0(length(bw_paths), ' bigWig + ', length(bed_paths), ' annotation + ',
                length(other_paths), ' other resource(s)'))

    # warn about local-only annotation paths (won't be reachable on other machines)
    local_bed <- bed_paths[!grepl('^(https?|ftp)://', bed_paths, ignore.case = TRUE)]
    if (length(local_bed) > 0L) {
      WARN(paste0(length(local_bed),
                  ' annotation path(s) reference local files (only reachable on the originating machine):'))
      for (lb in local_bed) WARN(paste0('  ', lb))
    }

    # ---- [3] FILE REACHABILITY ----
    cat('\n[3] FILE REACHABILITY\n')
    if (check_files) {
      to_check <- unique(c(bw_paths, bed_paths))
      INFO(paste0(length(to_check), ' unique file(s) to check'))
      for (p in to_check) {
        if (.check_reachable(p)) {
          PASS(p)
        } else {
          FAIL(.unreachable_msg(p))
        }
      }
    } else {
      INFO('Skipped (check_files = FALSE)')
    }

    # ---- [4] GROUPING PREVIEW ----
    cat('\n[4] GROUPING PREVIEW (default group_by = autoscalegroups)\n')
    sess <- tryCatch(suppressMessages(IGV2Session(xl_fname, load_annotations = FALSE)),
                     error = function(e) {
                       WARN(paste0('IGV2Session preview failed: ', conditionMessage(e)))
                       NULL
                     })
    if (!is.null(sess)) {
      ds_names <- names(sess$samples)
      INFO(paste0(length(ds_names), ' dataset(s) inferred:'))
      for (ds in ds_names) {
        n_leaves <- if (is.character(sess$samples[[ds]])) {
          length(sess$samples[[ds]])
        } else {
          length(unlist(sess$samples[[ds]]))
        }
        INFO(paste0('  ', ds, ' (', n_leaves, ' subgroup(s))'))
      }
      if (!is.null(sess$annotation_files)) {
        INFO(paste0(length(sess$annotation_files), ' annotation(s) recognised: ',
                    paste(names(sess$annotation_files), collapse = ', ')))
      }
    }

    return(.make_result())
  }

  ## ====================================================================
  ## Excel branch --  original sample-sheet validation continues below
  ## ====================================================================

  # ---- [2] SAMPLES ----
  cat('\n[2] SAMPLES\n')
  samples_df <- NULL
  tryCatch(
    { noout <- capture.output(samples_df <- readxl::read_excel(xl_fname, sheet = 'SAMPLES')) },
    error = function(cond) {}
  )
  if (is.null(samples_df)) {
    FAIL('Required sheet "SAMPLES" not found or unreadable')
    return(.make_result())
  }
  PASS('Sheet found')

  # clean all-NA rows/columns
  if (nrow(samples_df) > 1) {
    samples_df <- samples_df[!apply(apply(samples_df, 2, is.na), 1, all), , drop = FALSE]
    samples_df <- samples_df[, !apply(apply(samples_df, 2, is.na), 2, all), drop = FALSE]
  }

  mandatory_columns <- c('bigwig_directory', 'bigwig_file', 'strand', 'dataset', 'subgroup_1')
  missing_columns <- setdiff(mandatory_columns, colnames(samples_df))
  if (length(missing_columns) > 0) {
    FAIL(paste0('Missing mandatory column(s): ', paste(missing_columns, collapse = ', ')))
    return(.make_result())
  }
  PASS('All 5 mandatory columns present')

  top_row_NAs <- is.na(samples_df[1, mandatory_columns])
  if (any(top_row_NAs)) {
    FAIL(paste0('First row NA in mandatory column(s): ', paste(names(top_row_NAs)[top_row_NAs], collapse = ', ')))
  } else {
    PASS('First row of mandatory columns is complete')
  }

  # strand values
  strand_vals <- samples_df$strand[!is.na(samples_df$strand)]
  bad_strands <- setdiff(strand_vals, c('plus', 'minus'))
  if (length(bad_strands) > 0) {
    FAIL(paste0('Invalid strand value(s): ', paste(bad_strands, collapse = ', '), ' - must be "plus" or "minus"'))
  } else {
    PASS('All strand values valid ("plus"/"minus")')
  }

  # color column
  if ('color' %in% colnames(samples_df)) {
    color_vals <- samples_df$color[!is.na(samples_df$color)]
    if (length(color_vals) > 0) {
      bad_colors <- color_vals[!IsColor(color_vals)]
      if (length(bad_colors) > 0) {
        FAIL(paste0('Invalid color value(s): ', paste(unique(bad_colors), collapse = ', ')))
      } else {
        PASS('All color values valid')
      }
    }
  } else {
    WARN('No "color" column - default "#346C88" will be used')
  }

  # extra columns
  known_cols <- c('bigwig_directory', 'bigwig_file', 'strand', 'dataset', 'subgroup_1', 'color', 'batch')
  extra_cols <- setdiff(colnames(samples_df), known_cols)
  if (length(extra_cols) > 0) {
    non_subgroup <- extra_cols[!grepl('^subgroup_\\d+$', extra_cols)]
    if (length(non_subgroup) > 0) {
      WARN(paste0('Unrecognized column(s) will be ignored: ', paste(non_subgroup, collapse = ', ')))
    }
  }

  # apply fill-down once; reused for mixed-dirs INFO and for reachability checks
  filled_df <- tryCatch(FillDf(samples_df), error = function(e) NULL)

  # mixed-directory datasets (v2.0.0 supports this; flag informationally)
  if (!is.null(filled_df) && 'bigwig_directory' %in% colnames(filled_df) && 'dataset' %in% colnames(filled_df)) {
    dirs_per_dataset <- tapply(filled_df$bigwig_directory, filled_df$dataset,
                               function(x) length(unique(x[!is.na(x)])))
    mixed <- names(dirs_per_dataset)[dirs_per_dataset > 1]
    if (length(mixed) > 0) {
      INFO(paste0('Dataset(s) with multiple bigwig_directory values: ', paste(mixed, collapse = ', ')))
    }
  }

  # Scan SAMPLES for <FILL_ME:...> sentinels left over from a partial IGV
  # import. One WARN per cell so the user knows exactly which to fix.
  .scan_sentinels(samples_df, 'SAMPLES', WARN, PASS)

  # ---- [3] BIGWIG FILES ----
  cat('\n[3] BIGWIG FILES\n')
  if (check_files) {
    if (is.null(filled_df)) {
      FAIL('Could not apply fill-down to resolve paths')
    } else {
      bw_paths <- paste0(filled_df$bigwig_directory, filled_df$bigwig_file)
      bw_paths_unique <- unique(bw_paths)
      INFO(paste0(length(bw_paths_unique), ' unique file(s) to check'))
      for (bw in bw_paths_unique) {
        if (.check_reachable(bw)) {
          PASS(bw)
        } else {
          FAIL(.unreachable_msg(bw))
        }
      }
    }
  } else {
    INFO('Skipped (check_files = FALSE)')
  }

  # ---- [4] DATASET_OPTIONS ----
  cat('\n[4] DATASET_OPTIONS\n')
  params_df <- NULL
  tryCatch(
    { noout <- capture.output(params_df <- readxl::read_excel(xl_fname, sheet = 'DATASET_OPTIONS')) },
    error = function(cond) {}
  )
  if (is.null(params_df)) {
    INFO('Sheet not found - defaults will be used')
  } else {
    PASS('Sheet found')
    # clean all-NA rows/columns
    if (nrow(params_df) > 1) {
      params_df <- params_df[!apply(apply(params_df, 2, is.na), 1, all), , drop = FALSE]
      params_df <- params_df[, !apply(apply(params_df, 2, is.na), 2, all), drop = FALSE]
    }

    # Scan DATASET_OPTIONS for <FILL_ME:...> sentinels (the dataset column
    # mirrors SAMPLES so the same placeholders appear here).
    .scan_sentinels(params_df, 'DATASET_OPTIONS', WARN, PASS)

    if (!('dataset' %in% colnames(params_df))) {
      FAIL('"dataset" column missing in DATASET_OPTIONS')
    } else {
      sample_datasets <- unique(samples_df$dataset[!is.na(samples_df$dataset)])
      param_datasets  <- unique(params_df$dataset[!is.na(params_df$dataset)])
      in_samples_not_params <- setdiff(sample_datasets, param_datasets)
      in_params_not_samples <- setdiff(param_datasets, sample_datasets)
      if (length(in_samples_not_params) > 0) {
        WARN(paste0('Dataset(s) in SAMPLES but not DATASET_OPTIONS (defaults will be used): ', paste(in_samples_not_params, collapse = ', ')))
      }
      if (length(in_params_not_samples) > 0) {
        WARN(paste0('Dataset(s) in DATASET_OPTIONS but not SAMPLES (will be ignored): ', paste(in_params_not_samples, collapse = ', ')))
      }
      if (length(in_samples_not_params) == 0 & length(in_params_not_samples) == 0) {
        PASS('Dataset names match between SAMPLES and DATASET_OPTIONS')
      }
    }

    # validate known parameter values
    logical_params <- c('log2transform', 'batchCorrect', 'calcMean', 'negValsSet0',
                        'enhance_signals', 'negative_valued_bw', 'group_autoscale')
    for (lp in intersect(logical_params, colnames(params_df))) {
      vals <- as.character(params_df[[lp]])
      vals <- vals[!is.na(vals) & vals != '']
      bad <- vals[!(toupper(vals) %in% c('TRUE', 'FALSE', 'T', 'F'))]
      if (length(bad) > 0) {
        FAIL(paste0('"', lp, '" should be TRUE/FALSE, found: ', paste(unique(bad), collapse = ', ')))
      } else if (length(vals) > 0) {
        PASS(paste0('"', lp, '" values valid'))
      }
    }

    if ('bin_stats' %in% colnames(params_df)) {
      vals <- as.character(params_df$bin_stats)
      vals <- vals[!is.na(vals) & vals != '']
      bad <- vals[!(vals %in% c('mean', 'median', 'max'))]
      if (length(bad) > 0) {
        FAIL(paste0('"bin_stats" should be mean/median/max, found: ', paste(unique(bad), collapse = ', ')))
      } else if (length(vals) > 0) {
        PASS('"bin_stats" values valid')
      }
    }

    if ('pseudoCount' %in% colnames(params_df)) {
      vals <- as.character(params_df$pseudoCount)
      vals <- vals[!is.na(vals) & vals != '']
      bad <- vals[is.na(suppressWarnings(as.numeric(vals)))]
      if (length(bad) > 0) {
        FAIL(paste0('"pseudoCount" should be numeric, found: ', paste(unique(bad), collapse = ', ')))
      } else if (length(vals) > 0) {
        PASS('"pseudoCount" values valid')
      }
    }

    # unknown columns
    known_param_cols <- c('dataset', names(DefaultParameters()))
    unknown_cols <- setdiff(colnames(params_df), known_param_cols)
    if (length(unknown_cols) > 0) {
      WARN(paste0('Unrecognized column(s) in DATASET_OPTIONS: ', paste(unknown_cols, collapse = ', ')))
    }
  }

  # ---- [5] ANNOTATIONS ----
  cat('\n[5] ANNOTATIONS\n')
  anno_df <- NULL
  tryCatch(
    { noout <- capture.output(anno_df <- readxl::read_excel(xl_fname, sheet = 'ANNOTATIONS')) },
    error = function(cond) {}
  )
  if (is.null(anno_df)) {
    INFO('Sheet not found - no annotations')
  } else {
    PASS('Sheet found')

    anno_mandatory <- c('annotation_name', 'annotation_file')
    anno_missing <- setdiff(anno_mandatory, colnames(anno_df))
    if (length(anno_missing) > 0) {
      FAIL(paste0('Missing mandatory column(s): ', paste(anno_missing, collapse = ', ')))
    } else {
      PASS('Mandatory columns present ("annotation_name", "annotation_file")')
    }

    if ('annotation_packing' %in% colnames(anno_df)) {
      vals <- as.character(anno_df$annotation_packing)
      vals <- vals[!is.na(vals) & vals != '']
      valid_packing <- c('expanded', 'squished', 'collapsed', 'collapsed2')
      bad <- vals[!(vals %in% valid_packing)]
      if (length(bad) > 0) {
        FAIL(paste0('"annotation_packing" invalid value(s): ', paste(unique(bad), collapse = ', '),
                    ' - must be one of: ', paste(valid_packing, collapse = ', ')))
      } else if (length(vals) > 0) {
        PASS('"annotation_packing" values valid')
      }
    }

    if ('annot_cols' %in% colnames(anno_df)) {
      vals <- as.character(anno_df$annot_cols)
      vals <- vals[!is.na(vals) & vals != '' & toupper(vals) != 'NULL']
      if (length(vals) > 0) {
        bad <- vals[!IsColor(vals)]
        if (length(bad) > 0) {
          FAIL(paste0('Invalid annotation color(s): ', paste(unique(bad), collapse = ', ')))
        } else {
          PASS('"annot_cols" values valid')
        }
      }
    }

    # unknown columns
    known_anno_cols <- c('annotation_name', 'annotation_file', names(DefaultAnnotationOptions()))
    unknown_cols <- setdiff(colnames(anno_df), known_anno_cols)
    if (length(unknown_cols) > 0) {
      WARN(paste0('Unrecognized column(s) in ANNOTATIONS: ', paste(unknown_cols, collapse = ', ')))
    }
  }

  # ---- [6] ANNOTATION FILES ----
  cat('\n[6] ANNOTATION FILES\n')
  if (is.null(anno_df)) {
    INFO('No ANNOTATIONS sheet - skipped')
  } else if (!check_files) {
    INFO('Skipped (check_files = FALSE)')
  } else if (!('annotation_file' %in% colnames(anno_df))) {
    INFO('Skipped - "annotation_file" column missing')
  } else {
    anno_files <- as.character(anno_df$annotation_file)
    anno_files <- anno_files[!is.na(anno_files) & anno_files != '']
    anno_names <- as.character(anno_df$annotation_name)
    INFO(paste0(length(anno_files), ' file(s) to check'))
    for (i in seq_along(anno_files)) {
      label <- if (!is.na(anno_names[i])) paste0(anno_names[i], ' (', anno_files[i], ')') else anno_files[i]
      if (.check_reachable(anno_files[i])) {
        PASS(label)
      } else {
        FAIL(if (!is.na(anno_names[i]))
               paste0(anno_names[i], ' (', .unreachable_msg(anno_files[i]), ')')
             else
               .unreachable_msg(anno_files[i]))
      }
    }
  }

  # ---- [7] GLOBAL_OPTIONS ----
  cat('\n[7] GLOBAL_OPTIONS\n')
  options_df <- NULL
  tryCatch(
    { noout <- capture.output(options_df <- readxl::read_excel(xl_fname, sheet = 'GLOBAL_OPTIONS')) },
    error = function(cond) {}
  )
  if (is.null(options_df)) {
    INFO('Sheet not found - defaults will be used')
  } else {
    PASS('Sheet found')

    if (!all(c('Option', 'Value') %in% colnames(options_df))) {
      FAIL('Sheet must have columns "Option" and "Value"')
    } else {
      PASS('Columns "Option" and "Value" present')

      known_options <- names(DefaultPlotOptions())
      opt_names <- as.character(options_df$Option)
      unknown_opts <- opt_names[!(opt_names %in% known_options)]
      if (length(unknown_opts) > 0) {
        WARN(paste0('Unrecognized option(s): ', paste(unknown_opts, collapse = ', ')))
      }

      # try parsing each value
      opt_values <- as.character(options_df$Value)
      parse_failures <- c()
      for (i in seq_along(opt_values)) {
        tryCatch(
          { ParseOption(opt_values[i]) },
          error = function(e) { parse_failures <<- c(parse_failures, opt_names[i]) }
        )
      }
      if (length(parse_failures) > 0) {
        FAIL(paste0('Could not parse value(s) for: ', paste(parse_failures, collapse = ', ')))
      } else {
        PASS('All option values parseable')
      }
    }
  }

  # ---- Summary ----
  cat('\n')
  .make_result()
}

# ---- .scan_sentinels (helper for CheckSampleFile) ----

#' Scan a data.frame for <FILL_ME:...> sentinel cells (internal).
#'
#' @description Helper for \code{CheckSampleFile}. Emits one WARN per cell
#' matching the sentinel pattern, or one PASS if none are found. Uses the
#' WARN / PASS closures from the calling environment.
#'
#' @param df Data.frame to scan. NULL is treated as empty.
#' @param sheet_name Name of the source sheet (for the WARN message).
#' @param WARN,PASS Closures from the surrounding \code{CheckSampleFile} call.
#'
#' @keywords internal
.scan_sentinels <- function(df, sheet_name, WARN, PASS) {
  if (is.null(df) || nrow(df) == 0L) return(invisible(0L))
  pat <- "^<FILL_ME:"
  n_sent <- 0L
  for (col in colnames(df)) {
    vals <- as.character(df[[col]])
    hits <- which(!is.na(vals) & grepl(pat, vals))
    for (i in hits) {
      # +1 to account for header row when reporting the Excel row number
      WARN(paste0(sheet_name, " row ", i + 1L, ", column '", col,
                  "': placeholder '", vals[i], "' must be replaced"))
      n_sent <- n_sent + 1L
    }
  }
  if (n_sent == 0L) {
    PASS(paste0("No <FILL_ME:...> placeholders in ", sheet_name))
  }
  invisible(n_sent)
}



