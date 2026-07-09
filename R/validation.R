# Parameter validation functions


#' Parameter Display Name
#'
#' @description Internal function:
#' Returns the display name for a parameter based on the current interface.
#' Eliminates the repetitive ifelse(interface=='R', 'r_name', 'shiny_name') pattern.
#'
#' @keywords internal
#'
#' @param par_name the R parameter name (character)
#' @param interface 'R' or 'shiny'
#'
#' @return the display name string
#'
ParName = function(par_name, interface) {
  shiny_names <- c(
    track_width_cm          = 'Tracks Width',
    full_width_cm           = 'Full Plot Width',
    full_height_cm          = 'Full Plot Height',
    track_height_cm         = 'Tracks Height',
    genomic_scale_height_cm = 'Genomic Scale Height',
    annotation_height_cm    = 'Annotation Height',
    spacer_height_cm        = 'Spacer Height',
    panels_max_width_cm     = 'Panels Width',
    scale_panel_width_cm    = 'Tracks Scale Width',
    extra_space             = 'Expand Plotted Region',
    margin_width_cm         = 'Margins Width',
    panel_font_sizes        = 'Panel Text Font Size(s)',
    scale_font_size         = 'Genomic Scale Font',
    genomic_scale_font_size = 'Genomic Scale Font',
    feature_names_font_size = 'Feature Names Font Size',
    bgr_alpha               = 'Background  Opacity',
    feature_shading_alpha   = 'Shading Opacity',
    scaling_factor          = 'Scaling For On-Screen Display',
    annot_panel_font_size   = 'Annotation Title Font Size',
    header_font_sizes       = 'Header Font Size(s)',
    bin_start               = 'Bins Center',
    bin_size                = 'Bin Size',
    bins_per_cm             = 'Bins per cm',
    plotting_segment_order  = 'Plotting Segment Order',
    # ScrutinizeExpandAndNameParameter names
    incl_feature_names        = 'Display Feature Names',
    feature_names_above       = 'Feature Names Above Features',
    incl_feature_brackets     = 'Feature Bracket',
    incl_feature_shadings     = 'Highlight Individual Loci By Shaded Boxes',
    annotation_packing        = 'Annotation Packing',
    annot_cols                = 'Feature Colors',
    batchCorrect              = 'Batch Correction',
    log2transform             = 'log2-Transform Data',
    pseudoCount               = 'Pseudo Count',
    bin_stats                 = 'Binning Statistics',
    enhance_signals           = 'Enhance Signals',
    panel_separators          = 'Vertical and Horizontal Line-Separators',
    separators_lwds           = 'Line-Separator Weight(s)',
    separators_colors         = 'Line-Separator Color(s)',
    annot_panel_color         = 'Annotation Title Color',
    panel_text_colors         = 'Panel Text Colors',
    scale_font_color          = 'Data Scale Color',
    header_font_colors        = 'Header Color(s)',
    genomic_scale_font_color  = 'Genomic Scale Font Color',
    feature_names_font_color  = 'Feature Names Color',
    feature_shading_colors    = 'Shading Colors',
    bgr_colors                = 'Alternating Background Colors',
    intermingled_color        = 'Color Display of Data from Negative Strand',
    strands_alpha             = 'Forward/Reverse Track Opacities',
    title_field_height_cm     = 'Title field height',
    scientific_scale          = 'Scientific Notation on Data Scale(s)',
    group_autoscale           = 'Group Autoscale'
  )
  if (interface == 'R') par_name else {
    nm <- shiny_names[par_name]
    if (is.na(nm)) par_name else as.character(nm)
  }
}


#' Scrutinize Expand And Name Parameter
#'
#' @description Internal function: 
#' Helper function that will expand a common "one-dimensional" parameter to fit all the names of a named object
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param parameter 
#' @param name_vector 
#' @param use_names 
#' @param default_value 
#' @param expect_standard 
#' @param expect 
#' @param revert_to_default 
#' @param alt_par_name 
#' @param verbosity 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
ScrutinizeExpandAndNameParameter = function(parameter, name_vector, use_names=FALSE, default_value=NULL, expect_standard=NULL, expect=NULL, revert_to_default=FALSE, alt_par_name=NULL, verbosity){
  .messages = list('output'=list(), 'errors'=list(), 'warnings'=list())
  .var.name1 = deparse(substitute(parameter))
  if (use_names){
    .name.vector = names(name_vector)
  }else{
    .name.vector = name_vector
  }
  .var.name2 = deparse(substitute(name_vector))
  if (missing(.var.name2)){
    .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'no supplied "', .var.name2, '" argument - aborting')
  }else if (is.null(.name.vector)){
    .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"', .var.name2, '" argument is NULL - aborting')
  }
  if (missing(.var.name1)){
    .var.name1 = ifelse(is.null(alt_par_name), .var.name1, alt_par_name)
    if (is.null(default_value)){
      .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'no supplied "', .var.name1, '" argument with no default - aborting')
    }else{
      .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', 'no supplied "', .var.name1, '" argument - automatically setting to ', default_value)
      parameter = default_value
    }
  }else if (is.null(parameter)){
    .var.name1 = ifelse(is.null(alt_par_name), .var.name1, alt_par_name)
    if (length(default_value) > 1){
      .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument is NULL - automatically setting to ', paste(paste(default_value[2:length(default_value)-1], collapse=', '), '&', default_value[length(default_value)]))
    }else{
      .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument is NULL - automatically setting to ', default_value)
    }
    parameter = default_value
  }else{
    .var.name1 = ifelse(is.null(alt_par_name), .var.name1, alt_par_name)
  }
  if (length(.messages[['errors']])==0){
    if (!is.null(expect_standard)){
      if (expect_standard=='logical'){
        if (!all(is.logical(parameter)) | any(is.na(parameter))){
          if (revert_to_default & !is.null(default_value)){
            .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a logical vector - using default values',
                                                                                  '\n', '\t', '.) ', paste(default_value, collapse=','))
            parameter = default_value
          }else{
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a logical vector - aborting')
          }
        }
      }else if (expect_standard=='integer'){
        if (!all(is.integer(parameter)) | any(is.na(parameter))){ 
          if (revert_to_default & !is.null(default_value)){
            .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be an integer vector - using default values',
                                                                                  '\n', '\t', '.) ', paste(default_value, collapse=','))
            parameter = default_value
          }else{
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be an integer vector - aborting')
          }
        }
      }else if (expect_standard=='numeric'){
        if (!all(is.numeric(parameter)) | any(is.na(parameter))){ 
          if (revert_to_default & !is.null(default_value)){
            .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a numeric vector - using default values',
                                                                                  '\n', '\t', '.) ', paste(default_value, collapse=','))
            parameter = default_value
          }else{
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a numeric vector - aborting')
          }
        }
      }else if (expect_standard=='color'){
        if (!all(IsColor(parameter)) | any(is.na(parameter))){
          if (revert_to_default & !is.null(default_value)){
            .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a color vector - using default values',
                                                                                  '\n', '\t', '.) ', paste(default_value, collapse=','))
            parameter = default_value
          }else{
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a color vector - aborting')
          }
        }
      }else if (expect_standard=='character'){
        if (!all(is.character(parameter)) | any(is.na(parameter))){
          if (revert_to_default & !is.null(default_value)){
            .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a character vector - using default values',
                                                                                  '\n', '\t', '.) ', paste(default_value, collapse=','))
            parameter = default_value
          }else{
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a character vector - aborting')
          }
        }
      }else{
        .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"expect_standard" argument set to ', expect_standard, ' should be defined as either "logical", "character", "color", "integer" or "numeric" - aborting')
      }
    }else if (!is.null(expect)){
      if (!identical(setdiff(parameter, expect), numeric(0)) & !identical(setdiff(parameter, expect), character(0))){
        if (!is.null(default_value)){
          .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument contains unexpected values [',  paste(setdiff(parameter, expect), collapse=', '), '] - automatically setting to ', default_value,
                                                                                '\n', '\t', '.) allowed values are:',
                                                                                '\n', paste(paste('\t', '\t', expect), collapse='\n'))
          parameter = default_value
        }else{
          .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"', .var.name1, '" argument contains unexpected values [',  paste(setdiff(parameter, expect), collapse=', '), '] with no default replacement(s) - aborting',
                                                                            '\n', '\t', '.) allowed values are:',
                                                                            '\n', paste(paste('\t', '\t', expect), collapse='\n'))
        }
      }
    }else{
      .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'either "expect_standard" or "expect" arguments should be defined - aborting')
    }
  }
  if (length(.messages[['errors']])==0){
    if (length(parameter)==1 & length(parameter)!=length(.name.vector)){
      .parameter = structure(rep(parameter, length(.name.vector)), names=.name.vector)
    }else if (length(parameter)==length(.name.vector)){
      if (!is.null(names(parameter))){
        if (all(sort(names(parameter)) == sort(.name.vector))){
          .parameter = parameter[.name.vector]
        }else{
          .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'names of "', .var.name1, '" argument and "', .var.name2, '" are not in correspondance - aborting')
        }
      }else{
        .parameter = structure(parameter, names=.name.vector)
      }
    }else{
      if (!is.null(names(parameter))){
        if (identical(setdiff(.name.vector, names(parameter)), numeric(0))){
          .parameter = parameter[.name.vector]
        }else{
          .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'names of "', .var.name1, '" argument and "', .var.name2, '" are not in correspondance - aborting')
        }
      }else{
        .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'lengths of "', .var.name1, '" and "', .var.name2, '" arguments are not in correspondance - aborting')
      }
    }
  }
  PrintOutput(.messages, verbosity)
  if (length(.messages[['errors']])==0){
    return(.parameter)
  }else{
    return()
  }
}


#' Evaluate Numeric Value
#'
#' @description Internal function: 
#' Evaluate whether a vector/value is numeric and falls within recommmended range
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param num_val 
#' @param positive_val 
#' @param min_val 
#' @param max_val 
#' @param interval_obligatory 
#' @param turn_errors_to_warnings 
#' @param alt_par_name 
#' @param verbosity 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
EvaluateNumericValue = function(num_val, positive_val, min_val, max_val, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=NULL, verbosity){
  .messages = list('output'=list(), 'errors'=list(), 'warnings'=list())
  .var.name1 = deparse(substitute(num_val))
  .var.name1 = ifelse(is.null(alt_par_name), .var.name1, alt_par_name)
  if (is.numeric(num_val)){
    if (length(positive_val)==1){
      .positive.val = rep(positive_val, length(num_val))
    }else if (length(num_val) == length(positive_val)){
      .positive.val = positive_val
    }else{
      .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0(' - ', .var.name1, ' = ', paste(num_val, collapse=','), ';', '  there is a discrepancy between the assigned and expected values - ', ifelse(turn_errors_to_warnings, 'setting to default value', 'aborting'))
      PrintOutput(.messages, verbosity, no_line_change=turn_errors_to_warnings)
      return(FALSE)
    }
    if (length(min_val)==1){
      .min.val = rep(min_val, length(num_val))
    }else if (length(num_val) == length(min_val)){
      .min.val = min_val
    }else{
      .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0(' - ', .var.name1, ' = ', paste(num_val, collapse=','), ';', '  there is a discrepancy between the assigned and expected values - ', ifelse(turn_errors_to_warnings, 'setting to default value', 'aborting'))
      PrintOutput(.messages, verbosity, no_line_change=turn_errors_to_warnings)
      return(FALSE)
    }
    if (length(max_val)==1){
      .max.val = rep(max_val, length(num_val))
    }else if (length(num_val) == length(max_val)){
      .max.val = max_val
    }else{
      .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0(' - ', .var.name1, ' = ', paste(num_val, collapse=','), ';', '  there is a discrepancy between the assigned and expected values - ', ifelse(turn_errors_to_warnings, 'setting to default value', 'aborting'))
      PrintOutput(.messages, verbosity, no_line_change=turn_errors_to_warnings)
      return(FALSE)
    }
    if (identical(as.logical(num_val >= 0), .positive.val)){
      if (any(num_val < .min.val) | any(num_val > .max.val)){
        .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0(' - ', .var.name1, ' = ', paste(num_val, collapse=','), ':')
        for (i in 1:length(num_val)){
          if ((num_val < .min.val)[i] | (num_val > .max.val)[i]){
            .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0('\t', '.) ', .var.name1, '[', i, '] = ', num_val[i], ';', ' it should preferably be between ', .min.val[i], ' and ', .max.val[i])
          }
        }
        if (interval_obligatory){ 
          .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0('\t', '.) ', ifelse(turn_errors_to_warnings, 'setting to default value', 'aborting'))
          PrintOutput(.messages, verbosity)
          return(FALSE)
        } 
      }
      PrintOutput(.messages, verbosity)
      return(TRUE)
    }else{
      .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0(' - ', .var.name1, ' = ', paste(num_val, collapse=','), ' - ', ifelse(turn_errors_to_warnings, 'setting to default value', 'aborting'), ':'  )
      for (i in 1:length(num_val)){
        if ((num_val >=0)[i] != .positive.val[i]){
          .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0('\t', '.) ', .var.name1, '[', i, '] = ', num_val[i], ';', ' it should be a ', ifelse(.positive.val[i], 'positive', 'negative'), ' numeric value preferably between ', .min.val[i], ' and ', .max.val[i])
        }
      }
      PrintOutput(.messages, verbosity)
      return(FALSE)
    }
  }else{
    if (is.null(num_val)){
      num_val = 'NULL'
    }
    .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0(' - ', .var.name1, ' = ', paste(num_val, collapse=','), ';', ' it should be a numeric value/vector - ', ifelse(turn_errors_to_warnings, 'setting to default value', 'aborting'))
    PrintOutput(.messages, verbosity, no_line_change=turn_errors_to_warnings)
    return(FALSE)
  }
}


