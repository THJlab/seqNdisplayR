share <- list(title = "{seqNdisplayR} package",
              description = "Customizable and Reproducible Plotting of Sequencing Coverage Data")

options(shinyTree.setState = TRUE)
options(shinyTree.refresh  = TRUE)


##### functions to be added in app

#' Open Options Table
#'
#' @description Open the Options table (Excel sheet with info for app)
#'
#' @keywords internal
#'
#' @author SLA
#'
#' @return
#' 
#' @importFrom readxl read_excel
#' 
#' @examples
#' 
OpenOptionsTable = function(){
  libpaths = .libPaths()
  for (libpath in libpaths){
    lf = list.files(libpath)
    if (any(grepl('seqNdisplayR', lf))){
      options_table = paste0(libpath, '/seqNdisplayR/extdata/variable_defaults_and_help.xlsx') #@ '/seqNdisplayR/shiny/variable_defaults_and_help.xlsx'
    }
  }
  readxl::read_excel(options_table, sheet='Shiny_Args')
}

#@ -> find shiny folder and options table
#options_table = OpenOptionsTable()
#@ <- find shiny folder


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
#' @return
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

#' List Depth
#'
#' @description Internal function: 
#' Max number of nested levels in a nested list of lists
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param query_list 
#'
#' @return
#'
#' @examples
#' ListDepth(c('a','b','c'))
#' l = list('x'=c('a1','b1','c1'), 'y'=c('a2','b2','c2'))
#' ListDepth(l)
#' l = list('x0'=list('x1'=c('a1','b1','c1'), 'x2'=c('a2','b2','c2')), 'y'=c('a2','b2','c2'))
#' ListDepth(l)
#' 
ListDepth = function(query_list){
  ifelse(is.list(query_list), 1L + max(sapply(query_list, ListDepth)), 0L)
}

#' Is Empty 
#'
#' @description Internal function: 
#' Convenience function checks whether all NA or empty string
#'
#' @keywords internal
#' 
#' @author MS
#'
#' @param x vector
#'
#' @return TRUE/FALSE for each element in input vector
#' 
#' @examples
#' IsEmpty(NA)
#' IsEmpty(c(NA, NA))
#' IsEmpty(c('', NA))
#' IsEmpty(c('fnyg', NA)) 
#' 
IsEmpty = function(x){
  is.na(x) | x == ''
}


#' All Empty
#'
#' @description Internal function: 
#' Convenience function checks whether all NA or empty in a vector of strings
#'
#' @keywords internal
#' 
#' @author MS
#'
#' @param x vector
#'
#' @return TRUE/FALSE
#' 
#' @examples
#' AllEmpty(NA)
#' AllEmpty(c(NA, NA))
#' AllEmpty(c('', NA))
#' AllEmpty(c('fnyg', NA)) 
#' 
AllEmpty = function(x){
  sum( sapply(x, IsEmpty) ) == length(x)
}


#' Ordered Split
#'
#' @description Internal function: 
#' Split data frame by values in a column but maintain order as order of appearance. 
#' Different to base R split.data.frame where order of split objects are ordered as default factors.
#'
#' @keywords internal
#' 
#' @author MS
#'
#' @param x 
#' @param f 
#' @param drop 
#'
#' @return
#'
#' @examples
#' 
OrderedSplit = function(x, f, drop=TRUE){
  spl = split(x, f, drop)
  spl[unique(as.character(f))]
}


#' Get Colors
#'
#' @description Internal function: 
#' Get colors from a data frame containing at a minimum columns color and dataset
#'
#' @keywords internal
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
#' @importFrom dplyr distinct
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
      df_dist = dplyr::distinct(df, .data[[split_col_name]], color)
      colors = df_dist$color
      names(colors) = df_dist[[split_col_name]]
      return (colors)
    } else if ( AllEmpty(df[[split_col+1]]) ) { 
      split_col_name = colnames(df)[split_col]
      df_dist = dplyr::distinct(df, .data[[split_col_name]], color)
      colors = df_dist$color
      names(colors) = df_dist[[split_col_name]]
      return (colors)
    } else {
      return ( lapply(OrderedSplit(df, df[[split_col]], drop=TRUE), function(dfi) color_split(dfi, split_col+1)) )
    }
  }
  color_split(filled_df, start_col)
}



##### functions for split input values
#' Split Sliders
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param n 
#' @param varname 
#' @param suboptions 
#' @param vals 
#' @param optima 
#' @param step 
#'
#' @return
#' 
#' @import shiny
#'
#' @examples
#' 
split_sliders = function(n, varname, suboptions, vals, optima, step){
  if (n%%2==1){
    sliderInput(paste0(varname, '_subvar', 1+(n-1)/2),
                label=suboptions[1+(n-1)/2],
                value=vals[1+(n-1)/2],
                min=optima[1],
                max=optima[2],
                step=step)
    
  }else{
    p("")
  }
}

#' Split Sliders Panels
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param n 
#' @param varname 
#' @param slider_cells 
#' @param dataset_name 
#' @param vals 
#' @param optima 
#' @param step 
#'
#' @return
#' 
#' @import shiny
#'
#' @examples
#' 
split_sliders_panels = function(n, varname, slider_cells, dataset_name, vals, optima, step){
  if (n %in% slider_cells){
    sliderInput(paste0(varname, '_subvar', which(slider_cells==n)),
                label=ifelse(n==1, dataset_name, ''),
                value=vals[which(slider_cells==n)],
                min=optima[1],
                max=optima[2],
                step=step)
  }else{
    p("")
  }
}

#' Split Headers
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param n 
#' @param slider_cells 
#' @param levels 
#'
#' @return
#' 
#' @import shiny
#'
#' @examples
#' 
split_headers = function(n, slider_cells, levels){
  if (n %in% slider_cells){
    p(em(strong(levels[[1+(n-1)/2]])))
  }else{
    p('')
  }
}

#' Split Numeric Input
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param n 
#' @param varname 
#' @param suboptions 
#' @param vals 
#' @param optima 
#' @param step 
#'
#' @return
#' 
#' @import shiny
#'
#' @examples
#' 
split_numeric_input = function(n, varname, suboptions, vals, optima, step){
  if (n%%2==1){
    numericInput(paste0(varname, '_subvar', 1+(n-1)/2),
                 label=suboptions[1+(n-1)/2],
                 value=vals[1+(n-1)/2],
                 min=optima[1],
                 max=optima[2],
                 step=step)
  }else{
    p("")
  }
}

#' Split Numeric Input 2
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param n 
#' @param varname 
#' @param input_cells 
#' @param suboptions 
#' @param vals 
#' @param optima 
#' @param step 
#'
#' @return
#' 
#' @import shiny
#'
#' @examples
#' 
split_numeric_input2 = function(n, varname, input_cells, suboptions, vals, optima, step){
  if (n %in% input_cells){
    #cat(paste0('sni2: ', varname, '_subvar', which(input_cells==n)), '\n') #@cat
    numericInput(paste0(varname, '_subvar', which(input_cells==n)),
                 label=suboptions[which(input_cells==n)],
                 value=vals[which(input_cells==n)],
                 min=optima[1],
                 max=optima[2],
                 step=step)
  }else{
    p("")
  }
}

#' Split Numeric Panels
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param n 
#' @param varname 
#' @param slider_cells 
#' @param dataset_name 
#' @param vals 
#' @param optima 
#' @param step 
#'
#' @return
#' @import shiny
#'
#' @examples
#' 
split_numeric_panels = function(n, varname, slider_cells, dataset_name, vals, optima, step){
  if (n %in% slider_cells){
    numericInput(paste0(varname, '_subvar', which(slider_cells==n)),
                 label=ifelse(n==1, dataset_name, ''),
                 value=vals[which(slider_cells==n)],
                 min=optima[1],
                 max=optima[2],
                 step=step)
  }else{
    p("")
  }
}

#' Split Text Input
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param n 
#' @param varname 
#' @param suboptions 
#' @param vals 
#'
#' @return
#' 
#' @import shiny
#'
#' @examples
#' 
split_text_input = function(n, varname, suboptions, vals){
  if (n%%2==1){
    textInput(paste0(varname, '_subvar', 1+(n-1)/2),
              label=suboptions[1+(n-1)/2],
              placeholder=suboptions[1+(n-1)/2],
              value=vals[1+(n-1)/2])
  }else{
    p("")
  }
}

#' Split Color Input
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param n 
#' @param varname 
#' @param suboptions 
#' @param vals 
#' @param allowTransparent 
#'
#' @return
#' 
#' @import colourpicker
#' @import shiny
#'
#' @examples
#' 
split_color_input = function(n, varname, suboptions, vals, allowTransparent){
  if (n%%2==1){
    colourpicker::colourInput(paste0(varname, '_subvar', 1+(n-1)/2),
                              label=suboptions[1+(n-1)/2],
                              value=vals[1+(n-1)/2],
                              allowTransparent = allowTransparent,
                              returnName = TRUE,
                              closeOnClick = TRUE)
  }else{
    p("")
  }
}

#' Split Boolean Input
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param n 
#' @param varname 
#' @param suboptions 
#' @param vals 
#'
#' @return
#' 
#' @import shiny
#'
#' @examples
#' 
split_bool_input = function(n, varname, suboptions, vals){
  if (n%%2==1){
    checkboxInput(paste0(varname, '_subvar', 1+(n-1)/2),
                  label=suboptions[1+(n-1)/2],
                  value=vals[1+(n-1)/2])
  }else{
    p("")
  }
}


#### (A) CREATE INPUT ELEMENT (LAYOUT OF APP)

#' Create Input Element
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author MS/JR/SLA
#'
#' @param option 
#'
#' @return
#' 
#' @import shiny
#' @import spsComps
#' @import colourpicker
#'
#' @examples
#' 
create_input_element = function(option) {
  options_table = OpenOptionsTable()
  option_par <- options_table[options_table$shiny_varname == option,]
  if ( option_par$option_group == 'dataset_option' | option_par$option_group == 'annotation_option' ) {
    if ( option_par$option_class == 'bool' ) {
      spsComps::bsTooltip(
        checkboxGroupInput(option_par$shiny_varname,
                           option_par$shiny_label,
                           choices = c('all samples'),
                           selected = option_par$option_default == 'TRUE'),
        title=option_par$shiny_tooltip,
        placement ='left')
    } else if ( option_par$option_class == 'text_choices' ) {
      opt_choices  <- strsplit(option_par$option_options,',')[[1]]
      fluidRow(
        tags$head(
          tags$style(type="text/css", "#title {padding-left: 15px} #radios .shiny-input-radiogroup{padding-left: 25px}")
        ),
        tags$div(id = "title",spsComps::bsTooltip(p(option_par$shiny_label, style= 'font-weight: bold'),
                                                  title=option_par$shiny_tooltip,
                                                  placement ='left')),
        tags$div(id = "radios",radioButtons(option_par$shiny_varname,
                                            label = 'all samples',
                                            choices = opt_choices,
                                            selected = option_par$option_default,
                                            inline = TRUE)),
        tags$br())
    } else if ( option_par$option_class == 'text' ) {
      div_id=paste0(option_par$shiny_varname, '_outer')
      fluidRow(
        tags$head(tags$style("#title {padding-left: 15px}")),
        tags$div(id = "title",spsComps::bsTooltip(p(option_par$shiny_label, style= 'font-weight: bold'),
                                                  title=option_par$shiny_tooltip,
                                                  placement ='left')),
        tags$head(
          tags$style(type="text/css", paste0("#", div_id, " {padding-left: 25px}; #", option_par$shiny_varname, " {padding-left: 25px}"))
        ),
        tags$div(id = div_id,
                 tags$div(id=option_par$shiny_varname,
                          textInput(option_par$shiny_varname,  #@ paste0(option_par$shiny_varname,'XvalueX')
                                    label='all samples',
                                    placeholder = option_par$option_default,
                                    value = option_par$option_default))),
        tags$br()
      )
    } else if ( option_par$option_class == 'color' ) {
      div_id=paste0(option_par$shiny_varname, '_outer')
      fluidRow(
        tags$head(tags$style("#title {padding-left: 15px}")),
        tags$div(id = "title",spsComps::bsTooltip(p(option_par$shiny_label, style= 'font-weight: bold'),
                                                  title=option_par$shiny_tooltip,
                                                  placement ='left')),
        tags$head(
          tags$style(type="text/css", paste0("#", div_id, " {padding-left: 25px}; #", option_par$shiny_varname, " {padding-left: 25px}"))
        ),
        tags$div(id = div_id,
                 tags$div(id=option_par$shiny_varname,
                          colourpicker::colourInput(inputId=paste0('xyz_', option_par$shiny_varname), #@ option_par$shiny_varname
                                                    label='all samples',
                                                    value = ifelse(option_par$option_default=='NULL', 'white', option_par$option_default), #@ option_par$option_default
                                                    allowTransparent = TRUE,
                                                    returnName = TRUE,
                                                    closeOnClick = TRUE))),
        tags$br()
      )
    }else if ( option_par$option_class == 'numeric' ) {
      div_id=paste0(option_par$shiny_varname, '_outer')
      vals = as.numeric(strsplit(option_par$option_options, ';', fixed=TRUE)[[1]])
      min_val = vals[1]
      max_val = vals[2]
      if(is.na(min_val)){min_val=0}
      start_val = ifelse(length(vals)==3, vals[3], mean(c(min_val, max_val)))
      if(is.na(max_val)){max_val=start_val*2}
      if (option_par$shiny_varname != 'pseudoCount'){
        fluidRow(
          tags$head(tags$style("#title {padding-left: 15px}")),
          tags$div(id = "title",spsComps::bsTooltip(p(option_par$shiny_label, style= 'font-weight: bold'),
                                                    title=option_par$shiny_tooltip,
                                                    placement ='left')),
          tags$head(
            tags$style(type="text/css", paste0("#", div_id, " {padding-left: 25px}; #", option_par$shiny_varname, " {padding-left: 25px}"))
          ),
          tags$div(id = div_id,
                   tags$div(id=option_par$shiny_varname,
                            sliderInput(option_par$shiny_varname,
                                        label='all samples',
                                        value = start_val,
                                        min = min_val,
                                        max = max_val,
                                        step=0.001))),  #@ , width = '495px'
          tags$br()
        )
      }else{
        fluidRow(
          tags$head(tags$style("#title {padding-left: 15px}")),
          tags$div(id = "title",spsComps::bsTooltip(p(option_par$shiny_label, style= 'font-weight: bold'),
                                                    title=option_par$shiny_tooltip,
                                                    placement ='left')),
          tags$head(
            tags$style(type="text/css", paste0("#", div_id, " {padding-left: 25px}; #", option_par$shiny_varname, " {padding-left: 25px}"))
          ),
          tags$div(id = div_id,
                   tags$div(id=option_par$shiny_varname,
                            numericInput(option_par$shiny_varname, 
                                         label='all samples',
                                         value = start_val,
                                         min = min_val,
                                         max = max_val,
                                         step=0.001))),  #@ , width = '495px'
          tags$br()
        )
      }
    } else if ( option_par$option_class == 'special_argument' ) { ##@@1 -> ONLY manual_scales/force_scale at the moment
      spsComps::bsTooltip(
        checkboxInput(option_par$shiny_varname, option_par$shiny_label, value =ifelse(option_par$option_default=='NULL', FALSE, TRUE)),
        title=option_par$shiny_tooltip,
        placement ='left')
    } ##@@1 <-
  } else {
    if ( option_par$option_class == 'bool' ) {
      spsComps::bsTooltip(
        checkboxInput(option_par$shiny_varname, option_par$shiny_label, value = option_par$option_default == 'TRUE'),
        title=option_par$shiny_tooltip,
        placement ='left')
    }else if ( option_par$option_class == 'text' ) {
      if (option=='plotting_segments' | option=='plotting_segments_bottom'){
        w = '500px'
      }else{
        w = NULL
      }
      spsComps::bsTooltip(
        textInput(option_par$shiny_varname,
                  label =  option_par$shiny_label,
                  placeholder = option_par$option_default,
                  value = option_par$option_default,
                  width = w),
        title=option_par$shiny_tooltip,
        placement ='left')
    }else if ( option_par$option_class == 'color' ) {
      spsComps::bsTooltip(
        colourpicker::colourInput(option_par$shiny_varname,
                                  label =  option_par$shiny_label,
                                  value = option_par$option_default,
                                  allowTransparent = TRUE,
                                  returnName = TRUE,
                                  closeOnClick = TRUE),
        title=option_par$shiny_tooltip,
        placement ='left')
    } else if ( option_par$option_class == 'text_choices' ) {
      opt_choices  <- strsplit(option_par$option_options,',')[[1]]
      spsComps::bsTooltip(
        radioButtons(option_par$shiny_varname,
                     label = option_par$shiny_label,
                     choices = opt_choices,
                     selected = option_par$option_default,
                     inline = TRUE),
        title=option_par$shiny_tooltip,
        placement ='left')
    } else if ( option_par$option_class == 'numeric' ) {
      vals = as.numeric(strsplit(option_par$option_options, ';', fixed=TRUE)[[1]])
      min_val = vals[1]
      max_val = vals[2]
      if(is.na(min_val)){min_val=0}
      start_val = ifelse(length(vals)==3, vals[3], mean(c(min_val, max_val)))
      if(is.na(max_val)){max_val=start_val*2}
      spsComps::bsTooltip(
        sliderInput(option_par$shiny_varname,
                    label=option_par$shiny_label,
                    value=start_val,
                    min=min_val,
                    max=max_val),
        title=option_par$shiny_tooltip,
        placement ='left')
    } else if ( option_par$option_class == 'optional_numeric' ) {
      div_id = paste0(option_par$shiny_varname,"_div")
      if (suppressWarnings(is.na(as.numeric(option_par$option_default)))){
        automatic = TRUE
      }else{
        automatic = !as.logical(as.numeric(option_par$option_default))
      }
      vals = as.numeric(strsplit(option_par$option_options, ';', fixed=TRUE)[[1]])
      min_val = vals[1]
      max_val = vals[2]
      if(is.na(min_val)){min_val=0}
      if(is.na(max_val)){max_val=1}
      start_val = ifelse(length(vals)==3, vals[3], mean(c(min_val, max_val)))
      if (option_par$shiny_varname != 'binning_size' & option_par$shiny_varname != 'binning_start'){
        fluidRow(tags$p(tags$style(type="text/css", paste0("#checkbox {padding-left: 15px}"))),
                 tags$div(id = "checkbox",
                          spsComps::bsTooltip(
                            checkboxInput(option_par$shiny_varname,
                                          option_par$shiny_label,
                                          value = !automatic),
                            title=option_par$shiny_tooltip,
                            placement ='left')),
                 tags$p(tags$style(type="text/css", paste0("#", div_id, " {padding-left: 15px}"))),
                 tags$div(id = div_id,
                          sliderInput(paste0(option_par$shiny_varname,'_slider'),
                                      label=NULL,
                                      value=start_val,
                                      min=min_val,
                                      max=max_val))  #@ , width='500px', tags$br()
        )
      }else{
        fluidRow(tags$p(tags$style(type="text/css", paste0("#checkbox {padding-left: 15px}"))),
                 tags$div(id = "checkbox",
                          spsComps::bsTooltip(
                            checkboxInput(option_par$shiny_varname,
                                          option_par$shiny_label,
                                          value = !automatic),
                            title=option_par$shiny_tooltip,
                            placement ='left')),
                 tags$p(tags$style(type="text/css", paste0("#", div_id, " {padding-left: 15px}"))),
                 tags$div(id = div_id,
                          numericInput(paste0(option_par$shiny_varname,'_box'), #@ 2023-10-07
                                       label=NULL,
                                       value=start_val,
                                       min=min_val,
                                       max=max_val,
                                       step=0.5))
        )
      }
    }else if ( option_par$option_class == 'optional_text' ) {
      div_id = paste0(option_par$shiny_varname,"_div")
      fluidRow(tags$p(tags$style(type="text/css", paste0("#checkbox {padding-left: 15px}"))),
               tags$div(id = "checkbox",
                        spsComps::bsTooltip(
                          checkboxInput(option_par$shiny_varname,
                                        option_par$shiny_label,
                                        value = ifelse(option_par$option_default=='NULL', FALSE, TRUE)),
                          title=option_par$shiny_tooltip,
                          placement ='left')),
               tags$p(tags$style(type="text/css", paste0("#", div_id, " {padding-left: 15px}"))),
               tags$div(id = div_id,
                        textInput(paste0(option_par$shiny_varname,'_box'),
                                  label=NULL,
                                  placeholder = option_par$option_options) )
      )
    }else if (option_par$option_class=='split_numeric'){
      suboptions = strsplit(option_par$option_options, ';', fixed=TRUE)[[1]]
      valsNoptimaNstep = strsplit(option_par$option_default, ';', fixed=TRUE)[[1]]
      vals = as.numeric(strsplit(valsNoptimaNstep[1], ',', fixed=TRUE)[[1]])
      optima = as.numeric(strsplit(valsNoptimaNstep[2], ',', fixed=TRUE)[[1]])
      step = as.numeric(valsNoptimaNstep[3])
      if (length(vals)==1){
        vals = rep(vals, length(suboptions))
      }
      cellwidths = rep(0, 2*length(suboptions)-1)
      nCells = length(cellwidths)
      #cat(paste0(option_par$shiny_varname, ': ', nCells), '\n') #@cat seq(1,nCells)
      cellspacers = seq(2, nCells, 2)
      boxes = seq(1, nCells, 2)
      width_unit = 1/(length(cellspacers)+4*length(boxes))
      cellwidths[cellspacers] = paste0(100*width_unit, '%')
      cellwidths[boxes] = paste0(400*width_unit, '%')
      if (option_par$shiny_varname!='feat_extend'){
        spsComps::bsTooltip(
          do.call(what=splitLayout, args = c(lapply(seq(1,nCells), split_sliders, option_par$shiny_varname, suboptions, vals, optima, step),
                                             list(cellWidths=as.list(cellwidths)),
                                             list(width=list('500px')))),
          title=option_par$shiny_tooltip,
          placement ='left')
      }else{
        spsComps::bsTooltip(
          do.call(what=splitLayout, args = c(lapply(seq(1,nCells), split_numeric_input, option_par$shiny_varname, suboptions, vals, optima, step),
                                             list(cellWidths=as.list(cellwidths)),
                                             list(width=list('500px')))),
          title=option_par$shiny_tooltip,
          placement ='left')
      }
    }else if (option_par$option_class=='split_text'){
      suboptions = strsplit(option_par$option_options, ';', fixed=TRUE)[[1]]
      vals = strsplit(option_par$option_default, ',', fixed=TRUE)[[1]]
      if (any(vals=='FALSE') | any(vals=='TRUE')){
        vals = as.logical(vals)
      }
      if (length(vals)==1){
        vals = rep(vals, length(suboptions))
      }
      cellwidths = rep(0, 2*length(suboptions)-1)
      nCells = length(cellwidths)
      #cat(paste0(option_par$shiny_varname, ': ', nCells), '\n') #@cat seq(1,nCells)
      cellspacers = seq(2, nCells, 2)
      boxes = seq(1, nCells, 2)
      width_unit = 1/(length(cellspacers)+4*length(boxes))
      cellwidths[cellspacers] = paste0(100*width_unit, '%')
      cellwidths[boxes] = paste0(400*width_unit, '%')
      if (!is.logical(vals)){
        spsComps::bsTooltip(
          do.call(what=splitLayout, args = c(lapply(seq(1,nCells), split_text_input, option_par$shiny_varname, suboptions, vals),
                                             list(cellWidths=as.list(cellwidths)),
                                             list(width=list('500px')))),
          title=option_par$shiny_tooltip,
          placement ='left')
      }else{
        spsComps::bsTooltip(
          do.call(what=splitLayout, args = c(lapply(seq(1,nCells), split_bool_input, option_par$shiny_varname, suboptions, vals),
                                             list(cellWidths=as.list(cellwidths)),
                                             list(width=list('500px')))),
          title=option_par$shiny_tooltip,
          placement ='left')
      }
    }else if (option_par$option_class=='split_color'){
      suboptions = strsplit(option_par$option_options, ';', fixed=TRUE)[[1]]
      vals = strsplit(option_par$option_default, ',', fixed=TRUE)[[1]]
      if (length(vals)==1){
        vals = rep(vals, length(suboptions))
      }
      cellwidths = rep(0, 2*length(suboptions)-1)
      nCells = length(cellwidths)
      #cat(paste0(option_par$shiny_varname, ': ', nCells), '\n') #@cat seq(1,nCells)
      cellspacers = seq(2, nCells, 2)
      boxes = seq(1, nCells, 2)
      width_unit = 1/(length(cellspacers)+4*length(boxes))
      cellwidths[cellspacers] = paste0(100*width_unit, '%')
      cellwidths[boxes] = paste0(400*width_unit, '%')
      transparancy = ifelse(option_par$shiny_varname=='feature_color' | option_par$shiny_varname=='background_colors', FALSE, TRUE)
      spsComps::bsTooltip(
        do.call(what=splitLayout, args = c(lapply(seq(1,nCells), split_color_input, option_par$shiny_varname, suboptions, vals, transparancy),
                                           list(cellWidths=as.list(cellwidths)),
                                           list(width=list('500px')))
        ),
        title=option_par$shiny_tooltip,
        placement ='left')
    }else if ( option_par$option_class == 'special_argument' ) {
      if ( option=='panel_font_easy' | option=='panel_font'  | option=='panel_horizontal'){
        spsComps::bsTooltip(
          checkboxInput(option_par$shiny_varname, option_par$shiny_label, value =ifelse(option_par$option_default=='NULL', FALSE, TRUE)),
          title=option_par$shiny_tooltip,
          placement ='left')
      }else if (option=='header_font'){
        levels=c('Title', 'Subtitle', 'Scalebar')
        cellwidths = rep(0, 2*length(levels)-1)
        nCells = length(cellwidths)
        #cat(paste0(option_par$shiny_varname, ': ', nCells), '\n') #@cat seq(1,nCells)
        cellspacers = seq(2, nCells, 2)
        boxes = seq(1, nCells, 2)
        width_unit = 1/(length(cellspacers)+4*length(boxes))
        cellwidths[cellspacers] = paste0(100*width_unit, '%')
        cellwidths[boxes] = paste0(400*width_unit, '%')
        optimaNvals = as.numeric(strsplit(as.character(options_table[which(options_table$option_name=='header_font_sizes'),'option_options']), split=';')[[1]])
        optima = optimaNvals[1:2]
        vals = optimaNvals[3:5]
        step = 1
        fluidRow(
          tags$head(tags$style(type="text/css", paste0("#checkbox"))), #@ {padding-left: 15px}
          tags$div(id = "checkbox",
                   spsComps::bsTooltip(
                     checkboxInput(option_par$shiny_varname, option_par$shiny_label, value = ifelse(option_par$option_default=='NULL', FALSE, TRUE)),
                     title=option_par$shiny_tooltip,
                     placement ='left')),
          tags$head(tags$style(type="text/css", paste0("#header_font_div", " {padding-left: 15px}"))),
          tags$div(id = 'header_font_div',
                   do.call(what=splitLayout, args = c(lapply(seq(1,nCells), split_sliders, 'header_font', levels, vals, optima, step),
                                                      list(cellWidths=as.list(cellwidths)),
                                                      list(width=list('500px'))))
          )
        )
      }
    }
  }
}



# UI ####
ui <- fluidPage(
  useShinyjs(),
  shinybusy::use_busy_spinner(spin = "fading-circle"),
  tags$style(HTML(".shiny-split-layout>div {overflow: visible}")),  #@ this line allows the color widgets to be displayed "in front"
  # HEADER ####
  titlePanel( h1("seq'N'display'R", align = "left") ),
  h3( "Customizable and Reproducible Plotting of Sequencing Coverage Data", align='left' ),  #@ A Tool for 
  tags$br(),
  
  
  # TOP BUTTONS ####
  tags$head(
    tags$style(
      HTML(
        "#buttons_div .tooltip-inner{width: 200px; background-color:gray}"
      )
    )
  ),
  
  
  sidebarLayout(
    sidebarPanel(width=3,
                 div(id='buttons_div',
                     actionButton("plot", "Draw Plot"),
                     #note the tooltop, clean-up of space only visible upon mouse-over
                     shinyBS::bsTooltip(id = "plot", title = "The plot will appear in an independent window. </br> </br> The process can take some time depending on the number of tracks and annotations plotted as well as the arguments used.",
                                        placement = "right", trigger = "hover",),
                     suppressWarnings(downloadButton("save_pdf", "Save as PDF")),
                     shinyBS::bsTooltip(id = "save_pdf", title = "The plot will be saved as pdf. </br> </br> The process can take some time depending on the number of tracks and annotations plotted as well as the arguments used.",
                                        placement = "right", trigger = "hover"),
                     suppressWarnings(downloadButton("save_settings", "Save Settings")),
                     shinyBS::bsTooltip(id = "save_settings", title = "Save current settings in an sNdR sample (Excel) file for reuse.",
                                        placement = "right", trigger = "hover"),
                     ## TODO: remove debugging buttons
                     # actionButton("show_settings", "Show Settings"),
                     # shinyBS::bsTooltip(id = "show_settings", title = "Show settings of active session in console.",
                     #                    placement = "right", trigger = "hover"),
                     # actionButton("show_parameters", "Show Parameters"),
                     # shinyBS::bsTooltip(id = "show_parameters", title = "Show dataset-specific options (parameters) of active session in console.",
                     #                    placement = "right", trigger = "hover"),
                     # actionButton("show_samples", "Show Samples"),
                     # shinyBS::bsTooltip(id = "show_samples", title = "Show currently selected samples and their order",
                     #                    placement = "right", trigger = "hover"),
                     # actionButton("which_samples", "Show whichSamples"),
                     # shinyBS::bsTooltip(id = "which_samples", title = "Show currently whichSamples argument, selected samples for display. Samples listed in Show samples but not in here are still used for batch correction",
                     #                    placement = "right", trigger = "hover"),
                     # 
                     # actionButton("clean_console", "Clean Console"),
                     # shinyBS::bsTooltip(id = "clean_console", title = "Clean the text output displayed to the right",
                     #                    placement = "right", trigger = "hover"),
                     
                     actionButton("reset", "Reset Session"),
                     actionButton("reload_app", "Reset App"), #@ 2023-09-18 Reload Page
                     #actionButton("debug", "Custom Debug Info"),
                     shinyBS::bsTooltip(id = "reset", title = "Reset all options to values from the currently loaded template",
                                        placement = "right", trigger = "hover")
                     
                 )
    ),
    mainPanel(
      tags$head(
        tags$style(type="text/css", "#examples_sample_sheets_folder {background-color: #BD583735}")),
      tags$div(id="examples_sample_sheets_folder", tags$p("Example", strong(em("sNdR sample files")), "are in", seqNdisplayR::ExamplesSampleSheetsFolder())),
      tags$p("1. load a template and choose a locus in the Input section"),
      tags$p("2. modify any options in Input or Optional Arguments below"),
      tags$p("3. select on the left whether to plot on screen, save as PDF or save current settings to sNdR sample (Excel) file"),
      # tags$p(em("A convenient website to easily design pleasing color palettes, with colorblind friendly options can be found "), a("here", href="https://coolors.co/"), em('or'), a("here", href="https://medialab.github.io/iwanthue/")),
      tags$p(strong("Optional arguments marked with [*] should be used with caution - only recommended for experienced users! Consult the associated paper or R help for more details.")),
      tags$br(),
      #tags$p("Example", strong(em("Sample Sheets")), "are in", seqNdisplayR::ExamplesSampleSheetsFolder()),
      verbatimTextOutput("console"))
  ),
  tags$br(),
  
  # BODY SECTION ####
  fluidRow(
    column(
      12,
      offset = 0,
      navlistPanel(
        "Input",
        tabPanel(
          "Select Template and Locus",
          #@ p("Filling out the boxes in the 'Input' window is the minimum requirement for plotting. Multiple accessory arguments can be adjusted below, but plotting can be performed merely from importing the template excel file, specifying a locus by name or coordinates and pressing the 'Draw Plot' button (locus name overwrites coordinates)."),
          fileInput(
            "input_file",
            "Load sNdR sample file", #@ "Load excel file (or IGV session xml file)"
            multiple = FALSE,
            accept = c(".xls", ".xlsx", ".xlsm", '.xml'),
            width = 540
          ),
          verbatimTextOutput("File_import_msg"),
          #@ h4('Excel template import options'), #@ h4('Excel template and IGVsession import options'),
          spsComps::bsTooltip(
            checkboxInput("load_annotations", "Load annotations during import", value = TRUE),
            title='Load annotations already during import. This can take some time during session loading, but will save time when plotting.',
            placement ='right'),
          tags$br(),
          splitLayout(
            cellWidths = c('40%', '10%', '40%'), #@c(252,56, 252), #@ c('40%', '10%', '40%')
            textInput("gene", label ='Enter locus name', placeholder='e.g. LMO4'),
            p("or", align='center'),
            textInput("coordinates", label='Enter locus coordinates', placeholder='e.g. chr1:+:87325400:87351991'),
            width=560
          ),
          tags$p(em(
            "The locus name has to be present in one of the supplied annotations and match case. When entering locus name and coordinates simultaneously, only the locus name will be considered."
          )),
          tags$br(),
          create_input_element('verbosity')
        ),
        
        # tabPanel(
        #   "Template Load Options",
        #   h4('Excel template import options'), #@ h4('Excel template and IGVsession import options'),
        #   spsComps::bsTooltip(
        #     checkboxInput("load_annotations", "Load annotations during import", value = TRUE),
        #     title='Load annotations already during import. This can take some time during session loading, but will save time when plotting.',
        #     placement ='right'),
        #   tags$br(),
        #   h4('IGVsession-specific import options'),
        #   spsComps::bsTooltip(
        #     radioButtons("igv_groupby", "Group igv-loaded samples to datasets using:",
        #                  choices = list('Do not group'='dont_group',
        #                                 'IGV autoscale groups'='autoscalegroups',
        #                                 'Common prefix in names'='common_prefix'
        #                  ),
        #                  selected = 'dont_group'),
        #     title='Not every igv session is fully compatible with seqNdisplayR, but you should be able to load the session and then export to then modify the downloaded Excel sheet to create meaningful sample divisions, reload the Excel template and plot. The default "Do not group" option should be most robust, in particular together with setting plus,minus strand identifiers below as NULL.',
        #     placement ='right'),
        #   spsComps::bsTooltip(
        #     textInput("igv_strand_regex", "IGV-import: plus,minus strand identifiers",value = 'plus,minus'),
        #     title="Substring in bigwig file names used for strand assignment. Bigwig file names for plus/minus strand MUST be direct replacement of plus vs minus regex. If setting this option as NULL, will not try to assign strand. Together with Do not group option above this should work for most IGV session files.",
        #     placement ='right')
        # ),
        # 

        # Sample Selection ####
        tabPanel(
          "Data Overview and Selection",
          #@ p('You can reorder within each dataset, and select which samples to display. Reordering and selection are used for plotting by using the dataset-specific whichSamples parameter and this will also be saved in the Excel session like that. Reordering of datasets does not work for now. This has to be done in the Excel sheet. Note also that whichSamples is currently not respected when drawing the tree upon loading of an Excel template.'),
          h4('Tracks to plot:'),
          shinyTree::shinyTree("tree", checkbox=TRUE, dragAndDrop=TRUE, multiple = TRUE, animation = FALSE, themeIcons = FALSE)
        ),
        

        # Track Colors ####
        tabPanel(
          "Track Colors",
          #h4('Track colors:'),
          tags$br(),
          column(
          10,
          offset = 0,
          create_input_element('track_colors')
          )
        ),
        
        # Optional Args ####
        "Optional Arguments",
        tabPanel(
          "Plot Layout",
          tags$br(),
          column(
            10,
            offset = 0,
            create_input_element('pdf_scale'),
            create_input_element('dummy'),
            create_input_element('header_display'),
            create_input_element('header_name'),
            tags$head(
              tags$style(type="text/css", "#header_name_div {padding-left: 15px}")
            ),
            div(id = "header_name_div",
                div(id = "header_name_box")
            ),
            create_input_element('include_genomic_scale'),
            create_input_element('genomic_scale_on_top'),
            create_input_element('for_op'),
            create_input_element('bothstrands'),
            create_input_element('intermingled'),
            tags$head(
              tags$style(type="text/css", "#intermingled_div") #@ {padding-left: 15px}
            ),
            div(id = "intermingled_div",
                div(id = "intermingled_colors",
                    create_input_element('intrmngld_col')
                )
            ),
            create_input_element('neg_as_neg'),
            create_input_element('reverse_strand'),
            create_input_element('plotting_segments'),
            create_input_element('plotting_segments_bottom'),
          )
        ),
        
        tabPanel(
          "Plot Display Parameters",
          tags$br(),
          column(
            10,
            offset = 0,
            create_input_element('feat_extend'),
            create_input_element('tracks_height'),
            create_input_element('title_field_height_cm'),
            create_input_element('genomic_scale_height_cm'),
            create_input_element('annot_height_cm'), #@ 2023-06-19
            create_input_element('spacer_height'),
            create_input_element('tracks_width_in_cm'),
            create_input_element('margin_width_I'),
            create_input_element('tracks_name_width'),
            create_input_element('include_tracks_scale'),
            create_input_element('scale_per_space'),
            create_input_element('plot_height'),
            create_input_element('plot_width')
          )
        ),
        
        tabPanel(
          "Dataset-Specific Options",
          tags$br(),
          column(
            10,
            offset = 0,
            create_input_element('calcMean'),
            create_input_element('negative_valued_bw'),
            create_input_element('binning_type'),
            create_input_element('enhance_signal'),
            create_input_element('log2transform'),
            create_input_element('pseudoCount'),
            create_input_element('negValsSet0'),
            create_input_element('batch_correct')
          )
        ),
        
        tabPanel(
          "Panel Display",
          tags$br(),
          column(
            10,
            offset = 0,
            h4('Panel Text Display:'),
            create_input_element('panels_width_fixed'),
            create_input_element('panel_horizontal'),
            tags$head(
              tags$style(type="text/css", "#panel_horizontal_div {padding-left: 15px}")
            ),
            div(id = "panel_horizontal_div",
                p("Display panel text horizontally:", style= 'font-weight: bold'),
                div(id = "panel_horizontal_checkboxes")
            ),
            create_input_element('print_one_line_sample_names'),
            create_input_element('replicate_names'),
            create_input_element('pan_col'),
            create_input_element('panel_font_easy'),
            tags$head(
              tags$style(type="text/css", "#panel_font_easy_div {padding-left: 15px}")
            ),
            div(id = "panel_font_easy_div",
                div(id = 'panel_font_easy_boxes')
            ),
            create_input_element('panel_font'),
            tags$head(
              tags$style(type="text/css", "#panel_font_div {padding-left: 15px}")
            ),
            div(id = "panel_font_div",
                div(id = "panel_font_boxes_headers"),
                div(id = "panel_font_boxes")
            ),
            tags$br(),
            h4('Spacing, Background and Separators'),
            create_input_element('incl_first_panel'),
            create_input_element('horizon_space'),
            create_input_element('panel_separator'),
            create_input_element('sep_col'),
            create_input_element('sep_thick'),
            create_input_element('alternating_background_usage'),
            tags$head(
              tags$style(type="text/css", "#alternating_background_usage_div") #@  {padding-left: 15px}
            ),
            div(id = "alternating_background_usage_div",
                div(id = "alternating_background_usage_choices",
                    create_input_element('background_colors'),
                    create_input_element('background_opacity')
                )
            )
          )
        ),
        
        tabPanel(
          "Track Binning",
          tags$br(),
          column(
            10,
            offset = 0,
            p("Data binning concerns how coverage data is summarized into bins before plotting. By default optimal binning conditions are determined based on locus size but the individual binning settings can be changed below."),
            tags$br(),
            create_input_element('binning_size'),
            create_input_element('binning_start'),
            create_input_element('bins_cm')
          )
        ),
        
        tabPanel(
          "Header and Genomic Region",
          tags$br(),
          column(
            10,
            offset = 0,
            h4('Header'),
            create_input_element('header_font'),
            tags$head(
              tags$style(type="text/css", "#header_font_div {padding-left: 15px}")
            ),
            div(id = "header_font_div",
                div(id = "header_font_boxes")
            ),
            create_input_element('header_color'),
            h4('Genomic region'),
            create_input_element('gen_scal_font'),
            create_input_element('gen_scal_font_col')
          )
        ),
        
        tabPanel(
          "Data Scale Display",
          tags$br(),
          column(
            10,
            offset = 0,
            create_input_element('group_autoscale'),
            create_input_element('manual_scales'), ##@@0a ->
            tags$head(
              tags$style(type="text/css", "#manual_scales_div {padding-left: 15px}")
            ),
            div(id = "manual_scales_div",
                div(id = "manual_scales_boxes"
                )
            ),  ##@@0a <-
            create_input_element('scale_scientific_format'),
            create_input_element('scale_character_size'),
            create_input_element('scale_col')
          )
        ),
        
        tabPanel(
          "Annotation Display",
          tags$br(),
          column(
            10,
            offset = 0,
            create_input_element('annotation_compac'),
            create_input_element('annotations_names'),
            create_input_element('mass_centre'),
            create_input_element('feature_above'),
            create_input_element('feature_alternate'),
            create_input_element('locus_arrow'),
            create_input_element('which_annot_colors'),
            create_input_element('annotation_character_size'),
            create_input_element('annotation_character_color'),
            tags$br(),
            h4('Annotation Titles'),
            create_input_element('annot_font'),
            create_input_element('annot_col_name'),
            tags$br(),
            h4('Loci Shading'),
            create_input_element('incl_locus_colors'),
            create_input_element('feature_color'),
            create_input_element('feature_shading_alpha')
          )
        )
      )
    )
  )
)



# SERVER ####

#' Server
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author MS/JR/SLA
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' 
#' @import shiny
#' @import colourpicker
#' @import shinyTree
#' @import shinyjs
#' @import seqNdisplayR
#' @import spsComps
#'
#' @examples
#' 
server <- function(input, output, session) {
  options_table = OpenOptionsTable()
  CurrentSession <- reactiveVal(NULL) # a seqNRdisplaySession Object for current session
  CurrentSessionFname <- reactiveVal('') # filename to prevent reloading
  CurrentSessionIdx <- reactiveVal(0) # index to be able to address dynamic UI elements 
  textLog <- reactiveVal("") # reactive text log
  
  
  
  # reactive track selection tree display ####
  ## from list x creates nested node list for tree where all nodes are selected
  set_all_selected <- function(x) {
    if ( is.list(x) ) {
      structure(lapply(x, set_all_selected), stselected=T, stopened=TRUE)
    } else {
      names(x) <- x
      lapply(x, function(xi) structure(xi, stselected=T, stopened=TRUE))
    }
  }
  
  ## from list x creates nested node list for tree where all nodes are deselected
  set_all_deselected <- function(x) {
    if ( is.list(x) ) {
      structure(lapply(x, set_all_deselected), stselected=F, stopened=TRUE)
    } else {
      names(x) <- x
      structure(as.list(x), stselected=F, stopened=TRUE)
    }
  }
  
  ## from list x creates nested node list for tree where all nodes are selected if present in x
  set_tree_nodes <- function(samples, whichSamples) {
    if( is.list(samples) ) {
      nodes <- lapply(names(samples), function(dataset) {
        if ( is.null(whichSamples[[dataset]]) ){
          structure(set_all_selected(samples[[dataset]]),
                    stselected=T, stopened=T)
        } else if ( is.na(whichSamples[[dataset]]) ) {
          structure(set_all_deselected(samples[[dataset]]),
                    stselected=F, stopened=T)
        } else if ( identical(unlist(samples[[dataset]]), unlist(whichSamples[[dataset]])) ){
          structure(set_all_selected(samples[[dataset]]),
                    stselected=T, stopened=T)
        } else {
          structure(set_tree_nodes(samples[[dataset]], whichSamples[[dataset]]),
                    stopened=T)
        }
      })
      names(nodes) <- names(samples)
      structure(nodes, stopened=T)
    } else {
      names(samples) <- samples
      lapply(samples, function(sample) structure(sample, stselected=(sample %in% whichSamples), stopened=TRUE))
    }
  }
  
  ## nested list of selected nodes from shinyTree t
  get_selected_tree <- function(t) {
    sel_t <- t
    if ( is.list(sel_t[[1]]) ) {
      l <- lapply(sel_t, get_selected_tree)
      sel <- !sapply(l, function(li) {is.null(li) | length(li) == 0})
      l <- l[sel]
      names(l) <- names(sel_t)[sel]
      l
    } else {
      sel_x <- sapply(t, function(ti) !is.null(attr(ti, "stselected")))
      if ( length(sel_x) == 0 ) {
        NULL
      } else {
        names(sel_t[sel_x])
      }
    }
  }
  
  ## get samples selected in the tree
  GetSelectedSamples <- function(){
    tree <- input[['tree']]
    #ups the tree cannot be selected without first being shown
    # a decision made in shinyTree package we use for the tree
    if ( is.null(tree) ) {
      #cat('** NULL', '\n') #@ 2023-10-07
      #@CurrentSession()$samples #@ 2023-10-07
      NULL
    } else {
      #cat('** not NULL', '\n') #@ 2023-10-07
      get_selected_tree(tree)
    }
  }
  
  # responsive elements show/hide behavior ####
  observe({
    toggle(id = "pdf_output_folder_div", condition = input$pdf_output_folder)
  })
  
  observe({
    toggle(id = "pdf_name_div", condition = input$pdf_name)
  })
  
  observe({
    toggle(id = "header_name_div", condition = input$header_name)
  })
  
  observe({
    toggle(id = "intermingled_div", condition = input$intermingled)
  })
  
  observe({
    toggle(id = "panel_horizontal_div", condition = input$panel_horizontal)
  })
  
  observe({
    toggle(id = "panel_font_easy_div", condition = input$panel_font_easy)
  })
  
  observe({
    toggle(id = "panel_font_div", condition = input$panel_font)
  })
  
  observe({
    toggle(id = "tracks_height_div", condition = input$tracks_height)
  })
  
  observe({
    toggle(id = "title_field_height_cm_div", condition = input$title_field_height_cm)
  })
  
  observe({
    toggle(id = "genomic_scale_height_cm_div", condition = input$genomic_scale_height_cm)
  })
  
  observe({
    toggle(id = "annot_height_cm_div", condition = input$annot_height_cm) #@ 2023-06-19
  })
  
  observe({
    toggle(id = "spacer_height_div", condition = input$spacer_height)
  })
  
  observe({
    toggle(id = "tracks_width_in_cm_div", condition = input$tracks_width_in_cm)
  })
  
  observe({
    toggle(id = "tracks_name_width_div", condition = input$tracks_name_width)
  })
  
  observe({
    toggle(id = "scale_per_space_div", condition = input$scale_per_space)
  })
  
  observe({
    toggle(id = "plot_height_div", condition = input$plot_height)
  })
  
  observe({
    toggle(id = "plot_width_div", condition = input$plot_width)
  })
  
  observe({
    toggle(id = "alternating_background_usage_div", condition = input$alternating_background_usage)
  })
  
  observe({
    toggle(id = "annotation_character_size_div", condition = input$annotation_character_size)
  })
  
  observe({
    toggle(id = "annot_font_div", condition = input$annot_font)
  })
  
  observe({
    toggle(id = "header_font_div", condition = input$header_font)
  })
  
  observe({
    toggle(id = "gen_scal_font_div", condition = input$gen_scal_font)
  })
  
  observe({
    toggle(id = "binning_size_div", condition = input$binning_size)
  })
  
  observe({
    toggle(id = "binning_start_div", condition = input$binning_start)
  })
  
  observe({
    toggle(id = "scale_character_size_div", condition = input$scale_character_size)
  })
  
  observe({
    toggle(id = "manual_scales_div", condition = input$manual_scales) ##@@0b <->
  })
  
  
  
  # update session UI to loaded template ####
  # set all values in ui elements to the ones from a seqNdisplayR session (imported Excel)
  update_ui_to_session = function(seqNdisplayR_session) {
    prev_session_idx <- CurrentSessionIdx() - 1
    dataset_names = names(seqNdisplayR_session$samples)
    
    # global options ####
    global_options = options_table[options_table$option_group == 'global',]
    for ( i in 1:nrow(global_options) ) {
      opt_line = global_options[i,]
      opt = global_options$option_name[i]
      
      if ( opt %in% names(seqNdisplayR_session) ) {
        if ( opt_line$option_class == 'bool' ) {
          updateCheckboxInput(session, opt_line$shiny_varname, value = seqNdisplayR_session[[opt]])
        } else if ( opt_line$option_class == 'text' ) {
          updateTextInput(session, opt_line$shiny_varname, value = DeparseOption(seqNdisplayR_session[[opt]])) #@ 2022-10-07 seqNdisplayR::DeparseOption
        } else if ( opt_line$option_class == 'text_choices' ) { #@ ->
          options = strsplit(opt_line$option_options, split=',', fixed=TRUE)[[1]]
          updateRadioButtons(session, opt_line$shiny_varname, selected=DeparseOption(seqNdisplayR_session[[opt]])) #@ 2022-10-07 seqNdisplayR::DeparseOption
        } else if ( opt_line$option_class == 'color' ) {
          colourpicker::updateColourInput(session, opt_line$shiny_varname, value = DeparseOption(seqNdisplayR_session[[opt]])) #@ 2022-10-07 seqNdisplayR::DeparseOption
        } else if ( opt_line$option_class == 'numeric' ) {
          updateSliderInput(session, opt_line$shiny_varname, value = seqNdisplayR_session[[opt]])
        } else if ( opt_line$option_class == 'optional_numeric' ) {
          if (is.null(seqNdisplayR_session[[opt]])){
            automatic = TRUE
          }else if (suppressWarnings(is.na(as.numeric(seqNdisplayR_session[[opt]])))){
            automatic = TRUE
          }else{
            automatic = !as.logical(as.numeric(seqNdisplayR_session[[opt]]))
          }
          if( !automatic ){
            updateCheckboxInput(session, opt_line$shiny_varname, value = TRUE)
            if (opt != 'bin_size' & opt != 'bin_start'){
              updateSliderInput(session, paste0(opt_line$shiny_varname, '_slider'), value = seqNdisplayR_session[[opt]]) #@ 2023-10-07
            }else{
              updateSliderInput(session, paste0(opt_line$shiny_varname, '_box'), value = seqNdisplayR_session[[opt]]) #@ 2023-09-25 '_box' <- '_slider'  
            }
          } else {
            updateCheckboxInput(session, opt_line$shiny_varname, value = FALSE)
          }
        }else if ( opt_line$option_class == 'optional_text' ) { #@ -> 2023-10-07
          if( !is.null(seqNdisplayR_session[[opt]]) ){
            updateCheckboxInput(session, opt_line$shiny_varname, value = TRUE)
            updateTextInput(session, paste0(opt_line$shiny_varname, '_box'), value = seqNdisplayR_session[[opt]] ) #@ 2023-10-07
          } else {
            updateCheckboxInput(session, opt_line$shiny_varname, value = FALSE)
          } #@ <- 2023-10-07
        }else if ( opt_line$option_class == 'split_numeric' ) {
          suboptions = strsplit(opt_line$option_options, ';', fixed=TRUE)[[1]]
          for (n in seq_along(suboptions)){
            if (opt_line$shiny_varname!='feat_extend'){
              updateSliderInput(session, paste0(opt_line$shiny_varname, '_subvar', n), value = seqNdisplayR_session[[opt]][n])
            }else{
              updateNumericInput(session, paste0(opt_line$shiny_varname, '_subvar', n), value = seqNdisplayR_session[[opt]][n])
            }
          }
        }else if ( opt_line$option_class == 'split_text' ){
          suboptions = strsplit(opt_line$option_options, ';', fixed=TRUE)[[1]]
          vals = seqNdisplayR_session[[opt]]
          if (length(vals)==1){
            vals = rep(vals, length(suboptions))
          }
          for (n in seq_along(suboptions)){
            if (opt_line$shiny_varname!='panel_separator'){
              updateTextInput(session, paste0(opt_line$shiny_varname, '_subvar', n), value = vals[n])
            }else{
              updateCheckboxInput(session, paste0(opt_line$shiny_varname, '_subvar', n), value = vals[n])
            }
          }
        }else if ( opt_line$option_class == 'split_color' ){
          suboptions = strsplit(opt_line$option_options, ';', fixed=TRUE)[[1]]
          vals = seqNdisplayR_session[[opt]]
          if (length(vals)==1){
            vals = rep(vals, length(suboptions))
          }
          for (n in seq_along(suboptions)){
            colourpicker::updateColourInput(session, paste0(opt_line$shiny_varname, '_subvar', n), value = vals[n])
          }
        }else if ( opt_line$option_class == 'special_argument' ){
          if ( opt=='panel_font_sizes' | opt=='panel_font_size_list' | opt=='horizontal_panels_list'){
            if ( !is.null(seqNdisplayR_session[[opt]]) ){
              updateCheckboxInput(session, opt_line$shiny_varname, value = TRUE)
              
              ##TO DO: Insert elements for new datasets using inserUI and delete elements from old datasets
              dataset_group_depths = sapply(rev(dataset_names), function(name) ListDepth(seqNdisplayR_session$samples[[name]]) + 1)
              if (opt=='panel_font_sizes'){
                dataset_group_depth = max(dataset_group_depths)
                levels=c('First Panel', paste0('Inner Panel ', dataset_group_depth:1))
                for (n in seq_along(levels)){
                  updateSliderInput(session, paste0('panel_font_easy', '_subvar', n), value = seqNdisplayR_session[[opt]][n])
                }
              }else{
                for ( name in rev(dataset_names) ) {
                  dataset_group_depth = dataset_group_depths[[name]]
                  levels=c('First Panel', paste0('Inner Panel ', dataset_group_depth:1))
                  for (n in seq_along(levels)){
                    if (opt=='panel_font_size_list'){
                      updateSliderInput(session, paste0('panel_font_', name, '_subvar', n), value = seqNdisplayR_session[[opt]][[name]][n])
                    }else{
                      updateCheckboxGroupInput(session, paste0(opt_line$shiny_varname, '_', name), choices=levels, selected=levels[seqNdisplayR_session[[opt]][[name]]]) #@
                    }
                  }
                }
              }
            }
          }else if (opt=='header_font_sizes'){
            if ( !is.null(seqNdisplayR_session[[opt]]) ){
              updateCheckboxInput(session, opt_line$shiny_varname, value = TRUE)
              for (n in 1:3){
                updateSliderInput(session, paste0('header_font', '_subvar', n), value = seqNdisplayR_session[[opt]][n])
              }
            }
          }
        }
      }
    }
    
    
    # tree ####
    output$tree <- shinyTree::renderTree( {
      whichSamples <- lapply(seqNdisplayR_session$parameters, function(p) p$whichSamples)
      names(whichSamples) <- names(seqNdisplayR_session$parameters)
      set_tree_nodes(seqNdisplayR_session$samples,
                     whichSamples)
    } )
    
    
    # dataset-specific options ####
    dataset_options = options_table[options_table$option_group == 'dataset_option',]
    for ( i in 1:nrow(dataset_options) ) {
      opt_line = dataset_options[i,]
      opt = dataset_options$option_name[i]
      # cat(paste0(opt, ': ', opt_line$shiny_varname), '\n') #%
      para = lapply(dataset_names, function(d) seqNdisplayR_session$parameters[[d]][[opt]])
      names(para) = dataset_names
      if ( opt_line$option_class == 'bool' ) {
        updateCheckboxGroupInput(session,
                                 opt_line$shiny_varname,
                                 choices = character(0),
                                 selected = character(0))
        
        updateCheckboxGroupInput(session,
                                 opt_line$shiny_varname,
                                 choices = names(para),
                                 selected = names(para)[unlist(para)])
      } else {
        
        ## idea is to remove for each sample name and line with sample name and parameter
        ## entries from previous loaded template are removed (except one, ie anchor)
        ## this anchor (or the dummy created before template loading) serve as anchor to add sample-lines below
        ## after this the anchor is removed
        
        anchor_elem <- opt_line$shiny_varname
        
        shiny_elems <- grep(paste0(anchor_elem, '_XvalueX', prev_session_idx, '_'), names(input), value=TRUE)
        if ( length(shiny_elems) != 0 ){
          for ( elem in shiny_elems ) {
            removeUI(selector = paste0("#", elem))
            #@runjs(paste0("Shiny.onInputChange('", elem,"', null)"))
          }
        }
        
        if ( opt_line$option_class == 'special_argument' ) { ##@@2 ->
          if ( !is.null(seqNdisplayR_session[[opt]]) | any(!is.na(unlist(para))) ){ #$ condition added 230519
            updateCheckboxInput(session, opt_line$shiny_varname, value=TRUE ) 
            # cat("input manual scales:", '\n')
            # cat(class(input), '\n')
            # cat(paste(names(input$manual_scales), collapse='\n'), '\n')
            # shiny_elems2 <- grep(paste0(anchor_elem), names(input), value=TRUE)
            # cat(shiny_elems2, '\n')
          }
        } ##@@2 <-
        
        for ( name in rev(names(para)) ) {
          #@ alt_name = gsub('\\s+', 'YvalueY', name) #@
          alt_name = gsub('\\s+', paste0('Y', which(names(para)==name), 'Y'), name) #@
          #@ alt_name = gsub("[[:punct:]]", "ZvalueZ", alt_name)
          alt_name = gsub("[[:punct:]]", paste0('Z', which(names(para)==name), 'Z'), alt_name)
          dataset_id = paste0(anchor_elem, '_XvalueX', CurrentSessionIdx(), '_', alt_name)
          if ( opt_line$option_class == 'text' ) {
            insertUI(
              selector = paste0('#', anchor_elem),
              where = "afterEnd",
              ui = tags$div(id=dataset_id,
                            textInput(inputId = dataset_id,
                                      label = name,
                                      value = DeparseOption(para[[name]])
                            ))
            )
          } else if ( opt_line$option_class == 'numeric' ) {
            vals = as.numeric(strsplit(opt_line$option_options, ';', fixed=TRUE)[[1]])
            min_val = vals[1]
            max_val = vals[2]
            if(is.na(min_val)){min_val=0}
            start_val = ifelse(length(vals)==3, vals[3], mean(c(min_val, max_val)))
            if(is.na(max_val)){max_val=start_val*2}
            if (opt_line$shiny_varname != 'pseudoCount'){
              insertUI(
                selector = paste0('#', anchor_elem),
                where = "afterEnd",
                ui = tags$div(id=dataset_id,
                              sliderInput(inputId = dataset_id,
                                          label = name,
                                          min=min_val,
                                          max=max_val,
                                          value = DeparseOption(para[[name]]),
                                          step=0.001)
                )
              )
            }else{
              insertUI(
                selector = paste0('#', anchor_elem),
                where = "afterEnd",
                ui = tags$div(id=dataset_id,
                              numericInput(inputId = dataset_id,
                                           label = name,
                                           min=min_val,
                                           max=max_val,
                                           value = DeparseOption(para[[name]]),
                                           step=0.001)
                )
              )
            }
          } else if ( opt_line$option_class == 'text_choices' ) {
            opt_choices = strsplit(opt_line$option_options,',')[[1]]
            insertUI(selector = paste0('#', anchor_elem),
                     where = "afterEnd",
                     ui = tags$div(id=dataset_id,
                                   radioButtons(inputId = dataset_id,
                                                label = name,
                                                choices = opt_choices,
                                                selected = DeparseOption(para[[name]]),
                                                inline = TRUE)))
          }else if ( opt_line$option_class == 'special_argument' ) { ##@@3 -> ONLY manual_scales/force_scale at the moment
            #$ -->  condition added 230519
            if ( any(!is.na(unlist(para))) ){
              if (!is.null(para[[name]])){
                levels = para[[name]]
                for (n in seq_along(levels)){
                  if (!is.na(para[[name]][n])){
                    updateNumericInput(session, paste0(dataset_id, '_subvar', n), value = para[[name]][n])
                  }
                }
              }
            }else if (!is.null(seqNdisplayR_session[[opt]])){
              if (!is.null(seqNdisplayR_session[[opt]][[name]])){
                levels = length(seqNdisplayR_session[[opt]][[name]])
                for (n in seq_along(levels)){
                  updateNumericInput(session, paste0(dataset_id, '_subvar', n), value = seqNdisplayR_session[[opt]][[name]][n])
                }
              }
            }
            #$ <--
          } ##@@3 <-
        }
        
        #hide the anchor
        if ( opt_line$option_class != 'special_argument'){
          shinyjs::hide(id = anchor_elem)
        }
      }
    }
    
    
    # panels horizontal special case of expandable option upon enable ####
    ## remove previous existing checkboxGroups
    shiny_elems = grep(paste0('panel_horizontal_XvalueX', prev_session_idx, '_'), names(input), value=TRUE)
    if (length(shiny_elems) > 0){
      for ( elem in shiny_elems ) {
        #cat(elem, '\n') #@cat
        removeUI(selector = paste0('#', elem))
      }
    }
    
    for ( name in rev(dataset_names) ) {
      #@ alt_name = gsub('\\s+', 'YvalueY', name) #@
      alt_name = gsub('\\s+', paste0('Y', which(dataset_names==name), 'Y'), name) #@
      #@ alt_name = gsub("[[:punct:]]", "ZvalueZ", alt_name)
      alt_name = gsub("[[:punct:]]", paste0('Z', which(dataset_names==name), 'Z'), alt_name)
      dataset_id = paste0('panel_horizontal_XvalueX', CurrentSessionIdx(), '_', alt_name)
      
      dataset_group_depth = ListDepth(seqNdisplayR_session$samples[[name]]) + 1
      levels=c('First Panel', paste0('Inner Panel ', dataset_group_depth:1))
      if (is.null(seqNdisplayR_session[['horizontal_panels_list']][[name]])){
        vals = levels
      }else{
        vals = levels[seqNdisplayR_session[['horizontal_panels_list']][[name]]] 
      }
      
      insertUI(
        selector = '#panel_horizontal_checkboxes',
        where = "afterEnd",
        ui = checkboxGroupInput(
          inputId = dataset_id,
          label = name,
          choices = levels,
          selected = vals
        )
      )
    }
    
    
    # panels fonts easy special case of expandable option upon enable ####
    ##@ ->this version of removeUI is defunct?? can be removed??
    # shiny_elems = grep(paste0('panel_font_easy_boxes_XvalueX', prev_session_idx), names(input), value=TRUE)
    # if (length(shiny_elems) > 0){
    #   for ( elem in shiny_elems ) {
    #     cat(elem, '\n') #@cat
    #     removeUI(selector = paste0('#', elem))
    #     shinyjs::runjs(paste0("Shiny.onInputChange(", elem, ", null)"))
    #   }
    #   #@removeUI(selector = '#panel_font_easy_boxes')
    # }##@ <-
    ##@ -> replace with this:
    if( CurrentSessionIdx() > 1){
      removeUI(selector = paste0("#panel_font_easy_boxes_container", CurrentSessionIdx()-1))
      shinyjs::runjs(paste0("Shiny.onInputChange(#panel_font_easy_boxes_container", CurrentSessionIdx()-1, ", null)"))
    }##@ <-
    
    dataset_group_depths = sapply(rev(dataset_names), function(name) ListDepth(seqNdisplayR_session$samples[[name]]) + 1)
    dataset_group_depth = max(dataset_group_depths)
    levels=c('First Panel', paste0('Inner Panel ', dataset_group_depth:1))
    cellwidths = rep(0, 2*length(levels)-1)
    nCells = length(cellwidths)
    #cat(paste0(option_par$shiny_varname, ': ', nCells), '\n') #@cat seq(1,nCells)
    cellspacers = seq(2, nCells, 2)
    boxes = seq(1, nCells, 2)
    width_unit = 1/(length(cellspacers)+4*length(boxes))
    cellwidths[cellspacers] = paste0(100*width_unit, '%')
    cellwidths[boxes] = paste0(400*width_unit, '%')
    optimaNvals = as.numeric(strsplit(as.character(global_options[which(global_options$option_name=='panel_font_sizes'),'option_options']), split=';')[[1]])
    optima = optimaNvals[1:2]
    if (is.null(seqNdisplayR_session$panel_font_sizes)){
      vals = rep(optimaNvals[3], length(levels))
    }else{
      vals = seqNdisplayR_session$panel_font_sizes
    }
    step = 1
    dataset_id = paste0('panel_font_easy_boxes_XvalueX', CurrentSessionIdx()) #@ tags$div(id = dataset_id,
    
    ##@ <- Latest fix for panel_font_easy UI element insertion:
    insertUI(selector = '#panel_font_easy_boxes',
             where = "afterEnd",
             ui = tags$div(id=paste0('panel_font_easy_boxes_container',CurrentSessionIdx()),
                           do.call(what=splitLayout, args = c(lapply(seq(1,nCells), split_sliders, dataset_id, levels, vals, optima, step),  #@ 'panel_font_easy'
                                                              list(cellWidths=as.list(cellwidths)),
                                                              list(width=list('500px')))))
    )
    ##@ <-
    
    ##@ ->
    #### panel font size list special case of expandable option upon enable
    
    optimaNvals = as.numeric(strsplit(as.character(global_options[which(global_options$option_name=='panel_font_size_list'),'option_options']), split=';')[[1]])
    optima = optimaNvals[1:2]
    max_levels = max(dataset_group_depth) + 1
    cellwidths = rep(0, 2*max_levels-1)
    nCells = length(cellwidths)
    #cat(paste0(option_par$shiny_varname, ': ', nCells), '\n') #@cat seq(1,nCells)
    cellspacers = seq(2, nCells, 2)
    boxes = seq(1, nCells, 2)
    width_unit = 1/(length(cellspacers)+4*length(boxes))
    cellwidths[cellspacers] = paste0(100*width_unit, '%')
    cellwidths[boxes] = paste0(400*width_unit, '%')
    
    ## the headers for each column
    ### remove from previous session
    if( CurrentSessionIdx() > 1){
      removeUI(selector = paste0("#panel_font_boxes_header_container", CurrentSessionIdx()-1))
      shinyjs::runjs(paste0("Shiny.onInputChange(#panel_font_boxes_header_container", CurrentSessionIdx()-1, ", null)"))
    }
    
    ### add new
    dataset_group_depth = max(dataset_group_depths)
    levels=c('First Panel', paste0('Inner Panel ', dataset_group_depth:1))
    slider_cells = which(seq_along(cellwidths) %% 2==1)
    insertUI(
      selector = '#panel_font_boxes_headers',
      where = "afterEnd",
      ui = tags$div(id=paste0('panel_font_boxes_header_container',CurrentSessionIdx()),
                    do.call(what=splitLayout, args = c(lapply(seq(1,nCells), split_headers, slider_cells, levels),
                                                       list(cellWidths=as.list(cellwidths)),
                                                       list(width=list('500px')))))
    )
    
    ## the individual rows of sliders
    ### remove from previous session
    if( CurrentSessionIdx() > 1){
      for(i in 1:length(dataset_names)){ #@ 1:10 #ups: rough since we at the moment don't keep track how many levels of divs there are to remove
        removeUI(selector = paste0("#panel_font_boxes_container", CurrentSessionIdx()-1))
        shinyjs::runjs(paste0("Shiny.onInputChange(#panel_font_boxes_container", CurrentSessionIdx()-1, ", null)"))
      }
    }
    
    ### build the new ones
    for ( name in rev(dataset_names) ) { #@ rev(dataset_names)
      dataset_group_depth = dataset_group_depths[[name]]
      levels=c('First Panel', paste0('Inner Panel ', dataset_group_depth:1))
      if (is.null(seqNdisplayR_session$panel_font_size_list) & is.null(seqNdisplayR_session$panel_font_sizes)){
        vals = rep(optimaNvals[3], length(levels))
      }else if (!is.null(seqNdisplayR_session$panel_font_size_list)){
        vals = seqNdisplayR_session$panel_font_size_list[[name]]
      }else{
        if (length(seqNdisplayR_session$panel_font_sizes) == length(levels)){
          vals = seqNdisplayR_session$panel_font_sizes
        }else{
          vals = c(seqNdisplayR_session$panel_font_sizes[1], rev(rev(seqNdisplayR_session$panel_font_sizes)[1:(length(levels)-1)]))
        }
      }
      slider_cells = which(seq_along(cellwidths) %% 2==1)
      if (length(vals) < max_levels){
        slider_cells = slider_cells[c(1, rev(rev(1:max_levels)[seq_along(length(levels)-1)]))]
      }
      step = 1 #@
      
      insertUI(selector = '#panel_font_boxes',
               where = "afterEnd",
               ui = tags$div(id=paste0('panel_font_boxes_container',CurrentSessionIdx()),
                             do.call(what=splitLayout, args = c(lapply(seq(1,nCells), split_sliders_panels, paste0('panel_font_', name), slider_cells, name, vals, optima, step),
                                                                list(cellWidths=as.list(cellwidths)),
                                                                list(width=list('500px')))))
      )
    }
    
    ##@ <-
    ##@ -> ##@@4 ->
    ## force scale special case of expandable option upon enable
    ### remove the old container
    if(CurrentSessionIdx() > 1){
      for(i in 1:length(dataset_names)){ #@ (1:10)
        removeUI(selector = paste0('#manual_scales_boxes_container', CurrentSessionIdx()-1))
        shinyjs::runjs(paste0("Shiny.onInputChange(#manual_scales_boxes_container", CurrentSessionIdx()-1, " null)"))
      }
    }
    ### collect general layout options
    optimaNvals = as.numeric(strsplit(as.character(dataset_options[which(dataset_options$option_name=='force_scale'),'option_options']), split=';')[[1]])
    optima = optimaNvals[1:2]
    start_val = ifelse(length(optimaNvals)==3, vals[3], mean(optimaNvals))
    max_levels = 2
    cellwidths = rep(0, 2*max_levels-1)
    nCells = length(cellwidths)
    #cat(paste0(option_par$shiny_varname, ': ', nCells), '\n') #@cat seq(1,nCells)
    cellspacers = seq(2, nCells, 2)
    boxes = seq(1, nCells, 2)
    width_unit = 1/(length(cellspacers)+4*length(boxes))
    cellwidths[cellspacers] = paste0(100*width_unit, '%')
    cellwidths[boxes] = paste0(400*width_unit, '%')
    step = 1
    
    ### build the new ones
    for ( name in rev(dataset_names) ) {
      #$ -->  condition added 230519
      levels = names(seqNdisplayR_session[['bigwigs']])[sapply(names(seqNdisplayR_session[['bigwigs']]), function(.strand) (name %in% names(seqNdisplayR_session[['bigwigs']][[.strand]])))]
      if ( any(!is.na(unlist(para))) ){ 
        if (!is.null(para[[name]])){
          vals = para[[name]]
          vals[is.na(vals)] = -1
        }
      }else{
        if (is.null(seqNdisplayR_session[['force_scale']])){
          vals = rep(-1, length(levels))
        }else if (is.null(seqNdisplayR_session[['force_scale']][[name]])){
          vals = rep(-1, length(levels))
        }else{
          vals = seqNdisplayR_session[['force_scale']][[name]]
          vals[which(is.na(vals))] = -1
        }
      }
      #$ <--
      input_cells = which(seq_along(cellwidths) %% 2==1)
      if (length(vals) < max_levels){
        input_cells = input_cells[1]
      }
      insertUI(
        selector = '#manual_scales_boxes',
        where = "afterEnd",
        ui = tags$div(id=paste0('manual_scales_boxes_container', CurrentSessionIdx()),
                      do.call(what=splitLayout, args = c(lapply(seq(1,nCells), split_numeric_input2, paste0(dataset_options[which(dataset_options$option_name=='force_scale'),'shiny_varname'], '_', CurrentSessionIdx(), '_', name), input_cells, paste(name, paste0('(', levels, ')'), sep = ' '), vals, optima, step),  #@ added " '_', CurrentSessionIdx(), "
                                                         list(cellWidths=as.list(cellwidths)),
                                                         list(width=list('500px')))))
      )
    }
    ##@ <- ##@@4 <-
    
    #### annotation options specific to each annotation
    anno_options <- options_table[options_table$option_group == 'annotation_option',]
    for ( i in 1:nrow(anno_options) ) {
      opt_line <- anno_options[i,]
      opt <- anno_options$option_name[i]
      opts <- seqNdisplayR_session[[opt]]
      
      if ( length(names(opts)) == 0 ) {
        opts <- opt_line$option_default
        names(opts) <- 'NA'
      }
      
      
      if ( opt_line$option_class == 'bool' ) {
        updateCheckboxGroupInput(session,
                                 opt_line$shiny_varname,
                                 choices = names(opts),
                                 selected = names(opts)[unlist(opts)])
      } else {
        
        ## idea is to remove for each sample name and line with sample name and parameter
        ## entries from previous loaded template are removed (except one, ie anchor)
        ## this anchor (or the dummy created before template loading) serve as anchor to add sample-lines below
        ## after this the anchor is removed
        prev_session_idx = CurrentSessionIdx() - 1
        anchor_elem <- opt_line$shiny_varname
        
        shiny_elems <- grep(paste0(anchor_elem, '_XvalueX', prev_session_idx, '_'), names(input), value=TRUE)
  
        if ( length(shiny_elems) != 0 ) {
          for ( elem in shiny_elems ) {
            if (grepl("^xyz_", elem)){
              elem = sub("^xyz_", "", elem)
            }
            removeUI(selector = paste0('#', elem))
          }
        }
        
        ##insert one text input per annotation name
        for ( name in rev(names(opts)) ) {
          #@ alt_name = gsub('\\s+', 'YvalueY', name) #@ colourInput does not take whitespace names apparantly
          alt_name = gsub('\\s+', paste0('Y', which(names(opts)==name), 'Y'), name) #@ colourInput does not take whitespace names apparantly
          #@ alt_name = gsub("[[:punct:]]", "ZvalueZ", alt_name)
          alt_name = gsub("[[:punct:]]", paste0('Z', which(names(opts)==name), 'Z'), alt_name)
          annot_id = paste0(opt_line$shiny_varname, '_XvalueX', CurrentSessionIdx(), '_', alt_name)
          if ( opt_line$option_class == 'text' ) {
            insertUI(
              selector = paste0('#', anchor_elem),
              where = "afterEnd",
              ui = tags$div(id=annot_id,
                            textInput(annot_id,
                                      label = name,
                                      value = DeparseOption(opts[[name]]))
              )
            )
          } else if ( opt_line$option_class == 'color' ) {
            val = ifelse(DeparseOption(opts[[name]])=='NULL', 'white', DeparseOption(opts[[name]]))
            insertUI(
              selector = paste0('#', anchor_elem), ##TODO: check style of anchor
              where = "afterEnd",
              ui = tags$div(id=annot_id,
                            colourpicker::colourInput(
                              inputId = paste0('xyz_',annot_id), 
                              label = name,
                              value = val,
                              allowTransparent = TRUE,
                              returnName = TRUE,
                              closeOnClick = TRUE))
            )
          } else if ( opt_line$option_class == 'numeric' ) {
            insertUI(
              selector = paste0('#', anchor_elem),
              where = "afterEnd",
              ui = tags$div(id=annot_id,
                            numericInput(inputId = annot_id,
                                         label = name,
                                         value = DeparseOption(opts[[name]]))
              )
            )
          } else if ( opt_line$option_class == 'text_choices' ) {
            opt_choices = strsplit(opt_line$option_options,',')[[1]]
            insertUI(
              selector = paste0('#', anchor_elem),
              where = "afterEnd",
              ui = tags$div(id = annot_id,
                            radioButtons(inputId = annot_id,
                                         label = name,
                                         choices = opt_choices,
                                         selected = DeparseOption(opts[[name]]),
                                         inline=TRUE)))
          }
        }
        #remove the anchor
        #if ( opt_line$option_class != 'color' ){ #@
        shinyjs::hide(id = anchor_elem)
        #}  #@
      }
    }
    
    # Track Colors ####
    opts = unlist(seqNdisplayR_session[['colors']])
    #cat(length(opts), '\n') #@
    
    prev_session_idx = CurrentSessionIdx() - 1
    anchor_elem = 'track_colors'
    
    shiny_elems = grep(paste0(anchor_elem, '_XvalueX', prev_session_idx, '_'), names(input), value=TRUE)
    #cat(length(shiny_elems), '\n') #@
    #cat(paste(shiny_elems, collapse='\n'), '\n')
    
    if ( length(shiny_elems) != 0 ) {
      for ( elem in shiny_elems ) {
        if (grepl("^xyz_", elem)){
          elem = sub("^xyz_", "", elem)
        }
        removeUI(selector = paste0('#', elem))
      }
    }

    ##insert one text input per track name
    for ( name in rev(names(opts)) ) {
      #@ alt_name = gsub('\\s+', 'YvalueY', name) #@ colourInput does not take whitespace names apparantly
      alt_name = gsub('\\s+', paste0('Y', which(names(opts)==name), 'Y'), name) #@ colourInput does not take whitespace names apparantly
      #@ alt_name = gsub("[[:punct:]]", "ZvalueZ", alt_name)
      alt_name = gsub("[[:punct:]]", paste0('Z', which(names(opts)==name), 'Z'), alt_name)
      color_id = paste0('track_colors', '_XvalueX', CurrentSessionIdx(), '_', alt_name)
      val = ifelse(DeparseOption(opts[[name]])=='NULL', 'black', DeparseOption(opts[[name]]))
      insertUI(
        selector = paste0('#', 'track_colors'), ##TODO: check style of anchor
        where = "afterEnd",
        ui = tags$div(id=color_id,
                      colourpicker::colourInput(
                        inputId = paste0('xyz_',color_id), 
                        label = name,
                        value = val,
                        allowTransparent = TRUE,
                        returnName = TRUE,
                        closeOnClick = TRUE))
      )
    }
    #remove the anchor
    shinyjs::hide(id = anchor_elem)
  }
  
  
  
  # reactive Excel or igv template load ####
  LoadTemplate = reactive({
    filename = input$input_file$name
    if ( is.null(filename) ) {
      return(NULL)
    }
    if ( !(grepl('.xml$', filename) | grepl('.xls$', filename) | grepl('.xlsx$', filename))  ) {
      output$console = renderText({'Please provide valid template file in .xls or .xlsx format'}) #@ 'Please provide valid template file in .xls, .xlsx or igv session .xml file'
      return(NULL)
    }
    if ( CurrentSessionFname() != filename ) {
      fname = input$input_file$datapath[1]
      show_modal_spinner(spin='circle', text='Loading and parsing template')
      loaded_session = NULL
      
      shinyCatch({
        if ( grepl('.xml$', fname)  ) {
          if ( is.null(input$igv_strand_regex) ) {
            igv_strand_regex = NULL
          } else {
            igv_strand_regex = list('+'=sub(",.*","", input$igv_strand_regex),
                                    '-'=sub(".*,","", input$igv_strand_regex))
          }
          x = capture.output(loaded_session = seqNdisplayR::LoadIGVSession(fname, group_by=input$igv_groupby, strand_regex=igv_strand_regex))
          output$console = renderText({paste(x, collapse  = "\n")})  
        } else if ( grepl('.xls$', filename) | grepl('.xlsx$', filename) ) {
          x <- capture.output(loaded_session <- seqNdisplayR::LoadExcel(fname, load_annotations = input$load_annotations))
          output$console = renderText({paste(x, collapse  = "\n")})
        } else {
          output$console = renderText({'Please provide valid template file in .xls, .xlsx or igv session .xml file'})
        }
        if ( !is.null(loaded_session) ){
          #fill all option field with values from session
          CurrentSessionIdx(CurrentSessionIdx() + 1)
          update_ui_to_session(loaded_session)
          #set reactive val CurrentSession to the loaded session!
          CurrentSessionFname(filename)
          CurrentSession(loaded_session)
        } else {
          loaded_session = CurrentSession()
        }
      }, position ='top-center', blocking_level ='message', shiny=T)
      
      remove_modal_spinner()
      
      #remember to return the session
      loaded_session
    } else {
      CurrentSession()
    }
  })
  
  
  
  # status message for import ####
  # Note: this also ensures that the session is loaded automatically!
  output$File_import_msg <- renderText(
    if( !is.null(LoadTemplate()) ) {
      if ( CurrentSessionFname() == '' | is.null(CurrentSession()) ){
        'Please load a valid session file.'
      } else {
        paste0('from ', CurrentSessionFname(), '\n',
               'loaded session with datasets: ',
               paste(names(CurrentSession()$samples), collapse = ', '),
               '\n')
      }
    }
  )
  
  
  
  # feature and locus retrieval ####
  GetFeature <- reactive(input$gene)
  
  GetLocus <- reactive({
    coord <- input$coordinates
    
    if ( coord != '' ) {
      coord = gsub(' ', '', coord)
      if (grepl(':+:', coord, fixed=TRUE)){
        strand = '+'
        strandless_coord = sub(':+:', ':', coord, fixed=TRUE)
      }else if (grepl(':-:', coord, fixed=TRUE)){
        strand = '-'
        strandless_coord = sub(':-:', ':', coord, fixed=TRUE)
      }else{
        strand = '+'
        strandless_coord = coord
      }
      if (grepl(',', strandless_coord, fixed=TRUE)){
        commaless_coord = gsub(',', '', strandless_coord) # paste(strsplit(strandless_coord, ',')[[1]], collapse='')
      }else{
        commaless_coord = strandless_coord
      }
      if (grepl('-', commaless_coord, fixed=TRUE)){
        dashNcommaless_coord = gsub('-', ':', commaless_coord) # paste(strsplit(commaless_coord, '-')[[1]], collapse=':')
      }else{
        dashNcommaless_coord = commaless_coord
      }
      configured_coord = strsplit(dashNcommaless_coord, ':')[[1]]
      c(configured_coord[1], strand, configured_coord[2:3])
    } else {
      coord
    }
    
  })
  
  # get all setting, options and parameters if changed in shiny ####
  ## called ie before plot or save
  GetShinyGlobalOptions <- reactive({
    opts <- options_table$option_name[options_table$option_group == 'global']
    
    l <- lapply(opts, function(opt) {
      opt_line <- options_table[options_table$option_name == opt,]
      shiny_varname <- opt_line$shiny_varname
      if ( any(grepl(paste0('^', shiny_varname), names(input))) ) {
        if (shiny_varname %in% names(input)){
          #cat(shiny_varname, '\n') #@ 2023-10-07
          value <- input[[shiny_varname]]
          #if (shiny_varname == "tracks_height"){ cat(value, '\n')} #@ 2023-10-07
        }else{
          var_names = grep(paste0('^', shiny_varname), names(input), value=TRUE)
          if (any(grepl('_subvar', var_names))){
            var_numbers = as.numeric(sapply(var_names, function(var_name) strsplit(var_name, split=paste0(shiny_varname, '_subvar'))[[1]][2]))
            re_order = order(var_numbers)
            var_names = var_names[re_order]
          }
          value <- lapply(var_names, function(var) input[[var]])
        }
        if ( opt_line$option_class == 'text' | opt_line$option_class == 'color') {
          value <- ParseOption(value) #@ 2022-10-07 seqNdisplayR::ParseOption
        }else if( opt_line$option_class == 'optional_numeric' ) {
          if (value){
            if (shiny_varname != 'binning_size' & shiny_varname != 'binning_start'){
              value <- input[[paste0(shiny_varname, '_slider')]]
            }else{
              value <- as.numeric(input[[paste0(shiny_varname, '_box')]]) #@ 2023-10-07
              #value <- as.numeric(input[[paste0(shiny_varname, '_slider')]])
            }
          }else{
            if (opt=="panels_max_width_cm" | opt=="scale_panel_width_cm" | opt=="bin_size"){
              value <- 'auto'
            }else{
              value <- NULL
            }
          }
          #@ ->
          #cat(paste(shiny_varname, '****', ifelse(is.null(value), 'NULL', value)), '\n')
          #@ <-
        }else if( opt_line$option_class == 'optional_text' ) {
          #cat(paste(shiny_varname, '****', input[[paste0(shiny_varname, '_box')]]), '\n') #@ 2023-10-07
          if (value){
            value <- input[[paste0(shiny_varname, '_box')]]
          }else{
            value <- NULL
          }
        }else if (opt_line$option_class=='split_numeric' | opt_line$option_class=='split_text' | opt_line$option_class=='split_color') {
          value = unlist(value)
        }else if (opt_line$option_class=='special_argument'){
          if (opt=='panel_font_sizes'){
            if (value){
              var_names = grep(paste0('^panel_font_easy_boxes_XvalueX', CurrentSessionIdx(), '_subvar'), names(input), value=TRUE)
              #cat(var_names, '\n') #@
              var_numbers = as.numeric(sapply(var_names, function(var_name) strsplit(var_name, split=paste0(shiny_varname, '_subvar'))[[1]][2]))
              #cat(var_numbers, '\n') #@
              #@ re_order = order(var_numbers) 
              #@ var_names = var_names[re_order]
              var_names = sort(var_names)
              #cat(var_names, '\n') #@
              value <- unlist(lapply(var_names, function(var) input[[var]]))
              #cat(value, '\n') #@
            }else{
              value = NULL
            }
          }else if (opt=='panel_font_size_list'){
            if (value){
              value = list()
              shiny_grps = grep('panel_font_easy', grep('panel_font_', names(input), value=TRUE), invert=TRUE, value=TRUE)
              shiny_grps2 = as.data.frame(do.call('rbind', strsplit(sub('panel_font_', '', shiny_grps), split='_subvar')))
              shiny_grps2[,2] = as.integer(shiny_grps2[,2])
              for ( dataset_name in unique(shiny_grps2[,1]) ) {
                sub_shiny_grps = which(shiny_grps2[,1]==dataset_name)
                sub_shiny_grps_ordered = sub_shiny_grps[order(shiny_grps2[sub_shiny_grps,2])]
                value[[dataset_name]] = structure(sapply(shiny_grps[sub_shiny_grps_ordered], function(shiny_grp) input[[shiny_grp]]), names=paste0("panel", 1:length(sub_shiny_grps_ordered)))
              }
            }else{
              value = NULL #@ list(NULL)
            }
          }else if (opt=='header_font_sizes'){
            if (value){
              var_names = grep(paste0('^', 'header_font_subvar'), names(input), value=TRUE)
              var_numbers = as.numeric(sapply(var_names, function(var_name) strsplit(var_name, split=paste0(shiny_varname, '_subvar'))[[1]][2]))
              re_order = order(var_numbers)
              var_names = var_names[re_order]
              value <- unlist(lapply(var_names, function(var) input[[var]]))
            }else{
              value = NULL
            }
          }else if (opt=='horizontal_panels_list'){
            if (value){
              value = list()
              shiny_chkbxgrps = names(input)[grepl(paste0('panel_horizontal_XvalueX', CurrentSessionIdx(), '_'), names(input))]
              #cat('horizontal_panels_list', '\n') #@cat
              #cat(shiny_chkbxgrps, '\n') #@cat
              for (dataset_name in names(CurrentSession()$samples)) {
                dataset_group_depth = ListDepth(CurrentSession()$samples[[dataset_name]]) + 1
                available_levels=c('First Panel', paste0('Inner Panel ', dataset_group_depth:1))
                #@ alt_name = gsub('\\s+', 'YvalueY', dataset_name)
                alt_name = gsub('\\s+', paste0('Y', which(names(CurrentSession()$samples)==dataset_name), 'Y'), dataset_name)
                #@ alt_name = gsub("[[:punct:]]", "ZvalueZ", alt_name)
                alt_name = gsub("[[:punct:]]", paste0('Z', which(names(CurrentSession()$samples)==dataset_name), 'Z'), alt_name)
                elem = paste0('panel_horizontal_XvalueX', CurrentSessionIdx(), '_', alt_name)
                checked_levels = input[[elem]]
                value[[dataset_name]] = (available_levels %in% checked_levels)
                #cat(paste0(dataset_name, ':', paste(value[[dataset_name]]), collapse=';'), '\n') #@cat
              }
            }else{
              value = NULL #@ list(NULL)
            }
          }
        }
        value
      } else {
        NULL
      }
    })
    names(l) <- opts
    
    ## special case for plotting_segment_order if bottom specified
    #cat(l$plotting_segment_order, '\n') #@cat
    if (!is.null(l$plotting_segment_order_bottom)){
      #cat(l$plotting_segment_order_bottom, '\n') #@cat
      l$plotting_segment_order = list('+'=l$plotting_segment_order, '-'=l$plotting_segment_order_bottom)
    }
    l
    
  })
  
  # get dataset options from shiny ####
  GetShinyDatasetOptions <- reactive({
    opts = options_table$option_name[options_table$option_group == 'dataset_option']
    opts = setdiff(opts, 'tracks_colors') #@ added 2023-05-29
    datasets = names(CurrentSession()$samples)
    #@ datasets_NSC = sapply(datasets, function(name) gsub('\\s+', 'YvalueY', name))
    datasets_NSC = sapply(datasets, function(name) gsub('\\s+', paste0('Y', which(datasets==name), 'Y'), name))
    #@ datasets_NSC = sapply(datasets_NSC, function(name) gsub("[[:punct:]]", "ZvalueZ", name))
    datasets_NSC = sapply(datasets_NSC, function(name) gsub("[[:punct:]]", paste0('Z', which(datasets_NSC==name), 'Z'), name))
    l <- lapply(opts, function(opt) {
      opt_line <- options_table[options_table$option_name == opt,]
      shiny_varname <- opt_line$shiny_varname
      if ( opt_line$option_class == 'bool' ) {
        selected <- input[[shiny_varname]]
        res <- (datasets %in% selected)
        names(res) <- datasets
        res
      } else if(opt_line$option_class == 'text_choices') {
        res = sapply(datasets_NSC,
                     function(dataset) {
                       ParseOption(input[[paste0(opt_line$shiny_varname,'_XvalueX',CurrentSessionIdx(), '_', dataset)]]) 
                     })
        names(res) <- datasets
        res
      } else if(opt_line$option_class == 'special_argument') {
        #cat(opt, '\n') #@cat
        #cat(shiny_varname, '\n') #@cat
        if (input[[shiny_varname]]){
          #cat('TRUE', '\n') #@cat
          res = list()
          shiny_grps = grep(paste0(shiny_varname, '_',CurrentSessionIdx(), '_'), names(input), value=TRUE)
          #@shiny_grps = grep(paste0(shiny_varname, '_XvalueX',CurrentSessionIdx(), '_'), names(input), value=TRUE)
          #cat(shiny_grps, '\n') #@cat
          #@shiny_grps1 = grep(paste0(shiny_varname), names(input), value=TRUE) #@'manual_scales_boxes_container'
          #cat(shiny_grps1, '\n') #@cat
          #@shiny_grps2 = as.data.frame(do.call('rbind', strsplit(sub(paste0(shiny_varname, '_XvalueX',CurrentSessionIdx(), '_'), '', shiny_grps), split='_subvar')))
          shiny_grps2 = as.data.frame(do.call('rbind', strsplit(sub(paste0(shiny_varname, '_',CurrentSessionIdx(), '_'), '', shiny_grps), split='_subvar')))
          shiny_grps2[,2] = as.integer(shiny_grps2[,2])
          for ( dataset_name in unique(shiny_grps2[,1]) ) {
            sub_shiny_grps = which(shiny_grps2[,1]==dataset_name)
            sub_shiny_grps_ordered = sub_shiny_grps[order(shiny_grps2[sub_shiny_grps,2])]
            sub_res = as.numeric(sapply(shiny_grps[sub_shiny_grps_ordered], function(shiny_grp) as.numeric(input[[shiny_grp]])))
            sub_res[sign(sub_res)==-1] = NA
            res[[dataset_name]] = paste(sub_res, collapse=',')
          }
          res = unlist(res)
        }else{
          #cat('FALSE', '\n') #@cat
          res = unlist(sapply(datasets, function(dataset) paste(c(NA, NA), collapse=',')))
        }
        res
      } else {
        res <- sapply(datasets_NSC,
                      function(dataset) {
                        ParseOption(input[[paste0(opt_line$shiny_varname, '_XvalueX', CurrentSessionIdx(), '_', dataset)]])
                      })
        names(res) <- datasets
        res
      }
    })
    
    names(l) <- opts
    l
  })
  
  
  # get annotation options from shiny ####
  GetShinyAnnotationOptions <- reactive({
    opts <- options_table$option_name[options_table$option_group == 'annotation_option']
    anno_names <- names(CurrentSession()$annots)
    #@ anno_names_NSC = sapply(anno_names, function(name) gsub('\\s+', 'YvalueY', name))
    anno_names_NSC = sapply(anno_names, function(name) gsub('\\s+', paste0('Y', which(anno_names==name), 'Y'), name))
    #@ anno_names_NSC = sapply(anno_names_NSC, function(name) gsub("[[:punct:]]", "ZvalueZ", name))
    anno_names_NSC = sapply(anno_names_NSC, function(name) gsub("[[:punct:]]", paste0('Z', which(anno_names_NSC==name), 'Z'), name))
    if( length(CurrentSession()$annots) == 0 ){
      return(NULL)
    }
    l <- lapply(opts, function(opt) {
      opt_line <- options_table[options_table$option_name == opt,]
      shiny_varname <- opt_line$shiny_varname
      if ( opt_line$option_class == 'bool' ) {
        checked <- input[[shiny_varname]]
        res <- (anno_names %in% checked)
        names(res) <- anno_names
        res
      } else if(opt_line$option_class == 'text_choices') {
        res <- sapply(anno_names_NSC,
                      function(anno) {
                        ParseOption(input[[paste0(opt_line$shiny_varname, '_XvalueX', CurrentSessionIdx(), '_',anno)]])
                      })
        names(res) <- anno_names
        res
      }else if (opt_line$option_class == 'color'){ ###@@@
        #alt_names = as.character(sapply(anno_names, function(name) gsub('\\s+', 'YvalueY', name))) #@ colourInput does not take whitespace names apparantly
        res <- sapply(anno_names_NSC,
                      function(anno) {
                        ParseOption(input[[paste0('xyz_', opt_line$shiny_varname, '_XvalueX', CurrentSessionIdx(), '_',anno)]])
                      })
        names(res) <- anno_names
        if (any(res=='white')){
          res[res=='white'] = 'NULL'
        }
        #cat(unlist(res), '\n') #@cat
        res
      }else{
        res <- sapply(anno_names_NSC,
                      function(anno) {
                        ParseOption(input[[paste0(opt_line$shiny_varname, '_XvalueX', CurrentSessionIdx(), '_',anno)]])
                      })
        names(res) <- anno_names
        res
      }
    })
    
    names(l) <- opts
    #@ ->
    # cat('annos', '\n')
    # for (lname in names(l)){
    #   cat(lname, '\n')
    #   v = l[[lname]]
    #   cat(paste0(names(v), ':', v), '\n')
    # }
    #@ <-
    l
  })
  
  
  # Track Colors ####
  # get Track Colors from shiny ####
  GetShinyTrackColors <- reactive({
    sample_names = names(CurrentSession()$samples)
    datasets = rlist::list.flatten(CurrentSession()$samples, use.names = TRUE, classes = "ANY")
    datasets_names = names(datasets)
    datasets_unlisted = structure(unlist(datasets), names=paste(rep(datasets_names, lengths(datasets)), unlist(datasets), sep='.'))
    #@ datasets_NSC = sapply(names(datasets_unlisted), function(name) gsub('\\s+', 'YvalueY', name))
    datasets_NSC = sapply(names(datasets_unlisted), function(name) gsub('\\s+', paste0('Y', which(names(datasets_unlisted)==name), 'Y'), name))
    #@ datasets_NSC = sapply(datasets_NSC, function(name) gsub("[[:punct:]]", "ZvalueZ", name))
    datasets_NSC = sapply(datasets_NSC, function(name) gsub("[[:punct:]]", paste0('Z', which(datasets_NSC==name), 'Z'), name))
    datasets_NSC =  structure(paste0('xyz_track_colors', '_XvalueX', CurrentSessionIdx(), '_', datasets_NSC), names=names(datasets_unlisted))
    res = sapply(as.character(datasets_NSC),
                  function(dataset) {
                    input[[dataset]]
                  })
    names(res) = names(datasets_unlisted)
    #cat(paste0('c("', paste(names(datasets_unlisted), collapse='", "'), '")'), '\n') #@ 2023-09-25 test
    #sample_matrix = do.call('rbind', sapply(names(datasets_unlisted), function(sep) strsplit(sep, split='.', fixed=T))) #@ 2023-09-25 no use
    l = lapply(sample_names, function(sample_name){
      sub_samps = grep(sample_name, names(datasets_unlisted), value=TRUE)
      subsample_matrix = do.call('rbind', sapply(sub_samps, function(sep) strsplit(sep, split='.', fixed=T)))
      subsample_matrix = as.data.frame(cbind(res[rownames(subsample_matrix)], subsample_matrix))
      colnames(subsample_matrix) = c('color', 'dataset', paste0('subgroup_', 1:(ncol(subsample_matrix)-2)))
      subsample_matrix
    })
    max_subgroups = max(sapply(l, function(df) ncol(df))) - 2
    sample_matrix_list = lapply(l, function(df) if (ncol(df)-2 < max_subgroups){ extra_cols = max_subgroups - (ncol(df)-2); 
                                                                                 extra_df = as.data.frame(matrix(NA, ncol=extra_cols, nrow=nrow(df)));
                                                                                 df = cbind(df, extra_df); 
                                                                                 colnames(df) = c('color', 'dataset', paste0('subgroup_', 1:(ncol(df)-2))); 
                                                                                 df
                                                                                }else{ df }  
      )
    sample_matrix = do.call("rbind", sample_matrix_list)
    GetColors(sample_matrix)
  })
  
  
  # build session from shiny elements ####
  seqNdisplayR_session <- reactive({
    template_session <- LoadTemplate()
    if (length(template_session$annots) == 0) {
      template_session['annots'] <- list(NULL)
    }
    
    track_colors = GetShinyTrackColors() ##@@
    template_session[['colors']] = track_colors ##@@
    
    shiny_session_global_options = GetShinyGlobalOptions()
    
    op_names = names(shiny_session_global_options)
    
    #@ template_session[op_names] = shiny_session_global_options[op_names]
    #@ ->  2023-06-19
    for (op_name in op_names){
      if (shiny_session_global_options[op_name]=='NULL'){
        template_session[op_name] = list(NULL)
        #cat(paste(op_name, '****', 'NULL*'), '\n')
      }else if (shiny_session_global_options[op_name]=='FALSE'){
        template_session[op_name] = FALSE
        #cat(paste(op_name, '****', 'FALSE*'), '\n')
      }else if (shiny_session_global_options[op_name]=='TRUE'){
        template_session[op_name] = TRUE
        #cat(paste(op_name, '****', 'TRUE*'), '\n')
      }else if (shiny_session_global_options[op_name]=='NA'){
        template_session[op_name] = NA
        #cat(paste(op_name, '****', 'NA*'), '\n')
      }else{
        #cat(op_name, '\n') #@ 2023-10-07
        #cat(shiny_session_global_options$track_height_cm, '\n') #@ 2023-10-07
        template_session[op_name] = shiny_session_global_options[op_name]
        #cat(paste(op_name, '****', template_session[op_name]), '\n')
        if (op_name == 'horizontal_panels_list'){
          template_session[op_name] = shiny_session_global_options[op_name]
          #cat(paste(op_name, '****'), '\n') #@ 2023-12-18
          #cat(paste0(names(template_session[[op_name]]), ': ', template_session[[op_name]]), '\n') #@ 2023-12-18
        }
      }
    }
    #@ <- 2023-06-19
    shiny_session_dataset_options <- GetShinyDatasetOptions()
    for ( sample_name in names(template_session$parameters) ) {
      alt_name = gsub('\\s+', paste0('Y', which(names(template_session$parameters)==sample_name), 'Y'), sample_name)
      alt_name = gsub("[[:punct:]]", paste0('Z', which(names(template_session$parameters)==sample_name), 'Z'), alt_name)
      for ( op in names(shiny_session_dataset_options) ) {
        opt = shiny_session_dataset_options[[op]]
        if ( sample_name %in% names(opt) ) {
          template_session$parameters[[sample_name]][[op]] = opt[[sample_name]]
        } else if ( alt_name %in% names(opt) ) {
          template_session$parameters[[sample_name]][[op]] = opt[[alt_name]]
        } else {
          template_session$parameters[[sample_name]][[op]] = FALSE
        }
      }
      for (op in c('horizontal_panels_list', 'panel_font_size_list')){
        if ( alt_name %in% names(template_session[[op]]) & alt_name != sample_name) { #@ 2023-12-18
          template_session[[op]][[sample_name]] = template_session[[op]][[alt_name]] 
          template_session[[op]][[alt_name]] = NULL
        }
      }
    }
    shiny_session_annotation_options <- GetShinyAnnotationOptions()
    if( !is.null(shiny_session_annotation_options) ) {
      op_names <- names(shiny_session_annotation_options)
      template_session[op_names] <- shiny_session_annotation_options[op_names]
    }
    
    ## which samples from tree
    which_samples <- GetSelectedSamples()
    #@ -> 2023-10-07
    if (is.null(which_samples)){
      which_samples = lapply(template_session$parameters, function(p) p$whichSamples)
      names(which_samples) <- names(template_session$parameters)
    }
    #@ <- 2023-10-07
    for ( sample_name in names(template_session$parameters) ) {
      #cat(paste('***', sample_name, '-', paste(which_samples[sample_name], collapse=', ')), '\n') #@ 2023-10-07
      if ( !(sample_name %in% names(which_samples)) ) {
        #exclude all
        template_session$parameters[[sample_name]][['whichSamples']] = NA
      } else if (identical(which_samples[[sample_name]], template_session$samples[[sample_name]])){
        #include all
        template_session$parameters[[sample_name]]['whichSamples'] = list(NULL)
      } else { #@
        #include specific cases
        template_session$parameters[[sample_name]][['whichSamples']] = which_samples[[sample_name]]
      }
    }
    
    output$console <- renderText({textLog()})
    
    template_session
  })
  
  
  
  # create plot when hitting plot button ####
  observeEvent(input$plot,
               {
                 if (is.null(input$input_file)) {
                   "Please load sNdR sample file and provide locus name or coordinates." #@ "Please load Excel or IGV template and provide locus name or coordinates."
                 } else {
                   feature <- GetFeature()
                   locus <- GetLocus()
                   if (feature == '' & locus[1] == '') {
                     output$console <- renderText({"Please provide locus name or coordinates for region to be plotted."})
                   } else {
                     session_to_plot <- seqNdisplayR_session()
                     #cat(paste0(names(session_to_plot[['horizontal_panels_list']]), ': ', session_to_plot[['horizontal_panels_list']]), '\n') #@ 2023-12-18
                     show_modal_spinner(spin='circle', text='Creating plot, please be patient. The plot will appear in a separate window')
                     x <- 'Plotting failed, please check your settings'
                     spsComps::shinyCatch({
                       if (feature != '') {
                         x <- capture.output(plot(session_to_plot, feature=feature, interface='shiny')) ##@@ , file='error_output.txt'
                       } else if (locus[1] != '') {
                         x <- capture.output(plot(session_to_plot, locus=locus, interface='shiny'))
                       }
                     },position = 'top-center',blocking_level='none', prefix='Plotting error', shiny=TRUE)
                     output$console <- renderText({paste(x, collapse  = "\n")})
                     remove_modal_spinner()
                   }
                 }
               })
  
  Rver_major = as.integer(R.version$major)
  Rver_minor = as.numeric(R.version$minor)
  new_R = Rver_major >= 4 & Rver_minor >= 3
  content_type = if (new_R) {"application/pdf"}else{NULL}
  output$save_pdf <- shiny::downloadHandler(
    filename = function() {
      feature = GetFeature()
      locus = GetLocus()
      if (locus[1] != ''){ #@ 2023-06-21 locus[1] <- locus
        locus[2] = ifelse(locus[2]=='+', 'plus', 'minus')
        locus_string = paste(locus, collapse='_')
      }else{
        locus_string = ''
      }
      if ( feature == ''  & locus[1] == '' ) {
        output$console = renderText({"Please provide locus name or coordinates for region to be plotted."})
      }else if ( feature != '' ){
        paste0("seqNdisplayR_", Sys.Date(), '_', feature, ifelse(new_R, '.pdf', ''))
      }else{
        paste0("seqNdisplayR_", Sys.Date(), '_', locus_string, ifelse(new_R, '.pdf', ''))
      }
    },
    content = function(file) {
      if ( is.null(input$input_file) ) {
        output$console = renderText({"Please load sNdR sample file and provide locus name or coordinates."}) #@ "Please load Excel or IGV template and provide locus name or coordinates."
      } else {
        feature = GetFeature()
        locus = GetLocus()
        if (feature == '' & locus[1] == '') {
          output$console = renderText({"Please provide locus name or coordinates for region to be plotted."})
        } else {
          session_to_plot = seqNdisplayR_session()
          show_modal_spinner(spin='circle', text=paste('Plotting to pdf, please be patient'))
          x = 'plotting failed, please check your settings'
          spsComps::shinyCatch({
            if (feature != '') {
              x <- capture.output(plot(session_to_plot, feature=feature, interface='shiny',
                                       pdf = TRUE,
                                       pdf_name = ifelse(new_R, basename(file), sub('.pdf', '', basename(file))), #@ 2022-10-11 #@ 2022-10-26 sub('.pdf', '', basename(file)) #@ 2023-05-22 basename(file)
                                       pdf_dir = dirname(file)))
            } else if (locus[1] != '') {
              x <- capture.output(plot(session_to_plot, locus=locus, interface='shiny',
                                       pdf = TRUE,
                                       pdf_name = ifelse(new_R, basename(file), sub('.pdf', '', basename(file))), #@ 2022-10-11 #@ 2022-10-26 sub('.pdf', '', basename(file)) #@ 2023-05-22 basename(file)
                                       pdf_dir = dirname(file)))
            }
          }, position = 'top-center', blocking_level='none', prefix='Plotting error', shiny=TRUE)
          output$console = renderText({paste0('PDF creation log:\n', paste(x, collapse  = "\n"))}) #@ 2022-10-10 pdfdir, pdfname <- file  #@ 2022-10-26 basename(file), '.pdf' <- pdfname
          remove_modal_spinner()
        }
      }
    },
    contentType = content_type #@ ifelse(new_R, "application/pdf", NULL)
  )

  # save settings to excel file --> when hitting save button ####
  output$save_settings <- shiny::downloadHandler(
    filename = function() {
      paste0("seqNdisplayRsession_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      #cat(file, '\n') #@ 2022-10-10
      if ( is.null(input$input_file) ) {
        output$console = renderText("Please first load sNdR sample file") #@ "Please first load Excel or IGV template"
      } else {
        loaded_session = seqNdisplayR_session()
        seqNdisplayR::Session2xlsx(loaded_session, path = file)
      }
    }
  )
  
  
  # Various information buttons for debugging ####
  # TO DO: remove debugging buttons
  observeEvent(input$show_settings, {
    shiny_session_global_options <- GetShinyGlobalOptions()
    shiny_session_annotation_options <- GetShinyAnnotationOptions()
    if ( !is.null(shiny_session_annotation_options) ) {
      opt_names <- c(names(shiny_session_global_options), names(shiny_session_annotation_options))
    } else {
      opt_names <- names(shiny_session_global_options)
    }
    
    cur_session <- seqNdisplayR_session()
    
    output$console <- renderText(
      paste(
        lapply(intersect(opt_names,names(cur_session)),
               function(i) paste0(i, '  -->  ', DeparseOption(cur_session[[i]]),  '\t', class(cur_session[[i]]))  #@ 2022-10-07 seqNdisplayR::DeparseOption
        ),
        collapse ='\n')
    )
  })
  
  observeEvent(input$show_parameters, {
    template_session <- LoadTemplate()
    shiny_session_dataset_options <- GetShinyDatasetOptions()
    
    textLog('dataset options:\n')
    for ( para in names(shiny_session_dataset_options) ) {
      textLog(paste0(textLog(), para, '\n'))
      for ( sample_name in names(shiny_session_dataset_options[[para]]) ) {
        textLog(paste0(textLog(), '  ', sample_name, ': ', DeparseOption(shiny_session_dataset_options[[para]][[sample_name]]), '\n')) #@ 2022-10-07 seqNdisplayR::DeparseOption
      }
    }
    output$console <- renderText( textLog() )
  })
  
  observeEvent(input$show_samples, {
    output$console  <- renderPrint({
      CurrentSession()$samples
    })
  })
  
  observeEvent(input$which_samples, {
    output$console  <- renderPrint({
      GetSelectedSamples()
    })
  })
  
  
  # Convenience buttons for shiny control #### 
  observeEvent(input$clean_console, {
    output$console = renderText('')
  })
  
  observeEvent(input$reset, {
    CurrentSessionFname('')
    LoadTemplate()
    output$console  <- renderPrint('All values were reset to template values')
  })
  
  observeEvent(input$reload_app, {
    refresh()
  })
  
  observeEvent(input$debug, {
    output$console  <- renderPrint(CurrentSession()$annot_panel_color)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server, options = list(width = 1600, height = 2000)) #@ keep an eye on options

