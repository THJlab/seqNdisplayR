# Utility functions

#' Is Color
#'
#' @description Internal function: 
#' Helper function that determines whether input (vector) is a color
#'
#' @keywords internal
#'
#' @author SLA (with help from https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation/13290832#13290832)
#'
#' @param put_color_vector putative vector of colors
#'
#' @return placeholder
#' 
#' @importFrom grDevices col2rgb rgb
#'
#' @examples
#' IsColor('black')
#' IsColor('white')
#' IsColor('gnyf')
#' IsColor(c('white', 'blue', 'gnyf'))
#' IsColor('#0098AD')
#' 
IsColor = function(put_color_vector) {
  sapply(put_color_vector, function(.put.color) {
    tryCatch(is.matrix(grDevices::col2rgb(.put.color)),
             error = function(e) FALSE)
  })
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
#' @return placeholder
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


#' Delete NULLs
#'
#' @description Internal function: 
#' Delele null/empty entries in a list
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param x_list 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
DeleteNULLs  =  function(x_list){
  x_list[unlist(lapply(x_list, length) != 0)]
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
#' @return placeholder
#'
#' @examples
#' NULL
#' 
OrderedSplit = function(x, f, drop=TRUE){
  spl = split(x, f, drop)
  spl[unique(as.character(f))]
}


#' Common Prefix
#'
#' @description Internal function: 
#' Find common prefix
#'
#' @keywords internal
#'
#' @author MS
#'
#' @param vector_of_strings 
#'
#' @return A single string which is a common prefix in all strings
#'
#' @examples
#' NULL
#' 
CommonPrefix = function(vector_of_strings) {
  if ( length(vector_of_strings) == 1 ) {
    ''
  } else {
    str1 = vector_of_strings[1]
    
    eq_chars = sapply(1:nchar(str1), function(i)
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


#' Print Output
#'
#' @description Internal function
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param messages a list with messages stored under 'output', 'warnings' and 'errors' 
#' @param verbosity indicated by an integer 0 to 3 referring to the levels 'off', 'no warnings', 'normal', and 'detailed'
#' @param no_line_change boolean whether a string of warning messages should be printed on the same line (TRUE) or different lines (FALSE)
#' @param caller optional string identifying which function generated the messages (for log)
#'
#' @return print output, warnings and errors based on messages list
#'
#' @examples
#' messages = list('output'=list('This is the output string'), 'errors'=list('This is an error string'), 'warnings'=list('This is a warning string'))
#' PrintOutput(messages, verbosity=0)
#' PrintOutput(messages, verbosity=1)
#' PrintOutput(messages, verbosity=2)
#' 
#' messages2 = list('output'=list('This is the output string and no errors or warnings'), 'errors'=list(), 'warnings'=list())
#' PrintOutput(messages2, verbosity=2)
#' 
PrintOutput = function(messages, verbosity, no_line_change=FALSE, caller=NULL){
  # Log all messages regardless of verbosity
  .log_messages(messages, caller)
  
  if (length(messages[['output']]) > 0 & verbosity>0){
    cat(paste(unlist(messages[['output']]), collapse='\n'), '\n')
  }
  if (length(messages[['warnings']]) > 0 & verbosity>1){
    cat('WARNING(s):', '\n')
    if (length(messages[['warnings']]) > 1){
      cat(paste(unlist(messages[['warnings']]), collapse='\n'), '\n')
    }else{
      cat(messages[['warnings']][[1]], ifelse(no_line_change, '', '\n'))
    }
  }
  if (length(messages[['errors']]) > 0 & verbosity>0){
    cat('ERROR(s):', '\n')
    cat(paste(unlist(messages[['errors']]), collapse='\n'), '\n')
  }
}


# ---- Package-level message log ----------------------------------------------
# Internal environment for storing the session log
.sNdR_log_env <- new.env(parent = emptyenv())
.sNdR_log_env$log <- list()
.sNdR_log_env$enabled <- TRUE

#' Log messages internally (called by PrintOutput)
#' @keywords internal
.log_messages = function(messages, caller = NULL) {
  if (!.sNdR_log_env$enabled) return(invisible(NULL))
  timestamp <- format(Sys.time(), "%H:%M:%S")
  caller_str <- if (!is.null(caller)) caller else ""
  
  for (type in c('output', 'warnings', 'errors')) {
    if (length(messages[[type]]) > 0) {
      for (msg in messages[[type]]) {
        .sNdR_log_env$log[[length(.sNdR_log_env$log) + 1]] <- list(
          time = timestamp,
          type = type,
          caller = caller_str,
          message = msg
        )
      }
    }
  }
}


#' View seqNdisplayR Message Log
#'
#' @description Developer tool: view all messages (output, warnings, errors)
#'   generated during the current session, regardless of verbosity settings.
#'   This is useful for troubleshooting when verbosity was set low during
#'   a plot call.
#'
#' @param type Filter by message type: 'all' (default), 'output', 'warnings', or 'errors'
#' @param last Show only the last N messages. NULL (default) shows all.
#' @param clear If TRUE, clear the log after displaying it. Default FALSE.
#'
#' @return Invisibly returns the log entries as a data.frame
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # After a plot call with verbosity='off':
#' sNdR_log()                  # see everything that happened
#' sNdR_log(type = 'errors')   # see only errors
#' sNdR_log(type = 'warnings') # see only warnings
#' sNdR_log(last = 20)         # see last 20 messages
#' sNdR_log(clear = TRUE)      # view and clear
#' }
#'
sNdR_log = function(type = 'all', last = NULL, clear = FALSE) {
  log <- .sNdR_log_env$log
  
  if (length(log) == 0) {
    cat("Log is empty.\n")
    return(invisible(data.frame()))
  }
  
  df <- data.frame(
    time    = sapply(log, `[[`, 'time'),
    type    = sapply(log, `[[`, 'type'),
    caller  = sapply(log, `[[`, 'caller'),
    message = sapply(log, `[[`, 'message'),
    stringsAsFactors = FALSE
  )
  
  if (type != 'all') {
    df <- df[df$type == type, , drop = FALSE]
  }
  
  if (!is.null(last) && nrow(df) > last) {
    df <- df[(nrow(df) - last + 1):nrow(df), , drop = FALSE]
  }
  
  if (nrow(df) == 0) {
    cat("No", if (type != 'all') type else "", "messages in log.\n")
  } else {
    # Pretty print
    for (i in seq_len(nrow(df))) {
      prefix <- switch(df$type[i],
        'errors'   = '[ERROR]  ',
        'warnings' = '[WARN]   ',
        'output'   = '[INFO]   '
      )
      caller_str <- if (df$caller[i] != "") paste0(" (", df$caller[i], ")") else ""
      cat(sprintf("%s %s%s %s\n", df$time[i], prefix, caller_str, df$message[i]))
    }
    cat(sprintf("\n--- %d messages (%d errors, %d warnings, %d info) ---\n",
                nrow(df),
                sum(df$type == 'errors'),
                sum(df$type == 'warnings'),
                sum(df$type == 'output')))
  }
  
  if (clear) {
    .sNdR_log_env$log <- list()
    cat("Log cleared.\n")
  }
  
  invisible(df)
}


#' Clear seqNdisplayR Message Log
#'
#' @description Developer tool: clear the internal message log.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sNdR_clear_log()
#' }
#'
sNdR_clear_log = function() {
  .sNdR_log_env$log <- list()
  invisible(NULL)
}


#' Initialize a Messages List
#'
#' @description Internal helper: create a fresh messages list for use
#'   with the PrintOutput pattern.
#'
#' @keywords internal
#'
#' @return a list with empty output, warnings and errors slots
#'
NewMessages = function() {
  list('output' = list(), 'warnings' = list(), 'errors' = list())
}


#' Add a Message to a Messages List
#'
#' @description Internal helper: append a message to the appropriate slot
#'   of a messages list.
#'
#' @keywords internal
#'
#' @param messages a messages list (from NewMessages)
#' @param type one of 'output', 'warnings', 'errors'
#' @param msg the message string
#'
#' @return the updated messages list
#'
AddMsg = function(messages, type, msg) {
  messages[[type]][[length(messages[[type]]) + 1]] <- msg
  messages
}


#' Numbering Spacers
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param stranded_list 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
NumberingSpacers = function(stranded_list){
  .index = 0
  if (!is.null(stranded_list[['+']])){
    .plus.spacers = grep('spacer', stranded_list[['+']], fixed=TRUE)
    if (length(.plus.spacers) > 0){
      stranded_list[['+']][.plus.spacers] = paste(stranded_list[['+']][.plus.spacers], .index + 1:length(.plus.spacers), sep='')
      .index = length(.plus.spacers)
    }
  }
  if (!is.null(stranded_list[['-']])){
    .minus.spacers = grep('spacer', stranded_list[['-']], fixed=TRUE)
    if (length(.minus.spacers) > 0){
      stranded_list[['-']][.minus.spacers] = paste(stranded_list[['-']][.minus.spacers], .index + 1:length(.minus.spacers), sep='')
    }
  }
  return(stranded_list)
}


#' Empty 2 Null
#'
#' @description Internal function: 
#' Helper used for parsing parameters$<dataset>$whichSamples
#' From chatGPT: The purpose of this function is to convert empty elements within a list to NULL. If an element is not a list or has a length of 0, it remains unchanged. However, if an element is a non-empty list, the function recursively processes each nested element to convert empty elements to NULL.
#'
#' @keywords internal
#'
#' @author MS
#'
#' @param x 
#'
#' @return placeholder
#' 
#' @examples
#' NULL
#' 
Empty2Null = function(x){
  if ( !is.list(x) ) {
    x
  } else if ( length(x) == 0 ) {
    NULL
  } else {
    lapply(x, Empty2Null)
  }
}


#' Flatten Nested List
#'
#' @description Internal function:
#' Recursively flatten a nested list into a single-level named list.
#' Base R replacement for rlist::list.flatten.
#' Names are concatenated with "." to preserve hierarchy
#' (e.g. list(a = list(b = 1)) becomes list("a.b" = 1)).
#'
#' @keywords internal
#'
#' @author SLA
#'
#' @param x a (nested) list
#' @param prefix internal parameter for recursion (do not set manually)
#'
#' @return a flat list with concatenated names
#'
#' @examples
#' l <- list(a = list(b = 1, c = 2), d = 3)
#' ListFlatten(l)
#' # $a.b  [1] 1
#' # $a.c  [1] 2
#' # $d    [1] 3
#'
ListFlatten = function(x, prefix = "") {
  if (!is.list(x)) return(list(x))
  out <- list()
  for (nm in names(x)) {
    full_name <- if (prefix == "") nm else paste(prefix, nm, sep = ".")
    val <- x[[nm]]
    if (is.list(val) && !is.data.frame(val)) {
      flat <- ListFlatten(val, prefix = full_name)
      out <- c(out, flat)
    } else {
      out[[full_name]] <- val
    }
  }
  out
}


#' Bind Rows with Fill
#'
#' @description Internal function:
#' Bind a list of data frames by row, filling missing columns with NA.
#' Base R replacement for dplyr::bind_rows.
#'
#' @keywords internal
#'
#' @param df_list a list of data frames
#'
#' @return a single data frame
#'
RbindFill = function(df_list) {
  df_list <- df_list[!sapply(df_list, is.null)]
  if (length(df_list) == 0) return(data.frame())
  if (length(df_list) == 1) return(df_list[[1]])
  all_cols <- unique(unlist(lapply(df_list, colnames)))
  df_list <- lapply(df_list, function(df) {
    missing <- setdiff(all_cols, colnames(df))
    if (length(missing) > 0) {
      df[missing] <- NA
    }
    df[, all_cols, drop = FALSE]
  })
  do.call(rbind, df_list)
}
