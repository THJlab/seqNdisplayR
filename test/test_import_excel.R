xl_fname <- 'inst/extdata/example_excel_template.xlsx'
xl_fname <- 'inst/extdata/minimal_example_excel_template.xlsx'


session <- load_excel(xl_fname, load_annotations = F)
# takes some time, since annotations are loaded...

session

print(session, verbose=T)


plot(session, feature='TAF1D', dummy_plot=T)
plot(session, locus=c('chr1','+',100,1000), dummy_plot=T) #chr1:+:100:1000

plot(session, feature='TAF1D', dummy_plot=T,horizontal_panels_list = list("TT-seq"=c(T,F), "RNA-seq"=c(T,F), "3-seq"=c(T,T,T,T), "ChIP-seq"=c(T,T)))


session$replicate_names <- NULL
plot(session, feature='TAF1D')





## From Seq2PlotR like options back to data frame and reparse again, should give pretty much same result, except batch info lost for now
resaved_session <- session_to_xlsx(session2, 'test/test_output/resaved_xl.xlsx')
saved_reloaded_session <- load_excel(resaved_session, load_annotations = T)

plot(saved_reloaded_session, feature='TAF1D')



##change some settings
session$bgr_colors <- c('#b8b691', '#bfc8e3')
plot(session, feature='TAF1D')




### DEBUG SECTION:
resaved_session <- session_to_xlsx(session, 'test/test_output/resaved_xl.xlsx')

session$parameters$`3-seq`$whichSamples <- list('-PAP' = list('total'=c('siCTRL'), '4sU'=c('siCTRL')))

resaved_session <- session_to_xlsx(session, 'test/test_output/resaved_xl_whichsamples.xlsx')

session$parameters$`3-seq`$whichSamples <- NA
resaved_session <- session_to_xlsx(session, 'test/test_output/resaved_xl_whichsamples.xlsx')


  external_args = list(feature='TAF1D', dummy_plot=T)

  #use default args except if present in ellipsis (...) (first priority), or in session (2nd priority)
  from_dots <- intersect(names(external_args), names(session))
  session[from_dots] <- external_args[from_dots]

  #add external_args not in args ((ie add feature or locus!))
  only_dots <- setdiff(names(external_args), names(session))
  session <- c(session, external_args[only_dots])

  # add default for all for some reason missing
  default_args = default_plot_options()
  only_default <- setdiff(names(default_args), names(session))
  session <- c(session, default_args[only_default])

  names(session) <- sub('^samples$', 'datasets', names(session))

  do.call('Seq2Plot', session)



  session$parameters$`3-seq`$whichSamples <- list('-PAP' = list('total'=c('siCTRL'), '4sU'=c('siCTRL')), '+PAP' = NULL)
  rlist::list.flatten(session$parameters$`3-seq`$whichSamples, use.names = TRUE, classes = "ANY")

  x <- c('test', 'notest')
  jsonlite::toJSON(x)
  names(x) <- c('a', 'b')
  jsonlite::toJSON(x)

  jsn <- jsonlite::toJSON(session$parameters$`3-seq`$whichSamples)
  jsn
  jsn_parsed <- jsonlite::fromJSON(jsn)
  jsn_parsed

  alist <- jsn_parsed
  empty_to_null <- function(x){
    if ( !is.list(x) ) {
      x
    } else if ( length(x) == 0 ) {
      NULL
    } else {
      lapply(x, empty_to_null)
    }
  }
  empty_to_null(jsn_parsed)


  ##in seqNdisplayRSession.R : session_to_xlsx include final line in relevant section
  para_df <- data.frame(dataset = names(session$parameters))
  para_df <- dplyr::bind_cols(para_df,
                              dplyr::bind_rows(lapply(session$parameters, function(para) sapply(para, deparse_option) ) ))
  para_df$whichSamples <- sapply(session$parameters, function(p) {
    if ( !is.null(p$whichSamples) ) {
      jsonlite::toJSON(p$whichSamples)
    } else {
      p$whichSamples
    }
  })


  ##in import_excel.R : get_parameters(samples_df, params_df)
  ## params_df <- para_df
  params <- lapply(split(params_df, params_df$dataset), function(xl) lapply(as.list(xl[,colnames(xl)!='dataset' & colnames(xl)!='whichSamples']), parse_option))
  dataset_names <- names(params)
  params <- lapply(dataset_names, function(dataset) {
    para <- params[[dataset]]
    whichSamples <- params_df$whichSamples[params_df$dataset == dataset][[1]]
    if ( is.null(whichSamples) ) {
      para['whichSamples'] <- list(NULL)
      para
    } else {
      para$whichSamples <- empty_to_null(jsonlite::fromJSON(whichSamples))
      para
    }

  })
  names(params) <- dataset_names
  lapply(params, function(para) para$whichSamples)

  resaved_session <- session_to_xlsx(session, 'test/test_output/resaved_xl_whichsamples.xlsx')
  saved_reloaded_session <- load_excel(resaved_session, load_annotations = T)




  xl_fname <- 'inst/extdata/example_excel_template.xlsx'
  xl_fname <- 'inst/extdata/minimal_example_excel_template.xlsx'

  params_df <- readxl::read_excel(xl_fname, sheet='DATASET_OPTIONS')
  session <- load_excel(xl_fname, load_annotations = F)


session$bigwigs[['+']]
session$parameters$`3-seq`

bigwigs_plus <- session$bigwigs[['+']]
whichSamples <- lapply(session$parameters, function(x) x$whichSamples)
x <- bigwigs_plus
x <- session$samples
y <- whichSamples
set_selected_using_selection <- function(x, y) {
  if ( is.list(x) ) {
    nested_list <- lapply(names(x), function(n)
    {
      if( n %in% names(y) ){
        set_selected_using_selection(x[[n]], y[[n]])
      } else {
        set_selected_using_selection(x[[n]], list(NULL))
      }
    })
    names(nested_list) <- names(x)
    structure(nested_list, stselected=identical(x,y), stopened=TRUE)
  } else {
    TRUE
  }
}
set_selected_using_selection(x, y)
}


set_all_selected <- function(x) {
  if ( is.list(x) ) {
    structure(lapply(x, set_all_selected), stselected=T, stopened=TRUE)
  } else {
    x
  }
}

## from list x creates nested node list for tree where all nodes are deselected
set_all_deselected <- function(x) {
  if ( is.list(x) ) {
    structure(lapply(x, set_all_selected), stselected=F, stopened=TRUE)
  } else {
    x
  }
}

## from list x creates nested node list for tree where all nodes are selected if present in x
set_tree_nodes <- function(x, x_checked) {
  if ( is.list(x) ) {
    structure(
      lapply(names(x), function(n) {
        if ( n %in% names(x_checked) ) {
          if ( is.null(x_checked[[n]]) ) {
            set_tree_nodes(x[[n]], x_checked[[n]])
          } else if ( is.na(x_checked[[n]]) ) {
            set_all_deselected(x[[n]])
          }
        } else{
          set_all_deselected(x[[n]])
        }
      }),
      stselected=T,
      stopened=TRUE)
  } else {
    x
  }
}


whichSamples <- lapply(session$parameters, function(p) p[['whichSamples']])
set_tree_nodes(session$bigwigs[['+']], whichSamples)

x <- session$bigwigs[['+']]
x_checked <- whichSamples



### about annot_color = NULL

xl_fname <- 'inst/extdata/seqNdisplayR_sample_sheet_simple.xlsx'
session <- load_excel(xl_fname, load_annotations = F)
session$annot_cols

session_to_xlsx(session, 'test/test_output/resaved_xl_tmp1.xlsx')

session$annot_cols['in-house'] <- list(NULL)

session_to_xlsx(session, 'test/test_output/resaved_xl_tmp2.xlsx')
## From Seq2PlotR like options back to data frame and reparse again, should give pretty much same result, except batch info lost for now


# errer comes from following lines from seqNdislayRSession.R (line 264+):
annos <- data.frame('annotation_name' = names(session$annotation_files),
                    'annotation_file' = as.character(session$annotation_files))

#@anno_display_option_names <- names(default_annotation_options())
anno_display_options <- session[anno_display_option_names]
#ensure consistent sorting
anno_display_options <- lapply(anno_display_options, function(x) sapply(x, deparse_option))
anno_display_options_df <- as.data.frame(anno_display_options)

if you can create a data.frame that looks like the intended output and throws no errors its fixed

what about:
  anno_display_options <- lapply(anno_display_options, function(x) {
    nx <- names(x)
    x <- sapply(x, deparse_option)
    names(x) <- nx})

