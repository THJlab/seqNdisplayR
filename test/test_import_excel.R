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
