### test of almost all options

library(seqNdisplayR)


#@ list the files supplied with package here

xl_fname <- system.file('extdata', 'example_excel_template.xlsx', package='seqNdisplayR')

xl_fname <- system.file('extdata', 'minimal_example_excel_template2.xlsx', package='seqNdisplayR')

session <- load_excel(xl_fname, load_annotations = T)
# takes some time, since annotations are loaded...

print(session)

print(session, verbose=T)

session$horizontal_panels_list
## arguments to be tested in the context of only one plotted region:
feat = 'LMO4'
plot(session, feature=feat)

#session[['parameters']][['TT-seq']][['calcMean']] = FALSE
#session[['parameters']][['RNA-seq']][['calcMean']] = FALSE
#plot(session, feature=feat, replicate_names='')

plot(session, feature='eggplant')
plot(session, feature=feat, dummy_plot=TRUE, bin_start='xyz', verbosity='off', interface='shiny')
plot(session, feature=feat, dummy_plot=TRUE, bin_start='xyz', verbosity='no warnings', interface='shiny')
plot(session, feature=feat, dummy_plot=TRUE, bin_start='xyz', verbosity='normal', interface='shiny')
plot(session, feature=feat, dummy_plot=TRUE, bin_start='xyz', verbosity='detailed', interface='shiny')
plot(session, feature=feat, bin_start='xyz', strands_alpha='qwerty')
plot(session, feature=feat, bin_start='xyz', strands_alpha='qwerty', scaling_factor='2x')
plot(session, feature=feat, full_width_cm=15)
plot(session, feature=feat, verbosity='detailed', interface='shiny')
plot(session, feature=feat, verbosity='detailed', interface='R')
plot(session, feature=feat, verbosity='detailed', interface='shiny', strands_intermingled=FALSE)
plot(session, feature=feat, verbosity='detailed', interface='R', strands_intermingled=FALSE)
plot(session, feature=feat, verbosity='detailed', interface='shiny', both_strands=FALSE)
plot(session, feature=feat, verbosity='detailed', interface='R', both_strands=FALSE)
plot(session, feature='TAF1D', verbosity='detailed', interface='shiny', both_strands=FALSE)
plot(session, feature='TAF1D', verbosity='detailed', interface='R', both_strands=FALSE)



plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=5, margin_width_cm=NULL, scale_panel_width_cm=0.4, track_width_cm=12) ## delib error
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=5, margin_width_cm=NULL, scale_panel_width_cm=0.4, track_width_cm=12, verbosity='off') ## delib error
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=5, margin_width_cm=NULL, scale_panel_width_cm=0.4, track_width_cm=12, verbosity='no warnings') ## delib error
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=5, margin_width_cm=NULL, scale_panel_width_cm=0.4, track_width_cm=12, verbosity='detailed') ## delib error

plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=10, scale_panel_width_cm=0.4, track_width_cm=NULL) ## delib error
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=5, scale_panel_width_cm=0.4, track_width_cm=NULL)
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=1.5, scale_panel_width_cm=0.4, track_width_cm=NULL )
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=2, scale_panel_width_cm=0.3, track_width_cm=NULL )
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=3, scale_panel_width_cm=0.4, track_width_cm=NULL )
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=3.8, scale_panel_width_cm=0.4, track_width_cm=NULL )
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=3.8, scale_panel_width_cm=0.6, track_width_cm=NULL, scale_fontsize=5 )
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=3.8, scale_panel_width_cm=0.5, track_width_cm=NULL )
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=3.8, scale_panel_width_cm=0.5, track_width_cm=NULL, panel_text_colors=c('maroon', 'magenta') )
plot(session, feature=feat, full_width_cm=15, panels_max_width_cm=3.8, scale_panel_width_cm=0.5, track_width_cm=NULL, panel_text_colors=c('lilac', 'magenta') )
plot(session, feature=feat, full_width_cm=12, panels_max_width_cm=3.8, scale_panel_width_cm=0.6, track_width_cm=NULL )
plot(session, feature=feat, full_width_cm=12, panel_font_sizes=c(5,5), panels_max_width_cm=3.4, scale_panel_width_cm=0.6, track_width_cm=NULL )

plot(session, feature=feat, panel_font_sizes=c(5,5,5)) ## deliberate error
plot(session, feature=feat, panel_font_size_list=list('eggplant'=c(5,5,5), 'peach'=c(5,6)) ) ## deliberate error
plot(session, feature=feat, panel_font_size_list=list('TT-seq'=c(5,5), 'RNA-seq'=c(5,5), '3-seq'=c(5,6), 'ChIP-seq'=c(5,4)) ) ## deliberate error
plot(session, feature=feat, panel_font_size_list=list('TT-seq'=c(6,5), 'RNA-seq'=c(6,5), '3-seq'=c(6,5,5,5), 'ChIP-seq'=c(5,4)) )

plot(session, feature=feat, horizontal_panels_list=list('eggplant'=c(F,F,F), 'peach'=c(F,F)) ) ## deliberate error
plot(session, feature=feat, horizontal_panels_list=list('TT-seq'=c(T,T), 'RNA-seq'=c(T,T), '3-seq'=c(T,T,T,T), 'ChIP-seq'=c(T,T)) )
plot(session, feature=feat, horizontal_panels_list=list('TT-seq'=c('A',5), 'RNA-seq'=c(T,T), '3-seq'=c(T,T,T,T), 'ChIP-seq'=c(T,T)) ) ## deliberate error

xl_fname <- system.file('extdata', 'example_excel_template.xlsx', package='seqNdisplayR')
session <- load_excel(xl_fname, load_annotations = T)
feat='LMO4'

plot(session, feature=feat, annots=NULL, dummy_plot=TRUE, force_scale=force_scale)
plot(session, feature=feat, dummy_plot=TRUE, force_scale=list('+'=c('TT-seq'=NA, 'RNA-seq'=NA, '3-seq'=NA, 'ChIP-seq'=NA), '-'=c('TT-seq'=NA, 'RNA-seq'=NA, '3-seq'=NA, 'ChIP-seq'=NA)))
plot(session, feature=feat, dummy_plot=TRUE, force_scale=list('+'=c('TT-seq'=150, 'RNA-seq'=200, '3-seq'=50, 'ChIP-seq'=20), '-'=c('TT-seq'=15, 'RNA-seq'=20, '3-seq'=5, 'ChIP-seq'=NA)))
plot(session, feature=feat, dummy_plot=TRUE, force_scale=list('+'=c('TT-seq'=NA, 'RNA-seq'=NA, '3-seq'=20, 'ChIP-seq'=NA), '-'=c('TT-seq'=NA, 'RNA-seq'=NA, '3-seq'=5, 'ChIP-seq'=NA)))
plot(session, feature=feat, dummy_plot=TRUE, force_scale=c('TT-seq'=150, 'RNA-seq'=200, '3-seq'=50, 'ChIP-seq'=20))
plot(session, feature=feat, dummy_plot=TRUE, force_scale=list('+'=200, '-'=20))
plot(session, feature=feat, dummy_plot=TRUE, force_scale=200)
plot(session, feature=feat, force_scale=list('+'=c('TT-seq'=150, 'RNA-seq'=200, '3-seq'=50, 'ChIP-seq'=20), '-'=c('TT-seq'=15, 'RNA-seq'=20, '3-seq'=5, 'ChIP-seq'=NA)), pdf=TRUE, pdf_name='testings_testings2')

plot(session, locus=c('chr12','+','6527255','6545046'), annots=NULL, dummy_plot=TRUE, header='GAPDH')
plot(session, locus=c('chr12','+','6527255','6545046'), annots=NULL, dummy_plot=TRUE, header='GAPDH', force_scale=list('+'=c('TT-seq'=NA, 'RNA-seq'=NA, '3-seq'=NA, 'ChIP-seq'=NA), '-'=c('TT-seq'=NA, 'RNA-seq'=NA, '3-seq'=NA, 'ChIP-seq'=NA)))
plot(session, locus=c('chr12','+','6527255','6545046'), annots=NULL, dummy_plot=TRUE, header='GAPDH', force_scale=list('+'=c('TT-seq'=150, 'RNA-seq'=200, '3-seq'=50, 'ChIP-seq'=20), '-'=c('TT-seq'=15, 'RNA-seq'=20, '3-seq'=5, 'ChIP-seq'=NA)))
plot(session, locus=c('chr12','+','6527255','6545046'), annots=NULL, dummy_plot=TRUE, header='GAPDH', force_scale=list('+'=c('TT-seq'=150, 'RNA-seq'=200, '3-seq'=50, 'ChIP-seq'=20), '-'=c('TT-seq'=15, 'RNA-seq'=20, '3-seq'=5, 'ChIP-seq'=NA)), verbosity='detailed')
plot(session, locus=c('chr12','+','6527255','6545046'), annots=NULL, dummy_plot=TRUE, header='GAPDH', force_scale=list('+'=c('TT-seq'=150, 'RNA-seq'=200, '3-seq'=50, 'ChIP-seq'=20), '-'=c('TT-seq'=15, 'RNA-seq'=20, '3-seq'=5, 'ChIP-seq'=NA)), verbosity='off')

plot(session, feature=feat, annotation_height_cm=0.24, track_height_cm=0.48)
plot(session, feature=feat, incl_track_scales=FALSE)
plot(session, feature=feat, full_width_cm=20, track_width_cm=NULL) ## ERROR! This one ok.
plot(session, feature=feat, full_width_cm=20, track_width_cm=NULL, scale_panel_width_cm=0.6, panels_max_width_cm=4.4)
plot(session, feature=feat, full_width_cm=20, track_width_cm=NULL, scale_panel_width_cm=0.6, panels_max_width_cm=3.4)
plot(session, feature=feat, full_width_cm=20, track_width_cm=NULL, scale_panel_width_cm=0.6, panels_max_width_cm=3.8)
plot(session, feature=feat, full_width_cm=20, track_width_cm=NULL, incl_track_scales=FALSE, panels_max_width_cm=3.8)



#chr12:+:6527255:6545046


plot(session, feature=feat, header="peach_emoji", pdf=FALSE, pdf_name="peach_emoji2")
session$parameters$`3-seq`$whichSamples = NA
plot(session, feature=feat)
session$parameters$`3-seq`$whichSamples = c("-PAP", "xPAP")
plot(session, feature=feat)
session$parameters$`3-seq`$whichSamples = c("xPAP")
plot(session, feature=feat)
session$parameters$`3-seq`$whichSamples = list("xPAP"="total")
plot(session, feature=feat)
session$parameters$`3-seq`$whichSamples = list("total"=list("xPAP"=c("siCTRL", "siEXOSC3"), "-PAP"=c("siCTRL", "siEXOSC3")), "4sU"=list("xPAP"=c("siCTRL", "siEXOSC3"), "-PAP"=c("siCTRL", "siEXOSC3")))
plot(session, feature=feat)
session$parameters$`3-seq`$whichSamples = list("total"=list("siCTRL"=c("-PAP","xPAP"), "siEXOSC3"=c("-PAP","xPAP")))
plot(session, feature=feat)

feat = 'LMO4'
plot(session, feature=feat)
plot(session, feature=feat, horizontal_panels_list=list("3-seq"=c(T,T,T,T), "TT-seq"=c(T,T), "RNA-seq"=c(T,T), "ChIP-seq"=c(T,T)))
#header,empty-spacer,scale,RNA-seq,line-spacer,TT-seq,line-spacer,3-seq,line-spacer,ChIP-seq,empty-spacer,unstranded-beds,empty-spacer,annotationsannotations
plotting_segment_order = list('+'= c('header','empty-spacer','empty-spacer','scale','empty-spacer', '3-seq','empty-spacer','TT-seq','empty-spacer','RNA-seq','empty-spacer','ChIP-seq','empty-spacer','annotations'),
'-'=c('thick-spacer','annotations','empty-spacer','RNA-seq','empty-spacer','TT-seq','empty-spacer', '3-seq'))

plot(session, feature=feat, strands_intermingled=FALSE, plotting_segment_order=plotting_segment_order)

plotting_segment_order = list('+'= c('header','scale','3-seq','line-spacer','TT-seq','line-spacer','RNA-seq','line-spacer','ChIP-seq','empty-spacer','annotations','thickline-spacer','unstranded-beds'),
                              '-'=c('thickline-spacer','annotations','empty-spacer','3-seq','line-spacer','TT-seq','line-spacer','RNA-seq'))

plot(session, feature=feat, strands_intermingled=FALSE, plotting_segment_order=plotting_segment_order)

plotting_segment_order = list('+'= c('header','empty-spacer','scale','empty-spacer','3-seq','line-spacer','TT-seq','line-spacer','RNA-seq','line-spacer','ChIP-seq','empty-spacer','annotations','thickline-spacer','empty-spacer','unstranded-beds'),
                              '-'=c('empty-spacer', 'thickline-spacer','annotations','empty-spacer','3-seq','line-spacer','TT-seq','line-spacer','RNA-seq'))
#header,empty-spacer,scale,empty-spacer,3-seq,line-spacer,TT-seq,line-spacer,RNA-seq,line-spacer,ChIP-seq,empty-spacer,annotations,thickline-spacer,empty-spacer,unstranded-beds
#empty-spacer,thickline-spacer,annotations,empty-spacer,3-seq,line-spacer,TT-seq,line-spacer,RNA-seq
plot(session, feature=feat, strands_intermingled=FALSE, plotting_segment_order=plotting_segment_order)

plotting_segment_order = list('+'= c('header','empty-spacer','scale','empty-spacer','3-seq','line-spacer','TT-seq','line-spacer','RNA-seq','line-spacer','ChIP-seq','empty-spacer','annotations','thickline-spacer','empty-spacer','unstranded-beds'),
                              '-'=c('empty-spacer', 'thickline-spacer','annotations','empty-spacer','RNA-seq','line-spacer','TT-seq','line-spacer','3-seq'))


plotting_segment_order = list('+'= c('header','empty-spacer','scale','empty-spacer','ChIP-seq','line-spacer','3-seq','line-spacer','TT-seq','line-spacer','RNA-seq','empty-spacer','annotations','thickline-spacer','empty-spacer','unstranded-beds'),
                              '-'=c('empty-spacer', 'thickline-spacer','annotations','empty-spacer','RNA-seq','line-spacer','TT-seq','line-spacer','3-seq'))

plotting_segment_order = list('+'= c('header','empty-spacer','scale','empty-spacer','ChIP-seq','line-spacer','TT-seq','line-spacer','RNA-seq','empty-spacer','annotations','thickline-spacer','empty-spacer','unstranded-beds'),
                              '-'=c('empty-spacer', 'thickline-spacer','annotations','empty-spacer','RNA-seq','line-spacer','TT-seq'))

plot(session, feature=feat, strands_intermingled=FALSE, plotting_segment_order=plotting_segment_order)

plot(session, feature=feat, strands_intermingled=FALSE, plotting_segment_order=plotting_segment_order, incl_first_panel=TRUE, print_one_line_sample_names=TRUE)

plot(session, feature=feat, strands_intermingled=FALSE, plotting_segment_order=plotting_segment_order, incl_first_panel=TRUE, print_one_line_sample_names=TRUE, panel_separators=c(TRUE, TRUE), separators_colors=c('cornflowerblue','black', 'black'))



# which_samples = list("3-seq"=list(
#                                  "xPAP"=list(
#                                              "total"=c("siCTRL", "siEXOSC3"),
#                                              "4sU"=c("siCTRL", "siEXOSC3")),
#                                  "-PAP"=list(
#                                              "total"=c("siCTRL", "siEXOSC3"),
#                                             "4sU"=c("siCTRL", "siEXOSC3"))),
#                     "TT-seq"=c("siCTRL", "siEXOSC3"),
#                     "RNA-seq"=c("siCTRL", "siEXOSC3"),
#                     "ChIP-seq"="RNAPII")
#
#
# which_samples2 = which_samples
# #unlist(which_samples[[sample_name]]) == unlist(template_session$samples[[sample_name]])
# sample_name = "3-seq"
# unlist(which_samples[[sample_name]]) == unlist(which_samples2[[sample_name]])
# rlist::list.flatten(which_samples, use.names = TRUE, classes = "ANY")
#
# identical(which_samples2,which_samples)

LMO4_tracks = plot(session, feature=feat, output_tracks=TRUE)

### defaults
plot(session, feature=feat, preloaded_tracks=LMO4_tracks)

### alternating_background: TRUE/FALSE
plot(session, feature=feat, preloaded_tracks=LMO4_tracks)
plot(session, feature=feat, preloaded_tracks=LMO4_tracks, alternating_background=FALSE)
# bgr_colors=c('#C1B49A', '#F1F1F2')
plot(session, feature=feat, preloaded_tracks=LMO4_tracks, bgr_colors=c('#b8b691', '#bfc8e3'))
# bgr_alpha=0.2
plot(session, feature=feat, preloaded_tracks=LMO4_tracks, bgr_colors=c('#b8b691', '#bfc8e3'), bgr_alpha=0.1)

### strands_alpha: numeric values between 50 and 100 for both plus and minus strand, default: c(100,50)
plot(session, feature=feat, preloaded_tracks=LMO4_tracks)
plot(session, feature=feat, preloaded_tracks=LMO4_tracks, strands_alpha=c(100,100))
plot(session, feature=feat, preloaded_tracks=LMO4_tracks, strands_alpha=c(50,100))


### both_strands: TRUE/FALSE (should be tested for both plus and minus strand and on pre-loaded tracks vs not)
feat = 'LMO4'
plot(session, feature=feat, preloaded_tracks=LMO4_tracks, both_strands=FALSE)
session$parameters$`3-seq`$enhance_signals = TRUE
plot(session, feature=feat, preloaded_tracks=LMO4_tracks, both_strands=FALSE)
session$parameters$`3-seq`$bin_stats = "max"
plot(session, feature=feat, preloaded_tracks=LMO4_tracks, both_strands=FALSE)

feat = 'TAF1D'
TAF1D_tracks = plot(session, feature=feat, output_tracks=TRUE)
plot(session, feature=feat, preloaded_tracks=TAF1D_tracks)
plot(session, feature=feat, preloaded_tracks=TAF1D_tracks, both_strands=FALSE, neg_vals_neg_strand=FALSE, strands_alpha=100)
plot(session, feature=feat, preloaded_tracks=TAF1D_tracks, both_strands=FALSE, actual_strand_direction=FALSE, neg_vals_neg_strand=FALSE, strands_alpha=100)

plot(session, feature=feat, both_strands=FALSE, actual_strand_direction=FALSE, neg_vals_neg_strand=FALSE) # tracks not preloaded
plot(session, feature=feat, strands_intermingled=FALSE, strands_alpha=100) # tracks not preloaded
plot(session, feature=feat, strands_intermingled=FALSE, neg_vals_neg_strand=FALSE, strands_alpha=100)

feat = 'C11orf54' # opposite of TAF1D
C11orf54_tracks = plot(session, feature=feat, output_tracks=TRUE)
plot(session, feature=feat, preloaded_tracks=C11orf54_tracks)
plot(session, feature=feat, preloaded_tracks=C11orf54_tracks, both_strands=FALSE)
plot(session, feature=feat, both_strands=FALSE) # tracks not preloaded
plot(session, feature=feat, strands_intermingled=FALSE, strands_alpha=100) # tracks not preloaded

extra_space=c(1.5,1.5)  ## should not go beyond genomic boundaries



plot(session, feature="OAZ1")
plot(session, feature="DELE1")

for (feat in c('LMO4', 'RPL30', 'GAPDH', 'TAF1D', 'NOP56', 'DHFR')){
  pre_loaded_tracks = plot(session, feature=feat, output_tracks=TRUE)
  plot(session, feature=feat, preloaded_tracks=pre_loaded_tracks, both_strands=TRUE, strands_intermingled=TRUE)
  plot(session, feature=feat, preloaded_tracks=pre_loaded_tracks, both_strands=TRUE, strands_intermingled=FALSE, neg_vals_neg_strand=TRUE)
  plot(session, feature=feat, preloaded_tracks=pre_loaded_tracks, both_strands=TRUE, strands_intermingled=FALSE, neg_vals_neg_strand=FALSE)
  plot(session, feature=feat, preloaded_tracks=pre_loaded_tracks, both_strands=FALSE, neg_vals_neg_strand=FALSE)
  plot(session, feature=feat, preloaded_tracks=pre_loaded_tracks, both_strands=FALSE, actual_strand_direction=FALSE, neg_vals_neg_strand=FALSE)

  for (nom in names(session$parameters)){
    session$parameters[[nom]][['calcMean']] = FALSE
  }
  pre_loaded_tracks = plot(session, feature=feat, output_tracks=TRUE)
  plot(session, feature=feat)
  plot(session, feature=feat, replicate_names='r')
  plot(session, feature=feat, replicate_names=NULL)

  for (nom in names(session$parameters)){
    session$parameters[[nom]][['calcMean']] = TRUE
  }


  plot(session, feature=feat, preloaded_tracks=preloaded_tracks, annotation_packing='collapsed')
  plot(session, feature=feat, preloaded_tracks=pre_loaded_tracks, annotation_packing='collapsed2')
  plot(session, feature=feat, preloaded_tracks=pre_loaded_tracks, annotation_packing='expanded')
  plot(session, feature=feat, preloaded_tracks=pre_loaded_tracks, annotation_packing='squished')
}


seqNdisplay(samples, colors, bigwig_dirs, bigwigs, parameters, plotting_segment_order=NULL, preloaded_tracks=NULL, output_tracks=FALSE, output_parameters=FALSE, input_parameters=NULL,
         both_strands=TRUE, strands_intermingled=TRUE, neg_vals_neg_strand=TRUE, actual_strand_direction=TRUE, alternating_background=TRUE, bgr_colors=c('#C1B49A', '#F1F1F2'), bgr_alpha=0.2, strands_alpha=c(100,50),
         feature='TAF1D', locus=NULL, extra_space=c(1.5,1.5),
         annots=annots, annotation_packing='collapsed', annot_cols=NULL, annot_panel_color='steelblue', annot_panel_font_size=NULL,
         bin_start=NULL, bin_size='auto', bins_per_cm=250, track_width_cm=15, full_width_cm=NULL, full_height_cm=NULL, track_height_cm=0.3,
         panels_max_width_cm='auto', margin_width_cm=0.05, fixed_panel_width=FALSE, horizontal_panels_list=NULL, panel_font_sizes=NULL, panel_font_size_list=NULL, panel_text_colors=c('darkgreen', 'black'),
         horizontal_spacers=TRUE, panel_separators=c(FALSE, TRUE), separators_lwds=c(0.5, 1, 0.5), separators_colors='black', incl_first_panel=TRUE, print_one_line_sample_names=FALSE, replicate_names=NULL,
         incl_track_scales=TRUE, scientific_scale=c('allow', 'all', 'none')[1], force_scale=NULL, scale_font_size=NULL, scale_panel_width_cm='auto', scale_font_color='darkred',
         header=NULL, suppress_header=FALSE, header_font_sizes=NULL, header_font_colors=c('black', 'darkgray', 'black'),
         include_genomic_scale=TRUE, genomic_scale_on_top=TRUE, genomic_scale_font_size=NULL, genomic_scale_font_color='black',
         incl_feature_names=TRUE, feature_names_above=FALSE, feature_names_alternating=TRUE, feature_names_font_size=NULL, incl_feature_brackets=c(FALSE,TRUE), incl_feature_shadings=c(FALSE,TRUE), feature_shading_colors=c('steelblue', 'hotpink'), feature_shading_alpha=0.05, center_of_mass=FALSE, feature_names_font_color='black',
         dummy_plot=FALSE, pdf=FALSE, pdf_name=NULL, pdf_dir='./testplotting', scaling_factor=1.2)
