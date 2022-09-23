##################################################################################################
###### seqNdisplayR: A Tool for Customizable and Reproducible Plotting of Sequencing Coverage Data
##################################################################################################

.onLoad <- function(libname, pkgname) {
  packageStartupMessage("\nWelcome to seqNdisplayR\nThis package sets the global locale to C.\n\nThis software is free and comes with ABSOLUTELY NO WARRANTY!")
  Sys.setlocale(locale="C")
}

#### Constants and Defaults (package-wide available arguments)
line_width_scaling_factor = 0.25/0.38 # scaling factor to use for the line width (if multiplied with 1 it will give 0.5-pt line width in the pdf with default dimensions)
points_per_cm = 28.3465
arrow_constant = 0.4                  # used for scaling directional arrows in annotation with feature heights
cm_to_in = 0.393701					          # 1 cm = 0.393701 inches
std_letter_width = 0.023              # gives the width of the standard letter if multiplied by font size
std_letter_height = 0.045             # gives the height of the standard letter if multiplied by font size
min_font_size = 4                     # the minimum font size for all formats
annot_panel_dist = 0.4                # right margin for annotation title panel

## default vertical proportions of various plotting segment types relative to 'seq' tracks
plot_vertical_parameters = c('header'=2.2,  ## vertical units used for header segment
                             'seq'=1, ## vertical units used for each sequencing track ??????FIXED!!!
                             'scale'=0.8, ## vertical units used for the genomic scale
                             'line-spacer'=0.2, ## vertical units used for spacers
                             'empty-spacer'=0.2, ## vertical units used for empty spacers (w/o possibility of horizontal lines)
                             'thickline-spacer'=0.4, ## vertical units used for annotation separator
                             'annot'=0.8,  # per annotation line...if annotation_packing is not squished
                             'annot_squished'=0.4, # per annotation line...if annotation_packing is squished
                             'annot_text_segment'=0.8) #


## markup code:
# #@ recently changed - currently testing
# #$ suggested improvement


#' Default plot options
#'
#' @return A named list of default plot options
#'
#' @export
default_plot_options <- function(){
  list(plotting_segment_order=NULL,                               #(v)
       preloaded_tracks=NULL,                                     ## not in app
       output_tracks=FALSE,                                       ## not in app
       output_parameters=FALSE,                                   ## not in app
       input_parameters=NULL,                                     ## not in app
       both_strands=TRUE,                                         #(v)
       strands_intermingled=TRUE,                                 #(v)
       neg_vals_neg_strand=FALSE,                                  #(v)
       reverse_strand_direction=FALSE,                              #(v)
       alternating_background=TRUE,                               #(v)
       bgr_colors=c('#C1B49A', '#F1F1F2'),                        #(v) two colors (hex or name) - will auto-default with warning message
       bgr_alpha = 0.2,                                           #(v) positive numeric value (0-1)
       strands_alpha=c(100,100),                                  #(v) positive numeric values (0-100)
       intermingled_color=c('same', 'complementary', 'analogous_right', 'analogous_left')[1],
       extra_space=c(1.5, 1.5),                                   #(v) positive numeric values
       annot_panel_color='steelblue',                             #(v) color (hex or name)
       annot_panel_font_size=NULL,                                #(v) NULL or positive integer value
       bin_start=NULL,                                            #(v) NULL or positive integer value
       bin_size='auto',                                           #(v) 'auto'/'automatic' or positive integer value (>=1)
       bins_per_cm=250,                                           #(v) positive integer value (>=1)
       track_width_cm=12,                                         #(v) NULL or positive numeric value
       full_width_cm=NULL,                                        #(v) NULL or positive numeric value (*caution)
       full_height_cm=NULL,                                       #(v) NULL or positive numeric value (*caution)
       track_height_cm=0.3,                                       #(v) NULL or positive numeric value
       title_field_height_cm=0.66,                                #(v) NULL or positive numeric value
       genomic_scale_height_cm=0.24,                              #(v) NULL or positive numeric value
       annotation_height_cm=0.24,                                 #(v) NULL or positive numeric value
       spacer_height_cm=0.06,                                     #(v) NULL or positive numeric value
       panels_max_width_cm='auto',                                #(v) 'auto'/'automatic' or positive numeric value
       margin_width_cm=0.05,                                      #(v) NULL or positive numeric value - will auto-default with warning message
       fixed_panel_width=FALSE,                                   #(v)
       horizontal_panels_list=NULL,                               #(v)% needs a function to assess the input
       panel_font_sizes=NULL,                                     #(v)% needs a function to assess the input
       panel_font_size_list=NULL,                                 #(v)% needs a function to assess the input
       panel_text_colors=c('darkgreen', 'black'),                 #(v) two colors (hex or name) - will auto-default with warning message
       horizontal_spacers=TRUE,                                   #(v)
       panel_separators=c(FALSE, TRUE),                           #(v) will auto-default
       separators_lwds=c(0.5, 1, 0.5),                            #(v) three positive numeric values - will auto-default with warning message
       separators_colors='black',                                 #(v) one or three colors (hex or name) - will auto-default with warning message
       incl_first_panel=TRUE,                                     #(v)
       print_one_line_sample_names=FALSE,                         #(v)
       replicate_names=NULL, # 'rep'                              #(v) NULL or character value
       incl_track_scales=TRUE,                                    #(v)
       scientific_scale=c('allow', 'all', 'none')[1],             #(v) one of 'allow', 'all', 'none'
       force_scale=NULL,                                          #(v)
       scale_font_size=NULL,                                      #(v) NULL or positive integer value (>4)
       scale_panel_width_cm='auto',                               #(v) 'auto'/'automatic' or positive numeric value
       scale_font_color='darkred',                                #(v) color (hex or name) - will auto-default with warning message
       header=NULL,                                               #(v)
       suppress_header=FALSE,                                     #(v)
       header_font_sizes=NULL,                                    #(v) NULL or one or three positive integer value (>4)
       header_font_colors=c('black', 'darkgray', 'black'),        #(v) one or three colors (hex or name) - will auto-default with warning message
       include_genomic_scale=TRUE,                                #(v)
       genomic_scale_on_top=TRUE,                                 #(v)
       genomic_scale_font_size=NULL,                              #(v) NULL or positive integer value (>4)
       genomic_scale_font_color='black',                          #(v) color (hex or name) - will auto-default with warning message
       feature_names_alternating=TRUE,                            #(v)
       feature_names_font_size=NULL,                              #(v) NULL or positive integer value (>4)
       feature_shading_colors=c('steelblue', 'hotpink'),          #(v) two colors (hex or name) - will auto-default with warning message
       feature_shading_alpha=0.05,                                #(v) positive numeric value (0-1)
       center_of_mass=FALSE,                                      #(v)
       feature_names_font_color='black',                          #(v) color (hex or name) - will auto-default with warning message
       dummy_plot=FALSE,                                          #(v)
       scaling_factor=1,                                          #(v) positive numeric value - will auto-default with warning message
       verbosity=c('off', 'no warnings', 'normal', 'detailed')[3],#
       interface=c('R', 'shiny')[1]                               # output directed towards R- or shiny-users
       )
}

#' Default Annotation Display Options
#'
#' @details Default values for options regarding annotation display in seqNdisplayR session object.
#' @return A named list of default annotation-display options.
#'
#' @export
default_annotation_options <- function() {
  list('incl_feature_names'=TRUE,                                 #(v)
       'feature_names_above'=FALSE,                               #(v)
       'incl_feature_brackets'=TRUE,                              #(v)
       'incl_feature_shadings'=TRUE,                              #(v)
       'annotation_packing'='collapsed2',                         #(v)
       'annot_cols'='black'                                       #(v)
  )
}

#' Default Dataset-Specific Options
#'
#' @details Default values for parameters object in seqNdisplayR session object.
#' @return A named list of default Dataset-specific options.
#'
#' @export
default_parameters <- function() {
  list(
    'whichSamples' = NULL,
    'bin_stats' = c('mean','median', 'max')[1],
    'enhance_signals'=FALSE,
    'log2transform'=FALSE,
    'pseudoCount'=1,
    'batchCorrect'=FALSE,
    'batch'=NULL,
    'whichReps'=NULL,
    'negative_valued_bw'=FALSE,
    'calcMean'=TRUE,
    'negValsSet0'=FALSE,
    'force_scale'=NULL,
    'group_autoscale'=TRUE
  )
}

#### MAIN Plot FUNCTIONS

#' seq'N'display
#'
#'
#' @param 70+_arguments see xxx #@
#'
#' @details
#' Main function to create a plot. See xxx #@
#'
#' @return
#'
#' @examples
#'
#' @export
seqNdisplay = function(
  datasets, colors, bigwig_dirs, bigwigs, parameters, plotting_segment_order=NULL, preloaded_tracks=NULL, output_tracks=FALSE, output_parameters=FALSE, input_parameters=NULL,
  both_strands=TRUE, strands_intermingled=TRUE, neg_vals_neg_strand=TRUE, reverse_strand_direction=FALSE, alternating_background=TRUE, bgr_colors=c('#C1B49A', '#F1F1F2'), bgr_alpha=0.2, strands_alpha=c(100,100), intermingled_color='same',
  feature=NULL, locus=NULL, extra_space=c(1.5,1.5),
  annots=NULL, annotation_packing='collapsed2', annot_cols=NULL, annot_panel_color='steelblue', annot_panel_font_size=NULL,
  bin_start=NULL, bin_size='automatic', bins_per_cm=250, track_width_cm=10, full_width_cm=NULL, full_height_cm=NULL, track_height_cm=0.3, title_field_height_cm=0.66, genomic_scale_height_cm=0.24, annotation_height_cm=0.24, spacer_height_cm=0.06,
  panels_max_width_cm='auto', margin_width_cm=0.05, fixed_panel_width=FALSE, horizontal_panels_list=NULL, panel_font_sizes=NULL, panel_font_size_list=NULL, panel_text_colors=c('darkgreen', 'black'),
  horizontal_spacers=TRUE, panel_separators=c(FALSE, TRUE), separators_lwds=c(0.5, 1, 0.5), separators_colors='black', incl_first_panel=TRUE, print_one_line_sample_names=FALSE, replicate_names='rep',
  group_autoscale=TRUE, incl_track_scales=TRUE, scientific_scale=c('allow', 'all', 'none')[1], force_scale=NULL, scale_font_size=NULL, scale_panel_width_cm='auto', scale_font_color='darkred',
  header=NULL, suppress_header=FALSE, header_font_sizes=NULL, header_font_colors=c('black', 'darkgray', 'black'),
  include_genomic_scale=TRUE, genomic_scale_on_top=TRUE, genomic_scale_font_size=NULL, genomic_scale_font_color='black',
  incl_feature_names=TRUE, feature_names_above=FALSE, feature_names_alternating=TRUE, feature_names_font_size=NULL, incl_feature_brackets=TRUE, incl_feature_shadings=TRUE, feature_shading_colors=c('steelblue', 'hotpink'), feature_shading_alpha=0.05, center_of_mass=FALSE, feature_names_font_color='black',
  dummy_plot=FALSE, pdf=FALSE, pdf_name=NULL, pdf_dir='./testplotting', scaling_factor=1, verbosity='normal', interface='R', ...){

  t1 = Sys.time()
  #### -> check parameters part 1 - abort or auto correct
  .verbosity = structure(0:3, names=c('off', 'no warnings', 'normal', 'detailed'))[as.character(ScrutinizeExpandAndNameParameter(verbosity, 1, use_names=FALSE, default_value='normal', expect_standard=NULL, expect=c('off', 'no warnings', 'normal', 'detailed'), revert_to_default=TRUE, alt_par_name=NULL, verbosity=3))]
  .interface = as.character(ScrutinizeExpandAndNameParameter(interface, 1, use_names=FALSE, default_value='R', expect_standard=NULL, expect=c('R', 'shiny'), revert_to_default=TRUE, alt_par_name=NULL, verbosity=.verbosity))
  if (verbosity > 2){ .detailed.output = list() }
  #datasets, colors, bigwig_dirs, bigwigs, parameters,
  if (is.null(feature) & is.null(locus)){
    if (.verbosity > 0){ cat('ERROR: choose a genomic region for plotting by either assigning locus name or coordinates - aborting', '\n') }
    return()
  }
  bigwig_dirs = lapply(bigwig_dirs, function(X) if (grepl('/$', X)){X}else{paste0(X, '/')})
  if (!is.null(track_width_cm)){ if (!EvaluateNumericValue(track_width_cm, positive_val=TRUE, min_val=3, max_val=25, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'track_width_cm', 'Tracks Width'), .verbosity)){ return() }}
  if (!is.null(full_width_cm)){ if (!EvaluateNumericValue(full_width_cm, positive_val=TRUE, min_val=5, max_val=30, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'full_width_cm', 'Full Plot Width'), .verbosity)){ return() }}
  if (!is.null(full_height_cm)){ if (!EvaluateNumericValue(full_height_cm, positive_val=TRUE, min_val=5, max_val=30, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'full_height_cm', 'Full Plot Height'), .verbosity)){ return() }}
  if (!is.null(track_height_cm)){ if (!EvaluateNumericValue(track_height_cm, positive_val=TRUE, min_val=0.2, max_val=1, interval_obligatory=TRUE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'track_height_cm', 'Tracks Height'), .verbosity)){ return() }}
  if (!is.null(full_height_cm) & !is.null(track_height_cm)){
    if (.verbosity > 1){
      .warning.message = paste0('WARNING: ', ifelse(interface=='R', '"track_height_cm"', '"Tracks Height"'), ' = ', track_height_cm, ' and ', ifelse(interface=='R', '"full_height_cm"', '"Full Plot Height"'), ' = ', full_height_cm,
                              '\n', '\t', '.) ', 'one of the arguments should be a positive numeric value and the other should be NULL',
                              '\n', '\t', '.) ', ifelse(interface=='R', '"track_height_cm"', '"Tracks Height"'), ' set to NULL')
      cat(.warning.message, '\n')
    }
    track_height_cm = NULL
  }else if (is.null(full_height_cm) & is.null(track_height_cm)){
    if (.verbosity > 0){
      .error.message = paste0(' - ', 'both ', ifelse(interface=='R', '"track_height_cm"', '"Tracks Height"'), ' and ', ifelse(interface=='R', '"full_height_cm"', '"Full Plot Height"'), ' are NULL - one of them has to be defined')
      cat(.error.message, '\n')
    }
    return()
  }
  if (!is.null(genomic_scale_height_cm)){ if (!EvaluateNumericValue(genomic_scale_height_cm, positive_val=TRUE, min_val=0.2, max_val=0.5, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'genomic_scale_height_cm', 'Genomic Scale Height'), .verbosity)){ return() }}
  if (!is.null(annotation_height_cm)){ if (!EvaluateNumericValue(annotation_height_cm, positive_val=TRUE, min_val=0.2, max_val=0.5, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'annotation_height_cm', 'Annotation Height'), .verbosity)){ return() }}
  if (!is.null(spacer_height_cm)){ if (!EvaluateNumericValue(spacer_height_cm, positive_val=TRUE, min_val=0.01, max_val=0.2, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'spacer_height_cm', 'Spacer Height'), .verbosity)){ return() }}
  if (!is.null(panels_max_width_cm)){ if (panels_max_width_cm!='auto' & panels_max_width_cm!='automatic'){if (!EvaluateNumericValue(panels_max_width_cm, positive_val=TRUE, min_val=0, max_val=30, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'panels_max_width_cm', 'Panels Width'), .verbosity)){ return() }}}
  if (!is.null(scale_panel_width_cm)){ if (scale_panel_width_cm!='auto' & scale_panel_width_cm!='automatic'){if (!EvaluateNumericValue(scale_panel_width_cm, positive_val=TRUE, min_val=0, max_val=1, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'scale_panel_width_cm', 'Tracks Scale Width'), .verbosity)){ return() }}}
  if (!is.null(feature)) {if (!EvaluateNumericValue(extra_space, positive_val=TRUE, min_val=0, max_val=100, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'extra_space', 'Expand Plotted Region'), .verbosity)){ return() }}
  if (!EvaluateNumericValue(margin_width_cm, positive_val=TRUE, min_val=0, max_val=0.25, interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'margin_width_cm', 'Margins Width'), .verbosity)){ margin_width_cm=0.05; if (.verbosity>1){cat(margin_width_cm, '\n')} }
  if (!is.null(panel_font_sizes)){ if (!EvaluateNumericValue(panel_font_sizes, positive_val=TRUE, min_val=4, max_val=15, interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'panel_font_sizes', 'Panel Text Font Size(s)'), .verbosity)){ panel_font_sizes=NULL; if (.verbosity>1){cat('NULL', '\n')} }}
  if (!is.null(scale_font_size)){ if (!EvaluateNumericValue(scale_font_size, positive_val=TRUE, min_val=4, max_val=8, interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'scale_font_size', 'Genomic Scale Font'), .verbosity)){ scale_font_size=NULL; if (.verbosity>1){cat('NULL', '\n')} }}
  #@if (!is.null(genomic_scale_font_size)){ if (!EvaluateNumericValue(genomic_scale_font_size, positive_val=TRUE, min_val=4, max_val=8, interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'genomic_scale_font_size', 'Genomic Scale Font'), .verbosity)){ genomic_scale_font_size=NULL; if (.verbosity>1){cat('NULL', '\n')} }}
  if (!is.null(feature_names_font_size)){ if (!EvaluateNumericValue(feature_names_font_size, positive_val=TRUE, min_val=4, max_val=12, interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'feature_names_font_size', 'Feature Names Font Size'), .verbosity)){ feature_names_font_size=NULL; if (.verbosity>1){cat('NULL', '\n')} }}
  if (!EvaluateNumericValue(bgr_alpha, positive_val=TRUE, min_val=0, max_val=0.25, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'bgr_alpha', 'Background  Opacity'), .verbosity)){ bgr_alpha=0.2; if (.verbosity>1){cat(bgr_alpha, '\n')} }
  if (!EvaluateNumericValue(feature_shading_alpha, positive_val=TRUE, min_val=0, max_val=0.25, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'feature_shading_alpha', 'Shading Opacity'), .verbosity)){ feature_shading_alpha=0.05; if (.verbosity>1){cat(feature_shading_alpha, '\n')} }
  if (!EvaluateNumericValue(scaling_factor, positive_val=TRUE, min_val=0.5, max_val=10, interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'scaling_factor', 'Scaling For On-Screen Display'), .verbosity)){ scaling_factor=1; if (.verbosity>1){cat(scaling_factor, '\n')} }
  .plot.widths.cm = PlotWidths(panels_max_width_cm, scale_panel_width_cm, margin_width_cm, track_width_cm, full_width_cm, incl_track_scales, .verbosity, .interface)
  if (is.null(.plot.widths.cm)){ return() }
  #### <- check parameters part 1 - abort or auto correct
  #### -> load all annotations
  .annotations=NULL;.incl.feature.names=NULL;.feature.names.above=NULL;.annot.cols = NULL;.incl.feature.brackets=NULL;.incl.feature.shadings=NULL;.annotation.packing=NULL;.annot.cols=NULL;.annot.info=NULL
  if (!is.null(annots)){
    .annots.class = unique(unlist(lapply(annots, class)))
    if (length(.annots.class)==1){
      if (.annots.class=='GRanges'){
        .annotations = annots
      }else if (.annots.class=='character'){
        .annotations = ReadInAnnotations(annots, .verbosity)
      }
    }else{
      if (.verbosity > 0) { cat('ERROR(s):\n - the provided annotation is not of the correct format - aborting', '\n') }
      return()
    }
    .incl.feature.names = ScrutinizeExpandAndNameParameter(incl_feature_names, .annotations, use_names=TRUE, default_value=TRUE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'incl_feature_names', 'Display Feature Names'), verbosity=.verbosity)
    .feature.names.above = ScrutinizeExpandAndNameParameter(feature_names_above, .annotations, use_names=TRUE, default_value=FALSE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'feature_names_above', 'Feature Names Above Features'), verbosity=.verbosity)
    if (!is.null(.feature.names.above)){
      if (feature_names_alternating & !strands_intermingled){
        .feature.names.above = list('+'=.feature.names.above, '-'=!.feature.names.above)
      }else{
        .feature.names.above = list('+'=.feature.names.above, '-'=.feature.names.above)
      }
    }
    .incl.feature.brackets = ScrutinizeExpandAndNameParameter(incl_feature_brackets, .annotations, use_names=TRUE, default_value=TRUE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'incl_feature_brackets', 'Feature Bracket'), verbosity=.verbosity)
    .incl.feature.shadings = ScrutinizeExpandAndNameParameter(incl_feature_shadings, .annotations, use_names=TRUE, default_value=TRUE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'incl_feature_shadings', 'Highlight Individual Loci By Shaded Boxes'), verbosity=.verbosity)
    .annotation.packing = ScrutinizeExpandAndNameParameter(annotation_packing, .annotations, use_names=TRUE, default_value='collapsed2', expect_standard=NULL, expect=c('expanded', 'squished', 'collapsed', 'collapsed2'), revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'annotation_packing', 'Annotation Packing'), verbosity=.verbosity)
    if (!is.null(annot_cols)){
      if (any(annot_cols=='NULL')){
        .replacement.col = ifelse('seashell4' %in% unlist(colors), ifelse('thistle3' %in% unlist(colors), 'slategray3', 'thistle3'), 'seashell4')
        annot_cols[names(which(annot_cols=='NULL'))] = .replacement.col
        .annot.cols = ScrutinizeExpandAndNameParameter(annot_cols, .annotations, use_names=TRUE, default_value='black', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'annot_cols', 'Feature Colors'), verbosity=.verbosity)
        .null.index = which(.annot.cols==.replacement.col)
        .annot.cols = as.list(.annot.cols)
        .annot.cols[.null.index] = list(NULL)
      }else{
        .annot.cols = ScrutinizeExpandAndNameParameter(annot_cols, .annotations, use_names=TRUE, default_value='black', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'annot_cols', 'Feature Colors'), verbosity=.verbosity)
        .annot.cols = as.list(.annot.cols)
      }
    }
    #### -> check parameters part 2 - abort and return NULL if any of the crucial parameters are NULL
    if ( any(sapply(list(.annotations,.incl.feature.names,.feature.names.above,.annot.cols,.incl.feature.brackets,.incl.feature.shadings,.annotation.packing), function(parameter) is.null(parameter))) ){ return() }
  }
  #### <- load all annotations
  #### -> check parameters part 3 - abort or auto correct
  .batch.correction = ScrutinizeExpandAndNameParameter(unlist(lapply(parameters, function(p) p$batchCorrect)), datasets, use_names=TRUE, default_value=FALSE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'batchCorrect', 'Batch Correction'), verbosity=.verbosity)
  .log2.transform = ScrutinizeExpandAndNameParameter(unlist(lapply(parameters, function(p) p$log2transform)), datasets, use_names=TRUE, default_value=FALSE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'log2transform', 'log2-Transform Data'), verbosity=.verbosity)
  .batch.log2 = structure(as.logical(.batch.correction + .log2.transform), names=names(.batch.correction))
  .pseudocounts = ScrutinizeExpandAndNameParameter(unlist(lapply(parameters, function(p) if(!is.null(p$pseudoCount)){p$pseudoCount}else{-1})), datasets, use_names=TRUE, default_value=1, expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'pseudoCount', 'Pseudocount'), verbosity=.verbosity)
  for (.i in which(.batch.log2)){
    if (!EvaluateNumericValue(.pseudocounts[.i], positive_val=TRUE, min_val=0, max_val=1000000, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=paste(ifelse(interface=='R', 'pseudoCount', 'Pseudo Count'), names(.pseudocounts[.i])), .verbosity)){
      parameters[[names(.pseudocounts[.i])]][['pseudoCount']] = 1
    }
  }
  bin_stats = sapply(names(datasets), function(dataset) parameters[[dataset]][['bin_stats']])
  .bin.stats = ScrutinizeExpandAndNameParameter(bin_stats, datasets, use_names=TRUE, default_value='mean', expect_standard=NULL, expect=c('mean', 'median', 'max'), revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'bin_stats', 'Binning Statistics'), verbosity=.verbosity)
  enhance_signals = sapply(names(datasets), function(dataset) parameters[[dataset]][['enhance_signals']])
  .enhance.signals = ScrutinizeExpandAndNameParameter(enhance_signals, datasets, use_names=TRUE, default_value=FALSE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'enhance_signals', 'Enhance Signals'), verbosity=.verbosity)
  .panel.separators = ScrutinizeExpandAndNameParameter(panel_separators, c('horizontal', 'vertical'), use_names=FALSE, default_value=FALSE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'panel_separators', 'Vertical and Horizontal Line-Separators'), verbosity=.verbosity)
  .separators.lwds = ScrutinizeExpandAndNameParameter(separators_lwds, c('line-spacer', 'thickline-spacer', 'vertical-spacer'), use_names=FALSE, default_value=c(0.5, 1, 0.5), expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'separators_lwds', 'Line-Separator Weight(s)'), verbosity=.verbosity)
  .separators.lwds = if (EvaluateNumericValue(.separators.lwds, positive_val=TRUE, min_val=0.5, max_val=10, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'separators_lwds', 'Line-Separator Weight(s)'), .verbosity)){.separators.lwds}else{ScrutinizeExpandAndNameParameter('dummy', c('line-spacer', 'thickline-spacer', 'vertical-spacer'), use_names=FALSE, default_value=c(0.5, 1, 0.5), expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'separators_lwds', 'Line-Separator Weight(s)'), verbosity=.verbosity)}
  .separators.colors = ScrutinizeExpandAndNameParameter(separators_colors, c('line-spacer', 'thickline-spacer', 'vertical-spacer'), use_names=FALSE, default_value='black', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'separators_colors', 'Line-Separator Color(s)'), verbosity=.verbosity)
  .annot.panel.color = ScrutinizeExpandAndNameParameter(annot_panel_color, 'annotation', use_names=FALSE, default_value='steelblue', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'annot_panel_color', 'Annotation Title Color'), verbosity=.verbosity)
  .panel.text.colors = ScrutinizeExpandAndNameParameter(panel_text_colors, c('panel_1st', 'panel'), use_names=FALSE, default_value=c('darkgreen', 'black'), expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'panel_text_colors', 'Panel Text Colors'), verbosity=.verbosity)
  .scale.font.color = ScrutinizeExpandAndNameParameter(scale_font_color, 'scale', use_names=FALSE, default_value='darkred', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'scale_font_color', 'Data Scale Color'), verbosity=.verbosity)
  .header.font.colors = ScrutinizeExpandAndNameParameter(header_font_colors, c('header', 'subheader', 'genomic_scale'), use_names=FALSE, default_value=c('black', 'darkgray', 'black'), expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'header_font_colors', 'Header Color(s)'), verbosity=.verbosity)
  .genomic.scale.font.color = ScrutinizeExpandAndNameParameter(genomic_scale_font_color, 'genomic_axis', use_names=FALSE, default_value='black', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'genomic_scale_font_color', 'Genomic Scale Font Color'), verbosity=.verbosity)
  .feature.names.font.color = ScrutinizeExpandAndNameParameter(feature_names_font_color, 'features', use_names=FALSE, default_value='black', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'feature_names_font_color', 'Feature Names Color'), verbosity=.verbosity)
  .feature.shading.colors = ScrutinizeExpandAndNameParameter(feature_shading_colors, 1:length(feature_shading_colors), use_names=FALSE, default_value=c('steelblue', 'hotpink'), expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'feature_shading_colors', 'Shading Colors'), verbosity=.verbosity)
  .bgr.colors = as.character(ScrutinizeExpandAndNameParameter(bgr_colors, c('odd', 'even'), use_names=FALSE, default_value=c('#C1B49A', '#F1F1F2'), expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'bgr_colors', 'Alternating Background Colors'), verbosity=.verbosity))
  .font.colors = structure(rep('black', 9), names=c('header', 'subheader', 'genomic_scale', 'genomic_axis', 'panel_1st', 'panel', 'scale', 'annotation', 'features'))
  .font.colors[c('annotation', names(.panel.text.colors), 'scale', names(.header.font.colors), 'genomic_axis', 'features')] = c(.annot.panel.color, .panel.text.colors, .scale.font.color, .header.font.colors, .genomic.scale.font.color, .feature.names.font.color)
  .intermingled.color = as.character(ScrutinizeExpandAndNameParameter(intermingled_color, 1, use_names=FALSE, default_value='same', expect_standard=NULL, expect=c('same', 'complementary', 'analogous_right', 'analogous_left'), revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'intermingled_color', 'Color Display of Data from Negative Strand'), verbosity=.verbosity))
  .strands.alpha = ScrutinizeExpandAndNameParameter(strands_alpha, c('+', '-'), use_names=FALSE, default_value=c(100, 100), expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'strands_alpha', 'Forward/Reverse Track Opacities'), verbosity=.verbosity)
  .strands.alpha = if(EvaluateNumericValue(.strands.alpha, positive_val=TRUE, min_val=20, max_val=100, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'strands_alpha', 'Forward/Reverse Track Opacities'), .verbosity)){.strands.alpha}else{ScrutinizeExpandAndNameParameter('dummy', c('+', '-'), use_names=FALSE, default_value=c(100, 100), expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'strands_alpha', 'Forward/Reverse Track Opacities'), verbosity=0)}
  .title.field.height.cm = ScrutinizeExpandAndNameParameter(title_field_height_cm, 1, use_names=FALSE, default_value=0.66, expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'title_field_height_cm', 'Title field height'), verbosity=.verbosity)
  .title.field.height.cm = if(EvaluateNumericValue(.title.field.height.cm, positive_val=TRUE, min_val=0.66, max_val=5, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'title_field_height_cm', 'Title field height'), .verbosity)){.title.field.height.cm}else{ScrutinizeExpandAndNameParameter('dummy', 1, use_names=FALSE, default_value=0.66, expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'title_field_height_cm', 'Title field height'), verbosity=0)}
  if (!is.null(bin_start)){if (!EvaluateNumericValue(bin_start, positive_val=TRUE, min_val=1, max_val=1000000000, interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'bin_start', 'Bins Center'), .verbosity)){ bin_start=NULL; if (.verbosity>1){cat('NULL', '\n')} }}
  .scientific.scale = as.character(ScrutinizeExpandAndNameParameter(scientific_scale, 1, use_names=FALSE, default_value='allow', expect_standard=NULL, expect=c('allow', 'all', 'none'), revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'scientific_scale', 'Scientific Notation on Data Scale(s)'), verbosity=.verbosity))
  .group.autoscale = ScrutinizeExpandAndNameParameter(group_autoscale, datasets, use_names=TRUE, default_value=TRUE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'group_autoscale', 'Group Autoscale'), verbosity=.verbosity)
  # abort and return NULL if any of the crucial parameters are NULL
  if ( any(sapply(list(.bin.stats,.enhance.signals,.panel.separators,.separators.lwds,.separators.colors,
                       .annot.panel.color,.panel.text.colors,.scale.font.color,.header.font.colors,.genomic.scale.font.color,
                       .feature.names.font.color,.strands.alpha), function(parameter) is.null(parameter))) ){ return() }
  #### <- check parameters part 3 - abort or auto correct
  #### -> defining plotting region
  if (is.null(feature)){
    .plotted.region = RegionGRanges(locus, .plot.widths.cm['track.width.cm'], feature=NULL, .annotations, bin_start, extra_space, .verbosity, .interface)
    if (is.null(header) & !suppress_header){
      header=''
    }
  }else{
    .plotted.region = RegionGRanges(locus=NULL, .plot.widths.cm['track.width.cm'], feature, .annotations, bin_start, extra_space, .verbosity, .interface)
    if (is.null(header) & !suppress_header){
      header=feature
    }
  }
  if (is.null(.plotted.region)){
    if (.verbosity > 0){ cat('ERROR(s):\n - there is no valid region defined for plotting - aborting', '\n') }
    return()
  }
  #### <- defining plotting region
  #### -> strands display
  .strand = as.character(S4Vectors::runValue(BiocGenerics::strand(.plotted.region)))
  if (both_strands){
    reverse_strand_direction=FALSE
    .strands.intermingled=strands_intermingled
    .neg.vals.neg.strand = ifelse(.strands.intermingled, TRUE, neg_vals_neg_strand)
    .rev.plotted.region = .plotted.region
    .rev.strand = ifelse(.strand=='+', '-', '+')
    BiocGenerics::strand(.rev.plotted.region) = ifelse(.strand=='+', '-', '+')
    .plotted.region = structure(list(.plotted.region, .rev.plotted.region), names=c(.strand, .rev.strand))[c('+', '-')]
  }else{
    .strands.intermingled=FALSE
    .neg.vals.neg.strand = neg_vals_neg_strand
    .plotted.region = structure(list(.plotted.region), names=.strand)
    if (.strand=='-'){
      .unstranded.datasets = setdiff(names(bigwigs[['+']]), names(bigwigs[['-']]))
      if (length(.unstranded.datasets) > 0){
        for (.unstranded.dataset in .unstranded.datasets){
          bigwigs[['-']][[.unstranded.dataset]] = bigwigs[['+']][[.unstranded.dataset]]
        }
      }
    }
  }
  #### <- strands display
  #### -> organize annotations in region
  if (!is.null(.annotations)){
    .annot.info = lapply(.plotted.region, OrganizeAnnotatedFeaturesInRegion, .annotations)
  }
  #### <- organize annotations in region
  #### -> organize panels, font sizes and other parameters
  .tracks.listed = lapply(structure(lapply(names(.plotted.region), function(.strand) structure(lapply(names(datasets), function(.seqtype) LoadAndTransformDataForTrack(.seqtype, .plotted.region[[.strand]], datasets, bigwigs, bigwig_dirs, parameters, get_subsamples=TRUE, print_order=FALSE, .verbosity)), names=names(datasets))), names=names(.plotted.region)), DeleteNULLs)
  if (length(.tracks.listed[['+']])==0){ #@ this whole thing added
    .tracks.listed[['+']] = NULL
  }else if (length(.tracks.listed[['-']])==0){
    .tracks.listed[['-']] = NULL
  }
  .plotted.samples = unique(unlist(lapply(names(.tracks.listed), function(n) names(.tracks.listed[[n]]))))
  .panels.list = lapply(.tracks.listed, OrganizedPanelsList)
  .stranded.beds = unlist(lapply(.annotations, function(gr) !all(as.character(GenomicRanges::strand(gr))=="*")))
  .unstranded.beds = NULL
  if (!is.null(.stranded.beds)){
    if (any(!.stranded.beds)){
      .unstranded.beds = names(.stranded.beds)[!.stranded.beds]
    }
  }
  .any.unstranded.beds = !is.null(.unstranded.beds)
  .any.stranded.beds = ifelse(length(setdiff(names(.stranded.beds), .unstranded.beds))>0, TRUE, FALSE)
  if (.any.stranded.beds | .any.unstranded.beds){
    .stranded.beds = structure(lapply(names(.plotted.region), function(.strand) {if (.strand=="+" | !both_strands){sapply(.stranded.beds, function(x) TRUE)}else{.stranded.beds}}), names=names(.plotted.region))
  }
  .stranded.datasets = structure(rep(FALSE, length(datasets)), names=names(datasets))
  .stranded.datasets[intersect(names(.panels.list[['+']]), names(.panels.list[['-']]))] = TRUE
  .plotting.segment.order = BuildScrutinizePlotSegmentOrder(plotting_segment_order, .plotted.region, datasets, .plotted.samples, header, include_genomic_scale, genomic_scale_on_top, incl_annot=!is.null(annots), horizontal_spacers, .tracks.listed, both_strands, .any.stranded.beds, .any.unstranded.beds, .strands.intermingled, .verbosity, .interface)
  if (is.null(.plotting.segment.order)){ return() }
  .plot.vertical.parameters = plot_vertical_parameters
  if (!is.null(track_height_cm)){ #@ ->
    .plot.vertical.parameters = UpdatePlotVerticalParameters(plot_vertical_parameters, track_height_cm, .title.field.height.cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm)
  }
  .estimated.plot.heights = AdjustEstimatedPlotHeights(structure(lapply(names(.plotted.region), function(.strand) EstimatePlotHeights(.annot.info[[.strand]], .incl.feature.names, .annotation.packing, .incl.feature.brackets, .plotting.segment.order[[.strand]], .tracks.listed[[.strand]], track_height_cm, full_height_cm, .stranded.beds[[.strand]], .plot.vertical.parameters, .verbosity, .interface)), names=names(.plotted.region)), .plot.vertical.parameters, full_height_cm, track_height_cm, .title.field.height.cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm)
  .est.track.height.cm.range = as.numeric(c(.estimated.plot.heights[[.strand]][['min.track.height.cm.est']], .estimated.plot.heights[[.strand]][['max.track.height.cm.est']]))
  .est.min.annot.height = min(unlist(lapply(names(.plotted.region), function(.strand) unlist(.estimated.plot.heights[[.strand]][['annot.heights.incl.text']]))))
  if (!is.null(annotation_height_cm) & is.null(track_height_cm)){
    .est.min.annot.height = annotation_height_cm*as.numeric(.est.min.annot.height/.plot.vertical.parameters['annot'])/min(.est.track.height.cm.range)
  }
  .prev.annot.height = .plot.vertical.parameters['annot']
  if (is.null(track_height_cm)){ #@ ->
    .plot.vertical.parameters = UpdatePlotVerticalParameters(.plot.vertical.parameters, mean(.est.track.height.cm.range), .title.field.height.cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm)
  } #@ <-
  for (s in names(.plotted.region)){
    .max.combined.track.vector = UpdateTrackVector(.estimated.plot.heights[[s]][['max.combined.track.vector']], .plot.vertical.parameters)
    .max.combined.track.vector[grep('annot', names(.max.combined.track.vector))] = .plot.vertical.parameters['annot'] * .max.combined.track.vector[grep('annot', names(.max.combined.track.vector))]/.prev.annot.height
    .min.combined.track.vector = UpdateTrackVector(.estimated.plot.heights[[s]][['min.combined.track.vector']], .plot.vertical.parameters)
    .min.combined.track.vector[grep('annot', names(.min.combined.track.vector))] = .plot.vertical.parameters['annot'] * .min.combined.track.vector[grep('annot', names(.min.combined.track.vector))]/.prev.annot.height
    .estimated.plot.heights[[s]][['max.combined.track.vector']] = .max.combined.track.vector
    .estimated.plot.heights[[s]][['min.combined.track.vector']] = .min.combined.track.vector
    .estimated.plot.heights[[s]][['track.vector']] = UpdateTrackVector(.estimated.plot.heights[[s]][['track.vector']], .plot.vertical.parameters)
  }
  .rec.font.sizes = RecommendedFontSizes(max(.est.track.height.cm.range), .est.min.annot.height, .plot.vertical.parameters)
  if (any(.rec.font.sizes < min_font_size)){
    .rec.font.sizes[which(.rec.font.sizes < min_font_size)] = min_font_size
  }
  if (is.null(header_font_sizes)){
    header_font_sizes = .rec.font.sizes[c('main', 'sub', 'scale')]
  }
  .header.font.sizes =  ScrutinizeExpandAndNameParameter(header_font_sizes, c('main', 'sub', 'scale'), use_names=FALSE, default_value=.rec.font.sizes[c('main', 'sub', 'scale')], expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'header_font_sizes', 'Header Fonts'), verbosity=.verbosity)
  .header.font.sizes = if (EvaluateNumericValue(.header.font.sizes, positive_val=TRUE, min_val=4, max_val=c(24, 18, 18), interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'header_font_sizes', 'Header Fonts'), .verbosity)){.header.font.sizes}else{ScrutinizeExpandAndNameParameter('dummy', c('main', 'sub', 'scale'), use_names=FALSE, default_value=.rec.font.sizes[c('main', 'sub', 'scale')], expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'header_font_sizes', 'Header Fonts'), verbosity=.verbosity)}
  .incl.reps = unlist(lapply(parameters, function(p) !p$calcMean))
  .panel.font.size.list = PanelFontSizeList(datasets, panel_font_sizes, panel_font_size_list, .incl.reps, replicate_names, .verbosity, .interface) #@ added .incl.reps, replicate_names
  if (!print_one_line_sample_names){
    .horizontal.panels.list = HorizontalPanelsList(datasets, horizontal_panels_list, .incl.reps, replicate_names, .verbosity, .interface) #@ added .incl.reps, replicate_names
  }else{
    .horizontal.panels.list = NULL
  }
  if (is.null(force_scale)){
    #@cat(paste('wouw', unlist(force_scale)), '\n') #@cat
    .force.scale.list = HandleForcedScaleFromParameters(parameters)
  }else{
    .force.scale.list = ForceScaleList(lapply(.tracks.listed, names), force_scale, strands=ifelse(both_strands, '+-', .strand), .verbosity, .interface)
  }
  if (!is.null(.panel.font.size.list)){
    .max.font.size = max( .rec.font.sizes[c('std', 'genomic_axis', 'signal_axis', 'annotation_features')], ifelse(is.null(annot_panel_font_size), NA, annot_panel_font_size), ifelse(is.null(feature_names_font_size), NA, feature_names_font_size), ifelse(is.null(scale_font_size), NA, scale_font_size), max(unlist(lapply(.panel.font.size.list, max, na.rm=TRUE)), na.rm=TRUE), na.rm=TRUE) #@ .rec.font.sizes['std'] -> .rec.font.sizes[c('std', 'genomic_axis', 'signal_axis', 'annotation_features')]
  }else{
    .max.font.size = max( .rec.font.sizes[c('std', 'genomic_axis', 'signal_axis', 'annotation_features')], ifelse(is.null(annot_panel_font_size), NA, annot_panel_font_size), ifelse(is.null(feature_names_font_size), NA, feature_names_font_size), ifelse(is.null(scale_font_size), NA, scale_font_size), na.rm=TRUE) #@ .rec.font.sizes['std'] -> .rec.font.sizes[c('std', 'genomic_axis', 'signal_axis', 'annotation_features')]
  }
  if (.max.font.size > 0){
    .letter.widths = 1:.max.font.size*std_letter_width
    .letter.heights = 1:.max.font.size*std_letter_height
    if (.max.font.size > min_font_size){
      .letter.widths[1:(min_font_size-1)] = NA
      .letter.heights[1:(min_font_size-1)] = NA
    }else{
      if (.verbosity > 1) { cat(paste0('WARNING(s):\n - the dimensions of the plot could be too small (font sizes below ', min_font_size, ')'), '\n') }
    }
  }else{
    if (.verbosity > 0) { cat('ERROR(s):\n - the dimensions of the plot are too small for any fonts to be visible - aborting', '\n') }
    return()
  }
  .feature.text = structure(lapply(names(.plotted.region), function(.strand) OrganizeAnnotationText(.plotted.region[[.strand]], .annot.info[[.strand]], .annotation.packing, .letter.widths, center_of_mass, .verbosity)), names=names(.plotted.region))
  .feature.text.org = structure(lapply(names(.plotted.region), function(.strand) OrganizeAllAnnotationTextsInPlottedRegion(.feature.text[[.strand]], .plotted.region[[.strand]], .letter.widths)), names=names(.plotted.region))
  .relative.annotation.height = structure(lapply(names(.plotted.region), function(.strand) RelativeAnnotationHeight(.annot.info[[.strand]], .estimated.plot.heights[[.strand]][['annot.heights']], .letter.heights, .incl.feature.names, .feature.text.org[[.strand]], .annotation.packing, .incl.feature.brackets, .stranded.beds[[.strand]])), names=names(.plotted.region))
  .total.annotation.lines = lapply(.relative.annotation.height, function(x) x[['annot.heights.combined']])
  .annot.heights.incl.text = lapply(.relative.annotation.height, function(x) x[['annot.heights.incl.text']])
  .plot.height.parameters = structure(lapply(names(.plotted.region), function(.strand) PlotHeightParameters(.estimated.plot.heights[[.strand]][['max.combined.track.vector']], .estimated.plot.heights[[.strand]][['track.vector']], .total.annotation.lines[[.strand]], colSums(do.call('rbind', .total.annotation.lines)), .annot.heights.incl.text[[.strand]], .estimated.plot.heights[[.strand]][['max.annot.lines']], .estimated.plot.heights[[.strand]][['annot.heights']], track_height_cm, full_height_cm, .title.field.height.cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm, .plot.vertical.parameters)), names=names(.plotted.region))
  if (is.null(feature_names_font_size)){
    .feature.names.font.size = min(.max.font.size, as.integer(.rec.font.sizes['annotation_features']))
  }else{
    if (.max.font.size < feature_names_font_size){
      if (.verbosity > 1) { cat(paste0('WARNING(s):\n - the provided "feature_names_font_size" argument [', feature_names_font_size, '] is too large. Changing to ', .max.font.size), '\n') }
      .feature.names.font.size = .max.font.size
    }else{
      .feature.names.font.size = feature_names_font_size
    }
  }
  .min.wordlength.left.panel = ifelse(!is.null(.annotations), max(nchar(names(.annotations))) + 3, 0)
  .panel.info = FinalizePanelsDimensions(structure(lapply(names(.tracks.listed), function(.strand) OrganizePanelsDimensions(names(.tracks.listed[[.strand]]), .min.wordlength.left.panel, replicate_names, print_one_line_sample_names, incl_first_panel, .plot.height.parameters[[.strand]], .feature.names.font.size, which(!is.na(.letter.heights)), .rec.font.sizes, scale_font_size, .horizontal.panels.list, .panel.font.size.list, .panels.list[[.strand]], .plot.widths.cm, .panel.separators, .strand, both_strands, .strands.intermingled, .stranded.datasets, fixed_panel_width, .verbosity)), names=names(.tracks.listed)), both_strands) #@ names(.plotted.region) -> names(.tracks.listed)
  .plot.width.parameters = .panel.info[[1]][['plot.width.parameters']]
  .full.width.cm = .plot.width.parameters[['full.width.cm']]
  if (!EvaluateNumericValue(.full.width.cm, positive_val=TRUE, min_val=5, max_val=50, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'full_width_cm', 'Full Plot Width'), .verbosity)){ return() }
  .scale.fontsize = .panel.info[[1]][['scale.fontsize']]
  if (!EvaluateNumericValue(.scale.fontsize, positive_val=TRUE, min_val=4, max_val=8, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'scale_font_size', 'Data Scale Font Size'), .verbosity)){ return() }
  if (is.null(feature_names_font_size)){
    .feature.names.font.size = min(.feature.names.font.size, min(unlist(lapply(.panel.info[[1]][['panel.font.size.list']], min, na.rm=TRUE)), na.rm=TRUE))
  }
  if (!EvaluateNumericValue(.feature.names.font.size, positive_val=TRUE, min_val=4, max_val=12, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'feature_names_font_size', 'Feature Names Font Size'), .verbosity)){ return() }
  if (is.null(annot_panel_font_size)){
    if (print_one_line_sample_names){
      .annot.panel.font.size = max(unlist(lapply(.panel.info[[1]][['panel.font.size.list']], function(x) mean(x[-1], na.rm=T))), na.rm=TRUE) #@
    }else{
      .annot.panel.font.size = round(max(unlist(lapply(.panel.info[[1]][['panel.font.size.list']], mean, na.rm=TRUE)), na.rm=TRUE), 0) #@
    }
  }else{
    .annot.panel.font.size = annot_panel_font_size
  }
  .annotation.panel.font.size = ScrutinizeExpandAndNameParameter(.annot.panel.font.size, 'annotation', use_names=FALSE, default_value=.rec.font.sizes['std'], expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'annot_panel_font_size', 'Annotation Title Font Size'), verbosity=.verbosity)
  if (!EvaluateNumericValue(.annotation.panel.font.size, positive_val=TRUE, min_val=4, max_val=12, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'annot_panel_font_size', 'Annotation Title Font Size'), .verbosity)){ return() }
  if (!is.null(bin_size)){ if (bin_size!='auto' & bin_size!='automatic'){if (!EvaluateNumericValue(bin_size, positive_val=TRUE, min_val=1, max_val=10000000, interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'bin_size', 'Bin Size'), .verbosity)){ bin_size='auto'; if (.verbosity>1){cat(NULL, '\n')} }}}
  if (!EvaluateNumericValue(bins_per_cm, positive_val=TRUE, min_val=50, max_val=1000, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'bins_per_cm', 'Bins per cm'), .verbosity)){ bins_per_cm=250; if (.verbosity>1){cat(bins_per_cm, '\n')} }
  #@.bins.per.cm = ScrutinizeExpandAndNameParameter(bins_per_cm, '1', use_names=FALSE, default_value=250, expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'bins_per_cm', 'Bins per cm'), verbosity=.verbosity)
  .bins.per.cm = as.integer(if(EvaluateNumericValue(bins_per_cm, positive_val=TRUE, min_val=50, max_val=1000, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'bins_per_cm', 'Bins per cm'), .verbosity)){bins_per_cm}else{ScrutinizeExpandAndNameParameter(250, 1, use_names=FALSE, default_value=250, expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ifelse(interface=='R', 'bins_per_cm', 'Bins per cm'), verbosity=0)})
  .bin.size = GetBinSize(bin_size, IRanges::width(.plotted.region[[.strand]]), .plot.width.parameters[['tracks.width.cm']], .bins.per.cm, .verbosity)
  if (!EvaluateNumericValue(.bin.size, positive_val=TRUE, min_val=1, max_val=1000000, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ifelse(interface=='R', 'bin_size', 'Bin Size'), .verbosity)){ return() }
  .fixed.plot.vertical.parameters = c('tracks'=!is.null(track_height_cm), 'header'=!is.null(.title.field.height.cm), 'scale'=!is.null(genomic_scale_height_cm), 'spacers'=!is.null(spacer_height_cm), 'annots'=!is.null(annotation_height_cm)) #@
  .basic.plot.parameters = AlignBasicPlotParameters(structure(lapply(names(.plotted.region), function(.strand) BasicPlotParameters(.strand, .plotted.region, .feature.names.font.size, .plot.height.parameters, .plot.width.parameters, .full.width.cm, full_height_cm, track_height_cm, .plot.vertical.parameters, .bin.size, .bins.per.cm, .plotting.segment.order, .tracks.listed, .unstranded.beds)), names=names(.plotted.region)), both_strands, .strands.intermingled, .fixed.plot.vertical.parameters)
  .final.feature.text.org = lapply(.feature.text.org, FinalOrganizedAnnotationTextsInPlottedRegion, names(annots), .feature.names.font.size)
  if (is.null(genomic_scale_font_size)){
    .genomic.scale.font.size = .rec.font.sizes['genomic_axis']
  }else{
    .genomic.scale.font.size = genomic_scale_font_size
  }
  if (!EvaluateNumericValue(.genomic.scale.font.size, positive_val=TRUE, min_val=4, max_val=.rec.font.sizes['genomic_axis'], interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ifelse(interface=='R', 'genomic_scale_font_size', 'Genomic Scale Font'), .verbosity)){ return() }
  #### <- organize panels, font sizes and other parameters
  #### -> load tracks
  if (is.null(preloaded_tracks)){
    if (!dummy_plot){
      if (.verbosity > 0){
        if (length(.plotted.samples) > 1){
          cat(paste('loading', paste(paste(.plotted.samples[1:(length(.plotted.samples)-1)], collapse=', '), '&', .plotted.samples[length(.plotted.samples)]), 'tracks from', paste(names(.tracks.listed), collapse=' & '), 'strand(s)'), '\n')
        }else{
          cat(paste('loading', paste(paste(.plotted.samples[1:(length(.plotted.samples)-1)], collapse=', ')), 'tracks from', paste(names(.tracks.listed), collapse=' & '), 'strand(s)'), '\n')
        }
      }
      .tracks = structure(lapply(names(.tracks.listed), function(.strand) LoadTracks(.plotted.region[[.strand]], datasets, bigwigs, bigwig_dirs, parameters, .verbosity)), names=names(.tracks.listed))
    }else{
      if (.verbosity > 0){
        if (length(.plotted.samples) > 1){
          cat(paste('dummy plotting, so not loading', paste(paste(.plotted.samples[1:(length(.plotted.samples)-1)], collapse=', '), '&', .plotted.samples[length(.plotted.samples)]), 'tracks from', paste(names(.tracks.listed), collapse=' & '), 'strand(s)'), '\n')
        }else{
          cat(paste('dummy plotting, so not loading', paste(paste(.plotted.samples[1:(length(.plotted.samples)-1)], collapse=', ')), 'tracks from', paste(names(.tracks.listed), collapse=' & '), 'strand(s)'), '\n')
        }
      }
      .tracks = .tracks.listed
    }
    if (output_tracks & !output_parameters){
      return(.tracks)
    }
  }else{
    .tracks = preloaded_tracks
    if (length(.tracks) == 2){
      if (!both_strands & .strand=='-'){
        if (length(.unstranded.datasets) > 0){
          for (.unstranded.dataset in .unstranded.datasets){
            .tracks[['-']][[.unstranded.dataset]] = .tracks[['+']][[.unstranded.dataset]]
          }
        }
      }
    }
  }
  if (output_parameters){
    .output.parameters = list()
    if (!output_tracks){
      return(.output.parameters)
    }else{
      return(list('tracks'=.tracks, 'output_parameters'=.output.parameters))
    }
  }
  #### <- load tracks
  #### -> pdf or on-screen plotting?
  .height.in = .basic.plot.parameters[[ifelse(.strands.intermingled, 3, 1)]][['plot.dim.in']][2]
  #@.height.in = .basic.plot.parameters[[1]][['plot.dim.in']][2]
  .width.in = .basic.plot.parameters[[ifelse(.strands.intermingled, 3, 1)]][['plot.dim.in']][1]
  #@.width.in = .basic.plot.parameters[[1]][['plot.dim.in']][1]
  if (!EvaluateNumericValue(.height.in, positive_val=TRUE, min_val=0.5, max_val=10.51182, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name='Calculated Height in Inches', .verbosity)){ return() } ## dimensions of A4 paper with 1.5 cm margins
  if (!EvaluateNumericValue(.width.in, positive_val=TRUE, min_val=2, max_val=10.51182, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name='Calculated Width in Inches', .verbosity)){ return() } ## dimensions of A4 paper with 1.5 cm margins
  if (pdf){
    .scaling.factor = 1
  }else{
    .scaling.factor = scaling_factor
  }
  if (verbosity > 2){
    # Plot dimensions (w or w/o scaling)
    .detailed.output[['"Plot Dimensions (w scaling)"']] = paste0('\t', '.) ', 'Plot Width: ', round(.scaling.factor*.width.in/cm_to_in, 3), ' cm',
                                                     '\n', '\t', '.) ', 'Plot Height: ', round(.scaling.factor*.height.in/cm_to_in, 3), ' cm')
    .detailed.output[['"Plot Dimensions (w/o scaling)"']] = paste0('\t', '.) ', 'Plot Width: ', round(.width.in/cm_to_in, 3), ' cm',
                                                                 '\n', '\t', '.) ', 'Plot Height: ', round(.height.in/cm_to_in, 3), ' cm')
    # Width of panels
    .plot.width.parameters = .panel.info[[1]][['plot.width.parameters']]
    .detailed.output[['"Width of panels (w/o scaling)"']] = paste0('\t', '.) ', 'All panels: ', round(.width.in/cm_to_in, 3), ' cm',
                                               '\n', '\t', '.) ', 'Left (sample names): ', round(.plot.width.parameters[['panels.max.width.cm']], 3), ' cm',
                                               '\n', '\t', '.) ', 'Scale: ', round(.plot.width.parameters[['scale.panel.width.cm']], 3), ' cm',
                                               '\n', '\t', '.) ', 'Margins: ', round(.plot.widths.cm[['margin.width.cm']], 3), ' cm',
                                               '\n', '\t', '.) ', 'Tracks: ', round(.plot.width.parameters[['tracks.width.cm']], 3), ' cm')
    # Height of panels
    .track.height.cm = .basic.plot.parameters[[1]][['track.height.cm']]
    .plot.height.parameters = .plot.vertical.parameters * .track.height.cm
    .detailed.output[['"Height of panels (w/o scaling)"']] = paste0('\t', '.) ', 'All panels: ', round(.height.in/cm_to_in, 3), ' cm',
                                                      '\n', '\t', '.) ', 'Header: ', round(.plot.height.parameters['header'], 3), ' cm/segment',
                                                      '\n', '\t', '.) ', 'Genomic scale: ', round(.plot.height.parameters['scale'], 3), ' cm/segment',
                                                      '\n', '\t', '.) ', 'Tracks: ', round(.plot.height.parameters['seq'], 3), ' cm/segment',
                                                      '\n', '\t', '.) ', 'Line-spacer: ', round(.plot.height.parameters['line-spacer'], 3), ' cm/segment',
                                                      '\n', '\t', '.) ', 'Empty-spacer: ', round(.plot.height.parameters['empty-spacer'], 3), ' cm/segment',
                                                      '\n', '\t', '.) ', 'Thickline-spacer: ', round(.plot.height.parameters['thickline-spacer'], 3), ' cm/segment',
                                                      '\n', '\t', '.) ', 'Annotation: ', round(.plot.height.parameters['annot'], 3), ' cm/line',
                                                      '\n', '\t', '.) ', 'Squished annotation: ', round(.plot.height.parameters['annot_squished'], 3), ' cm/line',
                                                      '\n', '\t', '.) ', 'Annotation text: ', round(.plot.height.parameters['annot_text_segment'], 3), ' cm/line')
    # Panels text orientation
    .horizontal.panels.list = .panel.info[[1]][['horizontal.panels.list']]
    .detailed.output[['"Panel text orientation"']] = paste(unlist(lapply(names(.horizontal.panels.list), function(x) paste0(c(paste0('\t', '.) ', x, ':', '\t'), sapply(.horizontal.panels.list[[x]], function(y) ifelse(y, 'HOR', 'VER'))), collapse=' ') )), collapse='\n')
    # Panel Font sizes
    .panel.font.size.list = .panel.info[[1]][['panel.font.size.list']]
    .detailed.output[['"Panel font sizes"']] = paste(unlist(lapply(names(.panel.font.size.list), function(x) paste0(c(paste0('\t', '.) ', x, ':', '\t'), .panel.font.size.list[[x]]), collapse=' ') )), collapse='\n')
    # Other Font sizes
    .detailed.output[['"Other font sizes"']] = paste0('\t', '.) ', 'Title: ', .header.font.sizes['main'],
                                                     '\n', '\t', '.) ', 'Subtitle: ', .header.font.sizes['sub'],
                                                     '\n', '\t', '.) ', 'Scalebar: ', .header.font.sizes['scale'],
                                                     '\n', '\t', '.) ', 'Genomic scale: ', .genomic.scale.font.size,
                                                     '\n', '\t', '.) ', 'Data scale: ', .scale.fontsize,
                                                     '\n', '\t', '.) ', 'Dataset names: ', max(sapply(.panel.font.size.list, function(x) x[1])),
                                                     '\n', '\t', '.) ', 'Sample names (max): ', max(unlist(lapply(.panel.font.size.list, function(x) x[2:length(x)]))),
                                                     '\n', '\t', '.) ', 'Sample names (min): ', min(unlist(lapply(.panel.font.size.list, function(x) x[2:length(x)]))),
                                                     '\n', '\t', '.) ', 'Annotation titles: ', .annot.panel.font.size,
                                                     '\n', '\t', '.) ', 'Annotation names: ', .feature.names.font.size)
    # Plotting Segment Order
    .detailed.output.vector = c()
    if (.strands.intermingled){
      .pso = c(.plotting.segment.order[['+']], 'scale')[c(rep(TRUE, length(.plotting.segment.order[['+']])), (!genomic_scale_on_top & include_genomic_scale))]
      .detailed.output.vector = c(.detailed.output.vector, paste0('\t', '.) ', ifelse(interface=='R', '+', 'upper'), ': ', paste(.pso, collapse=',')))
    }else{
      if (both_strands){
        for (.s in c('+', '-')){
          .pso = .plotting.segment.order[[.s]]
          .detailed.output.vector = c(.detailed.output.vector, paste0('\t', '.) ', ifelse(interface=='R', .s, ifelse(.s=='+', 'upper', 'lower')), ': ', paste(.pso, collapse=',')))
        }
      }else{
        .s = names(.plotting.segment.order)
        .pso = .plotting.segment.order[[.s]]
        .detailed.output.vector = c(.detailed.output.vector, paste0('\t', '.) ', ifelse(interface=='R', .s, 'upper'), ': ', paste(.pso, collapse=',')))
      }
    }
    .detailed.output[[ifelse(interface=='R', '"plotting_segment_order"', '"Plotting Segment Order"')]] = .detailed.output.vector
  }
  PreparePlottingInterface(plot_dim=c(.width.in, .height.in), pdf, pdf_name, pdf_dir, header, .bin.size, feature, .scaling.factor)
  ##### <- pdf or on-screen plotting?
  ##### -> plotting
  .plotting.ready.segment.order = NumberingSpacers(.plotting.segment.order)
  if (.strands.intermingled){
    .plotted.strand = '+-'
    .first.plot = TRUE
    .plotting.segments = c(.plotting.ready.segment.order[['+']], 'scale')[c(rep(TRUE, length(.plotting.ready.segment.order[['+']])), (!genomic_scale_on_top & include_genomic_scale))]
    for (.plotting.segment in .plotting.segments){
      PlotSegment(feature, .plotted.region, .plotted.strand, both_strands, .plotting.segment, .basic.plot.parameters, .neg.vals.neg.strand, .plot.width.parameters, .plot.vertical.parameters, .annot.info, .panel.info, .panels.list, .panel.separators, .separators.lwds, .separators.colors, incl_first_panel, print_one_line_sample_names, replicate_names, header, .header.font.sizes, .scaling.factor, .full.width.cm, genomic_scale_on_top, .genomic.scale.font.size, reverse_strand_direction, .bin.stats, dummy_plot, .tracks, .strands.alpha, .intermingled.color, .unstranded.beds, .annotation.packing, .annotation.panel.font.size, .incl.feature.names, .feature.names.font.size, .feature.names.above, .final.feature.text.org, .incl.feature.brackets, .incl.feature.shadings, .feature.shading.colors, feature_shading_alpha, .annot.cols, .group.autoscale, incl_track_scales, .scientific.scale, .scale.fontsize, .force.scale.list, .log2.transform, colors, alternating_background, .bgr.colors, bgr_alpha, .font.colors, .letter.widths, .letter.heights, .enhance.signals, .first.plot, .verbosity)
    }
  }else{
    for (.plotted.strand in names(.plotting.ready.segment.order)){
      .first.plot = which(names(.plotting.ready.segment.order)==.plotted.strand)==1
      for (.plotting.segment in .plotting.ready.segment.order[[.plotted.strand]]){
        PlotSegment(feature, .plotted.region, .plotted.strand, both_strands, .plotting.segment, .basic.plot.parameters, .neg.vals.neg.strand, .plot.width.parameters, .plot.vertical.parameters, .annot.info, .panel.info, .panels.list, .panel.separators, .separators.lwds, .separators.colors, incl_first_panel, print_one_line_sample_names, replicate_names, header, .header.font.sizes, .scaling.factor, .full.width.cm, genomic_scale_on_top, .genomic.scale.font.size, reverse_strand_direction, .bin.stats, dummy_plot, .tracks, .strands.alpha, .intermingled.color, .unstranded.beds, .annotation.packing, .annotation.panel.font.size, .incl.feature.names, .feature.names.font.size, .feature.names.above, .final.feature.text.org, .incl.feature.brackets, .incl.feature.shadings, .feature.shading.colors, feature_shading_alpha, .annot.cols, .group.autoscale, incl_track_scales, .scientific.scale, .scale.fontsize, .force.scale.list, .log2.transform, colors, alternating_background, .bgr.colors, bgr_alpha, .font.colors, .letter.widths, .letter.heights, .enhance.signals, .first.plot, .verbosity)
      }
    }
  }
  ##### -> plotting
  if (pdf){ suppressMessages( dev.off() ) }else{ suppressMessages( dev.set(which=2) ) }
  if (.verbosity > 2){
    cat(paste0('\n', 'Detailed Output:'), '\n')
    for (.det.out in names(.detailed.output)){
      cat(paste0(' - ', .det.out, ':'), '\n')
      cat(paste(.detailed.output[[.det.out]], collapse='\n'), '\n')
    }
    t2 = Sys.time()
    cat(paste('total plotting time:', format(difftime(t2, t1))), '\n')
  }
  if (.verbosity > 0){ cat('plot done', '\n') }
  ##### -> warnings
  # function printing all warnings
  ##### <- warnings
}


#### Functions

#' Print Output
#'
#' @details
#' Internal function: print output, warnings and errors based on messages list
PrintOutput = function(messages, verbosity, no_line_change=FALSE){
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


#' Plot Widths
#'
#' @details
#' Internal function: Make sense of the supplied width arguments
#'
PlotWidths = function(panels_max_width_cm, scale_panel_width_cm, margin_width_cm, track_width_cm, full_width_cm, incl_track_scales, verbosity, interface){
  .messages = list('output'=list(), 'errors'=list(), 'warnings'=list())
  .margin.width.cm = NULL
  if (is.numeric(margin_width_cm)){
    if (sign(margin_width_cm) >= 0){
      .margin.width.cm = margin_width_cm
    }
  }
  if (is.null(.margin.width.cm)){
    .arg.name = ifelse(interface=='R', '"margin_width_cm"', '"Margins Width"')
    .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', .arg.name,  ' argument is set to ', margin_width_cm, '; it should be a positive numeric value (recommended between 0 and 0.25 cm); automatically setting to 0.05 cm')
    .margin.width.cm = 0.05
  }
  .scale.panel.width.cm = NULL
  if (incl_track_scales){
    if (scale_panel_width_cm=='auto'){
      .scale.panel.width.cm = -1
    }else if (is.numeric(scale_panel_width_cm)){
      if (sign(scale_panel_width_cm) > 0){
        .scale.panel.width.cm = scale_panel_width_cm
      }
    }
    if (is.null(.scale.panel.width.cm)){
      .arg.name = ifelse(interface=='R', '"scale_panel_width_cm"', '"Tracks Scale Width"')
      .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', .arg.name, ' argument is set to ', scale_panel_width_cm, '; it should be either a numeric value (recommended between 0.5 and 2 cm) or set to "auto"')
    }
  }else{
    .scale.panel.width.cm = 0
  }
  .panels.max.width.cm = NULL
  if (panels_max_width_cm=='auto'){
    .panels.max.width.cm = -1
  }else if (is.numeric(panels_max_width_cm)){
    if (sign(panels_max_width_cm) > 0){
      .panels.max.width.cm = panels_max_width_cm
    }
  }
  if (is.null(.panels.max.width.cm)){
    .arg.name = ifelse(interface=='R', '"panels_max_width_cm"', '"Panels Width"')
    .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', .arg.name, ' argument is set to ', panels_max_width_cm, '; it should be either a numeric value (recommended between 1.5 and 6 cm) or set to "auto"')
  }
  .track.width.cm = NULL
  if (!is.null(track_width_cm)){
    if (is.numeric(track_width_cm)){
      if (sign(track_width_cm) > 0){
        .track.width.cm = track_width_cm
      }
    }
  }
  .full.width.cm = -1
  if (is.null(full_width_cm)){
    if (!is.null(.track.width.cm)){
      if (!is.null(.panels.max.width.cm) & .panels.max.width.cm != -1 & !is.null(.scale.panel.width.cm) & .scale.panel.width.cm != -1){
        .full.width.cm = .panels.max.width.cm + .scale.panel.width.cm + .track.width.cm + 2 * .margin.width.cm
      }
    }else{
      .arg.name1 = ifelse(interface=='R', '"full_width_cm"', '"Full Plot Width"')
      .arg.name2 = ifelse(interface=='R', '"track_width_cm"', '"Tracks Width"')
      .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'at least one of the arguments ', .arg.name1, ' and ', .arg.name2, ' should be assigned a numeric value (recommended between 5 and 25 cm)',
                              '\n', '\t', '.) ', .arg.name1, ': ', full_width_cm,
                              '\n', '\t', '.) ', .arg.name2, ': ', track_width_cm)
    }
  }else if (is.numeric(full_width_cm)){
    if (sign(full_width_cm) > 0){
      .full.width.cm = full_width_cm
      if (!is.null(.panels.max.width.cm) & .panels.max.width.cm != -1 & !is.null(.scale.panel.width.cm) & .scale.panel.width.cm != -1){
        if (is.null(.track.width.cm)){
          .track.width.cm = .full.width.cm - .panels.max.width.cm - .scale.panel.width.cm - 2 * .margin.width.cm
        }else{
          if (.full.width.cm != .panels.max.width.cm + .scale.panel.width.cm + .track.width.cm + 2 * .margin.width.cm){
            .arg.name1 = ifelse(interface=='R', '"full_width_cm"', '"Full Plot Width"')
            .arg.name2 = ifelse(interface=='R', '"track_width_cm"', '"Tracks Width"')
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'the provided "width" arguments do not fit together',
                                    '\n', '\t', '.) the following expression needs to be true:',
                                    '\n', '\t', '\t', 'full_width_cm = panels_max_width_cm + scale_panel_width_cm + track_width_cm + 2 * margin_width_cm',
                                    '\n', '\t', '.) ', 'consider setting one of the arguments ', .arg.name1, ' or ', .arg.name2, ' to NULL')
          }
        }
        if (sign(.track.width.cm) <= 0){
          .arg.name = ifelse(interface=='R', '"track_width_cm"', '"Tracks Width"')
          .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'the calculated ', .arg.name, ' argument is negative',
                                  '\n', '\t', '.) the following expression needs to be true:',
                                  '\n', '\t', '\t', 'full_width_cm = panels_max_width_cm + scale_panel_width_cm + track_width_cm + 2 * margin_width_cm')

        }
      }
      if (.panels.max.width.cm==-1){
        .arg.name1 = ifelse(interface=='R', '"panels_max_width_cm"', '"Panels Width"')
        .arg.name2 = ifelse(interface=='R', '"full_width_cm"', '"Full Plot Width"')
        .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', .arg.name1, ' argument is set to ', panels_max_width_cm, ', which is only accepted if ', .arg.name2, ' is NULL in which case it will be calculated based on the "optimal" organization of panels',
                                '\n', '\t', '.) if ', .arg.name2,' is correctly set to ', full_width_cm, ' cm, then change ', .arg.name1, ' to the max width of panels in centimeters (recommended numeric value between 1.5-6)')
      }
      if (.scale.panel.width.cm==-1){
        .arg.name1 = ifelse(interface=='R', '"scale_panel_width_cm"', '"Tracks Scale Width"')
        .arg.name2 = ifelse(interface=='R', '"full_width_cm"', '"Full Plot Width"')
        .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', .arg.name1, ' argument is set to ', scale_panel_width_cm, ', which is only accepted if ', .arg.name2, ' is NULL',
                                '\n', '\t', '.) if ', .arg.name2,' is correctly set to ', full_width_cm, ' cm, then change ', .arg.name1, ' to the desired width of scale panels in centimeters (recommended numeric value between 0.5-2)')
      }
    }else{
      .arg.name = ifelse(interface=='R', '"full_width_cm"', '"Full Plot Width"')
      .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', .arg.name, ' argument is set to ', full_width_cm, '; it should be either a positive numeric value (recommended between 5 and 25 cm) or set to NULL')
    }
  }else{
    .arg.name = ifelse(interface=='R', '"full_width_cm"', '"Full Plot Width"')
    .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', .arg.name, ' argument is set to ', full_width_cm, '; it should be either a positive numeric value (recommended between 5 and 25 cm) or set to NULL')
  }
  PrintOutput(.messages, verbosity)
  if (length(.messages[['errors']]) > 0){
    return()
  }else{
    return(c('panels.max.width.cm'=.panels.max.width.cm, 'scale.panel.width.cm'=.scale.panel.width.cm, 'margin.width.cm'=.margin.width.cm, 'track.width.cm'=.track.width.cm, 'full.width.cm'=.full.width.cm))
  }
}


#' Read In Annotations
#'
#' @details
#' Internal function: Read in annotations as GRanges objects from bed files - stored in list format
#'
ReadInAnnotations = function(annots, verbosity){
  if (verbosity > 0){ cat('reading annotations', '\n') }
  cat(paste(names(annots), collapse='\t'), '\n')
  if (!is.null(annots)){
    annotations = list()
    for (annot in names(annots)){
      annotations[[annot]] = rtracklayer::import(annots[[annot]], format='bed') #@ works on MacOS and Windows
    }
  }else{
    annotations = NULL
  }
  return(annotations)
}


#' Region GRanges
#'
#' @details
#' Internal function: Convert feature name or locus to be plotted to a GRanges object
#'
RegionGRanges = function(locus, tracks_width, feature=NULL, annotations=NULL, bin_start=NULL, extra_space=c(0.1,0.1), verbosity, interface){
  .messages = list('output'=list(), 'errors'=list(), 'warnings'=list())
  if (!is.null(feature) & !is.null(annotations)){
    .first.annot = which(sapply(names(annotations), function(x) feature %in% S4Vectors::mcols(annotations[[x]])$name))[1]
    if (is.na(.first.annot)){
      .first.annot = which(sapply(names(annotations), function(x) ifelse(length(which(grepl(paste0('^',feature,','), S4Vectors::mcols(annotations[[x]])$name) | grepl(paste0(',',feature, '$'), S4Vectors::mcols(annotations[[x]])$name) | grepl(paste0(',',feature, ','), S4Vectors::mcols(annotations[[x]])$name)))>0, T, F)))[1]
      if (is.na(.first.annot)){
        .arg.name = ifelse(interface=='R', '"feature"', '"locus"')
        .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'can\'t find your ', .arg.name, ' name in the annotation - aborting')
      }else{
        .first.half.statement = paste('couldn\'t exactly find', feature)
        feature = S4Vectors::mcols(annotations[[names(.first.annot)]])$name[which(grepl(paste0('^',feature,','), S4Vectors::mcols(annotations[[names(.first.annot)]])$name) | grepl(paste0(',',feature, '$'), S4Vectors::mcols(annotations[[names(.first.annot)]])$name) | grepl(paste0(',',feature, ','), S4Vectors::mcols(annotations[[names(.first.annot)]])$name))]
        .second.half.statement = paste(', but instead found', feature)
        .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', .first.half.statement, .second.half.statement)
      }
    }
    if (length(.messages[['errors']]) == 0){
      .feature.annot = annotations[[.first.annot]][which(S4Vectors::mcols(annotations[[.first.annot]])$name==feature)]
      .chrom = unique(as.character(GenomeInfoDb::seqnames(.feature.annot)))
      .strand = unique(as.character(BiocGenerics::strand(.feature.annot)))
      if (length(.chrom) > 1 | length(.strand) > 1){
        .arg.name = ifelse(interface=='R', '"feature"', '"locus"')
        .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'the ', .arg.name, ' name is ambiguous - aborting')
      }
      .chrom.start = min(IRanges::start(IRanges::ranges(.feature.annot)))
      .chrom.end = max(IRanges::end(IRanges::ranges(.feature.annot)))
    }
  }else if (!is.null(locus)){
    .chrom = locus[1]
    .strand = locus[2]
    .chrom.start = as.integer(locus[3])
    .chrom.end = as.integer(locus[4])
    extra_space=c(0,0)
  }else{
    .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'the provided information about the locus of interest is incomplete - aborting')
  }
  if (length(.messages[['errors']]) == 0){
    .gene.width = .chrom.end - .chrom.start + 1
    if (.gene.width < 100){
      .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', 'the supplied region is less than 100 bp wide (',  .gene.width, ') - adjusting to 100 bp (minimum width) based on start coordinate')
      .chrom.end = .chrom.start + 100
    }
    if (is.null(bin_start)){
      .bin.start = ifelse(.strand=='+', .chrom.start, .chrom.end)
    }else{
      if (bin_start < .chrom.start | bin_start > .chrom.end){
        .bin.start = ifelse(.strand=='+', .chrom.start, .chrom.end)
        .arg.name = ifelse(interface=='R', '"bin_start"', '"Bins Center"')
        .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', 'the set ',  .arg.name, ' is outside of the plotted region - setting to ', .bin.start)
      }
    }
    if(.strand=='-'){ extra_space = rev(extra_space) }
    .chrom.start = .chrom.start - as.integer(extra_space[1]*.gene.width)
    .chrom.end = .chrom.end + as.integer(extra_space[2]*.gene.width)
    PrintOutput(.messages, verbosity)
    return(GenomicRanges::GRanges(.chrom, .strand, ranges=IRanges::IRanges(.chrom.start, .chrom.end), bin.start=.bin.start, tracks.width=tracks_width))
  }else{
    PrintOutput(.messages, verbosity)
    return()
  }
}


#' Find Nonoverlapping IVs
#'
#' @details
#' Internal function: Find nonoverlapping intervals within a GRanges object for each interval (based on the SelfHits object obtained by findOverlaps function applied to the GRanges object beforehand)
#' - used together with "ConnectIVs" to organize the individual feature annotations, such that they take up as little space as possible
#'
FindNonoverlappingIVs = function(n_IV, n_IVs, overlaps){
  nonoverlapping.IVs = c(n_IV, setdiff(1:n_IVs, S4Vectors::subjectHits(overlaps)[which(S4Vectors::queryHits(overlaps)==n_IV)]))
  nonoverlapping.IVs = nonoverlapping.IVs[which(nonoverlapping.IVs >= n_IV)]
  return(nonoverlapping.IVs)
}


#' Connect IVs
#'
#' @details
#' Internal function: Connect non-overlapping intervals, such that the number of "lines" used in the annotation track is minimized
#' - used together with "FindNonoverlappingIVs" IVs to organize the individual feature annotations, such that they take up as little space as possible
#'
ConnectIVs = function(n_IV, nonoverlapping_IVs, remaining_IVs){
  .nonoverlapping = nonoverlapping_IVs[[rev(n_IV)[1]]]
  .nonoverlapping = .nonoverlapping[which(.nonoverlapping > rev(n_IV)[1])]
  .nonoverlapping = intersect(.nonoverlapping, remaining_IVs)
  if (length(.nonoverlapping) != 0){
    return( c(n_IV, ConnectIVs(.nonoverlapping[1], nonoverlapping_IVs, remaining_IVs)) )
  }else{
    return(n_IV)
  }
}


#' Organize Overlapping IVs
#'
#' @details
#' Internal function: Overlapping feature intervals (transcripts/genes/transcription units) are organized in separate lines to allow "non-overlapping" plotting
#'
OrganizeOverlappingIVs = function(annotation_gr, gap=-1L){
  if (length(annotation_gr) > 1){
    .overlaps = GenomicRanges::findOverlaps(annotation_gr, maxgap=gap)
    .IVs = 1:length(annotation_gr)
    .nonoverlapping.IVs = lapply(.IVs, FindNonoverlappingIVs, length(annotation_gr), .overlaps)
    .annotation.lines = list()
    .n = 1
    while (length(.IVs) > 0){
      .current.IVs = ConnectIVs(.n, .nonoverlapping.IVs, .IVs)
      .current.IVs = intersect(.current.IVs, .IVs)
      if (length(.current.IVs) > 0){
        .annotation.lines[[length(.annotation.lines)+1]] = .current.IVs
        .IVs = setdiff(.IVs, .current.IVs)
      }
      .n = .IVs[1]
    }
  }else{
    .annotation.lines = list(1)
  }
  return(.annotation.lines)
}


#' Annotated Features In Region
#'
#' @details
#' Internal function: This function is the first step in organizing the annotated features in the plotted region into various formats
#' Used together with "OrganizeOverlappingLoci" and "ConvertCollapsedFormat" to create a final organized version of the annotated features
#'
AnnotatedFeaturesInRegion = function(plotted_region, annotations){
  .strand = as.character(BiocGenerics::strand(plotted_region))
  .plot.width = IRanges::width(plotted_region)
  .tracks.width = S4Vectors::mcols(plotted_region)$tracks.width
  ##### pack the annotation into fewest possible lines - there should be a gap of at least 0.05 cm between annotations on same line
  .gap = as.integer(0.05*.plot.width/.tracks.width)
  .gap = ifelse(.gap > 1, .gap, 1)
  #####
  region.annotations = list()
  for (.annotation in names(annotations)){
    region.annotations[[.annotation]] = list()
    ##### find annotated features overlapping with plotted region
    ## if bed file has unstranded data - only place under plus strand visualization
    .data.strand = as.character(BiocGenerics::strand(annotations[[.annotation]]))
    if (all(.data.strand=="*")){
      BiocGenerics::strand(annotations[[.annotation]]) = .strand
    }
    if ( GenomicRanges::seqnames(GenomicRanges::seqinfo(plotted_region)) %in% GenomicRanges::seqnames(GenomicRanges::seqinfo(annotations[[.annotation]])) ){
      .subset.annotation = sort(IRanges::subsetByOverlaps(annotations[[.annotation]], plotted_region), decreasing = FALSE)
    }else{
      .subset.annotation = NULL
    }
    if (length(.subset.annotation) > 0){
      if (.strand == '-'){
        .subset.annotation = .subset.annotation[order(IRanges::end(.subset.annotation), decreasing=TRUE)]
      }
      ##### group based on feature name
      .feat.names = unique(S4Vectors::mcols(.subset.annotation)$name) ## if different genes overlap, their transcripts will be displayed in separate clusters
      ##### color - pick the most prevalent color in the collapsed set of transcripts
      .collapsed.cols = unlist(lapply(.feat.names, function(.feat.name) sort(S4Vectors::mcols(S4Vectors::subset(.subset.annotation, name==.feat.name))$itemRgb)[1]))
      ##### collapse .annotation based on feature name
      .collapsed.annot = unlist(as(lapply(.feat.names, function(.feat.name) {.ann=S4Vectors::subset(.subset.annotation, name==.feat.name); return(GenomicRanges::GRanges(IRanges::subsetByOverlaps(GenomicRanges::reduce(.ann), plotted_region), name=.feat.name))}), 'GRangesList'))
      if (!is.null(.collapsed.cols)){
        S4Vectors::mcols(.collapsed.annot)$itemRgb = .collapsed.cols
      }else{
        S4Vectors::mcols(.collapsed.annot)$itemRgb = '#000000'
      }
      # are collapsed transcript models extending past plotted region
      S4Vectors::mcols(.collapsed.annot)$on.from.start = FALSE
      S4Vectors::mcols(.collapsed.annot)$on.from.end = FALSE
      ##### check if feature regions overlap or not
      .is.disjoint = GenomicRanges::isDisjoint(.collapsed.annot)
      if (!.is.disjoint){
        .red.collapsed.annot = GenomicRanges::reduce(.collapsed.annot, with.revmap=TRUE)
        if (.strand == '-'){
          .red.collapsed.annot = .red.collapsed.annot[GenomicRanges::order(IRanges::end(.red.collapsed.annot), decreasing=TRUE)]
        }
        .revmap = S4Vectors::mcols(.red.collapsed.annot)$revmap
        .collapsed.names.list = list()
        .collapsed.names = rep(NA, length(.revmap))
        .collapsed.cols = rep(NA, length(.revmap))
        .disjoint = rep(T, length(.revmap))
        for (i in 1:length(.revmap)){
          if (.strand=='-'){
            .revmap.IVs = .revmap[[i]]
          }else{
            .revmap.IVs = rev(.revmap[[i]])
          }
          if (length(.revmap.IVs) > 1){
            .disjoint[i] = F
          }
          .collapsed.names.list[[i]] = S4Vectors::mcols(.collapsed.annot)$name[ .revmap.IVs ]
          .collapsed.cols[i] = sort(S4Vectors::mcols(.collapsed.annot)$itemRgb[ .revmap.IVs ])[1]
          .collapsed.names[i] = paste(.collapsed.names.list[[i]], collapse=',')
        }
        .red.collapsed.annot = GenomicRanges::GRanges(GenomicRanges::granges(.red.collapsed.annot), name=.collapsed.names, itemRgb=.collapsed.cols, disjoint=.disjoint)
        S4Vectors::mcols(.red.collapsed.annot)$collapsed.names = .collapsed.names.list
      }else{
        .red.collapsed.annot = .collapsed.annot
        S4Vectors::mcols(.red.collapsed.annot)$disjoint = T
        S4Vectors::mcols(.red.collapsed.annot)$collapsed.names = as(S4Vectors::mcols(.collapsed.annot)$name, 'list')
      }
      S4Vectors::mcols(.red.collapsed.annot)$on.from.start = FALSE
      S4Vectors::mcols(.red.collapsed.annot)$on.from.end = FALSE
      S4Vectors::mcols(.red.collapsed.annot) = S4Vectors::mcols(.red.collapsed.annot)[c('name', 'itemRgb', 'on.from.start', 'on.from.end', 'disjoint', 'collapsed.names')]
      # are collapsed transcript models extending past plotted region
      if (any(IRanges::start(.collapsed.annot) < IRanges::start(plotted_region)) | any(IRanges::end(.collapsed.annot) > IRanges::end(plotted_region))){
        # subset of transcript models that either start or end outside of plotted region
        .subset = which(IRanges::start(.collapsed.annot) < IRanges::start(plotted_region) | IRanges::end(.collapsed.annot) > IRanges::end(plotted_region))
        S4Vectors::mcols(.collapsed.annot)$on.from.start[.subset] = sapply(.subset, function(.n.trn) ifelse(IRanges::start(.collapsed.annot)[.n.trn] < IRanges::start(plotted_region), TRUE, FALSE) )
        S4Vectors::mcols(.collapsed.annot)$on.from.end[.subset] = sapply(.subset, function(.n.trn) ifelse(IRanges::end(.collapsed.annot)[.n.trn] > IRanges::end(plotted_region), TRUE, FALSE) )
        IRanges::start(.collapsed.annot)[S4Vectors::mcols(.collapsed.annot)$on.from.start] = IRanges::start(plotted_region)
        IRanges::end(.collapsed.annot)[S4Vectors::mcols(.collapsed.annot)$on.from.end] = IRanges::end(plotted_region)
      }
      if (any(IRanges::start(.red.collapsed.annot) < IRanges::start(plotted_region)) | any(IRanges::end(.red.collapsed.annot) > IRanges::end(plotted_region))){
        # subset of transcript models that either start or end outside of plotted region
        .subset = which(IRanges::start(.red.collapsed.annot) < IRanges::start(plotted_region) | IRanges::end(.red.collapsed.annot) > IRanges::end(plotted_region))
        S4Vectors::mcols(.red.collapsed.annot)$on.from.start[.subset] = sapply(.subset, function(.n.trn) ifelse(IRanges::start(.red.collapsed.annot)[.n.trn] < IRanges::start(plotted_region), TRUE, FALSE) )
        S4Vectors::mcols(.red.collapsed.annot)$on.from.end[.subset] = sapply(.subset, function(.n.trn) ifelse(IRanges::end(.red.collapsed.annot)[.n.trn] > IRanges::end(plotted_region), TRUE, FALSE) )
        IRanges::start(.red.collapsed.annot)[S4Vectors::mcols(.red.collapsed.annot)$on.from.start] = IRanges::start(plotted_region)
        IRanges::end(.red.collapsed.annot)[S4Vectors::mcols(.red.collapsed.annot)$on.from.end] = IRanges::end(plotted_region)
      }
      #.feature.annotations = GenomicRanges::GRangesList()
      .feature.annotations = list()
      .annotation.lines = list()
      for (.feat.name in .feat.names){
        #cat(.feat.name, '\n')
        .feat.annot = S4Vectors::subset(.subset.annotation, name==.feat.name)
        .annotation.lines[[.feat.name]] = OrganizeOverlappingIVs(.feat.annot, .gap)
        # are transcript models (exon-intron structure) interrupted
        S4Vectors::mcols(.feat.annot)$intron.from.start = FALSE
        S4Vectors::mcols(.feat.annot)$intron.from.end = FALSE
        if (!('blocks' %in% colnames((S4Vectors::mcols(.feat.annot))))){
          S4Vectors::mcols(.feat.annot)$blocks = IRanges::IRangesList(sapply(1:length(.feat.annot), function(x) IRanges::IRanges(start=1, end=IRanges::width(IRanges::ranges(.feat.annot[x])) + 1)))
        }
        if (any(IRanges::start(.feat.annot) < IRanges::start(plotted_region)) | any(IRanges::end(.feat.annot) > IRanges::end(plotted_region))){
          # subset of transcript models that either start or end outside of plotted region
          .subset = which(IRanges::start(.feat.annot) < IRanges::start(plotted_region) | IRanges::end(.feat.annot) > IRanges::end(plotted_region))
          # obtain the exons from those transcript models that are present within the plotted region
          exons = S4Vectors::mcols(.feat.annot)$blocks
          .exon.ranges = lapply(.subset, function(.n.trn) GenomicRanges::sort(IRanges::overlapsRanges(IRanges::IRanges(start=IRanges::start(.feat.annot)[.n.trn]-1 + IRanges::start(exons[[.n.trn]]), end=IRanges::start(.feat.annot)[.n.trn]-1 + IRanges::end(exons[[.n.trn]])), IRanges::ranges(plotted_region)), decreasing = FALSE) )
          # truncated starts and ends
          .trunc.starts = sapply(IRanges::start(.feat.annot)[.subset], function(.trn.start)  ifelse(.trn.start < IRanges::start(plotted_region), IRanges::start(plotted_region), .trn.start) )
          .trunc.ends = sapply(IRanges::end(.feat.annot)[.subset], function(.trn.end)  ifelse(.trn.end > IRanges::end(plotted_region), IRanges::end(plotted_region), .trn.end) )
          S4Vectors::mcols(.feat.annot)$intron.from.start[.subset] = sapply( 1:length(.exon.ranges), function(.n.trn) ifelse(length(.exon.ranges[[.n.trn]]) > 0, (min(IRanges::start(.exon.ranges[[.n.trn]])) != .trunc.starts[.n.trn]), TRUE))
          S4Vectors::mcols(.feat.annot)$intron.from.end[.subset] = sapply( 1:length(.exon.ranges), function(.n.trn) ifelse(length(.exon.ranges[[.n.trn]]) > 0, (max(IRanges::end(.exon.ranges[[.n.trn]])) != .trunc.ends[.n.trn]), TRUE))
          IRanges::start(.feat.annot)[.subset] = sapply( 1:length(.exon.ranges), function(.n.trn) ifelse(length(.exon.ranges[[.n.trn]]) > 0, min(IRanges::start(.exon.ranges[[.n.trn]])), .trunc.starts[.n.trn]) )
          IRanges::end(.feat.annot)[.subset] = sapply( 1:length(.exon.ranges), function(.n.trn) ifelse(length(.exon.ranges[[.n.trn]]) > 0, max(IRanges::end(.exon.ranges[[.n.trn]])), .trunc.ends[.n.trn]) )
          .exon.ranges = lapply( 1:length(.exon.ranges), function(.n.trn) {if (length(.exon.ranges[[.n.trn]]) > 0){IRanges::shift(.exon.ranges[[.n.trn]], shift=-min(IRanges::start(.exon.ranges[[.n.trn]]))+1 )}else{IRanges::shift(IRanges::ranges(.feat.annot), -IRanges::start(.feat.annot)+1)}} )
          S4Vectors::mcols(.feat.annot)$blocks[.subset] = as(.exon.ranges, 'IRangesList')
          if ("thick" %in% names(S4Vectors::mcols(.feat.annot))){
            S4Vectors::mcols(.feat.annot)$thick[.subset] = IRanges::IRanges(IRanges::start(.feat.annot)[.subset], width=0)
          }
        }
        .feature.annotations[[.feat.name]] = .feat.annot
      }
      .feature.annotations = as(.feature.annotations, "GRangesList")
    }else{
      .red.collapsed.annot=NULL
      .collapsed.annot=NULL
      .feature.annotations=NULL
      .annotation.lines=NULL
    }
    region.annotations[[.annotation]][['collapsed']] = .red.collapsed.annot ## per definition the collapsed 'feature' is a one line representation
    region.annotations[[.annotation]][['collapsed2']] = .collapsed.annot ## collapsed2 is equal to collapsed if there is no overlap between regions defined by different feature names - otherwise it will use the number of lines necessary
    region.annotations[[.annotation]][['expanded']] = .feature.annotations
    region.annotations[[.annotation]][['packing']] = .annotation.lines
  }
  return(region.annotations)
}


#' Organize Overlapping Loci
#'
#' @details
#' Internal function: This function is the second step in organizing the annotated features in the plotted region into various formats
#' Used after AnnotatedFeaturesInRegion and before ConvertCollapsedFormat to create a final organized version of the annotated features
#'
OrganizeOverlappingLoci = function(annot_info){
  for (.annot.name in names(annot_info)){
    annot_info[[.annot.name]][['packing2']] = NULL
    if (length(annot_info[[.annot.name]]) > 0){
      annot_info[[.annot.name]][['packing2']] = structure(lapply(rep(1, length(annot_info[[.annot.name]][['collapsed']])), list), names=S4Vectors::mcols(annot_info[[.annot.name]][['collapsed']])$name)
      .non.disjoint = !S4Vectors::mcols(annot_info[[.annot.name]][['collapsed']])$disjoint
      if (any(.non.disjoint)){
        for (.n.nd in (1:length(.non.disjoint))[.non.disjoint]){
          .merged.feat.names = S4Vectors::mcols(annot_info[[.annot.name]][['collapsed']])$collapsed.names[[.n.nd]]
          .merged.feats.name = paste(.merged.feat.names, collapse=',')
          .subset.annotation = annot_info[[.annot.name]][['collapsed2']][S4Vectors::mcols(annot_info[[.annot.name]][['collapsed2']])$name %in% .merged.feat.names]
          annot_info[[.annot.name]][['packing2']][[.merged.feats.name]] = OrganizeOverlappingIVs(.subset.annotation)
          annot_info[[.annot.name]][['expanded']][[.merged.feats.name]] = unlist(annot_info[[.annot.name]][['expanded']][.merged.feat.names])
          .n.trn.per.feature = cumsum(unlist(lapply(annot_info[[.annot.name]][['expanded']][.merged.feat.names], length)))
          .merged.trn.numbers = structure(lapply(1:length(.n.trn.per.feature), function(n) {if (n==1){1:.n.trn.per.feature[n]}else{(.n.trn.per.feature[n-1]+1):.n.trn.per.feature[n]}}), names=.merged.feat.names)
          annot_info[[.annot.name]][['packing']][[.merged.feats.name]] = list()
          for (.merged.feat.name in .merged.feat.names){
            .sub.packing = lapply(annot_info[[.annot.name]][['packing']][[.merged.feat.name]], function(x) .merged.trn.numbers[[.merged.feat.name]][x])
            annot_info[[.annot.name]][['packing']][[.merged.feats.name]] = c(annot_info[[.annot.name]][['packing']][[.merged.feats.name]], .sub.packing)
            annot_info[[.annot.name]][['expanded']][[.merged.feat.name]] = NULL
            annot_info[[.annot.name]][['packing']][[.merged.feat.name]] = NULL
          }
        }
      }
    }
  }
  return(annot_info)
}


#' Convert Collapsed Format
#'
#' @details
#' Internal function: This function is the third step in organizing the annotated features in the plotted region into various formats
#' Used after AnnotatedFeaturesInRegion and OrganizeOverlappingLoci to create a final organized version of the annotated features
#' Converts the format of "collapsed" and "collapsed2" from GRanges to GRangesLists to make the overall formatting uniform; one GRanges object per 'feature name' (e.g. "TAF1D" and "RP11-178H8.7")
#'
ConvertCollapsedFormat = function(annot_info){
  for (.annot.name in names(annot_info)){
    if (!is.null(annot_info[[.annot.name]][['collapsed']])){
      .strand = unique(as.character(BiocGenerics::strand(annot_info[[.annot.name]][['collapsed']])))
      .collapsed.grl = split(annot_info[[.annot.name]][['collapsed']], as.factor(1:length(annot_info[[.annot.name]][['collapsed']])))
      names(.collapsed.grl) = S4Vectors::mcols(annot_info[[.annot.name]][['collapsed']])$name
      annot_info[[.annot.name]][['collapsed']] = .collapsed.grl
      .collapsed2.grl = GenomicRanges::GRangesList()
      for (.collapsed.feat in names(.collapsed.grl)){
        .collapsed2.grl[[.collapsed.feat]] = annot_info[[.annot.name]][['collapsed2']][ S4Vectors::mcols(annot_info[[.annot.name]][['collapsed2']])$name %in% S4Vectors::mcols(.collapsed.grl[[.collapsed.feat]])$collapsed.names[[1]] ]  ## always only one entry in this list
      }
      annot_info[[.annot.name]][['collapsed2']] = .collapsed2.grl
    }
  }
  return(annot_info)
}


#' Organize Annotated Features In Region
#'
#' @details
#' Internal function: Wrapper function that executes the three above functions
#'
OrganizeAnnotatedFeaturesInRegion = function(plotted_region, annotations){
  annot_info = AnnotatedFeaturesInRegion(plotted_region, annotations)
  annot_info = OrganizeOverlappingLoci(annot_info)
  annot_info = ConvertCollapsedFormat(annot_info)
  return(annot_info)
}


#' is.color
#'
#' @details
#' Internal function: Helper function that determines whether input (vector) is a color
#'
is.color = function(put_color_vector) {
  sapply(put_color_vector, function(.put.color) {
    tryCatch(is.matrix(col2rgb(.put.color)),
             error = function(e) FALSE)
  })
}


#' Scrutinize Expand And Name Parameter
#'
#' @details
#' Internal function: Helper function that will expand a common "one-dimensional" parameter to fit all the names of a named object
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
        if (!all(is.logical(parameter)) | any(is.na(parameter))){ #@  added | any(is.na(parameter))
          if (revert_to_default & !is.null(default_value)){
            .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a logical vector - using default values',
                                                                                  '\n', '\t', '.) ', paste(default_value, collapse=','))
            parameter = default_value
          }else{
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a logical vector - aborting')
          }
        }
      }else if (expect_standard=='integer'){
        if (!all(is.integer(parameter)) | any(is.na(parameter))){ #@  added | any(is.na(parameter))
          if (revert_to_default & !is.null(default_value)){
            .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be an integer vector - using default values',
                                                                                  '\n', '\t', '.) ', paste(default_value, collapse=','))
            parameter = default_value
          }else{
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be an integer vector - aborting')
          }
        }
      }else if (expect_standard=='numeric'){
        if (!all(is.numeric(parameter)) | any(is.na(parameter))){ #@  added | any(is.na(parameter))
          if (revert_to_default & !is.null(default_value)){
            .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a numeric vector - using default values',
                                                                                  '\n', '\t', '.) ', paste(default_value, collapse=','))
            parameter = default_value
          }else{
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a numeric vector - aborting')
          }
        }
      }else if (expect_standard=='color'){
        if (!all(is.color(parameter)) | any(is.na(parameter))){ #@  added | any(is.na(parameter))
          if (revert_to_default & !is.null(default_value)){
            .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a color vector - using default values',
                                                                                  '\n', '\t', '.) ', paste(default_value, collapse=','))
            parameter = default_value
          }else{
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', '"', .var.name1, '" argument should be a color vector - aborting')
          }
        }
      }else if (expect_standard=='character'){
        if (!all(is.character(parameter)) | any(is.na(parameter))){ #@  added | any(is.na(parameter))
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
#' @details
#' Internal function: Evaluate whether a vector/value is numeric and falls within recommmended range
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
        #@.messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', .var.name1, ' = ', paste(num_val, collapse=','), ':')
        .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0(' - ', .var.name1, ' = ', paste(num_val, collapse=','), ':')
        #@.messages[[ifelse(interval_obligatory, 'errors', ifelse(turn_errors_to_warnings, 'warnings', 'errors'))]][[length(.messages[[ifelse(interval_obligatory, 'errors', ifelse(turn_errors_to_warnings, 'warnings', 'errors'))]])+1]] = paste0(' - ', .var.name1, ' = ', paste(num_val, collapse=','), ':')
        for (i in 1:length(num_val)){
          if ((num_val < .min.val)[i] | (num_val > .max.val)[i]){
            .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0('\t', '.) ', .var.name1, '[', i, '] = ', num_val[i], ';', ' it should preferably be between ', .min.val[i], ' and ', .max.val[i])
            #@.messages[[ifelse(interval_obligatory, 'errors', ifelse(turn_errors_to_warnings, 'warnings', 'errors'))]][[length(.messages[[ifelse(interval_obligatory, 'errors', ifelse(turn_errors_to_warnings, 'warnings', 'errors'))]])+1]] = paste0('\t', '.) ', .var.name1, '[', i, '] = ', num_val[i], ';', ' it should preferably be between ', .min.val[i], ' and ', .max.val[i])
          }
        }
        if (interval_obligatory){ #@$ ->
          .messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]][[length(.messages[[ifelse(turn_errors_to_warnings, 'warnings', 'errors')]])+1]] = paste0('\t', '.) ', ifelse(turn_errors_to_warnings, 'setting to default value', 'aborting'))
          PrintOutput(.messages, verbosity)
          return(FALSE)
        } #@$ <-
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


#' List Depth
#'
#' @details
#' Internal function: Max number of nested levels in a nested list of lists
#'
ListDepth = function(query_list){
  ifelse(is.list(query_list), 1L + max(sapply(query_list, ListDepth)), 0L)
}


#' Panel Font Size List
#'
#' @details
#' Internal function: Constructs a panel_font_size_list (a font size provided to each panel)
#'
PanelFontSizeList = function(samples, panel_font_sizes, panel_font_size_list, incl_reps, replicate_names, verbosity, interface){
  .messages = list('output'=list(), 'errors'=list(), 'warnings'=list())
  if (is.null(panel_font_size_list)){
    .panel.font.size.list=NULL
    .panel.font.sizes = NULL
    if (!is.null(panel_font_sizes)){
      .max.n.panels = max(sapply(names(samples), function(.seqtype) ListDepth(samples[[.seqtype]]) + 2))
      if (length(panel_font_sizes) %in% c(1,2,.max.n.panels)){
        .panel.font.sizes = as.numeric(ScrutinizeExpandAndNameParameter(panel_font_sizes, 1:length(panel_font_sizes), use_names=FALSE, default_value=NULL, expect_standard='numeric', expect=NULL, revert_to_default=FALSE, alt_par_name=ifelse(interface=='R', 'panel_font_sizes', 'Panel Text Font Size(s)'), verbosity=verbosity))
      }else{
        .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(ifelse(interface=='R', '"panel_font_sizes"', '"Panel Text Font Size(s)"'), ' numeric vector should be of length 1', ifelse(.max.n.panels > 2, ', 2 or ', ' or '), .max.n.panels, '; it has a length of ', length(panel_font_sizes), ' - will be automatically determined instead')
      }
    }
    if (!is.null(.panel.font.sizes)){
      .panel.font.size.list = list()
      for (.seqtype in names(samples)){
        .n.panels = ListDepth(samples[[.seqtype]]) + 2
        if (length(.panel.font.sizes)==1 | length(.panel.font.sizes)==.n.panels){
          .final.panel.font.sizes = ScrutinizeExpandAndNameParameter(.panel.font.sizes, paste0('panel', 1:.n.panels), use_names=FALSE, default_value=NULL, expect_standard='numeric', expect=NULL, revert_to_default=FALSE, alt_par_name=ifelse(interface=='R', 'panel_font_sizes', 'Panel Text Font Size(s)'), verbosity=verbosity)
        }else if (length(.panel.font.sizes)==2){
          .final.panel.font.sizes = c(.panel.font.sizes[1], rep(.panel.font.sizes[2], .n.panels-1))
          .final.panel.font.sizes = ScrutinizeExpandAndNameParameter(.final.panel.font.sizes, paste0('panel', 1:.n.panels), use_names=FALSE, default_value=NULL, expect_standard='numeric', expect=NULL, revert_to_default=FALSE, alt_par_name=ifelse(interface=='R', 'panel_font_sizes', 'Panel Text Font Size(s)'), verbosity=verbosity)
        }else if (length(.panel.font.sizes) > .n.panels){
          .final.panel.font.sizes = c(.panel.font.sizes[1], rev(rev(.panel.font.sizes)[1:(.n.panels-1)]))
        }
        .panel.font.size.list[[.seqtype]] = as.numeric(.final.panel.font.sizes)
        if (incl_reps[.seqtype] & !is.null(replicate_names)){ #@ ->
          .panel.font.size.list[[.seqtype]] = c(.panel.font.size.list[[.seqtype]], rev(.panel.font.size.list[[.seqtype]])[1])
        } #@ <-
      }
    }
  }else{
    .panel.font.size.list = NULL
    if (is.list(panel_font_size_list)){
      if (identical(sort(names(panel_font_size_list)), sort(names(samples)))){
        .panel.font.size.list = list()
        .no.fit = FALSE
        .no.numeric = FALSE
        for (.seqtype in names(samples)){
          .n.panels = ListDepth(samples[[.seqtype]]) + 2
          .panel.font.sizes = panel_font_size_list[[.seqtype]]
          if (is.numeric(.panel.font.sizes) & length(.panel.font.sizes)==.n.panels){
            .final.panel.font.sizes = ScrutinizeExpandAndNameParameter(.panel.font.sizes, paste0('panel', 1:.n.panels), use_names=FALSE, default_value=NULL, expect_standard='numeric', expect=NULL, revert_to_default=FALSE, alt_par_name=ifelse(interface=='R', 'panel_font_size_list', 'Detailed Panel Text Font Sizes'), verbosity=verbosity)
            .panel.font.size.list[[.seqtype]] = as.numeric(.final.panel.font.sizes)
            if (incl_reps[.seqtype] & !is.null(replicate_names)){ #@ ->
              .panel.font.size.list[[.seqtype]] = c(.panel.font.size.list[[.seqtype]], rev(.panel.font.size.list[[.seqtype]])[1])
            } #@ <-
          }else if (!is.numeric(.panel.font.sizes)){
            .no.numeric = TRUE
          }else{
            .no.fit = TRUE
          }
        }
        if (.no.numeric){
          .panel.font.size.list = NULL
          .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(ifelse(interface=='R', '"panel_font_size_list"', '"Detailed Panel Text Font Sizes"'), ' should contain numeric values - will be automatically determined instead')
        }
        if (.no.fit){
          .panel.font.size.list = NULL
          .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(ifelse(interface=='R', '"panel_font_size_list"', '"Detailed Panel Text Font Sizes"'), ' does not fit the dataset - will be automatically determined instead')
        }
      }
    }else{
      .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(ifelse(interface=='R', '"panel_font_size_list"', '"Detailed Panel Text Font Sizes"'), ' is not a list - will be automatically determined instead')
    }
  }
  PrintOutput(.messages, verbosity)
  return(.panel.font.size.list)
}


#' Horizontal Panels List
#'
#' @details
#' Internal function: Checks/constructs a horizontal_panels_list
#'
HorizontalPanelsList = function(samples, horizontal_panels_list, incl_reps, replicate_names, verbosity, interface){
  .messages = list('output'=list(), 'errors'=list(), 'warnings'=list())
  .horizontal.panels.list = NULL
  if (!is.null(horizontal_panels_list)){
    .horizontal.panels.list = NULL
    if (is.list(horizontal_panels_list)){
      if (identical(sort(names(horizontal_panels_list)), sort(names(samples)))){
        .horizontal.panels.list = list()
        .no.fit = FALSE
        .no.logical = FALSE
        for (.seqtype in names(samples)){
          .n.panels = ListDepth(samples[[.seqtype]]) + 2
          .horizontal.panels = horizontal_panels_list[[.seqtype]]
          if (is.logical(.horizontal.panels) & length(.horizontal.panels)==.n.panels){
            .final.horizontal.panels = ScrutinizeExpandAndNameParameter(.horizontal.panels, paste0('panel', 1:.n.panels), use_names=FALSE, default_value=NULL, expect_standard='logical', expect=NULL, revert_to_default=FALSE, alt_par_name=ifelse(interface=='R', 'horizontal_panels_list', 'Panels Text Orientation'), verbosity=verbosity)
            .horizontal.panels.list[[.seqtype]] = as.numeric(.final.horizontal.panels)
            if (incl_reps[.seqtype] & !is.null(replicate_names)){ #@ ->
              .horizontal.panels.list[[.seqtype]] = c(.horizontal.panels.list[[.seqtype]], TRUE)
            } #@ <-
          }else if (!is.logical(.horizontal.panels)){
            .no.logical = TRUE
          }else{
            .no.fit = TRUE
          }
        }
        if (.no.logical){
          .horizontal.panels.list = NULL
          .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(ifelse(interface=='R', '"horizontal_panels_list"', '"Panels Text Orientation"'), ' should contain logical values - will be automatically determined instead')
        }
        if (.no.fit){
          .horizontal.panels.list = NULL
          .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(ifelse(interface=='R', '"horizontal_panels_list"', '"Panels Text Orientation"'), ' does not fit the dataset - will be automatically determined instead')
        }
      }
    }else{
      .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(ifelse(interface=='R', '"horizontal_panels_list"', '"Panels Text Orientation"'), ' is not a list - will be automatically determined instead')
    }
  }
  PrintOutput(.messages, verbosity)
  return(.horizontal.panels.list)
}

#' Parse Option String
#'
#' Parse string into relevant R object class
#'
#' @param option_str string representation of the option
#'
#' @details String can represent a named list, unnamed list, named vector, unnamed vector or single value. If string contains ";" assumes a list; if string contains "," assumes a vector; individual strings are interprated as "NULL" -> NULL; if "TRUE" or "FALSE" --> TRUE/FALSE; if single number --> as.numeric; if single non-number --> as.character;
#'
# parse_option <- function(option_str) {
#   if( is.null(option_str) ){
#     NULL
#   }else if(grepl(';', option_str)){
#     option_list <- strsplit(option_str,';')[[1]]
#     option_list_names <- lapply(option_list, function(op) if(grepl(':', op)){sub(':.*', '', op)}else{NULL})
#     option_list <- lapply(option_list, function(op) parse_option(sub('.*:', '', op)))
#     names(option_list) <- option_list_names
#     option_list
#   }else if(grepl(',', option_str)){
#     sapply(strsplit(option_str,',')[[1]], parse_option, USE.NAMES = FALSE)
#   }else if( is.na(option_str) | option_str == '' ){  #same as empty cell in excel sheet
#     NULL
#   }else if(option_str == 'TRUE' | option_str == 'T'){
#     TRUE
#   }else if(option_str == 'FALSE' | option_str == 'F'){
#     FALSE
#   }else if(option_str == 'NULL'){
#     NULL
#   }else if( !is.na(suppressWarnings(as.numeric(option_str))) ){
#     as.numeric(option_str)
#   }else{
#     option_str
#   }
# }

#' Force Scale List
#'
#' @details
#' Internal function: Checks/constructs a force_scale_list
#' handle force_scale which is part of parameters but needs to passed differently to plot function
HandleForcedScaleFromParameters = function(pars){
  force_scales <- lapply(pars, function(para) {
    fc = para$force_scale
    if (is.null(fc)) {
      suppressWarnings(as.numeric(c(NA,NA)))
    }else{
      fc = strsplit(fc, split=',', fixed=TRUE)[[1]]
      suppressWarnings(as.numeric(fc))
    }
  })
  names(force_scales) <- names(pars)

  force_scale_list <- list(
    '+' = unlist(lapply(force_scales, function(x) x[1])),
    '-' = unlist(lapply(force_scales, function(x) if (length(x)==2){x[2]}else{NULL}))
  )
  return(force_scale_list)
}



#' Force Scale List
#'
#' @details
#' Internal function: Checks/constructs a force_scale_list
#'
ForceScaleList = function(samples, force_scale, strands, verbosity, interface){
  .messages = list('output'=list(), 'errors'=list(), 'warnings'=list())
  .force.scale.list = NULL
  if (!is.null(force_scale)){
    if (is.numeric(force_scale)){
      if (all(force_scale>0, na.rm=TRUE) & (length(force_scale)==1 | length(force_scale)==length(samples))){
        .force.scale.list = list()
        .force.scale = ScrutinizeExpandAndNameParameter(force_scale, samples, use_names=TRUE, default_value=NULL, expect_standard='numeric', expect=NULL, revert_to_default=FALSE, alt_par_name=ifelse(interface=='R', 'force_scale', 'Manual Scaling Max Value(s)'), verbosity=verbosity)
        if (strands == '+-'){
          .force.scale.list[['+']] = .force.scale
          .force.scale.list[['-']] = .force.scale
        }else{
          .force.scale.list[[strands]] = .force.scale
        }
      }else{
        .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0('length or sign of ', ifelse(interface=='R', '"force_scale"', '"Manual Scaling Max Value(s)"'), ' numeric vector does not fit the number of samples - the scale will not be forced')
      }
    }else if (is.list(force_scale)){
      if (strands == '+-'){ .strands = c('+', '-') }else{ .strands = strands }
      .no.fit = FALSE
      .no.numeric = FALSE
      .force.scale.list = list()
      for (.strand in .strands){
        if (length(force_scale[[.strand]])==1 | length(force_scale[[.strand]])==length(samples[[.strand]]) ){
          #@cat(paste0(.strand, .force.scale.list[[.strand]]), '\n') #@cat
          if (all(is.na(force_scale[[.strand]]))){
            .force.scale.list[[.strand]] = force_scale[[.strand]]
          }else if (is.numeric(force_scale[[.strand]]) & all(force_scale[[.strand]]>0, na.rm=TRUE)){
            .force.scale = force_scale[[.strand]]
            .nas = is.na(.force.scale)
            if (any(.nas)){
              .force.scale[.nas] = -1
            }
            .force.scale = ScrutinizeExpandAndNameParameter(.force.scale, samples[[.strand]], use_names=FALSE, default_value=NULL, expect_standard='numeric', expect=NULL, revert_to_default=FALSE, alt_par_name=ifelse(interface=='R', 'force_scale', 'Manual Scaling Max Value(s)'), verbosity=verbosity)
            if (any(.nas)){
              .force.scale[names(.nas)[.nas]] = NA
            }
            .force.scale.list[[.strand]] = .force.scale
          }else{
            .no.numeric = TRUE
          }
        }else{
          .no.fit = TRUE
        }
      }
      if (.no.numeric){
        .force.scale.list = NULL
        .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(ifelse(interface=='R', '"force_scale"', '"Manual Scaling Max Value(s)"'), ' list contains non-numeric values - the scale will not be forced')
      }
      if (.no.fit){
        .force.scale.list = NULL
        .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(ifelse(interface=='R', '"force_scale"', '"Manual Scaling Max Value(s)"'), ' list length of numeric vectors do not fit the number of samples - the scale will not be forced')
      }
    }else{
      .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(ifelse(interface=='R', '"force_scale"', '"Manual Scaling Max Value(s)"'), ' is not a list or a numeric value/vector - the scale will not be forced')
    }
  }
  PrintOutput(.messages, verbosity)
  return(.force.scale.list)
}


#' Sort Unlisted Sample Names
#'
#' @details
#' Internal function: Sort Unlisted Sample Names
#'
SortUnlistedSampleNames = function(unlisted_which_sample, unlisted_sample_names, incl_rep=F){
  .subsamples = strsplit(unlisted_which_sample, split='.', fixed=T)[[1]]
  if (incl_rep){
    .sample.rep.split = strsplit(.subsamples[length(.subsamples)], split='.rep', fixed=T)[[1]]
    .subsamples[length(.subsamples)] = .sample.rep.split[1]
    .n.rep = .sample.rep.split[2]
  }
  unlisted.sample.name = unlisted_sample_names
  for (i in 1:length(.subsamples)){
    unlisted.sample.name = grep(.subsamples[i], unlisted.sample.name, fixed=TRUE, value=T)
  }
  #@ ->
  if (length(unlisted.sample.name)>1){
    .sample.name.matrix = do.call('rbind', lapply(unlisted.sample.name, function(x) strsplit(x, split='.', fixed=T)[[1]]))
    unlisted.sample.name.index = 1:nrow(.sample.name.matrix)
    for (j in 1:ncol(.sample.name.matrix)){
      for (i in 1:length(.subsamples)){
        if (any(.sample.name.matrix[,j]==.subsamples[i])){
          unlisted.sample.name.index = intersect(unlisted.sample.name.index, which(.sample.name.matrix[,j]==.subsamples[i]))
        }
      }
    }
    unlisted.sample.name = unlisted.sample.name[unlisted.sample.name.index]
  }
  #@ <-
  if (incl_rep){
    unlisted.sample.name = paste0(unlisted.sample.name, '.rep', .n.rep)
  }
  return(unlisted.sample.name)
}


if (FALSE){
#@ ->
.tracks.listed.converted = list()
.sample.name.matrix = do.call('rbind', lapply(.tracks.listed[['+']][["3'end-seq"]], function(x) strsplit(x, split='.', fixed=T)[[1]]))
rles = lapply(apply(.sample.name.matrix, 2, Rle), function(x) structure(runLength(x), names=runValue(x)))
.tracks.listed.converted[['+']][["3'end-seq"]] = apply(.sample.name.matrix[,order(lengths(rles), decreasing=FALSE)], 1, paste, collapse='.')
#@ <-
}

#' Unpack Samples
#'
#' @details
#' Internal function:
#'
UnpackSamples = function(seqtype, samples, which_samples, which_reps, bigwig_list, bigwig_dirs, verbosity){
  .messages = list('output'=list(), 'errors'=list(), 'warnings'=list())
  .all.sample.names = samples[[seqtype]]
  .n.levels = ListDepth(.all.sample.names)
  if (.n.levels > 0){
    .unlisted.sample.names = rlist::list.flatten(.all.sample.names, use.names = TRUE, classes = "ANY")
    .unlisted.sample.names = paste(rep(names(.unlisted.sample.names), lengths(.unlisted.sample.names)), as.character(unlist(.unlisted.sample.names)), sep='.')
  }else{
    .unlisted.sample.names = .all.sample.names
  }
  .incl.rep = FALSE
  if (!is.null(which_samples)){
    .n.levels.which.samples = ListDepth(which_samples)
    if (.n.levels.which.samples > 0){
      .unlisted.which.samples = rlist::list.flatten(which_samples, use.names = TRUE, classes = "ANY")
      .unlisted.which.samples = paste(rep(names(.unlisted.which.samples), lengths(.unlisted.which.samples)), as.character(unlist(.unlisted.which.samples)), sep='.')
    }else{
      if (!is.numeric(which_samples)){
        .unlisted.which.samples = which_samples
      }else{
        .unlisted.which.samples = .unlisted.sample.names[which_samples]
      }
    }
    if (any(grepl('.rep\\d$', .unlisted.which.samples))){
      if (all(grepl('.rep\\d$', .unlisted.which.samples))){
        .unlisted.sample.names = sapply(.unlisted.which.samples, SortUnlistedSampleNames, .unlisted.sample.names, incl_rep=T)
        .incl.rep=T
        which_reps = NULL
      }else{
        .messages[['errors']][[length(.messages[['errors']])+1]] = paste0('some, but not all sample names are specified by replicate number (i.e. ends with ".rep" followed by integer)',
                                                                          '\n', paste(paste('\t', '.)', .unlisted.which.samples), collapse='\n'))
      }
    }else{
      .unlisted.sample.names = sapply(.unlisted.which.samples, SortUnlistedSampleNames, .unlisted.sample.names)
    }
  }
  if (length(.messages[['errors']])==0){
    .unlisted.which.reps = NULL
    if (!is.null(which_reps)){
      .n.levels.which.reps = ListDepth(which_reps)
      if (.n.levels.which.reps > 0){
        .unlisted.which.reps = rlist::list.flatten(which_reps, use.names = TRUE, classes = "ANY")
        .names.unlisted.which.reps = sapply(names(.unlisted.which.reps), SortUnlistedSampleNames, as.character(.unlisted.sample.names), incl_rep=F)
        names(.unlisted.which.reps) = as.character(.names.unlisted.which.reps)
      }else{
        if (is.null(names(which_reps))){
          .unlisted.which.reps = lapply(as.character(.unlisted.sample.names), function(x) which_reps);  names(.unlisted.which.reps) = as.character(.unlisted.sample.names)
        }else{
          .unlisted.which.reps = as.list(which_reps)
        }
      }
    }
    bw.files = list()
    for (.sample.name in as.character(.unlisted.sample.names)){
      .subsample.names = strsplit(.sample.name, split='.', fixed=T)[[1]]
      if (.incl.rep){
        .sample.rep.split = strsplit(.subsample.names[length(.subsample.names)], split='.rep', fixed=T)[[1]]
        .subsample.names[length(.subsample.names)] = .sample.rep.split[1]
        .n.rep = as.integer(.sample.rep.split[2])
      }
      .reps = bigwig_list[[seqtype]]
      for (.subsample.name in .subsample.names){
        .reps = .reps[[.subsample.name]]
      }
      if (!is.null(.unlisted.which.reps)){
        .reps = .reps[.unlisted.which.reps[[.sample.name]]]
      }
      if (length(.reps) > 0){
        if (.incl.rep){
          if (.n.rep <= length(.reps)){
            .reps = .reps[.n.rep]
          }else{
            .messages[['errors']][[length(.messages[['errors']])+1]] = paste0('the supplied replicate number does not exist',
                                                                              '\n', '\t', '.) ', .sample.name,
                                                                              '\n', paste(paste('\t', '\t', .reps), collapse='\n'))
          }
        }
        if (!is.null(which_reps)){
          .reps = .reps[which_reps[[.sample.name]]]
        }
        .n.reps = length(.reps)
        for (.n.rep in 1:.n.reps){
          .sample.rep.name = paste0(.sample.name, '.rep', .n.rep)
          .bw.file = paste0(bigwig_dirs[[seqtype]], .reps[.n.rep])
          bw.files[[.sample.rep.name]] = .bw.file
        }
      }else{
        .messages[['errors']][[length(.messages[['errors']])+1]] = paste0('the supplied sample name does not exist',
                                                                          '\n', '\t', '.) ', .sample.name,
                                                                          '\n', paste(paste('\t', '\t', as.character(.unlisted.sample.names)), collapse='\n'))
      }
    }
  }
  PrintOutput(.messages, verbosity)
  if (length(.messages[['errors']])==0){
    return(bw.files)
  }else{
    return(NULL)
  }
}


#' Load And Transform Data For Track
#'
#' @details
#' Internal function:
#'
LoadAndTransformDataForTrack = function(seqtype, plotted_region, samples, bigwigs, bigwig_dirs, parameters=NULL, get_subsamples=FALSE, print_order=FALSE, verbosity){
  .chrom = as.character(GenomeInfoDb::seqnames(plotted_region))
  .strand = as.character(BiocGenerics::strand(plotted_region))
  .chrom.start = IRanges::start(plotted_region)
  .chrom.end = IRanges::end(plotted_region)
  .dataset.present = TRUE
  if (seqtype %in% names(bigwigs[[.strand]])){
    .bw.list = list()
    if (is.null(parameters)){ # defaults
      .which.samples = NULL
      .which.reps = NULL
      .log2.transform = FALSE
      .pseudo.count = 1
      .batch.correct = FALSE
      .batch = NULL
      .neg.valued.bw = FALSE
      .calc.mean = FALSE
      .neg.vals.set.0 = FALSE
    }else{
      .param = parameters[[seqtype]]
      .which.samples = .param[['whichSamples']]
      .which.reps = .param[['whichReps']]
      .log2.transform = .param[['log2transform']]
      .pseudo.count = .param[['pseudoCount']]
      .batch.correct = .param[['batchCorrect']]
      .batch = .param[['batch']]
      .neg.valued.bw = .param[['negative_valued_bw']]
      .calc.mean = .param[['calcMean']] # should the mean of the replicated samples be calculated (T/F)
      .neg.vals.set.0 = .param[['negValsSet0']] # should negative values be set to 0 (T/F)
      if (!is.null(.which.samples)){
        if (is.list(.which.samples)){
          .unlisted.which.samples = rlist::list.flatten(.which.samples, use.names = TRUE, classes = "ANY")
          .unlisted.which.samples = paste(rep(names(.unlisted.which.samples), lengths(.unlisted.which.samples)), as.character(unlist(.unlisted.which.samples)), sep='.')
        }else if (any(is.na(.which.samples))){
          .unlisted.which.samples = .which.samples
          .dataset.present = FALSE
        }else{
          .unlisted.which.samples = .which.samples
        }
        if (any(grepl('.rep\\d$', .unlisted.which.samples))){
          .calc.mean = FALSE
        }
      }
    }
    if (.dataset.present){
      .bw.files = UnpackSamples(seqtype, samples, .which.samples, .which.reps, bigwigs[[.strand]], bigwig_dirs, verbosity)
      .sample.rep.names = names(.bw.files)
      if (any(is.na(.sample.rep.names))){
        if (verbosity > 0){
          cat('ERROR(s):', '\n')
          cat(' - there is a discrepancy between the requested and stored sample names (or numbers)', '\n')
          cat(paste('\t', '.)', 'stored sample names:', paste(.sample.rep.names, collapse=', '), length(.sample.rep.names), sep='\t'), '\n')
          cat(paste('\t', '\t', seqtype), '\n')
          if (is.numeric(.which.samples)){
            cat(paste('\t', '\t', '\t', 'requested sample numbers:', paste(.which.samples, collapse=', '), paste0('(', paste(.sample.rep.names, collapse=', '), ')') , sep='\t'), '\n')
          }else{
            cat(paste('\t', '\t', '\t', 'requested sample names:', paste(.unlisted.which.samples, collapse=', '), paste0('(', paste(.sample.rep.names, collapse=', '), ')') , sep='\t'), '\n')
          }
        }
        return(NULL)
      }
      if (get_subsamples | print_order){
        if (.calc.mean){
          sample.names = unique(sapply(.sample.rep.names, function(.sample.rep.name) strsplit(.sample.rep.name, split='.rep', fixed=TRUE)[[1]][1] ))
        }else{
          sample.names = .sample.rep.names
        }
        if (print_order){
          cat(paste('printing order of samples for', seqtype), '\n')
          cat(paste(sample.names, collapse='\n'), '\n')
        }
        if (get_subsamples){
          return(sample.names)
        }else{
          cat('and exiting', '\n')
          return()
        }
      }else{
        for (.sample.rep.name in .sample.rep.names){
          .bw.file = .bw.files[[.sample.rep.name]]
          if (file.exists(.bw.file) | url.exists(.bw.file)){
            .data.from.bw = tryCatch( expr = { rtracklayer::import(.bw.file, which=GenomicRanges::GRanges(seqnames=.chrom, ranges=IRanges::IRanges(start=.chrom.start, end=.chrom.end)), as='NumericList')[[1]] },
                                       error = function(e){ FALSE }, #@ cat(paste('error1', .sample.rep.name), '\n');
                                       warning = function(w){ FALSE }) #@ cat(paste('warning1', .sample.rep.name), '\n');
            if (sum(.data.from.bw)==0){
              if (is.logical(.data.from.bw)){
                .alt.chrom = strsplit(.chrom, split='chr')[[1]][2]
                .alt.data.from.bw = tryCatch(  expr = { rtracklayer::import(.bw.file, which=GenomicRanges::GRanges(seqnames=.alt.chrom, ranges=IRanges::IRanges(start=.chrom.start, end=.chrom.end)), as='NumericList')[[1]] },
                                               error = function(e){ FALSE }, #@ cat(paste('error2', .sample.rep.name), '\n');
                                               warning = function(w){ FALSE }) #@ cat(paste('warning2', .sample.rep.name), '\n');
                if (sum(.alt.data.from.bw)==0){
                  if (is.logical(.alt.data.from.bw)){
                    .bw.list[[.sample.rep.name]] = rep(0, .chrom.end - .chrom.start + 1)
                  }else{
                    .bw.list[[.sample.rep.name]] = .alt.data.from.bw
                  }
                }else{
                  .bw.list[[.sample.rep.name]] = .alt.data.from.bw
                }
              }else{
                .bw.list[[.sample.rep.name]] = .data.from.bw
              }
            }else{
              .bw.list[[.sample.rep.name]] = .data.from.bw
            }
          }else{
            if (verbosity > 1){
              cat('WARNING(s):', '\n')
              cat(' - non-existing file', '\n')
              cat(paste('\t', '.)', .bw.file), '\n')
            }
            .bw.list[[.sample.rep.name]] = rep(0, .chrom.end - .chrom.start + 1)
          }
        }
        .bw.matrix = do.call(cbind, .bw.list)
        rownames(.bw.matrix) = .chrom.start:.chrom.end
        if (FALSE){ #@ -> #@ auto-detection of major sign in bw files
          cumPos = sum(.bw.matrix[which(.bw.matrix > 0)])
          cumNeg = sum(.bw.matrix[which(.bw.matrix < 0)])
          if (verbosity > 1){
            if (abs(cumNeg) > cumPos){
              cat('WARNING(s):', '\n')
              cat(paste0(' - the bigwig files related to ', seqtype, ' ', .strand, '-strand contain ', ifelse(cumPos==0, 'only', 'mainly'), ' negative values'), '\n')
              cat(paste('\t', '.)', .bw.file), '\n')
            }
          }
        } #@ <-
        if (.neg.valued.bw & .strand=='-'){
          .bw.matrix = -1*.bw.matrix
        }
        if (.log2.transform){
          .signs.bw.matrix = sign(.bw.matrix)
          if (any(.signs.bw.matrix <= 0) | .pseudo.count < 1){ #@ any(.signs.bw.matrix <= 0) <- any(.signs.bw.matrix < 0); added "| .pseudo.count < 1"
            .abs.bw.matrix = abs(.bw.matrix) + .pseudo.count
            if (any(.abs.bw.matrix < 1)){
              .adj.pseudo.count = 1 - min(abs(.bw.matrix))
              if (verbosity > 1){
                cat(paste0('WARNING(s):'), '\n')
                cat(paste0('\t', '.) ', 'automatically adjusting pseudocount from ', .pseudo.count, ' to ', .adj.pseudo.count, ' for ', seqtype, ' to allow log2-transformation, because the data contain values equal to or less than zero'), '\n')
              }
              .pseudo.count = .adj.pseudo.count
            }
            rm(.abs.bw.matrix)
          }
          .bw.matrix = .signs.bw.matrix * log2(abs(.bw.matrix) + .pseudo.count)
        }
        rm('.bw.list')
        if (.batch.correct){
          if (is.null(.batch)){ # if no batch number is provided for each sample - the replicate number will be assumed as equivalent to batch number
            .batch = as.integer(sapply(colnames(.bw.matrix), function(rep) rev(strsplit(rep, split='.rep', fixed=TRUE)[[1]])[1] ))
          }else if (is.list(.batch)){
            .batch = rlist::list.flatten(.batch)
            .batch = structure(as.vector(unlist(.batch)), names=paste0(rep(names(.batch), lengths(.batch)), '.rep', as.character(unlist(sapply(lengths(.batch), function(x) 1:x)))))
          }
          if (is.null(names(.batch))){ # add sample+rep names to batch numbers
            names(.batch) = colnames(.bw.matrix)
          }
          .batch = .batch[colnames(.bw.matrix)]  # order batch numbers according to order of .bw.matrix
          .batches = unique(.batch)
          if (length(.batches) > 1){
            if (!.log2.transform){ # if signals are not overall log2transformed, they need to be for the batch correction
              .signs.bw.matrix = sign(.bw.matrix)
              if (any(.signs.bw.matrix <= 0) | .pseudo.count < 1){ #@ any(.signs.bw.matrix <= 0) <- any(.signs.bw.matrix < 0); added " | .pseudo.count < 1"
                .abs.bw.matrix = abs(.bw.matrix) + .pseudo.count
                if (any(.abs.bw.matrix < 1)){
                  .adj.pseudo.count = 1 - min(abs(.bw.matrix))
                  if (verbosity > 1){
                    cat(paste0('WARNING(s):'), '\n')
                    cat(paste0('\t', '.) ', 'automatically adjusting pseudocount from ', .pseudo.count, ' to ', .adj.pseudo.count, ' for ', seqtype, ' to allow batch correction, because the data contain values equal to or less than zero'), '\n')
                  }
                  .pseudo.count = .adj.pseudo.count
                }
                rm(.abs.bw.matrix)
              }
              .bw.matrix = .signs.bw.matrix * log2(abs(.bw.matrix) + .pseudo.count)
            }
            .bw.matrix = limma::removeBatchEffect(.bw.matrix, batch=.batch)
            if (!.log2.transform){ # if signals are not overall log2transformed, they need to be for the batch correction, and then they are re-transformed afterwards
              .bw.matrix = .signs.bw.matrix * (2^abs(.bw.matrix) - .pseudo.count)
            }
          }else{
            if (verbosity > 1){
              cat(paste0('WARNING(s):'), '\n')
              cat(paste0('\t', '.) ', 'batch correction is requested for ', seqtype, ' but no batch information is provided'), '\n')
            }
          }
        }
        track.list = list()
        if (.calc.mean){
          .sample.names = unique(sapply(.sample.rep.names, function(.sample.rep.name) strsplit(.sample.rep.name, split='.rep', fixed=T)[[1]][1] ))
          for (.sample.name in .sample.names){
            .all.replicate.names = sapply(colnames(.bw.matrix), function(x) strsplit(x, split='.rep', fixed=TRUE)[[1]][1])
            .replicate.names = names(.all.replicate.names[which(.all.replicate.names==.sample.name)])
            #@grep(paste0('^', .sample.name, '\\.rep'), colnames(.bw.matrix), fixed=FALSE, value=T)
            .mean.track = rowMeans(.bw.matrix[, .replicate.names, drop=FALSE])
            if (.neg.vals.set.0){
              .mean.track[which(.mean.track<0)] = 0
            }
            track.list[[.sample.name]] = .mean.track
          }
        }else{
          if (.neg.vals.set.0){
            .bw.matrix[which(.bw.matrix<0)] = 0
          }
          for (.sample.rep.name in colnames(.bw.matrix)){
            track.list[[.sample.rep.name]] = .bw.matrix[, .sample.rep.name]
          }
        }
        return(track.list)
      }
    }else{
      return()
    }
  }else{
    return()
  }
}


#' Delete NULLs
#'
#' @details
#' Internal function: delele null/empty entries in a list
#'
DeleteNULLs  <-  function(x_list){
  x_list[unlist(lapply(x_list, length) != 0)]
}


#' Load Tracks
#'
#' @details
#' Internal function:
#'
LoadTracks = function(plotted_region, samples, bigwigs, bigwig_dirs, parameters, verbosity){
  .tracks = list()
  for (.seqtype in names(samples)){
    #cat(paste('loading', .seqtype, 'data'), '\n')
    .tracks[[.seqtype]] = LoadAndTransformDataForTrack(.seqtype, plotted_region, samples, bigwigs, bigwig_dirs, parameters, get_subsamples=FALSE, print_order=FALSE, verbosity)
  }
  return(.tracks)
}


#' Organized Panels List
#'
#' @details
#' Internal function: Organize the panels with track-names to be plotted on the left side of the tracks
#'
OrganizedPanelsList = function(tracks_listed){
  panels.list = list()
  for (.seqtype in names(tracks_listed)){
    panels.list[[.seqtype]] = list()
    .subsamples = tracks_listed[[.seqtype]]
    .subsample.matrix = do.call('rbind', sapply(.subsamples, function(.sep) strsplit(.sep, split='.', fixed=T)))
    if (ncol(.subsample.matrix) > 1){ #@ ->
      .rles = lapply(apply(.subsample.matrix, 2, Rle), function(x) structure(runLength(x), names=runValue(x)))
      .subsample.matrix = .subsample.matrix[,order(lengths(.rles))]
    } #@ <-
    .n.levels = ncol(.subsample.matrix)
    for (.n.level in 1:.n.levels){
      .nextlayer.rle = S4Vectors::Rle(sapply(1:nrow(.subsample.matrix), function(r) paste(.subsample.matrix[r,1:.n.level], collapse=';')))
      .nextlayer.runs = S4Vectors::runLength(.nextlayer.rle)
      .nextlayer.names = as.character(sapply(S4Vectors::runValue(.nextlayer.rle), function(s) unlist(strsplit(s, split=";", fixed=TRUE))[.n.level]))
      panels.list[[.seqtype]][[.n.level]] = structure(.nextlayer.runs, names=.nextlayer.names)
    }
  }
  return(panels.list)
}


#' Plotting Segment Order
#'
#' @details
#' Internal function:
#'
PlottingSegmentOrder = function(plotting_segment_order, sample_names, header, include_genomic_scale, genomic_scale_on_top, incl_annot, horizontal_spacers){
  if (is.null(plotting_segment_order)){
    if (horizontal_spacers){
      .samples.names = rep('line-spacer', 2*length(sample_names)-1)
      .samples.names[seq(1, by=2, along.with=sample_names)] = sample_names
    }else{
      .samples.names = sample_names
    }
    if (genomic_scale_on_top){
      .plotting.segment.order = c('header', 'scale', .samples.names, 'empty-spacer', 'annotations')[c(!is.null(header), include_genomic_scale, rep(T, length(.samples.names)), ifelse(incl_annot, T, F), ifelse(incl_annot, T, F))]
    }else{
      .plotting.segment.order = c('header', .samples.names, 'scale', 'empty-spacer', 'annotations')[c(!is.null(header), rep(T, length(.samples.names)), include_genomic_scale, ifelse(incl_annot, T, F), ifelse(incl_annot, T, F))]
    }
  }else{
    .plotting.segment.order = plotting_segment_order
  }
  return(.plotting.segment.order)
}


#' Finalize Plotting Segment Order
#'
#' @details
#' Internal function:
#'
#'
FinalizePlottingSegmentOrder = function(plotting_segment_order, tracks_listed, both_strands, include_genomic_scale, genomic_scale_on_top, any_stranded_beds, any_unstranded_beds, strands_intermingled, verbosity, interface){
  if (both_strands){
    .unstranded.seqtypes = setdiff(names(tracks_listed[['+']]), names(tracks_listed[['-']]))
    if (is.list(plotting_segment_order)){
      .plotting.segment.order.plus = plotting_segment_order[['+']]
      .plotting.segment.order.minus = plotting_segment_order[['-']]
      if (any(.plotting.segment.order.minus %in% c('unstranded-beds', .unstranded.seqtypes))){
        if (verbosity > 1){
          if (any(.plotting.segment.order.minus %in% c('unstranded-beds')) & any(.plotting.segment.order.minus %in% c(.unstranded.seqtypes))){
            cat('WARNINGs:', '\n')
          }else{
            cat('WARNING:', '\n')
          }
          if (any(.plotting.segment.order.minus %in% c('unstranded-beds'))){
            cat(paste0(' - there are "unstranded" annotations in ', ifelse(interface=='R', '"plotting_segment_order[[\'-\']]"', '"Reverse Strand Plotting Segment Order"'), ' - these will be ignored'), '\n')
            cat(paste('\t', '.) "unstranded" annotations should be placed in', ifelse(interface=='R', '"plotting_segment_order[[\'+\']]"', '"(Forward) Plotting Segment Order"')), '\n')
          }
          if (any(.plotting.segment.order.minus %in% c(.unstranded.seqtypes))){
            cat(paste0(' - there are "unstranded" sequencing data tracks(s) in ', ifelse(interface=='R', '"plotting_segment_order[[\'-\']]"', '"Reverse Strand Plotting Segment Order"'), ' - these will be ignored'), '\n')
            cat(paste('\t', '.) "unstranded" sequencing data tracks(s) should be placed in', ifelse(interface=='R', '"plotting_segment_order[[\'+\']]"', '"(Forward) Plotting Segment Order"')), '\n')
            cat(paste('\t', '\t', .unstranded.seqtypes, '\n'))
          }
        }
        .plotting.segment.order.minus = .plotting.segment.order.minus[!.plotting.segment.order.minus %in% c('unstranded-beds', .unstranded.seqtypes)]
      }
    }else{
      .plotting.segment.order.plus = plotting_segment_order
      .plotting.segment.order.minus = .plotting.segment.order.plus[!.plotting.segment.order.plus %in% c('header', 'scale', 'unstranded-beds', .unstranded.seqtypes)]
    }
    if (include_genomic_scale & !genomic_scale_on_top){
      .plotting.segment.order.plus = .plotting.segment.order.plus[.plotting.segment.order.plus != 'scale']
      .plotting.segment.order.minus = c(.plotting.segment.order.minus, 'scale')
    }
    if (any_unstranded_beds & !("unstranded-beds" %in% .plotting.segment.order.plus)){ #@ added & !("unstranded-beds" %in% .plotting.segment.order.plus)
      .annot.index = which(.plotting.segment.order.plus == "annotations")
      if (!strands_intermingled){
        if (any_stranded_beds){
          .plotting.segment.order.plus = c(.plotting.segment.order.plus, "thickline-spacer", "unstranded-beds")
        }else{
          .plotting.segment.order.plus[.annot.index] = "unstranded-beds"
        }
      }else{
        if (any_stranded_beds){
          .plotting.segment.order.plus = c(plotting_segment_order[1:(.annot.index-1)], "unstranded-beds", "empty-spacer", plotting_segment_order[.annot.index:length(plotting_segment_order)])
        }else{
          .plotting.segment.order.plus[.annot.index] = "unstranded-beds"
        }
      }
    }
    if (!is.list(plotting_segment_order)){
      if ('annotations' %in% .plotting.segment.order.minus){
        if (any_stranded_beds){
          .plotting.segment.order.minus = c('thickline-spacer', 'annotations', 'empty-spacer', .plotting.segment.order.minus[!.plotting.segment.order.minus %in% c('annotations', 'empty-spacer')])
        }else{
          .plotting.segment.order.minus = c('empty-spacer', .plotting.segment.order.minus[!.plotting.segment.order.minus %in% c('annotations', 'empty-spacer')])
        }
      }else{
        .plotting.segment.order.minus = c('thickline-spacer', .plotting.segment.order.minus)
      }
    }
    .plotting.segment.order = list('+'=.plotting.segment.order.plus, '-'=.plotting.segment.order.minus)
  }else{
    if ("annotations" %in% plotting_segment_order){
      .annot.index = which(plotting_segment_order == "annotations")
      if (any_unstranded_beds & !("unstranded-beds" %in% plotting_segment_order)){ #@ added & !("unstranded-beds" %in% plotting_segment_order)
        if (any_stranded_beds){
          .plotting.segment.order = c(plotting_segment_order[1:(.annot.index-1)], "unstranded-beds", "empty-spacer", plotting_segment_order[.annot.index:length(plotting_segment_order)])
        }else{
          .plotting.segment.order[.annot.index] = "unstranded-beds"
        }
      }else{
        .plotting.segment.order = plotting_segment_order
      }
    }
    .plotting.segment.order = structure(list(.plotting.segment.order), names=names(tracks_listed))
  }
  for (.strand in names(.plotting.segment.order)){
    if (.strand == '+'){
      .spacers = grep('-spacer', .plotting.segment.order[[.strand]], fixed=TRUE)
      if (length(.spacers) > 0){
        if (.spacers[1]==1){
          #@.plotting.segment.order[[.strand]] = .plotting.segment.order[[.strand]][-1]
          .plotting.segment.order[[.strand]] = .plotting.segment.order[[.strand]][setdiff(1:length(.plotting.segment.order[[.strand]]), .spacers)[1]:length(.plotting.segment.order[[.strand]])]
        }
      }
    }else if (.strand == '-'){
      .spacers = grep('-spacer', .plotting.segment.order[[.strand]], fixed=TRUE)
      if (length(.spacers) > 0){
        if (rev(.spacers)[1]==length(.plotting.segment.order[[.strand]])){
          #@.plotting.segment.order[[.strand]] = .plotting.segment.order[[.strand]][-length(.plotting.segment.order[[.strand]])]
          .plotting.segment.order[[.strand]] = .plotting.segment.order[[.strand]][1:rev(setdiff(1:length(.plotting.segment.order[[.strand]]), .spacers))[1]]
        }
      }
    }
  }
  return(.plotting.segment.order)
}

FinalizePlottingSegmentOrder_obs = function(plotting_segment_order, tracks_listed, both_strands, include_genomic_scale, genomic_scale_on_top, any_stranded_beds, any_unstranded_beds, strands_intermingled){
  if (!is.list(plotting_segment_order)){
    if (both_strands){
      .unstranded.seqtypes = setdiff(names(tracks_listed[['+']]), names(tracks_listed[['-']]))
      .plotting.segment.order.plus = plotting_segment_order
      .plotting.segment.order.minus = .plotting.segment.order.plus[!.plotting.segment.order.plus %in% c('header', 'scale', 'unstranded-beds', .unstranded.seqtypes)]
      if (include_genomic_scale & !genomic_scale_on_top){
        .plotting.segment.order.plus = .plotting.segment.order.plus[.plotting.segment.order.plus != 'scale']
        .plotting.segment.order.minus = c(.plotting.segment.order.minus, 'scale')
      }
      if (any_unstranded_beds & !("unstranded-beds" %in% .plotting.segment.order.plus)){ #@ added & !("unstranded-beds" %in% .plotting.segment.order.plus)
        .annot.index = which(.plotting.segment.order.plus == "annotations")
        if (!strands_intermingled){
          if (any_stranded_beds){
            .plotting.segment.order.plus = c(.plotting.segment.order.plus, "thickline-spacer", "unstranded-beds")
          }else{
            .plotting.segment.order.plus[.annot.index] = "unstranded-beds"
          }
        }else{
          if (any_stranded_beds){
            .plotting.segment.order.plus = c(plotting_segment_order[1:(.annot.index-1)], "unstranded-beds", "empty-spacer", plotting_segment_order[.annot.index:length(plotting_segment_order)])
          }else{
            .plotting.segment.order.plus[.annot.index] = "unstranded-beds"
          }
        }
      }
      if ('annotations' %in% .plotting.segment.order.minus){
        if (any_stranded_beds){
          .plotting.segment.order.minus = c('thickline-spacer', 'annotations', 'empty-spacer', .plotting.segment.order.minus[!.plotting.segment.order.minus %in% c('annotations', 'empty-spacer')])
        }else{
          .plotting.segment.order.minus = c('empty-spacer', .plotting.segment.order.minus[!.plotting.segment.order.minus %in% c('annotations', 'empty-spacer')])
        }
      }else{
        .plotting.segment.order.minus = c('thickline-spacer', .plotting.segment.order.minus)
      }
      .plotting.segment.order = list('+'=.plotting.segment.order.plus, '-'=.plotting.segment.order.minus)
    }else{
      if ("annotations" %in% plotting_segment_order){
        .annot.index = which(plotting_segment_order == "annotations")
        if (any_unstranded_beds & !("unstranded-beds" %in% plotting_segment_order)){ #@ added & !("unstranded-beds" %in% plotting_segment_order)
          if (any_stranded_beds){
            .plotting.segment.order = c(plotting_segment_order[1:(.annot.index-1)], "unstranded-beds", "empty-spacer", plotting_segment_order[.annot.index:length(plotting_segment_order)])
          }else{
            .plotting.segment.order[.annot.index] = "unstranded-beds"
          }
        }else{
          .plotting.segment.order = plotting_segment_order
        }
      }
      .plotting.segment.order = structure(list(.plotting.segment.order), names=names(tracks_listed))
    }
  }else{
    .plotting.segment.order = plotting_segment_order
  }
  for (.strand in names(.plotting.segment.order)){
    if (.strand == '+'){
      .spacers = grep('-spacer', .plotting.segment.order[[.strand]], fixed=TRUE)
      if (length(.spacers) > 0){
        if (.spacers[1]==1){
          #@.plotting.segment.order[[.strand]] = .plotting.segment.order[[.strand]][-1]
          .plotting.segment.order[[.strand]] = .plotting.segment.order[[.strand]][setdiff(1:length(.plotting.segment.order[[.strand]]), .spacers)[1]:length(.plotting.segment.order[[.strand]])]
        }
      }
    }else if (.strand == '-'){
      .spacers = grep('-spacer', .plotting.segment.order[[.strand]], fixed=TRUE)
      if (length(.spacers) > 0){
        if (rev(.spacers)[1]==length(.plotting.segment.order[[.strand]])){
          #@.plotting.segment.order[[.strand]] = .plotting.segment.order[[.strand]][-length(.plotting.segment.order[[.strand]])]
          .plotting.segment.order[[.strand]] = .plotting.segment.order[[.strand]][1:rev(setdiff(1:length(.plotting.segment.order[[.strand]]), .spacers))[1]]
        }
      }
    }
  }
  return(.plotting.segment.order)
}


#' BuildScrutinizePlotSegmentOrder
#'
#' @details
#' Internal function:
#'
BuildScrutinizePlotSegmentOrder = function(plotting_segment_order, plotted_region, datasets, plotted_samples, header, include_genomic_scale, genomic_scale_on_top, incl_annot, horizontal_spacers, tracks_listed, both_strands, any_stranded_beds, any_unstranded_beds, strands_intermingled, verbosity, interface){
  .plotting.segment.order = NULL
  if (!is.null(plotting_segment_order)){
    if (is.list(plotting_segment_order) & both_strands & !strands_intermingled){ #@ added & both_strands & strands_intermingled
      if (identical(names(plotting_segment_order), names(plotted_region))){
        plotting_segment_order_temp = list()
        for (.plot.strand in names(plotting_segment_order)){
          if (.plot.strand == '+'){
            .plotting.segment.order.plus = plotting_segment_order[['+']]
            plotting_segment_order_temp_vector = ScrutinizeExpandAndNameParameter(.plotting.segment.order.plus, .plotting.segment.order.plus, use_names=FALSE, default_value=NULL, expect_standard=NULL, expect=c("header", "scale", "empty-spacer", "thickline-spacer", "line-spacer", "annotations", "unstranded-beds", names(datasets)), revert_to_default=FALSE, alt_par_name=ifelse(interface=='R', "plotting_segment_order[['+']]", 'Plotting Segment Order'), verbosity=verbosity)
          }else if (.plot.strand == '-'){
            .plotting.segment.order.minus = plotting_segment_order[['-']]
            plotting_segment_order_temp_vector = ScrutinizeExpandAndNameParameter(.plotting.segment.order.minus, .plotting.segment.order.minus, use_names=FALSE, default_value=NULL, expect_standard=NULL, expect=c("header", "scale", "empty-spacer", "thickline-spacer", "line-spacer", "annotations", "unstranded-beds", names(datasets)), revert_to_default=FALSE, alt_par_name=ifelse(interface=='R', "plotting_segment_order[['-']]", '(Reverse Strand) Plotting Segment Order'), verbosity=verbosity)
          }
          plotting_segment_order_temp[[.plot.strand]] = as.character(plotting_segment_order_temp_vector)
        }
        if ( any(sapply(plotting_segment_order_temp, function(parameter) length(parameter)==0)) ){ return() }
        .plotting.segment.order = plotting_segment_order_temp
      }else{
        if (verbosity > 0){
          cat('ERRORs:', '\n')
          cat(paste0(' - names of ', ifelse(interface=='R', '"plotting_segment_order"', '"Plotting Segment Order"'), ' list has to match to the plotted strands - aborting'), '\n')
          cat(paste('\t', '.)', 'names:', paste(names(plotted_region), collapse=' ')), '\n')
        }
        return()
      }
    }else{
      if (is.list(plotting_segment_order)){ #@ ->
        plotting_segment_order = plotting_segment_order[['+']]
      } #@ <-
      plotting_segment_order_temp_vector = as.character(ScrutinizeExpandAndNameParameter(plotting_segment_order, plotting_segment_order, use_names=FALSE, default_value=NULL, expect_standard=NULL, expect=c("header", "scale", "empty-spacer", "thickline-spacer", "line-spacer", "annotations", "unstranded-beds", names(datasets)), revert_to_default=FALSE, alt_par_name=ifelse(interface=='R', 'plotting_segment_order', 'Plotting Segment Order'), verbosity=verbosity))
      if (length(plotting_segment_order_temp_vector)==0){ return() }
      .plotting.segment.order = plotting_segment_order_temp_vector
    }
  }
  .plotting.segment.order = PlottingSegmentOrder(.plotting.segment.order, plotted_samples, header, include_genomic_scale, genomic_scale_on_top, incl_annot, horizontal_spacers)
  .plotting.segment.order = FinalizePlottingSegmentOrder(.plotting.segment.order, tracks_listed, both_strands, include_genomic_scale, genomic_scale_on_top, any_stranded_beds, any_unstranded_beds, strands_intermingled, verbosity, interface)
  .segment.summation = list()
  for (.strand in names(.plotting.segment.order)){
    .segment.summation[[.strand]] = sapply(c("header", "scale", "empty-spacer", "thickline-spacer", "line-spacer", "annotations", "unstranded-beds",names(datasets)), function(dataset_name) length((which(.plotting.segment.order[[.strand]]==dataset_name))))
    if (any(.segment.summation[[.strand]][c("header", "scale", "annotations", "unstranded-beds",names(datasets))] > 1)){
      if (verbosity > 0){
        cat('ERRORs:', '\n')
        cat(paste0(' - there can at maximum be one of the values "', paste(c("header", "scale", "annotations", "unstranded-beds",names(datasets)), collapse='", "'), '" in ', ifelse(interface=='R', '"plotting_segment_order"', '"Plotting Segment Order"'), ' list - aborting'), '\n')
        cat(paste('\t', '.)', paste0('"', paste(c("header", "scale", "annotations", "unstranded-beds",names(datasets)), collapse='", "')[which(.segment.summation[[.strand]][c("header", "scale", "annotations", "unstranded-beds",names(datasets))] > 1)], '" represented more than once')), '\n')
      }
      return()
    }
  }
  if (length(.segment.summation) == 2){
    .segment.summation.total = rowSums(as.data.frame(.segment.summation))
    if (.segment.summation.total[['header']] == 1){
      if (.segment.summation[['-']][['header']] == 1){
        if (verbosity > 0){
          cat('ERRORs:', '\n')
          cat(paste0(' - the "header" segment is placed in the minus strand plotting segments, it should be placed in the plus strand plotting segments - aborting'), '\n')
        }
        return()
      }
    }
    if (.segment.summation.total[['annotations']] > 0){
      if (.segment.summation.total[['annotations']] != 2){
        if (verbosity > 0){
          cat('ERRORs:', '\n')
          cat(paste0(' - if annotations are to be displayed the "annotations" segments should be present under plotting segments for both strands - aborting'), '\n')
          cat(paste('\t', '.)', '"annotations" segment only present under' ,ifelse(.segment.summation[['+']][['annotations']]==1, 'plus', 'minus'), 'strand'), '\n')
        }
        return()
      }
    }
  }
  return(.plotting.segment.order)
}


#' Numbering Spacers
#'
#' @details
#' Internal function:
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


#' Estimate Plot Heights
#'
#' @details
#' Internal function:
#'
EstimatePlotHeights = function(annot_info, incl_feature_names, annotation_packing, incl_feature_brackets, plotting_segment_order, tracks_listed, track_height_cm, full_height_cm, stranded_beds, plot_vertical_parameters, verbosity, interface){
  .messages = list('output'=list(), 'errors'=list(), 'warnings'=list())
  .warning.message = NULL
  if (is.null(track_height_cm) & is.null(full_height_cm)){
    .error.message = paste0(' - ', 'both ', ifelse(interface=='R', '"track_height_cm"', '"Tracks Height"'), ' and ', ifelse(interface=='R', '"full_height_cm"', '"Full Plot Height"'), ' are NULL - one of them has to be defined')
  }else if (is.null(full_height_cm)){
    .error.message = paste0(' - ', ifelse(interface=='R', '"track_height_cm"', '"Tracks Height"'), ' = ', track_height_cm, ' - it should be set to a positive numeric value (recommended 0.25-1 cm)')
    .warning.message = paste0(' - ', ifelse(interface=='R', '"track_height_cm"', '"Tracks Height"'), ' = ', track_height_cm, ' cm - the recommended value is 0.25-1 cm')
    if (is.numeric(track_height_cm)){
      if (sign(track_height_cm) > 0){
        .error.message = NULL
        if (track_height_cm >= 0.2 & track_height_cm <= 1){
          .warning.message = NULL
        }
      }
    }
  }else if (is.null(track_height_cm)){
    .error.message = paste0(' - ', ifelse(interface=='R', '"full_height_cm"', '"Full Plot Height"'), ' = ', full_height_cm, ' - it should be set to a positive numeric value')
    if (is.numeric(full_height_cm)){
      if (sign(full_height_cm) > 0){
        .error.message = NULL
      }
    }
  }else{
    .error.message = paste0(' - ', ifelse(interface=='R', '"track_height_cm"', '"Tracks Height"'), ' = ', track_height_cm, ' and ', ifelse(interface=='R', '"full_height_cm"', '"Full Plot Height"'), ' = ', full_height_cm,
                            '\n', '\t', '.)', ' one of the arguments track_height_cm and full_height_cm should be a positive numeric value and the other should be NULL')
  }
  .messages[['errors']][[length(.messages[['errors']])+1]] = .error.message
  .messages[['warnings']][[length(.messages[['warnings']])+1]] = .warning.message
  if (length(.messages[['errors']]) == 0){
    .annot.heights = list()
    .max.annot.lines = list()
    if (!is.null(annot_info)){
      if (any(stranded_beds)){
        .min.annot.heights.incl.text = list()
        .max.annot.heights.incl.text = list()
        for (.annot in names(annot_info)[stranded_beds]){
          if (length(annot_info[[.annot]]) > 0 ){
            if (annotation_packing[.annot]=='expanded' | annotation_packing[.annot]=='squished'){
              .max.annot.lines[[.annot]] = as.integer(max(lengths(annot_info[[.annot]][['packing']])))
              .annot.heights[[.annot]] = as.numeric(ifelse(annotation_packing[.annot]=='expanded', plot_vertical_parameters['annot'], plot_vertical_parameters['annot_squished']) * .max.annot.lines[[.annot]])
            }else if (annotation_packing[.annot]=='collapsed2'){
              .max.annot.lines[[.annot]] = as.integer(max(lengths(annot_info[[.annot]][['packing2']])))
              .annot.heights[[.annot]] = as.numeric(plot_vertical_parameters['annot'] * .max.annot.lines[[.annot]])
            }else{
              .max.annot.lines[[.annot]] = 1
              .annot.heights[[.annot]] = as.numeric(plot_vertical_parameters['annot'] * .max.annot.lines[[.annot]])
            }
          }else{
            .max.annot.lines[[.annot]] = 1
            .annot.heights[[.annot]] = as.numeric(plot_vertical_parameters['annot'] * .max.annot.lines[[.annot]])
          }
          .min.bracket.lines = length(annot_info[[.annot]][['collapsed2']]) - length(annot_info[[.annot]][['collapsed']]) + 1
          .min.bracket.heights = as.numeric(plot_vertical_parameters['annot'] * .min.bracket.lines)
          .min.annot.heights.incl.text[[.annot]] = .annot.heights[[.annot]] + ifelse(incl_feature_names[.annot], 1, 0) * (plot_vertical_parameters['annot_text_segment'] * .min.bracket.lines + ifelse(incl_feature_brackets[.annot], 1, 0) * .min.bracket.heights)
          .max.bracket.lines = length(annot_info[[.annot]][['collapsed2']])
          .max.bracket.heights = as.numeric(plot_vertical_parameters['annot'] * .max.bracket.lines)
          .max.annot.heights.incl.text[[.annot]] = as.numeric(.annot.heights[[.annot]] + ifelse(incl_feature_names[.annot], 1, 0) * (plot_vertical_parameters['annot_text_segment'] * .max.bracket.lines + ifelse(incl_feature_brackets[.annot], 1, 0) * .max.bracket.heights))
        }
        .min.annot.heights.combined = sum(unlist(.min.annot.heights.incl.text)) #@ removed [[.annot]]
        .max.annot.heights.combined = sum(unlist(.max.annot.heights.incl.text)) #@ removed [[.annot]]
      }else{
        .min.annot.heights.combined = 0
        .max.annot.heights.combined = 0
      }
    }else{
      .min.annot.heights.combined = 0
      .max.annot.heights.combined = 0
      .max.annot.heights.incl.text = 0
    }
    # setup plotting area - vertical part
    if ("annotations" %in% plotting_segment_order){
      .plotting.segment.order = plotting_segment_order[-which(plotting_segment_order=="annotations")]
    }else{
      .plotting.segment.order = plotting_segment_order
    }
    if ("unstranded-beds" %in% .plotting.segment.order){
      .plotting.segment.order =  .plotting.segment.order[-which( .plotting.segment.order=="unstranded-beds")]
    }else{
      .plotting.segment.order =  .plotting.segment.order
    }
    if (!is.null(tracks_listed)){ #@ if/else added
      .track.vector = unlist(lapply(.plotting.segment.order, function(.segment.type) if(.segment.type %in% names(plot_vertical_parameters)){plot_vertical_parameters[.segment.type]}else{structure(rep(plot_vertical_parameters['seq'], length(tracks_listed[[.segment.type]])), names=paste0(.segment.type, '_', tracks_listed[[.segment.type]]))} ))
    }else{
      .track.vector = NULL
    }
    .n.tracks = sum(.track.vector)
    .min.tracks.annots = .n.tracks + .min.annot.heights.combined
    .max.tracks.annots = .n.tracks + .max.annot.heights.combined
    if (is.null(full_height_cm)){
      .min.track.height.cm = track_height_cm
      .max.track.height.cm = track_height_cm
      .min.full.height.cm = as.numeric(.min.tracks.annots*track_height_cm)
      .max.full.height.cm = as.numeric(.max.tracks.annots*track_height_cm)
    }else{
      .max.track.height.cm = as.numeric(full_height_cm/.min.tracks.annots)
      .min.track.height.cm = as.numeric(full_height_cm/.max.tracks.annots)
      .min.full.height.cm = full_height_cm
      .max.full.height.cm = full_height_cm
    }
    # messages
    PrintOutput(.messages, verbosity)
    return(list('min.track.height.cm.est'=.min.track.height.cm, 'max.track.height.cm.est'=.max.track.height.cm, 'min.full.height.cm.est'=.min.full.height.cm, 'max.full.height.cm.est'=.max.full.height.cm, 'track.vector'=.track.vector, 'min.tracks.annots'=.min.tracks.annots, 'max.tracks.annots'=.max.tracks.annots, 'max.annot.lines'=.max.annot.lines, 'annot.heights'=.annot.heights, 'annot.heights.incl.text'=.max.annot.heights.incl.text, 'min.annot.heights.combined'=.min.annot.heights.combined, 'max.annot.heights.combined'=.max.annot.heights.combined))
  }else{
    # messages
    PrintOutput(.messages, verbosity)
    return()
  }
}


#' Adjust Estimated Plot Heights
#'
#' @details
#' Internal function:
#'
AdjustEstimatedPlotHeights_obs = function(estimated_plot_heights, full_height_cm, track_height_cm){
  .all.spacers = c()
  .min.combined.track.vector = c()
  .max.combined.track.vector = c()
  for (.strand in names(estimated_plot_heights)){
    .spacers = grep('spacer', names(estimated_plot_heights[[.strand]][['track.vector']]), fixed=TRUE)
    if (length(.spacers) > 0){
      names(estimated_plot_heights[[.strand]][['track.vector']])[.spacers] = paste(names(estimated_plot_heights[[.strand]][['track.vector']])[.spacers], length(.all.spacers) + 1:length(.spacers), sep='')
    }
    .min.combined.track.vector = c(.min.combined.track.vector, estimated_plot_heights[[.strand]][['track.vector']])
    .min.combined.track.vector[paste0('annot', .strand)] = estimated_plot_heights[[.strand]][['min.tracks.annots']]
    .max.combined.track.vector = c(.max.combined.track.vector, estimated_plot_heights[[.strand]][['track.vector']])
    .max.combined.track.vector[paste0('annot', .strand)] = estimated_plot_heights[[.strand]][['max.tracks.annots']]
    .all.spacers = c(.all.spacers, .spacers)
  }
  .min.tracks.annots = sum(.min.combined.track.vector)
  .max.tracks.annots = sum(.max.combined.track.vector)
  for (.strand in names(estimated_plot_heights)){
    if (is.null(full_height_cm)){
      estimated_plot_heights[[.strand]][['min.track.height.cm.est']] = track_height_cm
      estimated_plot_heights[[.strand]][['max.track.height.cm.est']] = track_height_cm
      estimated_plot_heights[[.strand]][['min.full.height.cm.est']] = .min.tracks.annots*track_height_cm
      estimated_plot_heights[[.strand]][['max.full.height.cm.est']] = .max.tracks.annots*track_height_cm
    }else{
      estimated_plot_heights[[.strand]][['min.track.height.cm.est']] = full_height_cm/.max.tracks.annots
      estimated_plot_heights[[.strand]][['max.track.height.cm.est']] = full_height_cm/.min.tracks.annots
      estimated_plot_heights[[.strand]][['min.full.height.cm.est']] = full_height_cm
      estimated_plot_heights[[.strand]][['max.full.height.cm.est']] = full_height_cm
    }
  }
  return(estimated_plot_heights)
}

AdjustEstimatedPlotHeights = function(estimated_plot_heights, plot_vertical_parameters, full_height_cm, track_height_cm, title_field_height_cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm){
  .all.spacers = c()
  .min.combined.track.vector = c()
  .max.combined.track.vector = c()
  for (.strand in names(estimated_plot_heights)){
    .spacers = grep('spacer', names(estimated_plot_heights[[.strand]][['track.vector']]), fixed=TRUE)
    if (length(.spacers) > 0){
      names(estimated_plot_heights[[.strand]][['track.vector']])[.spacers] = paste(names(estimated_plot_heights[[.strand]][['track.vector']])[.spacers], length(.all.spacers) + 1:length(.spacers), sep='')
    }
    .min.combined.track.vector = c(.min.combined.track.vector, estimated_plot_heights[[.strand]][['track.vector']])
    .min.combined.track.vector[paste0('annot', .strand)] = estimated_plot_heights[[.strand]][['min.annot.heights.combined']]
    .max.combined.track.vector = c(.max.combined.track.vector, estimated_plot_heights[[.strand]][['track.vector']])
    .max.combined.track.vector[paste0('annot', .strand)] = estimated_plot_heights[[.strand]][['max.annot.heights.combined']]
    .all.spacers = c(.all.spacers, .spacers)
  }
  .min.tracks.annots = sum(.min.combined.track.vector)
  .max.tracks.annots = sum(.max.combined.track.vector)
  for (.strand in names(estimated_plot_heights)){
    if (is.null(full_height_cm)){
      estimated_plot_heights[[.strand]][['min.track.height.cm.est']] = track_height_cm
      estimated_plot_heights[[.strand]][['max.track.height.cm.est']] = track_height_cm
      estimated_plot_heights[[.strand]][['min.full.height.cm.est']] = as.numeric(.min.tracks.annots*track_height_cm)
      estimated_plot_heights[[.strand]][['max.full.height.cm.est']] = as.numeric(.max.tracks.annots*track_height_cm)
    }else{
      .min.non.track.height.cm  = 0
      if (!is.null(title_field_height_cm)){
        .header.cm = title_field_height_cm * sum(grepl('header', names(.min.combined.track.vector)))
        .min.non.track.height.cm  = .min.non.track.height.cm  + .header.cm
      }
      if (!is.null(genomic_scale_height_cm)){
        .scale.cm = genomic_scale_height_cm * sum(grepl('scale', names(.min.combined.track.vector)))
        .min.non.track.height.cm  = .min.non.track.height.cm  + .scale.cm
      }
      if (!is.null(spacer_height_cm)){
        .spacer.cm = spacer_height_cm * (sum(grepl('-spacer', names(.min.combined.track.vector))) + sum(grepl('thickline-spacer', names(.min.combined.track.vector))))
        .min.non.track.height.cm  = .min.non.track.height.cm + .spacer.cm
      }
      .max.non.track.height.cm = .min.non.track.height.cm
      if (!is.null(annotation_height_cm)){
        .min.combined.annot.cm = annotation_height_cm * sum(.min.combined.track.vector[grep('annot', names(.min.combined.track.vector))]/plot_vertical_parameters['annot'])
        .max.combined.annot.cm = annotation_height_cm * sum(.max.combined.track.vector[grep('annot', names(.max.combined.track.vector))]/plot_vertical_parameters['annot'])
        .min.non.track.height.cm = .min.non.track.height.cm + .min.combined.annot.cm
        .max.non.track.height.cm = .max.non.track.height.cm + .max.combined.annot.cm
      }
      .n.tracks = sum(!as.logical(grepl('header', names(.min.combined.track.vector)) + grepl('scale', names(.min.combined.track.vector)) + grepl('-spacer', names(.min.combined.track.vector)) + grepl('annot', names(.min.combined.track.vector))))
      estimated_plot_heights[[.strand]][['min.track.height.cm.est']] = as.numeric((full_height_cm-.max.non.track.height.cm)/.n.tracks)
      estimated_plot_heights[[.strand]][['max.track.height.cm.est']] = as.numeric((full_height_cm-.min.non.track.height.cm)/.n.tracks)
      estimated_plot_heights[[.strand]][['min.full.height.cm.est']] = full_height_cm
      estimated_plot_heights[[.strand]][['max.full.height.cm.est']] = full_height_cm
    }
    estimated_plot_heights[[.strand]][['annot.heights']] = lapply(estimated_plot_heights[[.strand]][['annot.heights']], function(x) x/plot_vertical_parameters[['annot']]) #@
    estimated_plot_heights[[.strand]][['min.combined.track.vector']] = .min.combined.track.vector
    estimated_plot_heights[[.strand]][['max.combined.track.vector']] = .max.combined.track.vector
  }
  return(estimated_plot_heights)
}


#' Recommended Font Sizes
#'
#' @details
#' Internal function:
#'
RecommendedFontSizes = function(est_track_height_cm, est_min_annot_height, plot_vertical_parameters){
  .max.font.size.std.tracks = round( est_track_height_cm / std_letter_height, 0)
  .est.min.annot.height.cm = est_track_height_cm * est_min_annot_height
  .max.font.size.std.annot = round( .est.min.annot.height.cm / std_letter_height, 0) + 2
  .max.font.size.std = min(.max.font.size.std.tracks, .max.font.size.std.annot)
  #c('std', 'main', 'sub', 'scale', 'genomic_axis', 'signal_axis', 'annotation_features')
  .plot.vertical.parameters.cm = est_track_height_cm * plot_vertical_parameters
  .main = round(as.numeric(9*.plot.vertical.parameters.cm['header']/0.66), 0)
  .sub = round(as.numeric(6*.plot.vertical.parameters.cm['header']/0.66), 0)
  .scale = round(as.numeric(6*.plot.vertical.parameters.cm['header']/0.66), 0)
  .genomic.axis = round(as.numeric(5*.plot.vertical.parameters.cm['scale']/0.24), 0)
  .signal.axis = round(as.numeric(0.7*.max.font.size.std), 0)
  .annotation.features = round(as.numeric(6*.plot.vertical.parameters.cm['annot_text_segment']/0.24), 0)
  .recommended.font.sizes = structure(c(.max.font.size.std, .main, .sub, .scale, .genomic.axis, .signal.axis, .annotation.features), names=c('std', 'main', 'sub', 'scale', 'genomic_axis', 'signal_axis', 'annotation_features'))
  if (.recommended.font.sizes['std'] >= .recommended.font.sizes['main']){
    .recommended.font.sizes['std'] = max(.recommended.font.sizes['main']-1, min_font_size)
    .recommended.font.sizes['signal_axis'] = round(.recommended.font.sizes['std'] * 0.7, 0)
  }
  return(.recommended.font.sizes)
}


#' Coords Of Feat Name
#'
#' @details
#' Internal function: Given a chromosomal coordinate (for the center of the annotated feature name) the parameters necessary for plotting the word such that it fits inside the plotted region are calculated
#'
CoordsOfFeatName = function(chrom_coord_feature, feat_width_cm, word_width_cm, word_width_feat_fit, word_width_plot_fit, feature_gr, plotted_region){
  if (is.na(word_width_cm)){
    return(data.frame('chrom.coord.feature'=NA, 'adj'=NA, 'feat.start'=NA, 'feat.end'=NA, 'feat.width'=NA, 'feat.name.width'=NA, 'text.fit.feat'=NA, 'text.fit.plot'=NA))
  }else{
    # (1) check text relative to annotated feature
    .left.coord.feat.name = chrom_coord_feature - as.integer((0.5*word_width_cm/feat_width_cm)*IRanges::width(feature_gr))
    .right.coord.feat.name = chrom_coord_feature + as.integer((0.5*word_width_cm/feat_width_cm)*IRanges::width(feature_gr))
    .width.feat.name = .right.coord.feat.name - .left.coord.feat.name
    if (.left.coord.feat.name > IRanges::start(feature_gr) & .right.coord.feat.name < IRanges::end(feature_gr)){
      .chrom.coord.feature = as.integer(chrom_coord_feature)
      .adj = 0.5
    }else if (.right.coord.feat.name < IRanges::end(feature_gr)){
      .chrom.coord.feature = IRanges::start(feature_gr)
      .adj = 0
    }else if (.left.coord.feat.name > IRanges::start(feature_gr)){
      .chrom.coord.feature = IRanges::end(feature_gr)
      .adj = 1
    }else{
      .chrom.coord.feature = as.integer(chrom_coord_feature)
      .adj = 0.5
    }
    # (2) check text relative to plotted region
    .left.coord.feat.name.adj = .chrom.coord.feature - as.integer(.adj*(word_width_cm/feat_width_cm)*IRanges::width(feature_gr))
    .right.coord.feat.name.adj = .chrom.coord.feature + as.integer((1-.adj)*(word_width_cm/feat_width_cm)*IRanges::width(feature_gr))
    if (.left.coord.feat.name.adj > IRanges::start(plotted_region) & .right.coord.feat.name.adj < IRanges::end(plotted_region)){
      .chrom.coord.feature = .chrom.coord.feature
      .adj = 0.5
    }else if (.right.coord.feat.name.adj < IRanges::end(plotted_region)){
      .chrom.coord.feature = IRanges::start(plotted_region)
      .adj = 0
    }else if (.left.coord.feat.name.adj > IRanges::start(plotted_region)){
      .chrom.coord.feature = IRanges::end(plotted_region)
      .adj = 1
    }else{
      .chrom.coord.feature = .chrom.coord.feature
      .adj = 0.5
    }
    return(data.frame('chrom.coord.feature'=.chrom.coord.feature, 'adj'=.adj, 'feat.start'=IRanges::start(feature_gr), 'feat.end'=IRanges::end(feature_gr), 'feat.width'=IRanges::width(feature_gr), 'feat.name.width'=.width.feat.name, 'text.fit.feat'=word_width_feat_fit, 'text.fit.plot'=word_width_plot_fit))
  }
}


#' Organize Annotation Text
#'
#' @details
#' Internal function: Given a set of possible font sizes (e.g. helvetica 4-8 given by letter_widths) where should the feature names within the region be plotted
#'
OrganizeAnnotationText = function(plotted_region, annot_info, annotation_packing, letter_widths, center_of_mass=FALSE, verbosity){
  if (!is.null(annot_info)){
    feature.text = list()
    .tracks.width.cm = as.numeric(S4Vectors::mcols(plotted_region)$tracks.width)
    for (.annot.name in names(annot_info)){
      feature.text[[.annot.name]] = list()
      if (!is.null(annot_info[[.annot.name]][['collapsed']])){
        .combined.collapsed.grl = annot_info[[.annot.name]][['collapsed']]
        for (.feat.name in names(.combined.collapsed.grl)){
          .sub.feat.names = S4Vectors::mcols(.combined.collapsed.grl[[.feat.name]])$collapsed.names[[1]]
          for (.sub.feat.name in .sub.feat.names){
            if (!(.sub.feat.name %in% names(.combined.collapsed.grl))){
              .combined.collapsed.grl[[.sub.feat.name]] = annot_info[[.annot.name]][['collapsed2']][[.feat.name]][S4Vectors::mcols(annot_info[[.annot.name]][['collapsed2']][[.feat.name]])$name==.sub.feat.name]
            }
          }
        }
        .feat.widths.cm = sapply(.combined.collapsed.grl, function(gr) .tracks.width.cm*IRanges::width(gr)/IRanges::width(plotted_region))
        .word.widths.cm = as.matrix(sapply(names(.combined.collapsed.grl), function(.feat.name) nchar(.feat.name)*letter_widths))
        .word.widths.feat.fit = as.matrix(sapply(names(.feat.widths.cm), function(.feat.name) .word.widths.cm[,.feat.name] <= 0.8*.feat.widths.cm[.feat.name]))
        .word.widths.plot.fit = as.matrix(sapply(names(.feat.widths.cm), function(.feat.name) .word.widths.cm[,.feat.name] <= 0.8*.tracks.width.cm))
        if (center_of_mass){
          .chrom.coord.feature = structure(rep(NA, length(.combined.collapsed.grl)), names=names(.combined.collapsed.grl))
          for (.feat.name in names(.combined.collapsed.grl)){
            if (.feat.name %in% names(annot_info[[.annot.name]][['expanded']])){
              .gr = annot_info[[.annot.name]][['expanded']][[.feat.name]]
              if (any(S4Vectors::mcols(.gr)$intron.from.start)){
                IRanges::start(.gr)[S4Vectors::mcols(.gr)$intron.from.start] = IRanges::start(plotted_region)
              }
              if (any(S4Vectors::mcols(.gr)$intron.from.end)){
                IRanges::end(.gr)[S4Vectors::mcols(.gr)$intron.from.end] = IRanges::end(plotted_region)
              }
              .chrom.coord.feature[.feat.name] = as.integer(mean((IRanges::start(.gr) + IRanges::end(.gr))/2))
              if (!S4Vectors::mcols(.combined.collapsed.grl[[.feat.name]])$disjoint){
                for (.sub.feat.name in S4Vectors::mcols(.combined.collapsed.grl[[.feat.name]])$collapsed.names[[1]]){
                  .sub.gr = S4Vectors::subset(.gr, name==.sub.feat.name)
                  .chrom.coord.feature[.sub.feat.name] = as.integer(mean((IRanges::start(.sub.gr) + IRanges::end(.sub.gr))/2))
                }
              }
            }
          }
          if (any(is.na(.chrom.coord.feature))){
            if (verbosity > 0){
              cat('ERROR: "OrganizeAnnotationText" - check it out', '\n')
            }
          }
        }else{
          .chrom.coord.feature = sapply(.combined.collapsed.grl, function(gr) as.integer(mean(c(IRanges::start(gr), IRanges::end(gr)))) )
        }
        for (.feat.name in names(.chrom.coord.feature)){
          feature.text[[.annot.name]][[.feat.name]] = sapply( 1:length(letter_widths) , # which(!is.na(letter_widths))
                                                              function(.i)
                                                                CoordsOfFeatName(.chrom.coord.feature[.feat.name], .feat.widths.cm[.feat.name], .word.widths.cm[.i, .feat.name], .word.widths.feat.fit[.i,.feat.name], .word.widths.plot.fit[.i,.feat.name], .combined.collapsed.grl[[.feat.name]], plotted_region))
        }
        if (annotation_packing[.annot.name] != 'collapsed'){
          .feature.names = S4Vectors::mcols(unlist(annot_info[[.annot.name]][['collapsed2']]))$name
        }else{
          .feature.names = names(annot_info[[.annot.name]][['collapsed']])
        }
        feature.text[[.annot.name]] = feature.text[[.annot.name]][.feature.names]
      }
    }
    return(feature.text)
  }else{
    return()
  }
}


#' Organize All Annotation Texts In Plotted Region
#'
#' @details
#' Internal function: Determine the y-axis organization of names of annotated features within plotted region depending on font size
#'
OrganizeAllAnnotationTextsInPlottedRegion = function(feature_text, plotted_region, letter_widths){
  if (!is.null(feature_text)){
    .strand = as.character(BiocGenerics::strand(plotted_region))
    .annotation.brackets.names.gr.list = list()
    .annotation.brackets.names.gr.list2 = list()
    .annotation.brackets.names.packing.list = list()
    for (.annot.name in names(feature_text)){
      if (length(feature_text[[.annot.name]]) > 0){
        .annotation.brackets.names.gr.list[[.annot.name]] = lapply(1:length(letter_widths), function(.font.size) {
          if (is.na(letter_widths[.font.size])){
            return(NA)
          }else{
            unlist(as(sapply( names(feature_text[[.annot.name]]), function(.feat.name) {
              x = feature_text[[.annot.name]][[.feat.name]][,.font.size];
              .feat.name.gr = GenomicRanges::granges(plotted_region);
              IRanges::start(.feat.name.gr) = min(x$chrom.coord.feature - as.integer(x$adj*x$feat.name.width), x$feat.start);
              IRanges::end(.feat.name.gr) = max(x$chrom.coord.feature + as.integer((1-x$adj)*x$feat.name.width), x$feat.end);
              S4Vectors::mcols(.feat.name.gr)$name = .feat.name;
              S4Vectors::mcols(.feat.name.gr)$adj = x$adj;
              S4Vectors::mcols(.feat.name.gr)$coord = x$chrom.coord.feature;
              S4Vectors::mcols(.feat.name.gr)$feat.start = x$feat.start;
              S4Vectors::mcols(.feat.name.gr)$feat.end = x$feat.end;
              S4Vectors::mcols(.feat.name.gr)$adjustable = IRanges::width(.feat.name.gr) > x$feat.end - x$feat.start + 1
              return(.feat.name.gr)
            } ), 'GRangesList'))
          }
        })
        .annotation.brackets.names.gr.list2[[.annot.name]] = lapply(1:length(letter_widths), function(.font.size) {
          if (is.na(letter_widths[.font.size])){
            return(NA)
          }else{
            .annotation.brackets.names.gr = sort(.annotation.brackets.names.gr.list[[.annot.name]][[.font.size]])
            .overlaps = GenomicRanges::findOverlaps(.annotation.brackets.names.gr)
            .overlaps.nonself = .overlaps[which(S4Vectors::subjectHits(.overlaps) - S4Vectors::queryHits(.overlaps) != 0)]
            .n.overlaps.nonself = length(.overlaps.nonself)
            .adjustable.features = which(S4Vectors::mcols(.annotation.brackets.names.gr)$adjustable)
            for (.n.feature in .adjustable.features){
              if (.n.feature %in% S4Vectors::queryHits(.overlaps.nonself)){
                if (.n.feature == 1){
                  .dists.feat.start = IRanges::start(.annotation.brackets.names.gr[.n.feature]) - IRanges::start(plotted_region)
                }else{
                  .dists.feat.start = IRanges::start(.annotation.brackets.names.gr[.n.feature]) - IRanges::end(.annotation.brackets.names.gr)[1:(.n.feature-1)]
                }
                .subjects = S4Vectors::subjectHits(.overlaps.nonself)[S4Vectors::queryHits(.overlaps.nonself)==.n.feature]
                if (all(.dists.feat.start > 0)){
                  .dists.prev.feat= .dists.feat.start[.dists.feat.start > 0]
                  .min.dist.prev.feat = .dists.prev.feat[which(.dists.prev.feat==min(.dists.prev.feat))]
                  .subjects.right = .subjects[.subjects>.n.feature]
                  .subjects.right.gr = GenomicRanges::reduce(.annotation.brackets.names.gr[.subjects.right])
                  .width.rigth.overlap = IRanges::end(.annotation.brackets.names.gr[.n.feature]) - IRanges::start(.subjects.right.gr) + 1
                  .text.right.wiggle = max(IRanges::start(.annotation.brackets.names.gr[.n.feature])-S4Vectors::mcols(.annotation.brackets.names.gr[.n.feature])$feat.start, IRanges::end(.annotation.brackets.names.gr[.n.feature])-S4Vectors::mcols(.annotation.brackets.names.gr[.n.feature])$feat.end)
                  .min.dist.prev.feat = min(.min.dist.prev.feat, .width.rigth.overlap, .text.right.wiggle)
                }else{
                  .min.dist.prev.feat = 0
                }
                if (.n.feature == length(.annotation.brackets.names.gr)){
                  .dists.feat.end = IRanges::end(plotted_region) - IRanges::end(.annotation.brackets.names.gr[.n.feature])
                }else{
                  .dists.feat.end = IRanges::start(.annotation.brackets.names.gr)[(.n.feature+1):length(.annotation.brackets.names.gr)] - IRanges::end(.annotation.brackets.names.gr[.n.feature])
                }
                if (all(.dists.feat.end > 0)){
                  .dists.next.feat = .dists.feat.end[.dists.feat.end > 0]
                  .min.dist.next.feat = .dists.next.feat[which(.dists.next.feat==min(.dists.next.feat))]
                  .subjects.left = .subjects[.subjects<.n.feature]
                  .subjects.left.gr = GenomicRanges::reduce(.annotation.brackets.names.gr[.subjects.left])
                  .width.left.overlap = IRanges::end(.subjects.left.gr) - IRanges::start(.annotation.brackets.names.gr[.n.feature]) + 1
                  .text.left.wiggle = max(S4Vectors::mcols(.annotation.brackets.names.gr[.n.feature])$feat.start-IRanges::start(.annotation.brackets.names.gr[.n.feature]), S4Vectors::mcols(.annotation.brackets.names.gr[.n.feature])$feat.end-IRanges::end(.annotation.brackets.names.gr[.n.feature]))
                  .min.dist.next.feat = min(.min.dist.next.feat, .width.left.overlap, .text.left.wiggle)
                }else{
                  .min.dist.next.feat = 0
                }
                if (any(c(.min.dist.prev.feat, .min.dist.next.feat) > 0)){
                  .tmp.annotation.brackets.names.gr = .annotation.brackets.names.gr
                  .tmp.annotation.brackets.names.gr[.n.feature] = IRanges::shift(.tmp.annotation.brackets.names.gr[.n.feature], ifelse(.min.dist.prev.feat > .min.dist.next.feat, -.min.dist.prev.feat, .min.dist.next.feat))
                  S4Vectors::mcols(.tmp.annotation.brackets.names.gr[.n.feature])$coord = S4Vectors::mcols(.tmp.annotation.brackets.names.gr[.n.feature])$coord + ifelse(.min.dist.prev.feat > .min.dist.next.feat, -.min.dist.prev.feat, .min.dist.next.feat)
                  .tmp.overlaps = GenomicRanges::findOverlaps(.tmp.annotation.brackets.names.gr)
                  .tmp.overlaps.nonself = .tmp.overlaps[which(S4Vectors::subjectHits(.tmp.overlaps) - S4Vectors::queryHits(.tmp.overlaps) != 0)]
                  if (!all(.subjects %in% S4Vectors::subjectHits(.tmp.overlaps.nonself)[S4Vectors::queryHits(.tmp.overlaps.nonself)==.n.feature])){
                    .annotation.brackets.names.gr = .tmp.annotation.brackets.names.gr
                  }
                }
              }
            }
            if (.strand == '-'){
              .annotation.brackets.names.gr = .annotation.brackets.names.gr[order(IRanges::end(.annotation.brackets.names.gr), decreasing=TRUE)] # rev(.annotation.brackets.names.gr) #
            }
            return(.annotation.brackets.names.gr)
          }
        })
        .annotation.brackets.names.packing.list[[.annot.name]] = lapply(1:length(letter_widths), function(.font.size) {
          if (is.na(letter_widths[.font.size])){
            return(NA)
          }else{
            .annotation.brackets.names.gr = .annotation.brackets.names.gr.list2[[.annot.name]][[.font.size]]
            if (.strand == '-'){
              .annotation.brackets.names.gr = .annotation.brackets.names.gr[order(IRanges::end(.annotation.brackets.names.gr), decreasing=TRUE)]
            }
            annotation.lines = OrganizeOverlappingIVs(.annotation.brackets.names.gr)
            annotation.lines = lapply(annotation.lines, function(annotation.line) names(.annotation.brackets.names.gr)[annotation.line])
          }
          return(annotation.lines)
        })
      }else{
        .annotation.brackets.names.gr.list2[[.annot.name]]=NULL
        .annotation.brackets.names.packing.list[[.annot.name]]=NULL
      }
    }
    return(list('names.gr.list'=.annotation.brackets.names.gr.list2, 'names.packing.list'=.annotation.brackets.names.packing.list))
  }else{
    return()
  }
}

#' Update Plot Vertical Parameters
#'
#' @details
#' Internal function:
#'
UpdatePlotVerticalParameters = function(plot_vertical_parameters, track_height_cm_estimate, title_field_height_cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm){
  if (!is.null(title_field_height_cm)){
    plot_vertical_parameters['header'] = title_field_height_cm/track_height_cm_estimate
  }
  if (!is.null(genomic_scale_height_cm)){
    plot_vertical_parameters['scale'] = genomic_scale_height_cm/track_height_cm_estimate
  }
  if (!is.null(annotation_height_cm)){
    plot_vertical_parameters['annot'] = annotation_height_cm/track_height_cm_estimate
    plot_vertical_parameters['annot_text_segment'] = plot_vertical_parameters['annot']
    plot_vertical_parameters['annot_squished'] = 0.5*plot_vertical_parameters['annot']
  }
  if (!is.null(spacer_height_cm)){
    plot_vertical_parameters['line-spacer'] = spacer_height_cm/track_height_cm_estimate
    plot_vertical_parameters['empty-spacer'] = plot_vertical_parameters['line-spacer']
    plot_vertical_parameters['thickline-spacer'] = 2*plot_vertical_parameters['line-spacer']
  }
  return(plot_vertical_parameters)
}

#' Update Plot Vertical Parameters v2
#'
#' @details
#' Internal function:
#'
CalculateTrackHeight = function(combined_track_vector, total_annotation_lines, full_height_cm, title_field_height_cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm){
  .non.track.height.cm  = 0
  if (!is.null(title_field_height_cm)){
    .header.cm = title_field_height_cm * sum(grepl('header', names(combined_track_vector)))
    .non.track.height.cm  = .non.track.height.cm  + .header.cm
  }
  if (!is.null(genomic_scale_height_cm)){
    .scale.cm = genomic_scale_height_cm * sum(grepl('scale', names(combined_track_vector)))
    .non.track.height.cm  = .non.track.height.cm  + .scale.cm
  }
  if (!is.null(spacer_height_cm)){
    .spacer.cm = spacer_height_cm * (sum(grepl('-spacer', names(combined_track_vector))) + sum(grepl('thickline-spacer', names(combined_track_vector))))
    .non.track.height.cm  = .non.track.height.cm + .spacer.cm
  }
  if (!is.null(annotation_height_cm)){
    .combined.annot.cm = annotation_height_cm * total_annotation_lines
    .non.track.height.cm  = .non.track.height.cm + .combined.annot.cm
  }
  .n.tracks = sum(!as.logical(grepl('header', names(combined_track_vector)) + grepl('scale', names(combined_track_vector)) + grepl('-spacer', names(combined_track_vector)) + grepl('annot', names(combined_track_vector))))
  .track.height.cm = (full_height_cm-.non.track.height.cm)/.n.tracks
  return(.track.height.cm)
}


#' Update Track Vector
#'
#' @details
#' Internal function:
#'
UpdateTrackVector = function(track_vector, plot_vertical_parameters){
  track_vector[grep('header', names(track_vector))] = plot_vertical_parameters['header']
  track_vector[grep('scale', names(track_vector))] = plot_vertical_parameters['scale']
  track_vector[grep('empty-spacer', names(track_vector))] = plot_vertical_parameters['empty-spacer']
  track_vector[grep('line-spacer', names(track_vector))] = plot_vertical_parameters['line-spacer']
  track_vector[grep('thickline-spacer', names(track_vector))] = plot_vertical_parameters['thickline-spacer']
  if (any(grepl('annot', names(track_vector)))){
    track_vector[!as.logical(grepl('header', names(track_vector)) + grepl('scale', names(track_vector)) + grepl('-spacer', names(track_vector)) + grepl('annot', names(track_vector)))] = 1
  }else{
    track_vector[!as.logical(grepl('header', names(track_vector)) + grepl('scale', names(track_vector)) + grepl('-spacer', names(track_vector)))] = 1
  }
  return(track_vector)
}



#' Relative Annotation Height
#'
#' @details
#' Internal function:
#'
RelativeAnnotationHeight = function(annot_info, annot_heights, letter_heights, incl_feature_names, feature_text_org, annotation_packing, incl_feature_brackets, stranded_beds){
  .annot.heights.incl.text = list()
  if (!is.null(annot_info)){
    if (any(stranded_beds)){
      for (.annot in names(annot_info)[stranded_beds]){
        if (!is.null(feature_text_org[['names.packing.list']][[.annot]])){
          .bracket.lines = lengths(feature_text_org[['names.packing.list']][[.annot]])
        }else{
          .bracket.lines = rep(1, length(letter_heights))
        }
        .annot.heights.incl.text[[.annot]] = annot_heights[[.annot]] + ifelse(incl_feature_names[.annot], 1, 0) * (.bracket.lines + ifelse(incl_feature_brackets[.annot], 1, 0) * .bracket.lines)
      }
      .annot.heights.combined = colSums(do.call(rbind, .annot.heights.incl.text))
    }else{
      .annot.heights.combined = rep(0, length(letter_heights))
    }
  }else{
    .annot.heights.combined = rep(0, length(letter_heights))
  }
  return(list('annot.heights.combined'=.annot.heights.combined, 'annot.heights.incl.text'=.annot.heights.incl.text))
}


#' Plot Height Parameters
#'
#' @details
#' Internal function:
#'
PlotHeightParameters_obs = function(annot_info, track_vector, max_annot_lines, annot_heights, letter_heights, incl_feature_names, feature_text_org, annotation_packing, incl_feature_brackets, track_height_cm, full_height_cm, stranded_beds, plot_vertical_parameters){
  .annot.heights.incl.text = list()
  if (!is.null(annot_info)){
    if (any(stranded_beds)){
      for (.annot in names(annot_info)[stranded_beds]){
        if (!is.null(feature_text_org[['names.packing.list']][[.annot]])){
          .bracket.lines = lengths(feature_text_org[['names.packing.list']][[.annot]])
        }else{
          .bracket.lines = rep(1, length(letter_heights))
        }
        .bracket.heights = as.numeric(plot_vertical_parameters['annot'] * .bracket.lines)
        .annot.heights.incl.text[[.annot]] = annot_heights[[.annot]] + ifelse(incl_feature_names[.annot], 1, 0) * (plot_vertical_parameters['annot_text_segment'] * .bracket.lines + ifelse(incl_feature_brackets[.annot], 1, 0) * .bracket.heights)
      }
      .annot.heights.combined = colSums(do.call(rbind, .annot.heights.incl.text))
    }else{
      .annot.heights.combined = rep(0, length(letter_heights))
    }
  }else{
    .annot.heights.combined = rep(0, length(letter_heights))
  }
  # setup plotting area - vertical part
  .n.tracks = sum(track_vector)
  .n.tracks.annots = .n.tracks + .annot.heights.combined ## vector where indices correspond to font size of feature name
  if (is.null(full_height_cm)){
    .track.height.cm = rep(track_height_cm, length(.n.tracks.annots))
    .full.height.cm = .n.tracks.annots*.track.height.cm
  }else{
    .track.height.cm = full_height_cm/.n.tracks.annots
    .full.height.cm = rep(full_height_cm, length(.n.tracks.annots))
  }
  # plot height (1x) as function of font.size of annotated feature name
  .full.height.in = .full.height.cm * cm_to_in				 ## full_height_cm of figure in inches for pdf
  return(list('full.height.in'=.full.height.in, 'track.vector'=track_vector, 'n.tracks.annots'=.n.tracks.annots, 'track.height.cm'=.track.height.cm, 'max.annot.lines'=max_annot_lines, 'annot.heights'=annot_heights, 'annot.heights.incl.text'=.annot.heights.incl.text))
}


PlotHeightParameters = function(combined_track_vector, track_vector, annotation_lines, total_annotation_lines, annot_heights_incl_text, max_annot_lines, annot_heights, track_height_cm, full_height_cm, title_field_height_cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm, plot_vertical_parameters){
  if (!is.null(full_height_cm)){
    .annot.heights.incl.text = annot_heights_incl_text
    .annot.heights = lapply(annot_heights, function(x) rep(x, length(annotation_lines)))
    .annot.heights.combined = annotation_lines
    .track.vector = list()
    .track.height.cm = as.numeric(sapply(total_annotation_lines, function(.tal) CalculateTrackHeight(combined_track_vector, .tal, full_height_cm, title_field_height_cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm)))
    .full.height.cm = rep(NA, length(annotation_lines))
    .n.tracks.annots = rep(NA, length(annotation_lines))
    for (i in 1:length(annotation_lines)){
      .plot.vertical.parameters = UpdatePlotVerticalParameters(plot_vertical_parameters, .track.height.cm[i], title_field_height_cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm)
      .multiplier = rep(1, length(annotation_lines))
      .multiplier[i] = .plot.vertical.parameters['annot']
      .annot.heights.incl.text = lapply(.annot.heights.incl.text, function(x) x * .multiplier)
      .annot.heights = lapply(.annot.heights, function(x) x * .multiplier)
      .annot.heights.combined[i] = .annot.heights.combined[i] * .plot.vertical.parameters['annot']
      .track.vector[[i]] = UpdateTrackVector(track_vector, .plot.vertical.parameters)
      .n.tracks = sum(.track.vector[[i]])
      .n.tracks.annots[i] = .n.tracks + .annot.heights.combined[i] ## vector where indices correspond to font size of feature name
      .full.height.cm[i] = .n.tracks.annots[i]*.track.height.cm[i]
    }
  }else{
    .annot.heights.incl.text = lapply(annot_heights_incl_text, function(x) x * as.numeric(plot_vertical_parameters['annot']))
    .annot.heights = lapply(annot_heights, function(x) plot_vertical_parameters['annot']*rep(x, length(annotation_lines)))
    .annot.heights.combined = annotation_lines * as.numeric(plot_vertical_parameters['annot'])
    .n.tracks = sum(track_vector)
    .n.tracks.annots = .n.tracks + .annot.heights.combined ## vector where indices correspond to font size of feature name
    .track.height.cm = rep(track_height_cm, length(.n.tracks.annots))
    .full.height.cm = .n.tracks.annots*.track.height.cm
    .track.vector = lapply(1:length(.n.tracks.annots), function(x) track_vector)
  }
  # plot height (1x) as function of font.size of annotated feature name
  .full.height.in = .full.height.cm * cm_to_in				 ## full_height_cm of figure in inches for pdf
  return(list('full.height.in'=.full.height.in, 'track.vector'=.track.vector, 'n.tracks.annots'=.n.tracks.annots, 'track.height.cm'=.track.height.cm, 'max.annot.lines'=max_annot_lines, 'annot.heights'=.annot.heights, 'annot.heights.incl.text'=.annot.heights.incl.text))
}


#' Organize Panels Dimensions
#'
#' @details
#' Internal function: The overall panel dimensions can fixed upfront or it can be left open for the function to determine the dimension - with some initial parameters defined.
#'
OrganizePanelsDimensions = function(datasets, min_word_length, replicate_names, print_one_line_sample_names, incl_first_panel, plot_height_parameters, feature_names_font_size, font_size_range, recommended_font_sizes, scale_font_size, horizontal_panels_list, panel_font_size_list, panels_list, plot_widths_cm, panel_separators, strand, both_strands, strands_intermingled, stranded_samples, fixed_panel_width=FALSE, verbosity){
  # get vertical and horizontal restrictions
  .tracks.vector = plot_height_parameters[['track.vector']][[feature_names_font_size]]
  .track.height.cm = plot_height_parameters[['track.height.cm']][feature_names_font_size]
  .panels.max.width.cm = plot_widths_cm['panels.max.width.cm']
  .organised.panels = list()
  .panel.separator.cm = 0.2*0.3 # how wide should the separations between levels/panels be
  .word.extensions = 1.0 # 0.8 # how much more horizontal (relative to word) space (in standard letters) should be assigned (added) per word
  .word.vert.space = 1.5  # how much more vertical (relative to word) space (in standard letters) should be assigned (multiplied) per word
  # set up lists for analyses
  .penalties.list = list()
  .penalties.0hor.list = list()
  # find panel sizes for various font sizes for the different 'datasets'
  .n.levels.list = list()
  .n.chars.list = list()
  .heigths.cm.list = list()
  .subsample.matrices = list()
  if (!is.null(panel_font_size_list)){
    .font.size.range = min(unlist(panel_font_size_list)):max(unlist(panel_font_size_list))
  }else{
    .font.size.range = font_size_range
  }
  for (.dataset in datasets){
    .subsamples = grep(paste0("^", .dataset, "_"), names(.tracks.vector), value=TRUE)
    if (length(.subsamples)==0){
      .subsamples = grep(paste0(.dataset, "_"), names(.tracks.vector), fixed=TRUE, value=TRUE)
    }
    .subsamples = as.character(sapply(.subsamples, function(s) paste0(.dataset, '.', strsplit(s, split=paste0(.dataset, "_"), fixed=TRUE)[[1]][2]) ))
    .subsample.matrix = do.call('rbind', sapply(.subsamples, function(.sep) strsplit(.sep, split='.', fixed=T)))
    .n.levels = ncol(.subsample.matrix)
    if (all(grepl('^rep\\d$', .subsample.matrix[,.n.levels]))){
      if (is.null(replicate_names)){
        .subsample.matrix = .subsample.matrix[, 1:(.n.levels-1), drop=FALSE]
        .n.levels = ncol(.subsample.matrix)
      }else{
        .subsample.matrix[,.n.levels] = as.character(sapply(.subsample.matrix[,.n.levels], function(s) paste0(replicate_names, strsplit(s, split='rep', fixed=TRUE)[[1]][2])))
        names(panels_list[[.dataset]][[.n.levels-1]]) = .subsample.matrix[,.n.levels]
      }
    }
    .subsample.matrices[[.dataset]] = .subsample.matrix
    .incl.first.panel = incl_first_panel
    if (print_one_line_sample_names){
      .incl.first.panel = FALSE
      .n.levels = 2
      .descriptors = sapply(1:ncol(.subsample.matrix), function(.n.col) length(unique(.subsample.matrix[,.n.col]))!=1)
      if (all(!.descriptors)){
        .descriptors[length(.descriptors)] = TRUE
      }
      if (incl_first_panel){
        .descriptors[1] = TRUE
      }
      .one.line.sample.names = as.character(apply(.subsample.matrix[,.descriptors, drop=FALSE], 1, function(r) paste0(r, collapse='.')))
      .nchars.matrix = matrix(nrow=length(.one.line.sample.names), ncol=2, c(rep(0, length(.one.line.sample.names)), nchar(.one.line.sample.names)))
      rownames(.nchars.matrix) = .one.line.sample.names
    }else{
      .nchars.matrix = t(as.matrix(apply(.subsample.matrix, 1, nchar)))
    }
    .n.levels.list[[.dataset]] = .n.levels
    .heigths.cm = rep(NA, .n.levels)                  # the heights of the minimum unit of sub-panels
    .n.chars = rep(NA, .n.levels)                     # maximum number of characters in a given sub-panel
    # 1st level (vertical panel):
    .heigths.cm[1] = .track.height.cm * nrow(.subsample.matrix) * ifelse(both_strands & strands_intermingled & stranded_samples[.dataset], 2, 1)
    .n.chars[1] = max(nchar(datasets))  # the first panel width is coordinated between datasets
    # the other levels (vertical panels)
    if (.n.levels > 1){
      for (.n.level in 2:.n.levels){
        .heigths.cm[.n.level] = min(panels_list[[.dataset]][[.n.level-1]])*.track.height.cm * ifelse(both_strands & strands_intermingled & stranded_samples[.dataset], 2, 1)
        .n.chars[.n.level] =  max(.nchars.matrix[,.n.level])
      }
    }
    .n.chars.list[[.dataset]] = .n.chars
    .heigths.cm.list[[.dataset]] = .heigths.cm
    
    ### given a maximum word length set up matrices with widths and heights of each panel for each possible font size in fully 'horizontal' representation
    .panel.word.widths = list(t(matrix(ncol=.n.levels, nrow=length(.font.size.range), rep(.font.size.range, .n.levels), dimnames=list(paste0('f', .font.size.range), paste0('panel', 1:.n.levels)))) * (.n.chars + .word.extensions) * std_letter_width)
    .panel.word.heights = list(t(matrix(ncol=.n.levels, nrow=length(.font.size.range), rep(.font.size.range, .n.levels), dimnames=list(paste0('f', .font.size.range), paste0('panel', 1:.n.levels)))) * std_letter_height)
    .word.heights = .font.size.range * std_letter_height
    
    # setup matrices to calculate penalties - rows are representative of different horizontal/vertical configurations v0: 0 vertical panels, v1: first panel vertical, v2: two first panels vertical etc.
    # the horizontal penalties will be calculated as sum of estimated space outside dedicated plotting area
    # the vertical penalties will be given as -1 for each panel that has text outside dedicated plotting area
    .hor.penalties = matrix(NA, nrow=.n.levels, ncol=length(.font.size.range), dimnames=list(paste0('ver', 1:.n.levels-1), paste0('f', .font.size.range) ) )
    .ver.penalties = matrix(NA, nrow=.n.levels, ncol=length(.font.size.range), dimnames=list(paste0('ver', 1:.n.levels-1), paste0('f', .font.size.range) ) )
    
    # a separate set of penalties for the case that at least one of the 0th panels should be horizontal
    .hor.penalties.0hor = matrix(NA, nrow=.n.levels, ncol=length(.font.size.range), dimnames=list(paste0('ver', 1:.n.levels-1), paste0('f', .font.size.range) ) )
    .ver.penalties.0hor = matrix(NA, nrow=.n.levels, ncol=length(.font.size.range), dimnames=list(paste0('ver', 1:.n.levels-1), paste0('f', .font.size.range) ) )
    .outer.panel.widths = .panel.word.widths[[1]][1,]
    if (!.incl.first.panel){
      .outer.panel.widths = rep(0, length(.outer.panel.widths))
    }
    
    .subpanels = .heigths.cm[1]/.heigths.cm
    .n.panel.separators = .n.levels - 1 - ifelse(.incl.first.panel,0,1)
    .incl.panels = ifelse(.incl.first.panel, 1, 2):.n.levels
    for (.n.ver.panels in 0:(.n.levels-1)){
      if (.panels.max.width.cm==-1){
        .hor.penalties[.n.ver.panels+1,] = rep(0, ncol(.panel.word.widths[[.n.ver.panels+1]]))
        .hor.penalties.0hor[.n.ver.panels+1,] = rep(0, ncol(.panel.word.widths[[.n.ver.panels+1]]))
      }else{
        .panels.max.width.corr.cm = .panels.max.width.cm - .n.panel.separators * .panel.separator.cm
        .hor.penalties[.n.ver.panels+1,] = .panels.max.width.corr.cm - colSums(.panel.word.widths[[.n.ver.panels+1]][.incl.panels,,drop=FALSE])
        .inner.panel.max.widths = .panels.max.width.corr.cm - .outer.panel.widths
        .hor.penalties.0hor[.n.ver.panels+1,] = .hor.penalties[.n.ver.panels+1,]
        .rel.cols = which(.hor.penalties.0hor[.n.ver.panels+1,] > 0)
        if (any(.rel.cols > 0)){
          .hor.penalties.0hor[.n.ver.panels+1,.rel.cols] = (.inner.panel.max.widths - colSums(.panel.word.widths[[.n.ver.panels+1]][-1,, drop=FALSE]))[.rel.cols]
        }
      }
      if (.n.ver.panels==0){
        .ver.diff.matrix = .heigths.cm - .panel.word.heights[[.n.ver.panels+1]]
        .ver.diff.matrix.0hor = .ver.diff.matrix
        .ver.penalty.matrix = .subpanels*sign(.ver.diff.matrix)
        .ver.penalty.matrix[.ver.penalty.matrix >= 0] = 0
        .ver.penalty.matrix.0hor = .ver.penalty.matrix
      }else if (.n.ver.panels==1){
        .ver.diff.matrix[1,] = .heigths.cm[1] - nchar(.dataset) * .font.size.range * std_letter_width
        .ver.penalty.matrix = .subpanels*sign(.ver.diff.matrix)
        .ver.penalty.matrix[.ver.penalty.matrix >= 0] = 0
      }else{
        .ver.diff.matrix[.n.ver.panels+1, ] = colSums(do.call('rbind', lapply(nchar(names(panels_list[[.dataset]][[.n.ver.panels - 1]])), function(x) {y=.heigths.cm[.n.ver.panels] - x * .font.size.range * std_letter_width; y[y >= 0] = 0; return(y)} )))
        .ver.diff.matrix.0hor[.n.ver.panels+1, ] = .ver.diff.matrix[.n.ver.panels+1, ]
        .ver.penalty.matrix[.n.ver.panels+1, ] = colSums(do.call('rbind', lapply(nchar(names(panels_list[[.dataset]][[.n.ver.panels - 1]])), function(x) {y=sign(.heigths.cm[.n.ver.panels] - x * .font.size.range * std_letter_width); y[y >= 0] = 0; return(y)} )))
        .ver.penalty.matrix.0hor[.n.ver.panels+1, ] = .ver.penalty.matrix[.n.ver.panels+1, ]
      }
      .ver.penalties[.n.ver.panels+1,] = colSums(.ver.penalty.matrix[.incl.panels,, drop=FALSE])
      .ver.penalties.0hor[.n.ver.panels+1,] = colSums(.ver.penalty.matrix.0hor[.incl.panels,, drop=FALSE])
      if (.n.ver.panels < (.n.levels-1)){
        .panel.word.widths[[.n.ver.panels+2]] = .panel.word.widths[[.n.ver.panels+1]]
        .panel.word.widths[[.n.ver.panels+2]][.n.ver.panels+1,] = .word.vert.space * .word.heights
        .panel.word.heights[[.n.ver.panels+2]] = .panel.word.heights[[.n.ver.panels+1]]
        .panel.word.heights[[.n.ver.panels+2]][.n.ver.panels+1,] = .panel.word.widths[[.n.ver.panels+1]][.n.ver.panels+1,]
      }
    }
    .hor.penalties[.hor.penalties >= 0] = 0
    .hor.penalties.0hor[.hor.penalties.0hor >= 0] = 0
    .penalties = sign(sign(.hor.penalties) + sign(.ver.penalties))
    .penalties.0hor = sign(sign(.hor.penalties.0hor) + sign(.ver.penalties.0hor))
    .penalties.list[[.dataset]] = .penalties
    .penalties.0hor.list[[.dataset]] = .penalties.0hor
  }
  .eligible.font.sizes = apply(do.call('rbind', lapply(.penalties.list, function(m) apply(m, 2, function(c) any(c==0)))), 2, function(c) all(c))
  .common.font.size = ifelse(any(.eligible.font.sizes), .font.size.range[max(which(.eligible.font.sizes))], min(.font.size.range))
  .panel.config = list()
  if (!is.null(horizontal_panels_list)){
    .panel.config = horizontal_panels_list[datasets] #@ horizontal_panels_list
  }else if (any(.eligible.font.sizes)){
    for (.dataset in names(.penalties.list)){
      .config = rep(TRUE, .n.levels.list[[.dataset]])  ## TRUE refers to horizontal or not
      .n.ver.panels = max(which(.penalties.list[[.dataset]][,paste0('f', .common.font.size)]==0))-1
      if (.n.ver.panels > 0){
        .config[1:.n.ver.panels] = FALSE
      }
      .panel.config[[.dataset]] = .config
    }
  }else{
    for (.dataset in names(.penalties.list)){
      .config = rep(TRUE, .n.levels.list[[.dataset]])  ## TRUE refers to horizontal or not
      if (any(.penalties.list[[.dataset]][,paste0('f', .common.font.size)]==0)){
        .n.ver.panels = max(which(.penalties.list[[.dataset]][,paste0('f', .common.font.size)]==0))-1
        if (.n.ver.panels > 0){
          .config[1:.n.ver.panels] = FALSE
        }
      }else{
        .n.levels = .n.levels.list[[.dataset]]
        .n.panel.separators = .n.levels - 1 - ifelse(.incl.first.panel,0,1)
        .incl.panels = ifelse(.incl.first.panel, 1, 2):.n.levels
        
        # setup penalties vectors - names are representative of different horizontal/vertical configurations v0: 0 vertical panels, v1: first panel vertical, v2: two first panels vertical etc.
        # the penalties will be calculated as sum of estimated space outside dedicated plotting area
        .hor.penalties = structure(rep(NA, .n.levels), names=paste0('ver', 1:.n.levels-1))
        .ver.penalties = structure(rep(NA, .n.levels), names=paste0('ver', 1:.n.levels-1))
        
        ### set up matrices with widths and heights of each panel for each possible font size in fully 'horizontal' representation
        .heigths.cm = .heigths.cm.list[[.dataset]]
        .subpanels = .heigths.cm[1]/.heigths.cm
        .panel.word.widths = matrix(ncol=.n.levels, nrow=max(.subpanels))
        .panel.word.widths[,1] = .common.font.size * (max(nchar(datasets)) + .word.extensions) * std_letter_width
        .panel.word.heights = .common.font.size * matrix(1, ncol=.n.levels, nrow=max(.subpanels)) * std_letter_height
        .panel.heights = matrix(rep(.heigths.cm, each=max(.subpanels)), ncol=.n.levels, nrow=max(.subpanels))
        for (.n.level in 1:.n.levels){
          .conseq.entries = max(.subpanels)/.subpanels[.n.level]
          .panel.word.heights[rep(c(FALSE, rep(TRUE, .conseq.entries-1)), .subpanels[.n.level]), .n.level] = NA
          .panel.heights[rep(c(FALSE, rep(TRUE, .conseq.entries-1)), .subpanels[.n.level]), .n.level] = NA
          if (.n.level > 1){
            .panel.word.widths[,.n.level] = .common.font.size * (nchar(rep(names(panels_list[[.dataset]][[.n.level-1]]), each=.conseq.entries)) + .word.extensions) * std_letter_width
          }
        }
        for (.n.ver.panels in 0:(.n.levels-1)){
          if (.panels.max.width.cm==-1){
            .hor.penalty = rep(0, nrow(.panel.word.widths))
          }else{
            .panels.max.width.corr.cm = .panels.max.width.cm - .n.panel.separators * .panel.separator.cm
            .hor.penalty = .panels.max.width.corr.cm - rowSums(.panel.word.widths[,.incl.panels, drop=FALSE])
          }
          .hor.penalties[.n.ver.panels+1] = sum(.hor.penalty[which(.hor.penalty < 0)])
          .ver.penalty = .panel.heights - .panel.word.heights
          .ver.penalty[.ver.penalty > 0] = 0
          if (!.incl.first.panel){
            .ver.penalty[!is.na(.ver.penalty[,1]),1] = 0
          }
          .ver.penalties[.n.ver.panels+1] = sum(colSums(.ver.penalty, na.rm=TRUE))
          .panel.word.heights[ , .n.ver.panels+1] = .panel.word.widths[ , .n.ver.panels+1]
          .panel.word.widths[ , .n.ver.panels+1] = .word.vert.space * .common.font.size * std_letter_height
        }
        .penalties = .hor.penalties + .ver.penalties
        .config[1:(max(which(abs(.penalties)==min(abs(.penalties))))-1)] = FALSE
      }
      .panel.config[[.dataset]] = .config
    }
  }
  ## if one or more 1st panels need to be horizontal
  if (.incl.first.panel){
    if (any(as.logical(sapply(.panel.config, function(x) x[1]))) & is.null(horizontal_panels_list)){
      #@.eligible.font.sizes = apply(do.call('rbind', lapply(.penalties.0hor.list, function(m) apply(m, 2, function(c) any(c==0)))), 2, function(c) all(c))
      .eligible.font.sizes = apply(do.call('rbind', lapply(.penalties.list, function(m) apply(m, 2, function(c) any(c==0)))), 2, function(c) all(c))
      .panel.config = list()
      if (any(.eligible.font.sizes)){
        .common.font.size = .font.size.range[max(which(.eligible.font.sizes))]
        for (.dataset in names(.penalties.list)){ #@ .penalties.0hor.list <-> .penalties.list
          .config = rep(TRUE, .n.levels.list[[.dataset]])  ## TRUE refers to horizontal or not
          .n.ver.panels = max(which(.penalties.list[[.dataset]][,paste0('f', .common.font.size)]==0))-1 #@ .penalties.0hor.list <-> .penalties.list
          if (.n.ver.panels > 0){
            .config[1:.n.ver.panels] = FALSE
          }
          .panel.config[[.dataset]] = .config
        }
      }else{
        .common.font.size = min(.font.size.range)
        for (.dataset in names(.penalties.list)){ #@ .penalties.0hor.list <-> .penalties.list
          .config = rep(TRUE, .n.levels.list[[.dataset]])  ## TRUE refers to horizontal or not
          if (any(.penalties.list[[.dataset]][,paste0('f', .common.font.size)]==0)){ #@ .penalties.0hor.list <-> .penalties.list
            .n.ver.panels = max(which(.penalties.list[[.dataset]][,paste0('f', .common.font.size)]==0))-1 #@ .penalties.0hor.list <-> .penalties.list
            if (.n.ver.panels > 0){
              .config[1:.n.ver.panels] = FALSE
            }
          }else{
            .n.levels = .n.levels.list[[.dataset]]
            .n.panel.separators = .n.levels - 1
            # setup penalties vectors - names are representative of different horizontal/vertical configurations v0: 0 vertical panels, v1: first panel vertical, v2: two first panels vertical etc.
            # the penalties will be calculated as sum of estimated space outside dedicated plotting area
            .hor.penalties = structure(rep(NA, .n.levels), names=paste0('ver', 1:.n.levels-1))
            .ver.penalties = structure(rep(NA, .n.levels), names=paste0('ver', 1:.n.levels-1))
            
            ### set up matrices with widths and heights of each panel for each possible font size in fully 'horizontal' representation
            .heigths.cm = .heigths.cm.list[[.dataset]]
            .subpanels = .heigths.cm[1]/.heigths.cm
            .panel.word.widths = matrix(ncol=.n.levels, nrow=max(.subpanels))
            .panel.word.widths[,1] = .common.font.size * (max(nchar(datasets)) + .word.extensions) * std_letter_width
            .panel.word.heights = .common.font.size * matrix(1, ncol=.n.levels, nrow=max(.subpanels)) * std_letter_height
            .panel.heights = matrix(rep(.heigths.cm, each=max(.subpanels)), ncol=.n.levels, nrow=max(.subpanels))
            for (.n.level in 1:.n.levels){
              .conseq.entries = max(.subpanels)/.subpanels[.n.level]
              .panel.word.heights[rep(c(FALSE, rep(TRUE, .conseq.entries-1)), .subpanels[.n.level]), .n.level] = NA
              .panel.heights[rep(c(FALSE, rep(TRUE, .conseq.entries-1)), .subpanels[.n.level]), .n.level] = NA
              if (.n.level > 1){
                .panel.word.widths[,.n.level] = .common.font.size * nchar(rep(names(panels_list[[.dataset]][[.n.level-1]]), each=.conseq.entries)) * std_letter_width
              }
            }
            for (.n.ver.panels in 0:(.n.levels-1)){
              if (.panels.max.width.cm==-1){
                .hor.penalty = rep(0, nrow(.panel.word.widths))
              }else{
                .panels.max.width.corr.cm = .panels.max.width.cm - .n.panel.separators * .panel.separator.cm
                .hor.penalty = .panels.max.width.corr.cm - rowSums(.panel.word.widths)
              }
              .hor.penalties[.n.ver.panels+1] = sum(.hor.penalty[which(.hor.penalty < 0)])
              .ver.penalty = .panel.heights - .panel.word.heights
              .ver.penalty[.ver.penalty > 0] = 0
              .ver.penalties[.n.ver.panels+1] = sum(colSums(.ver.penalty, na.rm=TRUE))
              .panel.word.heights[ , .n.ver.panels+1] = .panel.word.widths[ , .n.ver.panels+1]
              .panel.word.widths[ , .n.ver.panels+1] = .word.vert.space * .common.font.size * std_letter_height
            }
            .penalties = .hor.penalties + .ver.penalties
            if (all(.penalties < 0)){ #@ ->  .config[1:(max(which(.penalties==min(abs(.penalties))))-1)] = FALSE
              if (.panels.max.width.cm!=-1){
                .lowest.penalty = which(abs(.penalties)==min(abs(.penalties))) - 1
                if (.lowest.penalty > 0){
                  .config[1:.lowest.penalty] = FALSE
                }
              }
            }else{
              .lowest.penalty = max(which(.penalties >=0)) - 1
              if (.lowest.penalty > 0){
                .config[1:.lowest.penalty] = FALSE
              }
            } #@ <-
          }
          .panel.config[[.dataset]] = .config
        }
      }
    }
  }
  ## check if minor adjustments are possible (max +1 in font size per consecutive panel)
  .max.outer.panel.widths = structure(.font.size.range * max(sapply(names(.panel.config), function(.dataset) ifelse(.panel.config[[.dataset]][1], nchar(.dataset), 1) * ifelse(.panel.config[[.dataset]][1], std_letter_width, .word.vert.space*std_letter_height))), names=paste0('f', .font.size.range))
  if (!.incl.first.panel){
    .max.outer.panel.widths = rep(0, length(.max.outer.panel.widths))
  }
  if (any(.eligible.font.sizes) & .common.font.size < max(.font.size.range)){
    .finetune.penalties.list = list()
    for (.dataset in datasets){
      .config = as.logical(.panel.config[[.dataset]])
      .n.levels = .n.levels.list[[.dataset]]
      .n.panel.separators = .n.levels - 1 - ifelse(.incl.first.panel,0,1)
      .incl.panels = ifelse(.incl.first.panel, 1, 2):.n.levels
      # setup penalties vectors - names are representative of different horizontal/vertical configurations v0: 0 vertical panels, v1: first panel vertical, v2: two first panels vertical etc.
      # the penalties will be calculated as sum of estimated space outside dedicated plotting area
      .hor.penalties = structure(rep(NA, .n.levels), names=paste0('+', 1:.n.levels-1))
      .ver.penalties = structure(rep(NA, .n.levels), names=paste0('+', 1:.n.levels-1))
      ### set up matrices with widths and heights of each panel for each possible font size in fully 'horizontal' representation
      .heigths.cm = .heigths.cm.list[[.dataset]]
      .subpanels = sapply(1:.n.levels, function(.n.level) length(runValue(Rle(.subsample.matrices[[.dataset]][,.n.level])))) #@ .heigths.cm[1]/.heigths.cm
      .panel.word.widths = matrix(ncol=.n.levels, nrow=max(.subpanels))
      .panel.word.widths[,1] = .common.font.size * (ifelse(!.config[1], nchar(.dataset), max(nchar(datasets))) + .word.extensions) * std_letter_width
      .panel.word.heights = .common.font.size * matrix(1, ncol=.n.levels, nrow=max(.subpanels)) * std_letter_height
      .panel.heights = matrix(rep(.heigths.cm, each=max(.subpanels)), ncol=.n.levels, nrow=max(.subpanels))
      #cat(paste0(.dataset, ': ', paste(.config, collapse='\t')), '\n') #@
      .hor.panels = which(.config)
      .hor.subpanels = .hor.panels[.hor.panels > 1]
      if (any(.hor.subpanels>0)){
        for (.n.level in .hor.subpanels){
          if (print_one_line_sample_names){
            .conseq.entries = max(.subpanels)/.subpanels[.n.level]
            .panel.word.widths[,.n.level] = .common.font.size * (nchar(rep(names(panels_list[[.dataset]][[.n.level-1]]), each=.conseq.entries)) + .word.extensions) * std_letter_width
          }else{
            if (sum(panels_list[[.dataset]][[.n.level-1]])==nrow(.panel.word.widths)){
              .panel.word.widths[,.n.level] = .common.font.size * (nchar(rep(names(panels_list[[.dataset]][[.n.level-1]]), panels_list[[.dataset]][[.n.level-1]])) + .word.extensions) * std_letter_width
            }else{
              .panel.word.widths[,.n.level] = .common.font.size * (nchar(names(panels_list[[.dataset]][[.n.level-1]])) + .word.extensions) * std_letter_width
            }
          }
        }
      }
      .panel.word.heights[,!.config] = .panel.word.widths[,!.config]
      .panel.word.widths[,!.config] = .word.vert.space * .common.font.size * std_letter_height
      for (.n.level in 1:.n.levels){
        .conseq.entries = max(.subpanels)/.subpanels[.n.level]
        .panel.word.heights[rep(c(FALSE, rep(TRUE, .conseq.entries-1)), .subpanels[.n.level]), .n.level] = NA
        .panel.heights[rep(c(FALSE, rep(TRUE, .conseq.entries-1)), .subpanels[.n.level]), .n.level] = NA
      }
      if (.panels.max.width.cm==-1){
        .hor.penalty = rep(0, nrow(.panel.word.widths))
      }else{
        .panels.max.width.corr.cm = .panels.max.width.cm - .n.panel.separators * .panel.separator.cm
        .hor.penalty = .panels.max.width.corr.cm - rowSums(.panel.word.widths[,.incl.panels, drop=FALSE])
      }
      .hor.penalties[1] = sum(.hor.penalty[which(.hor.penalty < 0)])
      .ver.penalty = .panel.heights - .panel.word.heights
      .ver.penalty[.ver.penalty > 0] = 0
      if (!.incl.first.panel){
        .ver.penalty[!is.na(.ver.penalty[,1]),1] = 0
      }
      .ver.penalties[1] = sum(colSums(.ver.penalty, na.rm=TRUE))
      for (.n.level in 1:(.n.levels-1)){
        if (.n.level==1){
          .panel.word.widths[,.n.level] = (.common.font.size + 1) * ifelse(.config[.n.level], (max(nchar(datasets)) + .word.extensions)*std_letter_width, .word.vert.space*std_letter_height)
          .panel.word.heights[!is.na(.panel.word.heights[,.n.level]),.n.level] = (.common.font.size + 1) * ifelse(!.config[.n.level], max(nchar(datasets))*std_letter_width, std_letter_height)
        }else{
          .conseq.entries = max(.subpanels)/.subpanels[.n.level]
          if (.config[.n.level]){
            #.panel.word.widths[,.n.level] = (.common.font.size + 1) * (nchar(rep(names(panels_list[[.dataset]][[.n.level-1]]), each=.conseq.entries)) + .word.extensions) * std_letter_width
            .panel.word.widths[,.n.level] = (.common.font.size + 1) * (nchar(rep(names(panels_list[[.dataset]][[.n.level-1]]), panels_list[[.dataset]][[.n.level-1]])) + .word.extensions) * std_letter_width
            .panel.word.heights[!is.na(.panel.word.heights[,.n.level]),.n.level] = (.common.font.size + 1) * std_letter_height
          }else{
            .panel.word.widths[,.n.level] = .word.vert.space * (.common.font.size + 1) * std_letter_height
            #@.panel.word.heights[!is.na(.panel.word.heights[,.n.level]),.n.level] = (.common.font.size + 1) * nchar(rep(names(panels_list[[.dataset]][[.n.level-1]]), each=.conseq.entries))[!is.na(.panel.word.heights[,.n.level])] * std_letter_width
            .panel.word.heights[!is.na(.panel.word.heights[,.n.level]),.n.level] = (.common.font.size + 1) * nchar(rep(names(panels_list[[.dataset]][[.n.level-1]]), panels_list[[.dataset]][[.n.level-1]]))[!is.na(.panel.word.heights[,.n.level])] * std_letter_width
          }
        }
        if (.panels.max.width.cm==-1){
          .hor.penalty = rep(0, nrow(.panel.word.widths))
        }else{
          .panels.max.width.corr.cm = .panels.max.width.cm - .n.panel.separators * .panel.separator.cm
          .hor.penalty = .panels.max.width.corr.cm - rowSums(.panel.word.widths[,.incl.panels, drop=FALSE])
          .inner.panel.max.width = as.numeric(.panels.max.width.corr.cm - .max.outer.panel.widths[paste0('f', .common.font.size+1)])
          .hor.penalty[which(.hor.penalty>0)] = .inner.panel.max.width  - rowSums(.panel.word.widths[which(.hor.penalty>0),2:.n.levels, drop=FALSE])
        }
        .hor.penalties[.n.level+1] = sum(.hor.penalty[which(.hor.penalty < 0)])
        .ver.penalty = .panel.heights - .panel.word.heights
        .ver.penalty[.ver.penalty > 0] = 0
        if (!.incl.first.panel){
          .ver.penalty[!is.na(.ver.penalty[,1]),1] = 0
        }
        .ver.penalties[.n.level+1] = sum(colSums(.ver.penalty, na.rm=TRUE))
      }
      .finetune.penalties.list[[.dataset]] = .hor.penalties + .ver.penalties
    }
    .add.font.sizes = sapply(.finetune.penalties.list, function(v) if (any(v==0)){max(which(v==0))-1}else{0} )
  }else{
    .add.font.sizes = structure(rep(0, length(datasets)), names=datasets)
  }
  .n.chars.hor = structure(lapply(datasets, function(.dataset) lapply(.panel.config, as.integer)[[.dataset]] * .n.chars.list[[.dataset]]), names=datasets)
  .n.chars.ver = lapply(.panel.config, function(x) abs(as.integer(x)-1) )
  .panel.font.size.list = list()
  .panel.width.list = list()
  for (.dataset in datasets){
    if (is.null(panel_font_size_list)){
      .font.sizes = rep(.common.font.size, .n.levels.list[[.dataset]])
      if (!any(.add.font.sizes==0)){
        .font.sizes[1:.add.font.sizes[.dataset]] = .common.font.size + 1
      }
    }else{
      .font.sizes = panel_font_size_list[[.dataset]]
    }
    .panel.font.size.list[[.dataset]] = .font.sizes
    .panel.width.list[[.dataset]] = (.n.chars.hor[[.dataset]] + .word.extensions) * .font.sizes * std_letter_width + .word.vert.space * .n.chars.ver[[.dataset]] * .font.sizes * std_letter_height
  }
  .last.panel.widths = unlist(lapply(.panel.width.list, function(x) rev(x)[1]))
  .unique.last.panel.widths = unique(.last.panel.widths)
  .inner.panels.width = max(sapply(.panel.width.list, function(x) sum(x)-x[1]))
  .outer.panel.width = ifelse(.incl.first.panel, max(sapply(.panel.width.list, function(x) x[1])), 0)
  if (length(.unique.last.panel.widths)==1){
    .panel.width.list = lapply(.panel.width.list, function(x) {x[1]=.outer.panel.width; if (length(x)>2){x[2:(length(x)-1)]=(.inner.panels.width-.unique.last.panel.widths)*x[2:(length(x)-1)]/sum(x[2:(length(x)-1)])}else{x[2]=.inner.panels.width}; return(x)})
  }else{
    .panel.width.list = lapply(.panel.width.list, function(x) {x[1]=.outer.panel.width; x[-1]=.inner.panels.width*x[-1]/sum(x[-1]); return(x)})
  }
  .panel.width.list = lapply(.panel.width.list, function(x) {l=length(x); if (l > 1){.n.panel.separators=l-1-ifelse(.incl.first.panel,0,1); if (.n.panel.separators>0){y=rep(NA, l+.n.panel.separators); y[setdiff(1:length(y), seq(ifelse(.incl.first.panel, 2, 3), length(y), by=2))]=x; y[seq(ifelse(.incl.first.panel, 2, 3), length(y), by=2)]=.panel.separator.cm}else{y=x}; return(y)}else{return(x)}})  #@ seq(1, length(y), by=2) <- setdiff(1:length(y), seq(ifelse(.incl.first.panel, 2, 3), length(y), by=2)) AND seq(ifelse(.incl.first.panel, 2, 3), length(y), by=2) <- seq(ifelse(.incl.first.panel, 2, 4), length(y), by=2)
  .total.panel.widths.cm = unlist(lapply(.panel.width.list, sum))
  .diff.total.panel.widths.cm = max(.total.panel.widths.cm) - .total.panel.widths.cm
  .panel.width.list = structure(lapply(1:length(.panel.width.list), function(n) {x=.panel.width.list[[n]]; x[length(x)]=x[length(x)]+.diff.total.panel.widths.cm[n]; return(x)}), names=names(.panel.width.list))
  if (is.null(scale_font_size)){
    .recommended.scale.font.size = recommended_font_sizes['signal_axis']
    .scale.fontsize = min(.common.font.size, .recommended.scale.font.size)
  }else{
    .scale.fontsize = scale_font_size
  }
  if (plot_widths_cm['scale.panel.width.cm']==-1){
    .log10.trans = log10(5*std_letter_width*.scale.fontsize)
    .digits = ifelse(.log10.trans > 0, -1, as.integer(.log10.trans) + sign(.log10.trans))
    .scale.panel.width.cm = as.numeric(10^(.digits)*(as.integer(5*std_letter_width*.scale.fontsize/10^(.digits)) + 1))
  }else{
    .scale.panel.width.cm = as.numeric(plot_widths_cm['scale.panel.width.cm']) # diff(plot_width_parameters[['coords.scale']])
  }
  if (.panels.max.width.cm==-1){
    fixed_panel_width = FALSE
    .min.word.length.cm = min_word_length * .common.font.size * std_letter_width - .scale.panel.width.cm
    .panels.max.width.cm = max(max(sapply(.panel.width.list, sum)), .min.word.length.cm)
  }
  if (plot_widths_cm['full.width.cm']==-1){
    .full.width.cm = as.numeric(.panels.max.width.cm + .scale.panel.width.cm + plot_widths_cm['track.width.cm'] + 2 * plot_widths_cm['margin.width.cm'])
  }else{
    .full.width.cm = as.numeric(plot_widths_cm['full.width.cm'])
  }
  .panel.too.narrow = as.logical(.panels.max.width.cm < max(sapply(.panel.width.list, sum)))
  if (.panel.too.narrow){
    if ((both_strands & strand=='+') | !both_strands){
      if (verbosity > 1){
        cat('WARNING: the left side panel appears to be too narrow - consider increasing Panel Width or supplying a smaller Panel Font Size', '\n')
      }
    }
    .panel.width.list = lapply(.panel.width.list, function(x) as.numeric(.panels.max.width.cm)*x/sum(x))
    .outer.panel.width = .panel.width.list[[1]][1]
  }
  .panel.width = ifelse(fixed_panel_width | .panel.too.narrow, .panels.max.width.cm, max(sapply(.panel.width.list, sum)))/.full.width.cm
  .first.panel.width = .outer.panel.width/.full.width.cm
  
  .non.panels.width = 1 - (.panels.max.width.cm + .scale.panel.width.cm)/.full.width.cm # the relative part of the full_width_cm used for x-axis of seq-tracks
  .left.coord.tracks = as.numeric(1 - .non.panels.width + plot_widths_cm['margin.width.cm']/.full.width.cm)
  .right.coord.tracks = as.numeric(1 - plot_widths_cm['margin.width.cm']/.full.width.cm)
  .tracks.width.cm = .full.width.cm*(.right.coord.tracks-.left.coord.tracks)
  .panels.max.width = .panels.max.width.cm/.full.width.cm
  .scale.panel.width = .scale.panel.width.cm/.full.width.cm
  .plot.width.parameters = list('coords.tracks'=c(.left.coord.tracks, .right.coord.tracks), 'coords.panels'=c(0, .panels.max.width), 'coords.scale'=c(.panels.max.width, .panels.max.width+.scale.panel.width), 'tracks.width.cm'=.tracks.width.cm, 'panels.max.width.cm'=.panels.max.width.cm, 'scale.panel.width.cm'=.scale.panel.width.cm, 'full.width.cm'=.full.width.cm )
  .organised.panels = list('font.size.index'=feature_names_font_size, 'scale.fontsize'=.scale.fontsize, 'panel.width'=.panel.width, 'first.panel.width'=.first.panel.width, 'horizontal.panels.list'=.panel.config, 'panel.font.size.list'=.panel.font.size.list, 'panel.width.list'=.panel.width.list, 'plot.width.parameters'=.plot.width.parameters)
  return(.organised.panels)
}


#' Finalize Panels Dimensions
#'
#' @details
#' Internal function:
#'
FinalizePanelsDimensions = function(panel_info, both_strands){
  if (both_strands){
    if (is.null(panel_info[['-']])){ #@ added this condition
      .panel.info = panel_info
    }else if (!identical(panel_info[['+']], panel_info[['-']])){
      .panel.info = list('+'=panel_info[['+']], '-'=panel_info[['+']])
      .unstranded.samples = setdiff(names(panel_info[['+']][['horizontal.panels.list']]), names(panel_info[['-']][['horizontal.panels.list']]))
      for (.unstranded.sample in .unstranded.samples){
        for (.name in c('horizontal.panels.list', 'panel.font.size.list', 'panel.width.list')){
          .panel.info[['-']][[.name]][[.unstranded.sample]] = NULL
        }
      }
    }else{
      .panel.info = panel_info
    }
  }else{
    .panel.info = panel_info
  }
  return(.panel.info)
}


#' Get Bin Size
#'
#' @details
#' Internal function:
#'
GetBinSize = function(bin_size, plot_width, tracks_width_cm, bins_per_cm, verbosity){
  .messages = list('output'=list(), 'errors'=list())
  .autodetermine = FALSE
  if (!is.null(bin_size)){
    if (bin_size=='auto' | bin_size == 'automatic'){
      .bases.per.cm = plot_width/tracks_width_cm
      .bin.size = as.integer(.bases.per.cm/bins_per_cm)
      if (.bin.size==0){
        .bin.size = 1
      }
      .autodetermine = TRUE
    }else if (class(bin_size)=='numeric'){
      .bin.size = as.integer(bin_size)
      .autodetermine = FALSE
    }
  }else{
    .bin.size = 1
    .autodetermine = TRUE
  }
  .messages[['output']][[length(.messages[['output']])+1]] = paste(ifelse(.autodetermine, 'automatically setting bin size to', 'bin size'), .bin.size)
  PrintOutput(.messages, verbosity)
  return(.bin.size)
}


#' Final Organized Annotation Texts In Plotted Region
#'
#' @details
#' Internal function:
#'
FinalOrganizedAnnotationTextsInPlottedRegion = function(organized_annotation_texts, annotation_names, font_size){
  final.organized.annotation.texts = list('names.gr.list'=list(), 'names.packing.list'=list())
  for (.name in annotation_names){
    final.organized.annotation.texts[['names.gr.list']][[.name]] = organized_annotation_texts[['names.gr.list']][[.name]][[font_size]]
    final.organized.annotation.texts[['names.packing.list']][[.name]] = organized_annotation_texts[['names.packing.list']][[.name]][[font_size]]
  }
  return(final.organized.annotation.texts)
}


#' Basic Plot Parameters
#'
#' @details
#' Internal function:
#'
BasicPlotParameters = function(plotted_strand, plotted_region, feature_names_font_size, plot_height_parameters, plot_width_parameters, full_width_cm, full_height_cm=NULL, track_height_cm=0.3, plot_vertical_parameters, bin_size, bins_per_cm, plotting_segment_order, tracks_listed, unstranded_beds){
  # binning
  .plot.width = IRanges::width(plotted_region[[plotted_strand]])
  .tracks.width.cm = plot_width_parameters[['tracks.width.cm']]
  .bases.per.cm = .plot.width/.tracks.width.cm
  .bins.per.cm = .bases.per.cm/bin_size
  .points.per.bin = points_per_cm/.bins.per.cm
  .bin.width = 2*line_width_scaling_factor*.points.per.bin #@ 2*line_width_scaling_factor ~ 1pt in pdf
  if (.bases.per.cm < bins_per_cm){
    .bin.width = .bin.width * bins_per_cm/.bases.per.cm  # to ensure same density of colors
  }
  .bin.start = S4Vectors::mcols(plotted_region[[plotted_strand]])$bin.start
  .bin.info = c(bin_size, .bin.width)
  if (length(plot_height_parameters[[plotted_strand]][['annot.heights.incl.text']]) > 0){
    .annotations.heights = sapply(names(plot_height_parameters[[plotted_strand]][['annot.heights.incl.text']]), function(.annot.name) plot_height_parameters[[plotted_strand]][['annot.heights.incl.text']][[.annot.name]][feature_names_font_size])
    if (!is.null(unstranded_beds)){
      if (unstranded_beds %in% names(.annotations.heights)){
        .unstranded.beds.heights = .annotations.heights[unstranded_beds]
        .annotations.heights = .annotations.heights[-which(names(.annotations.heights)==unstranded_beds)]
      }
    }
    if (length(.annotations.heights) > 0){
      names(.annotations.heights) = paste0(names(.annotations.heights), plotted_strand)
    }
  }
  if (!is.null(tracks_listed)){ #@ added if/else
    .track.vector = unlist(lapply(plotting_segment_order[[plotted_strand]],
                                  function(.segment.type) if(.segment.type %in% names(plot_vertical_parameters))
                                  { plot_vertical_parameters[.segment.type] }else{ if(.segment.type=='annotations'){ .annotations.heights }else if(.segment.type=='unstranded-beds'){ .unstranded.beds.heights }else{ structure(rep(plot_vertical_parameters['seq'], length(tracks_listed[[plotted_strand]][[.segment.type]])), names=paste0(.segment.type, '_', tracks_listed[[plotted_strand]][[.segment.type]])) }} ))
  }else{
    .track.vector = NULL
  }
  .n.tracks = sum(.track.vector)
  if (is.null(full_height_cm)){
    .track.height.cm = track_height_cm
    .full.height.cm = .n.tracks*track_height_cm
  }else{
    .full.height.cm = plot_height_parameters[[plotted_strand]][['full.height.in']][feature_names_font_size]/cm_to_in
    .track.height.cm = .full.height.cm/.n.tracks
  }
  # height information for plot
  .n.tracks = plot_height_parameters[[plotted_strand]][['n.tracks.annots']][feature_names_font_size]
  .mean.window.height = 1/.n.tracks	## sets the relative height of each track window
  .windows.height = c('top'=1, 1-cumsum(.track.vector*.mean.window.height)); .windows.height[length(.windows.height)] = 0

  # plot dimensions (1x)
  .full.width.in = full_width_cm * cm_to_in					 ## full_width_cm of figure in inches for pdf
  .full.height.in = plot_height_parameters[[plotted_strand]][['full.height.in']][feature_names_font_size]				 ## full_height_cm of figure in inches for pdf
  .plot.dim.in = c(.full.width.in, .full.height.in)
  .annot.heights = lapply(plot_height_parameters[[plotted_strand]][['annot.heights']], function(x) x[feature_names_font_size]) #@
  #@.annot.heights = lapply(plot_height_parameters[[plotted_strand]][['annot.heights']], function(x) x[feature_names_font_size] * as.numeric(plot_vertical_parameters['annot'])) #@
  return(list('track.vector'=.track.vector, 'windows.height'=.windows.height, 'max.annot.lines'=plot_height_parameters[[plotted_strand]][['max.annot.lines']], 'annot.heights'=.annot.heights, 'plot.dim.in'=.plot.dim.in, 'track.height.cm'=.track.height.cm, 'bin.info'=.bin.info))
}


#' Align Basic Plot Parameters
#'
#' @details
#' Internal function:
#'
AlignBasicPlotParameters = function(basic_plot_parameters, both_strands, strands_intermingled, fixed_plot_vertical_parameters){
  .basic.plot.parameters = basic_plot_parameters
  if (both_strands){
    .height.in = .basic.plot.parameters[['+']][['plot.dim.in']][2] + .basic.plot.parameters[['-']][['plot.dim.in']][2]
    .spacer.names = NumberingSpacers(list('+'=names(.basic.plot.parameters[['+']][['track.vector']]), '-'=names(.basic.plot.parameters[['-']][['track.vector']])))
    names(.basic.plot.parameters[['+']][['track.vector']]) = .spacer.names[['+']]
    names(.basic.plot.parameters[['-']][['track.vector']]) = .spacer.names[['-']]
    names(.basic.plot.parameters[['+']][['windows.height']])[1+1:length(.spacer.names[['+']])] = .spacer.names[['+']]
    names(.basic.plot.parameters[['-']][['windows.height']])[1+1:length(.spacer.names[['-']])] = .spacer.names[['-']]
    if (strands_intermingled){
      .track.vector.names = unique(c(names(.basic.plot.parameters[['+']][['track.vector']]), names(.basic.plot.parameters[['-']][['track.vector']])))
      .annot.names = names(.basic.plot.parameters[["-"]][["max.annot.lines"]])
      .unstranded.beds.names = setdiff(names(.basic.plot.parameters[["+"]][["max.annot.lines"]]), .annot.names)
      .spacer.indices = rev(grep('spacer', .track.vector.names, fixed=TRUE))
      .split.spacer.indices = split(.spacer.indices, cumsum(c(1, diff(.spacer.indices) != -1)))
      if (.split.spacer.indices[[1]][1]==length(.track.vector.names)){
        .track.vector.names = .track.vector.names[-.split.spacer.indices[[1]]]
      }
      .track.vector.plus = structure(sapply(.track.vector.names, function(.name) .basic.plot.parameters[['+']][['track.vector']][.name], USE.NAMES=FALSE), names=.track.vector.names)
      .track.vector.plus[is.na(.track.vector.plus)] = 0
      .track.vector.minus = structure(sapply(.track.vector.names, function(.name) .basic.plot.parameters[['-']][['track.vector']][.name], USE.NAMES=FALSE), names=.track.vector.names)
      .track.vector.minus[is.na(.track.vector.minus)] = 0
      .track.vector = .track.vector.plus + .track.vector.minus
      if (!fixed_plot_vertical_parameters[1]){ #@ ->
        .unadjusted.track.vector.sum = sum(.track.vector)
        .diff = sum(c(.basic.plot.parameters[['+']][['track.vector']], .basic.plot.parameters[['-']][['track.vector']])) - .unadjusted.track.vector.sum
        .indices = list()
        .weights = list()
        .indices[['header']] = grep('^header$', names(.track.vector))
        .weights[['header']] = 1
        .indices[['scale']] = grep('^scale$', names(.track.vector))
        .weights[['scale']] = 1
        .indices[['spacers']] = grep('-spacer', names(.track.vector))
        .weights[['spacers']] = .track.vector[.indices[['spacers']]]/min(.track.vector[.indices[['spacers']]])
        .indices[['annots']] = sort(unlist(lapply(c(.annot.names, .unstranded.beds.names), function(a) grep(paste0('^', a), names(.track.vector)))))
        .weights[['annots']] = .track.vector[.indices[['annots']]]/min(.track.vector[.indices[['annots']]])
        .indices[['tracks']] = setdiff(1:length(.track.vector), unlist(.indices[1:4]))
        .weights[['tracks']] = .track.vector[.indices[['tracks']]]/min(.track.vector[.indices[['tracks']]])
        .diff.indices = unlist(.indices[names(fixed_plot_vertical_parameters)[!fixed_plot_vertical_parameters]])
        .diff.weights = unlist(.weights[names(fixed_plot_vertical_parameters)[!fixed_plot_vertical_parameters]])
        .track.vector[.diff.indices] = .track.vector[.diff.indices] + .diff*.diff.weights/sum(.diff.weights)
      } #@ <-
      .windows.height = c('top'=1, 1-cumsum(.track.vector)/sum(.track.vector)); .windows.height[length(.windows.height)] = 0
      .basic.plot.parameters[['+-']] = list('track.vector'=.track.vector, 'windows.height'=.windows.height)
      .annot.names = names(.basic.plot.parameters[['+']][['max.annot.lines']])
      .basic.plot.parameters[['+-']][['max.annot.lines']] = structure(lapply(.annot.names, function(.annot.name) .basic.plot.parameters[['+']][['max.annot.lines']][[.annot.name]] + ifelse(.annot.name %in% .unstranded.beds.names, 0, .basic.plot.parameters[['-']][['max.annot.lines']][[.annot.name]])), names=.annot.names)
      .basic.plot.parameters[['+-']][['annot.heights']] = structure(lapply(.annot.names, function(.annot.name) .basic.plot.parameters[['+']][['annot.heights']][[.annot.name]] + ifelse(.annot.name %in% .unstranded.beds.names, 0, .basic.plot.parameters[['-']][['annot.heights']][[.annot.name]])), names=.annot.names)
      .basic.plot.parameters[['+-']][['plot.dim.in']] = c(.basic.plot.parameters[['+']][['plot.dim.in']][1], cm_to_in * sum(.basic.plot.parameters[['+']][['track.height.cm']] * .track.vector)) #@ added
      .basic.plot.parameters[['+-']][['track.height.cm']] = .basic.plot.parameters[['+']][['track.height.cm']]
      .basic.plot.parameters[['+-']][['bin.info']] = .basic.plot.parameters[['+']][['bin.info']]
    }else{
      .windows.height.adjusted = .basic.plot.parameters[['+']][['windows.height']]*.basic.plot.parameters[['+']][['plot.dim.in']][2] + .basic.plot.parameters[['-']][['plot.dim.in']][2]
      .basic.plot.parameters[['+']][['windows.height']] = .windows.height.adjusted/max(.windows.height.adjusted)
      .basic.plot.parameters[['-']][['windows.height']] = (.basic.plot.parameters[['-']][['windows.height']]*.basic.plot.parameters[['-']][['plot.dim.in']][2])/max(.windows.height.adjusted)
    }
    .basic.plot.parameters[['+']][['plot.dim.in']][2] = .height.in
    .basic.plot.parameters[['-']][['plot.dim.in']][2] = .height.in
  }else{
    .only.strand = names(.basic.plot.parameters)
    .temp.basic.plot.parameters = structure(list(names(.basic.plot.parameters[[.only.strand]][['track.vector']])), names=.only.strand)
    .spacer.names = NumberingSpacers(.temp.basic.plot.parameters)
    names(.basic.plot.parameters[[.only.strand]][['track.vector']]) = .spacer.names[[.only.strand]]
    names(.basic.plot.parameters[[.only.strand]][['windows.height']])[1+1:length(.spacer.names[[.only.strand]])] = .spacer.names[[.only.strand]]
  }
  return(.basic.plot.parameters)
}


#' Prepare Plotting Interface
#'
#' @details
#' Internal function:
#'
PreparePlottingInterface = function(plot_dim, pdf, pdf_name, pdf_dir, header, bin_size, feature, scaling_factor){
  .full.width.in = plot_dim[1]
  .full.height.in = plot_dim[2]
  if (pdf){
    if (is.null(pdf_dir)){
      .pdf.dir = getwd()
    }else{
      .pdf.dir = pdf_dir
    }
    if (is.null(pdf_name)){
      if (!is.null(header)){
        .pdf.name = paste0(header, '_', 'bin', bin_size)
      }else{
        if (!is.null(feature)){
          .pdf.name = paste0(feature, '_', 'bin', bin_size)
        }else{
          .pdf.name = paste0(paste0(unlist(strsplit(as.character(Sys.Date()), split='-', fixed=T)), collapse=''), '_tracksPlotR_', 'bin', bin_size)
        }
      }
    }else{
      .pdf.name = pdf_name
    }
    if (length(grep(pattern=.pdf.name, list.files(.pdf.dir), fixed=T)) >= 1){
      .pdf.name = paste0(.pdf.name, '_v', 1+length(grep(pattern=.pdf.name, list.files(.pdf.dir), fixed=T)))
    }
    .pdf.name = paste0(.pdf.name, '.pdf')
    if (!dir.exists(.pdf.dir)){
      dir.create(.pdf.dir, recursive=TRUE)
    }
    pdf(paste0(.pdf.dir, '/', .pdf.name), width=.full.width.in, height=.full.height.in)
  }else{
    dev.new(width=scaling_factor*.full.width.in, height=scaling_factor*.full.height.in, noRStudioGD=T) ## opens a new plotting window with the indicated dimensions
  }
}

#' hex2hsl
#'
#' @details
#' Internal function:
#'
hex2hsl = function(c){
  ## based on https://bitbucket.org/mattgove/color-theory/src/master/mg_colors.py 
  c_rgb_vector = as.vector(col2rgb(c))/255
  r = c_rgb_vector[1]
  g = c_rgb_vector[2]
  b = c_rgb_vector[3]
  c_max = max(c_rgb_vector)
  c_min = min(c_rgb_vector)
  delta = c_max - c_min
  lightness = (c_max + c_min) / 2
  if (delta == 0){
    hue = 0
    saturation = 0
  }else{
    if (lightness < 0.5){
      saturation = delta / (c_max + c_min)
    }else{
      saturation = delta / (2 - c_max - c_min)
    } 
    delta_r = (((c_max - r)/6) + (delta/2)) / delta
    delta_g = (((c_max - g)/6) + (delta/2)) / delta
    delta_b = (((c_max - b)/6) + (delta/2)) / delta
    if (r == c_max){
      hue = delta_b - delta_g
    }else if (g == c_max){
      hue = (1/3) + delta_r - delta_b
    }else if (b == c_max){
      hue = (2/3) + delta_g - delta_r
    }
  }
  if (hue < 0){
    hue = hue + 1
  }else if (hue > 1){
    hue = hue - 1
  }
  hue = hue*360
  saturation = saturation*100
  lightness = lightness*100
  return(c(hue, saturation, lightness))
}

#' adjust_color_phi
#'
#' @details
#' Internal function:
#'
adjust_color_phi = function(c_hsl, phi=180){
  hue = c_hsl[1] + phi
  if (hue >= 360){
    hue = hue - 360
  }else if (hue < 0){
    hue = hue + 360
  }
  adjusted_c_hsl = c(hue, c_hsl[2:3])
  return(adjusted_c_hsl)
}

#' hsl2hex
#'
#' @details
#' Internal function:
#'
hsl2hex = function(c_hsl){
  ## from https://www.rapidtables.com/convert/color/hsl-to-rgb.html
  h = c_hsl[1]
  s = c_hsl[2]/100
  l = c_hsl[3]/100
  c = (1 - abs(2*l - 1)) * s
  x = c * (1 - abs( (h / 60) %% 2 - 1 ) )
  m = l - c/2
  if (h >= 0 & h < 60){
    rgb_prime = c(c, x, 0)
  }else if (h >= 60 & h < 120){
    rgb_prime = c(x, c, 0)
  }else if (h >= 120 & h < 180){
    rgb_prime = c(0, c, x)
  }else if (h >= 180 & h < 240){
    rgb_prime = c(0, x, c)
  }else if (h >= 240 & h < 300){
    rgb_prime = c(x, 0, c)
  }else if (h >= 300 & h < 360){
    rgb_prime = c(c, 0, x)
  }
  RGB = c(rgb_prime[1]+m, rgb_prime[2]+m, rgb_prime[3]+m)
  return(rgb(RGB[1], RGB[2], RGB[3]))
}

#' Convert Color
#'
#' @details
#' Internal function:
#'
convert_color = function(c, phi=180){
  c_hsl = hex2hsl(c)
  c_hsl_conv = adjust_color_phi(c_hsl, phi)
  c_conv = hsl2hex(c_hsl_conv)
  return(c_conv)
}


#' Plot Header
#'
#' @details
#' Internal function:
#'
PlotHeader = function(windows_height, n_segment, coords_tracks, full_width_cm,  plot_width, header, header_font_sizes, chrom, both_strands, plotted_strand, plot_start, plot_end, font_colors, font_family, first_plot, scaling_factor){
  par(fig=c(0,1,windows_height[n_segment+1],windows_height[n_segment]), mai=scaling_factor*c(0, 0, 0, 0), bg='transparent', col='black', new=ifelse(n_segment==1 & first_plot, F, T))
  plot(0, 0, type='n', xlim=c(-1,1), ylim=c(-1,1), ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i')
  text(x=-1 + 2*coords_tracks[1], y=0.5, labels=header, adj=0, col=font_colors['header'], cex=header_font_sizes['main']*scaling_factor/12, font=4, family=font_family)
  text(x=-1 + 2*coords_tracks[1], y=-0.5, labels=paste(chrom, ' ', ifelse(!both_strands, ifelse(plotted_strand=='+', 'plus', 'minus'), ''), ' ', plot_start, '-', plot_end, sep=''), adj=0, cex=header_font_sizes['sub']*scaling_factor/12, font=1, col=font_colors['subheader'], family=font_family)
  ## scalebar
  .track.width = (coords_tracks[2]-coords_tracks[1])*full_width_cm
  .bp.per.cm = plot_width/.track.width
  .oom.bp.per.cm = nchar(as.integer(.bp.per.cm))-1 ## oom = orders of magnitude
  .nearest.int = round(as.integer(.bp.per.cm)/10^.oom.bp.per.cm, 0)
  .scales = c(1, 5, 10)
  .which.scale = .scales[which(abs(.scales-.nearest.int)==min(abs(.scales-.nearest.int)))[1]] * 10^.oom.bp.per.cm
  .scale.length.cm = .which.scale/.bp.per.cm
  .scale.length = .scale.length.cm/full_width_cm
  segments(y0=0.5, x0=0.9-2*.scale.length, x1=0.9, lwd=scaling_factor*line_width_scaling_factor*1.5, col=font_colors['genomic_scale'], lend=1)
  .scale.label = ifelse(.oom.bp.per.cm >=3, paste0(.which.scale/1E3, ' kb'), paste0(.which.scale, ' bp'))
  text(x=0.9-.scale.length, y=-0.25, labels=.scale.label, adj=0.5, cex=header_font_sizes['scale']*scaling_factor/12, col=font_colors['genomic_scale'], family=font_family)
}


#' Coords Of Genomic Scale
#'
#' @details
#' Internal function:
#'
CoordsOfGenomicScale = function(chrom_coord, font_size, coords_tracks, full_width_cm, plot_width, plot_start, plot_end){
  .coord.width.cm = nchar(chrom_coord) * std_letter_width*font_size
  if (is.na(.coord.width.cm)){
    return(NA)
  }else{
    .plot.start = ifelse(plot_start <  plot_end, plot_start, plot_end)
    .plot.end = ifelse(plot_start <  plot_end, plot_end, plot_start)
    .left.coord = chrom_coord - as.integer((0.5*.coord.width.cm/(diff(coords_tracks)*full_width_cm))*plot_width)
    .right.coord = chrom_coord + as.integer((0.5*.coord.width.cm/(diff(coords_tracks)*full_width_cm))*plot_width)
    if (.left.coord > .plot.start & .right.coord < .plot.end){
      .chrom.coord = as.integer(chrom_coord)
      .adj = 0.5
    }else if (.right.coord < .plot.end){
      .chrom.coord = .plot.start
      .adj = 0
    }else if (.left.coord > .plot.start){
      .chrom.coord = .plot.end
      .adj = 1
    }else{
      .chrom.coord = as.integer(chrom_coord)
      .adj = 0.5
    }

    if (diff(c(plot_start, plot_end)) < 0){
      .adj = abs(.adj-1)
    }

    return(data.frame('chrom.coord'=.chrom.coord, 'adj'=.adj))
  }
}


#' Plot Scale
#'
#' @details
#' Internal function:
#'
PlotScale = function(windows_height, n_segment, coords_tracks, full_width_cm, genomic_scale_on_top, plot_width, plot_start, plot_end, first_plot, font_color, font_family, genomic_scale_font_size, scaling_factor){
  par(fig=c(coords_tracks[1],coords_tracks[2],windows_height[n_segment+1],windows_height[n_segment]), mai=scaling_factor*c(0, 0, 0, 0), new=ifelse(n_segment==1 & first_plot, F, T))
  plot(0, 0, type='n', xlim=c(plot_start, plot_end), ylim=c(-1,1), ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i')
  abline(h=ifelse(genomic_scale_on_top, -1, 1)*0.8, col=font_color['genomic_axis'], lwd=scaling_factor*line_width_scaling_factor*0.5, lend=1)
  .tickmarks = as.integer(axTicks(side=1))
  .full.width.coord.letters.cm = sum(nchar(format(.tickmarks, nsmall=0))) * (std_letter_width*genomic_scale_font_size) #@
  .tracks.width.cm = diff(coords_tracks)*full_width_cm #@
  if (.full.width.coord.letters.cm > .tracks.width.cm){ #@ ->
    if (length(.tickmarks) %% 2 == 1){
      .remaining.indices = seq(1, length(.tickmarks), 2)
    }else{
      .dist.plot.start = min(.tickmarks - plot_start)
      .dist.plot.end = min(plot_end - .tickmarks)
      if (.dist.plot.start < .dist.plot.end){
        .remaining.indices = seq(2, length(.tickmarks), 2)
      }else{
        .remaining.indices = seq(1, length(.tickmarks), 2)
      }
    }
    .tickmarks = .tickmarks[.remaining.indices]
  } #@ <-
  .scale.plot.coords = as.integer(unlist(lapply(.tickmarks, function(.tickmark) CoordsOfGenomicScale(.tickmark, genomic_scale_font_size, coords_tracks, full_width_cm, plot_width, as.integer(plot_start), as.integer(plot_end))['chrom.coord'])))
  .scale.plot.adjs = as.numeric(unlist(lapply(.tickmarks, function(.tickmark) CoordsOfGenomicScale(.tickmark, genomic_scale_font_size, coords_tracks, full_width_cm, plot_width, plot_start, plot_end)['adj'])))
  segments(x0=axTicks(side=1), y0=ifelse(genomic_scale_on_top, -1, 1)*0.8, y1=ifelse(genomic_scale_on_top, -1, 1)*0.4, col=font_color['genomic_axis'], lwd=scaling_factor*line_width_scaling_factor*0.5, lend=1)
  .center.adj = .scale.plot.adjs==0.5
  .left.adj = .scale.plot.adjs==0
  .right.adj = .scale.plot.adjs==1
  if (any(.center.adj)){
    text(x=.scale.plot.coords[.center.adj], y=rep(ifelse(genomic_scale_on_top, 1, -1)*0.4, sum(.center.adj)), labels=format(.tickmarks[.center.adj], nsmall=0), adj=0.5, col=font_color['genomic_axis'], cex=genomic_scale_font_size*scaling_factor/12, family=font_family)
  }
  if (any(.left.adj)){
    text(x=.scale.plot.coords[.left.adj], y=rep(ifelse(genomic_scale_on_top, 1, -1)*0.4, sum(.left.adj)), labels=format(.tickmarks[.left.adj], nsmall=0), adj=0, col=font_color['genomic_axis'], cex=genomic_scale_font_size*scaling_factor/12, family=font_family)
  }
  if (any(.right.adj)){
    text(x=.scale.plot.coords[.right.adj], y=rep(ifelse(genomic_scale_on_top, 1, -1)*0.4, sum(.right.adj)), labels=format(.tickmarks[.right.adj], nsmall=0), adj=1, col=font_color['genomic_axis'], cex=genomic_scale_font_size*scaling_factor/12, family=font_family)
  }
}


#' Plot Spacer
#'
#' @details
#' Internal function:
#'
PlotSpacer = function(windows_height, spacer_segment, right_border, plotted_strand, neg_vals_neg_strand, panel_separators, separators_lwds, separators_colors, scaling_factor){
  .left.border = 1 - right_border
  par(fig=c(.left.border,right_border,windows_height[spacer_segment],windows_height[which(names(windows_height)==spacer_segment)-1]), mai=scaling_factor*c(0, 0, 0, 0), new=TRUE)
  if (grepl('thickline-spacer', spacer_segment, fixed=TRUE)){
    plot(c(-1,1), c(0,0), type='l', xlim=c(-1, 1), ylim=c(-1,1), ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i', col=separators_colors['thickline-spacer'], lwd=scaling_factor*line_width_scaling_factor*separators_lwds['thickline-spacer'], lend=1)
  }else if (grepl('line-spacer', spacer_segment, fixed=TRUE)){
    if (panel_separators['horizontal']){
      plot(c(-1,1), ifelse(plotted_strand=='+-', 0, ifelse(plotted_strand=='-' & neg_vals_neg_strand, 0.8, -0.8))+c(0,0), type='l', xlim=c(-1, 1), ylim=c(-1,1), ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i', col=separators_colors['line-spacer'], lwd=scaling_factor*line_width_scaling_factor*separators_lwds['line-spacer'], lend=1)
    }
  }
}


#' Plot Panels
#'
#' @details
#' Internal function:
#'
PlotPanels = function(plotting_segment, plotted_strand, panel_info, panels_list, panel_separators, separators_lwds, separators_colors, incl_first_panel, print_one_line_sample_names, replicate_names, plot_width_parameters, windows_height, vertical_slots, segment_top, full_width_cm, font_color, font_family, colors, first_plot, letter_heights, scaling_factor){
  ### sample info at the left side of the plots
  .panel.width.list = panel_info[[plotted_strand]]$panel.width.list
  .n.panels = length(.panel.width.list[[plotting_segment]])
  .n.panel.separators = ifelse(incl_first_panel & !print_one_line_sample_names, as.integer(.n.panels/2), as.integer((.n.panels-1)/2))
  .panels = ifelse(incl_first_panel & !print_one_line_sample_names, 1, 2):.n.panels
  .separators = c()
  if (.n.panel.separators>0){
    .separators = seq(ifelse(incl_first_panel | print_one_line_sample_names, 2, 3), .n.panels, by=2) #@ 3 <- 4
    .panels = setdiff(.panels, .separators)
  }
  .subsample.matrix = do.call('cbind', lapply(panels_list[[plotted_strand]][[plotting_segment]], function(v) rep(names(v), v)))
  if (incl_first_panel){
    .subsample.matrix = cbind(rep(plotting_segment, nrow(.subsample.matrix)), .subsample.matrix)
  }
  .n.levels = ncol(.subsample.matrix)
  if (all(grepl('^rep\\d$', .subsample.matrix[,.n.levels]))){
    if (is.null(replicate_names)){
      .subsample.matrix = .subsample.matrix[, 1:(.n.levels-1), drop=FALSE]
      .n.levels = ncol(.subsample.matrix)
    }else{
      .subsample.matrix[,.n.levels] = as.character(sapply(.subsample.matrix[,.n.levels], function(s) paste0(replicate_names, strsplit(s, split='rep', fixed=TRUE)[[1]][2])))
      names(panels_list[[plotted_strand]][[plotting_segment]][[length(panels_list[[plotted_strand]][[plotting_segment]])]]) = .subsample.matrix[,.n.levels]
    }
  }
  if (print_one_line_sample_names){
    .n.levels = 2
    .descriptors = sapply(1:ncol(.subsample.matrix), function(.n.col) length(unique(.subsample.matrix[,.n.col]))!=1)
    if (all(!.descriptors)){
      .descriptors[length(.descriptors)] = TRUE
    }
    if (incl_first_panel){
      .descriptors[1] = TRUE
    }
    .one.line.sample.names = as.character(apply(.subsample.matrix[,.descriptors, drop=FALSE], 1, function(r) paste0(r, collapse='.')))
  }
  .right.border = round(plot_width_parameters$coords.panels[2], 5)
  .left.border = round(.right.border - panel_info[[plotted_strand]]$panel.width, 5)
  .panel.borders = .left.border + c(0, round(cumsum(.panel.width.list[[plotting_segment]])/full_width_cm, 5))
  if (incl_first_panel & !print_one_line_sample_names){
    .n.panels.iv = 1:.n.panels
  }else{
    .n.panels.iv = 2:.n.panels
  }
  for (.n.panel in .n.panels.iv){
    if (.n.panel %in% .panels){
      .panel = which(.panels==.n.panel) + ifelse(incl_first_panel & !print_one_line_sample_names, 0, 1) #@
      #@ if (.n.panel == 2){
      #@   .panel = 2
      #@ }else{
      #@   .panel = which(.panels==.n.panel)
      #@ }
    }
    if (print_one_line_sample_names){
      .sub.panels = structure(rep(1, length(.one.line.sample.names)), names=.one.line.sample.names)
    }else{
      if (.panel==1){
        .sub.panels = structure(nrow(.subsample.matrix), names=plotting_segment)
      }else{
        .sub.panels = panels_list[[plotted_strand]][[plotting_segment]][[.panel-1]]
      }
    }
    .sub.panels.borders = c(0, as.integer(cumsum(.sub.panels)))
    for (.sub.panel in 1:length(.sub.panels)){
      .vertical.slots = segment_top + .sub.panels.borders[.sub.panel]:.sub.panels.borders[.sub.panel+1]
      par(fig=c(.panel.borders[.n.panel], .panel.borders[.n.panel+1], windows_height[max(.vertical.slots)],windows_height[min(.vertical.slots)]), mai=scaling_factor*c(0, 0, 0, 0), new=ifelse(segment_top==1 & first_plot, F, T))
      .yax.transposed = .vertical.slots - mean(.vertical.slots)
      .y.min = min(.yax.transposed - 0.5)
      .y.max = max(.yax.transposed + 0.5)
      if (.n.panel != .n.panels){
        .x.min = -1
        .x.max = 1
      }else{
        .x.min = -.panel.width.list[[plotting_segment]][.n.panels]
        .x.max = 0
      }
      plot(0, 0, type='n', xlim=c(.x.min,.x.max), ylim=c(.y.min, .y.max), ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i')
      if (.n.panel %in% .panels){
        .panel.font.size.list = panel_info[[plotted_strand]]$panel.font.size.list
        .font.size.first.panel = .panel.font.size.list[[plotting_segment]][.panel]
        .trackname.cex = .font.size.first.panel*scaling_factor/12
        .horizontal.panels.list = panel_info[[plotted_strand]]$horizontal.panels.list
        .horizontal = .horizontal.panels.list[[plotting_segment]][.panel]
        if (print_one_line_sample_names){ #@ ->
          .horizontal = TRUE
        } #@ <-
        if (.horizontal){
          text(x=ifelse(.n.panel != .n.panels, 0.8, -0.5*std_letter_width), y=0, labels=names(.sub.panels)[.sub.panel], adj=c(1, 0.5), srt=0, cex=.trackname.cex, family=font_family, font=ifelse(.panel==1,4,1), col=font_color[ifelse(.panel==1,'panel_1st','panel')])
        }else{
          text(x=mean(c(.x.min, .x.max)), y=0, labels=names(.sub.panels)[.sub.panel], adj=c(0.5, 0.5), srt=90, cex=.trackname.cex, family=font_family, font=ifelse(.panel==1,4,1), col=font_color[ifelse(.panel==1,'panel_1st','panel')])
        }
      }else if (.n.panel %in% .separators){
        if (panel_separators['vertical'] & .n.panel != .n.panels){
          segments(x0=0, y0=.y.min+0.1, y1=.y.max-0.1, col=separators_colors['vertical-spacer'], lwd=scaling_factor*line_width_scaling_factor*separators_lwds['vertical-spacer'], lend=1)
        }
      }
    }
  }
}


#' Plot Matrix
#'
#' @details
#' Internal function:
#'
PlotMatrix = function(plotted_region, basic_plot_parameters, plot_start, plot_end, plot_width, bin_size, reverse_strand_direction, sample_subset, dummy_plot, tracks, plotting_segment, bin_stats){
  .strand = unique(as.character(BiocGenerics::strand(plotted_region)))
  .bin.start = S4Vectors::mcols(plotted_region)$bin.start
  .bin.width = basic_plot_parameters$bin.info[2]
  .n.bins.total = as.integer(IRanges::width(plotted_region)/bin_size)
  .n.bins.before = as.integer(abs(.bin.start - plot_start)/bin_size)
  .n.bins.after = as.integer(abs(plot_end - .bin.start)/bin_size)
  .coords = as.numeric(sapply((-.n.bins.before+1):(.n.bins.after), function(.n.bin) mean(.bin.start + ifelse(.strand=='+' | !reverse_strand_direction, 1, -1)*c((.n.bin-1)*bin_size, .n.bin*bin_size-1))))
  if (bin_size!=1){
    .plot.mat = matrix(0, nrow=length(.coords), ncol=length(sample_subset), dimnames=list(as.character(.coords), sample_subset))
  }else{
    .plot.mat = matrix(0, nrow=plot_width, ncol=length(sample_subset), dimnames=list(as.character(plot_start:plot_end), sample_subset))
  }
  if (!dummy_plot){
    .seq.data = tracks[[plotting_segment]]
    for (.seq.sample in sample_subset){
      .score = .seq.data[[.seq.sample]]
      if (bin_size==1){
        if (.strand=='+' | !reverse_strand_direction){
          .plot.mat[,.seq.sample] = .score
        }else{
          .plot.mat[,.seq.sample] = rev(.score)
        }
      }else{
        if (.strand=='+' | !reverse_strand_direction){
          if (bin_stats[plotting_segment]=='mean'){
            .plot.mat[,.seq.sample] = sapply((-.n.bins.before+1):.n.bins.after, function(.n.bin) mean(.score[abs(.bin.start - plot_start) + ((.n.bin-1)*bin_size+1):(.n.bin*bin_size)]))
          }else if (bin_stats[plotting_segment]=='median'){
            .plot.mat[,.seq.sample] = sapply((-.n.bins.before+1):.n.bins.after, function(.n.bin) median(.score[abs(.bin.start - plot_start) + ((.n.bin-1)*bin_size+1):(.n.bin*bin_size)]))
          }else if (bin_stats[plotting_segment]=='max'){
            .plot.mat[,.seq.sample] = sapply((-.n.bins.before+1):.n.bins.after, function(.n.bin) max(.score[abs(.bin.start - plot_start) + ((.n.bin-1)*bin_size+1):(.n.bin*bin_size)]))
          }
        }else{
          if (bin_stats[plotting_segment]=='mean'){
            .plot.mat[,.seq.sample] = sapply((-.n.bins.before+1):.n.bins.after, function(.n.bin) mean(rev(.score)[abs(.bin.start - plot_start) + ((.n.bin-1)*bin_size+1):(.n.bin*bin_size)]))
          }else if (bin_stats[plotting_segment]=='median'){
            .plot.mat[,.seq.sample] = sapply((-.n.bins.before+1):.n.bins.after, function(.n.bin) median(rev(.score)[abs(.bin.start - plot_start) + ((.n.bin-1)*bin_size+1):(.n.bin*bin_size)]))
          }else if (bin_stats[plotting_segment]=='max'){
            .plot.mat[,.seq.sample] = sapply((-.n.bins.before+1):.n.bins.after, function(.n.bin) max(rev(.score)[abs(.bin.start - plot_start) + ((.n.bin-1)*bin_size+1):(.n.bin*bin_size)]))
          }
        }
      }
    }
  }
  return(.plot.mat)
}


#' Plot Data
#'
#' @details
#' Internal function:
#'
PlotData = function(plotting_segment, plot_mat, colors, strands_alpha, intermingled_color, sample_subset, windows_height, coords_tracks, coords_scale, first_plot, neg_vals_neg_strand, plotted_strand, y_par, plot_start, plot_end, bin_width, group_autoscale, incl_track_scales, scientific_scale, scale_font_size, log2transformed, full_width_cm, font_colors, font_family, scaling_factor, letter_widths, enhance_signals, scale_warning=NULL, verbosity){
  if (verbosity > 0) { cat(paste('plotting',  plotting_segment, 'data for samples'), '\n') }
  .base.cols = unlist(colors[[plotting_segment]])
  .enhance = enhance_signals[plotting_segment]
  if (plotted_strand=='+-' & length(plot_mat) == 1){
    .plotted.strand = '+'
  }else{
    .plotted.strand = plotted_strand
  }
  for (.seq.sample in sample_subset){
    .n.segment = which(names(windows_height)==paste(plotting_segment, .seq.sample, sep='_'))-1
    if (verbosity > 0) { cat(paste0('\t', .seq.sample)) }
    .vertical.slots = which(names(windows_height)==paste0(plotting_segment, '_', .seq.sample))
    par(fig=c(coords_tracks[1],coords_tracks[2],windows_height[max(.vertical.slots)],windows_height[min(.vertical.slots)-1]), mai=scaling_factor*c(0, 0, 0, 0), new=ifelse(.n.segment==1 & first_plot, F, T))
    .base.seq.sample = unlist(strsplit(.seq.sample, split='.rep', fixed=T))[1]
    .y.val = structure(c(0, 0), names=c('+', '-'))
    .y.exp = structure(c(0, 0), names=c('+', '-'))
    .n.decimals = structure(c(NA, NA), names=c('+', '-'))
    if (.plotted.strand=='+-' & length(plot_mat)==2){
      .y.max = as.numeric(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['+']][['max']])), y_par[['+']][['max']], y_par[['+']][['max']][.seq.sample]))
      .y.min = -as.numeric(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['-']][['max']])), y_par[['-']][['max']], y_par[['-']][['max']][.seq.sample])) #@ -y_par[['-']]['max']
      .y.limits = structure(c(-1.5, 1.5), names=c('min', 'max')) # structure(c(.y.min, .y.max), names=c('min', 'max'))
      .y.val['+'] = as.numeric(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['+']][['val']])), y_par[['+']][['val']], y_par[['+']][['val']][.seq.sample])) #@ y_par[['+']]['val']
      .y.val['-'] = as.numeric(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['-']][['val']])), y_par[['-']][['val']], y_par[['-']][['val']][.seq.sample])) #@ y_par[['-']]['val']
      .n.decimals['+'] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['+']][['dec']])), y_par[['+']][['dec']], y_par[['+']][['dec']][.seq.sample])) #@ y_par[['+']]['dec']
      .n.decimals['-'] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['-']][['dec']])), y_par[['-']][['dec']], y_par[['-']][['dec']][.seq.sample])) #@ y_par[['-']]['dec']
      .scientific =  as.logical(ifelse(scientific_scale=='all', TRUE, ifelse(scientific_scale=='none', FALSE, (ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['+']][['sci']])), y_par[['+']][['sci']], y_par[['+']][['sci']][.seq.sample]) | ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['-']][['sci']])), y_par[['-']][['sci']], y_par[['-']][['sci']][.seq.sample]))))) #@ (y_par[['+']]['sci'] | y_par[['-']]['sci'])
      .y.exp['+'] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['+']][['exp']])), y_par[['+']][['exp']], y_par[['+']][['exp']][.seq.sample])) #@ y_par[['+']]['exp']
      .y.exp['-'] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['-']][['exp']])), y_par[['-']][['exp']], y_par[['-']][['exp']][.seq.sample])) #@ y_par[['-']]['exp']
      .plot.mat = plot_mat[['+']] / .y.val['+']
    }else{
      .y.max = as.numeric(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[[.plotted.strand]][['max']])), y_par[[.plotted.strand]][['max']], y_par[[.plotted.strand]][['max']][.seq.sample])) #@ y_par[[.plotted.strand]]['max']
      .y.min = 0
      .y.limits = structure(sort(ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*c(0, 1.5)), names=c('min', 'max'))
      .y.val[.plotted.strand] = as.numeric(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[[.plotted.strand]][['val']])), y_par[[.plotted.strand]][['val']], y_par[[.plotted.strand]][['val']][.seq.sample])) #@ y_par[[.plotted.strand]]['val']
      .n.decimals[.plotted.strand] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[[.plotted.strand]][['dec']])), y_par[[.plotted.strand]][['dec']], y_par[[.plotted.strand]][['dec']][.seq.sample])) #@ y_par[[.plotted.strand]]['dec']
      .scientific = as.logical(ifelse(scientific_scale=='all', TRUE, ifelse(scientific_scale=='none', FALSE, ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[[.plotted.strand]][['sci']])), y_par[[.plotted.strand]][['sci']], y_par[[.plotted.strand]][['sci']][.seq.sample])))) #@ y_par[[.plotted.strand]]['sci']
      .y.exp[.plotted.strand] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[[.plotted.strand]][['exp']])), y_par[[.plotted.strand]][['exp']], y_par[[.plotted.strand]][['exp']][.seq.sample])) #@ y_par[[.plotted.strand]]['exp']
      .plot.mat = plot_mat[[.plotted.strand]] / .y.val[.plotted.strand]
    }
    if (any(.plot.mat > 1.5)){ .plot.mat[which(.plot.mat > 1.5)] = 1.5 }
    plot(0, 0, type='n', xlim=c(plot_start, plot_end), ylim=.y.limits, ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i')
    .adj.colors = structure(unlist(lapply(c('+', '-'), function(.strand) {.adj.rgb = (255 - (ifelse(!.enhance, strands_alpha[.strand], 100)/100)*(255 - as.vector(col2rgb(.base.cols[.base.seq.sample]))))/255; .adj.color = rgb(.adj.rgb[1], .adj.rgb[2], .adj.rgb[3]); return(.adj.color)})), names=c('+', '-'))
    if (.plotted.strand=='+-'){
      #abline(h=0, col='darkgray', lwd=scaling_factor*line_width_scaling_factor*0.25, lend=1) #@ 
      lines(as.integer(rownames(.plot.mat)), ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*.plot.mat[,.seq.sample], type='h', lend=1,
            lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col=.adj.colors['+']) #@ ifelse(.enhance, 2, 1) -> ifelse(.enhance, 5, 1)
      .plot.mat = plot_mat[['-']] / .y.val['-']
      if (any(.plot.mat > 1.5)){ .plot.mat[which(.plot.mat > 1.5)] = 1.5 }
      if (intermingled_color!='same'){
        if (intermingled_color=='complementary'){
          .adj.colors['-'] = sapply(.adj.colors['-'], function(c) convert_color(c, phi=180))
        }else if (intermingled_color=='analogous_right'){
          .adj.colors['-'] = sapply(.adj.colors['-'], function(c) convert_color(c, phi=30))
        }else if (intermingled_color=='analogous_left'){
          .adj.colors['-'] = sapply(.adj.colors['-'], function(c) convert_color(c, phi=-30))
        }
      }
      lines(as.integer(rownames(.plot.mat)), -1*.plot.mat[,.seq.sample], type='h', lend=1,
            lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col=.adj.colors['-'])  #@ ifelse(.enhance, 2, 1) -> ifelse(.enhance, 5, 1)
      abline(h=0, col='whitesmoke', lwd=scaling_factor*line_width_scaling_factor*0.5, lend=1) #@ 
    }else{
      lines(as.integer(rownames(.plot.mat)), ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*.plot.mat[,.seq.sample], type='h', lend=1,
            lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col=.adj.colors[.plotted.strand])  #@ ifelse(.enhance, 2, 1) -> ifelse(.enhance, 5, 1)
    }
    if (incl_track_scales){
      .coords.per.mm = 2/(diff(coords_scale)*full_width_cm*10)
      .length.ticks = 0.5*.coords.per.mm
      par(fig=c(coords_scale[1],coords_scale[2],windows_height[max(.vertical.slots)],windows_height[min(.vertical.slots)-1]), mai=scaling_factor*c(0, 0, 0, 0), new=ifelse(.n.segment==1 & first_plot, F, T))
      plot(0, 0, type='n', xlim=c(-1,1), ylim=.y.limits, ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i')
      .y0 = ifelse(.plotted.strand=='+-', -1, 0)
      .y1 = ifelse(.plotted.strand=='+-', 1, ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1))
      segments(x0=1, y0=.y0, y1=.y1, lwd=scaling_factor*line_width_scaling_factor*0.5, col=font_colors['scale'], lend=1)
      segments(x0=1, x1=1-.length.ticks, y0=.y0, lwd=scaling_factor*line_width_scaling_factor*0.5, col=font_colors['scale'], lend=1)
      segments(x0=1, x1=1-.length.ticks, y0=.y1, lwd=scaling_factor*line_width_scaling_factor*0.5, col=font_colors['scale'], lend=1)
      if (.plotted.strand=='+-'){
        segments(x0=1, x1=1-.length.ticks, y0=0, lwd=scaling_factor*line_width_scaling_factor*0.5, col=font_colors['scale'], lend=1)
      }
      if (.scientific){
        .y.plus.label = paste0(format(.y.val['+']/10^.y.exp['+'], nsmall=1), 'e', as.character(.y.exp['+']))
        .y.minus.label = paste0(format(.y.val['-']/10^.y.exp['-'], nsmall=1), 'e', as.character(.y.exp['-']))
      }else{
        .y.plus.label = as.character(.y.val['+'])
        .y.minus.label = as.character(.y.val['-'])
      }
      .y.labels = c('+'=.y.plus.label, '-'=.y.minus.label)
      if (is.null(scale_font_size)){
        .n.char.y.val = as.integer(max(sapply(c('+', '-'), function(.str) nchar(.y.labels[.str])), na.rm=TRUE))
        .scale.panel.width.cm = diff(coords_scale)*full_width_cm
        .y.val.widths.cm = letter_widths*.n.char.y.val
        if (any(.y.val.widths.cm <= .scale.panel.width.cm, na.rm=T)){
          .scale.fontsize = which(.y.val.widths.cm == max(.y.val.widths.cm[.y.val.widths.cm <= .scale.panel.width.cm], na.rm=T))
          .scale.fontsize = ifelse(.scale.fontsize >= 5, 5, 4)
        }else{
          if (is.null(scale_warning)){
            scale_warning = round(.y.val.widths.cm[4]/full_width_cm, 2)
          }else{
            scale_warning = max(scale_warning, round(.y.val.widths.cm[4]/full_width_cm, 2))
          }
          .scale.fontsize = 4
        }
      }else{
        .scale.fontsize = scale_font_size
      }
      if (.plotted.strand=='+-'){
        text(x=1-1.5*.length.ticks, y=.y1, labels=.y.labels['+'], adj=1, cex=.scale.fontsize*scaling_factor/12, family=font_family, col=font_colors['scale'])
        text(x=1-1.5*.length.ticks, y=.y0, labels=.y.labels['-'], adj=1, cex=.scale.fontsize*scaling_factor/12, family=font_family, col=font_colors['scale'])
        if (log2transformed[plotting_segment]){
          text(x=-0.8, y=0, labels=expression('l'[2]), adj=0, cex=.scale.fontsize*scaling_factor/12, family=font_family, col=font_colors['scale'])
        }
        text(x=1-1.5*.length.ticks, y=0, labels=0, adj=1, cex=.scale.fontsize*scaling_factor/12, family=font_family, col=font_colors['scale'])
      }else{
        .y.label = .y.labels[.y.val!=0]
        text(x=1-1.5*.length.ticks, y=.y1, labels=.y.label, adj=1, cex=.scale.fontsize*scaling_factor/12, family=font_family, col=font_colors['scale'])
        if (log2transformed[plotting_segment]){
          text(x=-0.8, y=.y1/2, labels=expression('l'[2]), adj=0, cex=.scale.fontsize*scaling_factor/12, family=font_family, col=font_colors['scale'])
        }
      }
    }
  }
  if (verbosity > 0) { cat('\n') }
}


#' Y Parameters
#'
#' @details
#' Internal function:
#'
YParameters = function(plot_mat, plotting_segment, force_scale_list, group_autoscale){
  if (!is.null(force_scale_list)){
    if (is.null(force_scale_list[[plotting_segment]]) | is.na(force_scale_list[[plotting_segment]])){
      if (as.logical(group_autoscale[plotting_segment])){
        .y.val = structure(max(plot_mat, na.rm=T), names='max')
      }else{
        .y.val = apply(plot_mat, 2, max, na.rm=T) #@ new part
      }
    }else{
      .y.val = structure(as.numeric(ifelse(is.na(force_scale_list[plotting_segment]), max(plot_mat, na.rm=T), force_scale_list[plotting_segment])), names='forced')
    }
  }else{
    if (as.logical(group_autoscale[plotting_segment])){
      .y.val = structure(max(plot_mat, na.rm=T), names='max')
    }else{
      .y.val = apply(plot_mat, 2, max, na.rm=T) #@ new part
    }
  }
  .scientific = structure(rep(FALSE, length(.y.val)), names=names(.y.val))
  .n.decimals = structure(rep(0, length(.y.val)), names=names(.y.val))
  if (any(.y.val==0)){
    .y.val[which(.y.val==0)] = 1
  }
  .exponent = as.integer(log10(.y.val)) + ifelse(.y.val < 1, -1, 0)
  .y.val = 10^.exponent*round(.y.val/10^.exponent, 1)
  if (any(.y.val < 1)){
    .n.decimals[which(.y.val < 1)] = abs(.exponent[which(.y.val < 1)])
  }
  if (any(abs(.exponent) > 2)){
    .scientific[which(abs(.exponent) > 2)] = TRUE
  }
  .y.val = round(.y.val, .n.decimals)
  .final.exponent = as.integer(log10(.y.val)) + ifelse(.y.val < 1, -1, 0)
  .y.max = 1.5*.y.val
  return(list('max'=.y.max, 'val'=.y.val, 'dec'=.n.decimals, 'sci'=.scientific, 'exp'=.final.exponent))
}

YParameters_obs = function(plot_mat, plotting_segment, force_scale, group_autoscale){
  if (is.null(force_scale)){
    .y.val = max(plot_mat, na.rm=T)
  }else{
    .y.val = as.numeric(ifelse(is.na(force_scale[plotting_segment]), max(plot_mat, na.rm=T), force_scale[plotting_segment]))
  }
  .scientific = FALSE
  .n.decimals = 0
  if (.y.val==0){
    .y.val = 1
  }
  .exponent = as.integer(log10(.y.val)) + ifelse(.y.val < 1, -1, 0)
  .y.val = 10^.exponent*round(.y.val/10^.exponent, 1)
  if (abs(.exponent) > 2){
    .scientific = TRUE
    .n.decimals = 1
  }else if (.y.val < 1){
    .n.decimals = abs(.exponent)
  }
  .y.val = round(.y.val, .n.decimals)
  .final.exponent = as.integer(log10(.y.val)) + ifelse(.y.val < 1, -1, 0)
  .y.max = 1.5*.y.val
  return(c('max'=.y.max, 'val'=.y.val, 'dec'=.n.decimals, 'sci'=.scientific, 'exp'=.final.exponent))
}



#' Segment Top
#'
#' @details
#' Internal function:
#'
SegmentTop = function(plotting_segment, plotted_strand, windows_height, annot_info, dummy_plot, tracks, unstranded_beds_names, verbosity){
  if (plotting_segment %in% names(windows_height)){
    .segment.top = which(names(windows_height)==plotting_segment)-1
  }else{
    if (plotting_segment == 'annotations'){
      if (!is.null(annot_info)){
        if (length(unstranded_beds_names) > 0){
          .stranded.beds.names = names(annot_info[[plotted_strand]])[sort(sapply(setdiff(names(annot_info[[plotted_strand]]), unstranded_beds_names), function(n) which(names(annot_info[[plotted_strand]])==n)))]
        }else{
          .stranded.beds.names = names(annot_info[[plotted_strand]])
        }
        .first.annot.name = paste0(.stranded.beds.names[1], plotted_strand)
        .segment.top = which(names(windows_height)==.first.annot.name)-1
      }else{
        if (verbosity > 0){ cat('ERROR: no annotations provided', '\n') }
      }
    }else if (plotting_segment == "unstranded-beds"){
      .segment.top = which(names(windows_height)==unstranded_beds_names[1])-1
    }else{
      if (!dummy_plot){
        .segment.top = which(names(windows_height)==paste(plotting_segment, names(tracks[[plotted_strand]][[plotting_segment]])[1], sep='_'))-1
      }else{
        .segment.top = which(names(windows_height)==paste(plotting_segment, tracks[[plotted_strand]][[plotting_segment]][1], sep='_'))-1
      }
    }
  }
  return(.segment.top)
}


#' Plot Annotation
#'
#' @details
#' Internal function:
#'
PlotAnnotation = function(annot_info, stranded, annot_cols, annotation_packing, plotted_region, plotted_strand, substrand, basic_plot_parameters, plot_start, plot_end, plot_width, bin_size, reverse_strand_direction, incl_feature_names, feature_names_above, incl_feature_brackets, incl_feature_shadings, feature_shading_colors, feature_shading_alpha, plot_width_parameters, plot_vertical_parameters, final_feature_text_org, windows_height, feature_font_size, annotation_panel_font_size, annot_panel_dist=0.4, coords_tracks, font_colors, font_family, first_plot, scaling_factor, verbosity){
  .strand = substrand # ifelse(plotted_strand == '+' | plotted_strand == '+-', '+', '-')
  .bin.start = S4Vectors::mcols(plotted_region)$bin.start
  .bin.width = basic_plot_parameters[[plotted_strand]]$bin.info[2]
  .n.bins.before = as.integer(abs(.bin.start - plot_start)/bin_size)
  .n.bins.after = as.integer(abs(plot_end - .bin.start)/bin_size)
  .coords = sapply((-.n.bins.before+1):.n.bins.after, function(.n.bin) mean(.bin.start + ifelse(.strand=='+' | !reverse_strand_direction, 1, -1)*c((.n.bin-1)*bin_size, .n.bin*bin_size-1)))
  .coords.per.mm = IRanges::width(plotted_region)/(plot_width_parameters$tracks.width.cm*10)
  .length.arrows = 0.363*.coords.per.mm/arrow_constant
  .direction.arrows = ifelse(.strand=='+', -1, +1)*.length.arrows
  .line.width = 4*scaling_factor*line_width_scaling_factor
  .y.scaling = as.numeric(plot_vertical_parameters['annot']/0.8) #@ as.numeric(plot_vertical_parameters['annot'] * basic_plot_parameters[[plotted_strand]][['track.height.cm']])/(0.8*0.3)
  for (.annotation in names(annot_info[[.strand]])){
    if (stranded){
      .stranded.annotation = paste0(.annotation, .strand)
    }else{
      .stranded.annotation = .annotation
    }
    if (verbosity > 0){ cat(paste('plotting', .stranded.annotation, 'annotation'), '\n') }
    .n.segment = which(names(windows_height)==.stranded.annotation)-1
    par(fig=c(coords_tracks[1],coords_tracks[2],windows_height[.n.segment+1],windows_height[.n.segment]), mai=scaling_factor*c(0, 0, 0, 0), new=ifelse(.n.segment==1 & first_plot, F, T))
    if (length(annot_info[[.strand]][[.annotation]]) > 0){
      if (annotation_packing[.annotation] == 'expanded' | annotation_packing[.annotation] == 'squished'){
        .subset.annotation = annot_info[[.strand]][[.annotation]][['expanded']]
        .packing = annot_info[[.strand]][[.annotation]][['packing']]
        .include.introns = TRUE
      }else if (annotation_packing[.annotation] == 'collapsed'){
        .subset.annotation = annot_info[[.strand]][[.annotation]][['collapsed']]
        .packing = structure(lapply(rep(1, length(.subset.annotation)), list), names=names(.subset.annotation))
        .include.introns = FALSE
      }else if (annotation_packing[.annotation] == 'collapsed2'){
        .subset.annotation = annot_info[[.strand]][[.annotation]][['collapsed2']]
        .packing = annot_info[[.strand]][[.annotation]][['packing2']]
        .include.introns = FALSE
      }
      .annot.steps = as.numeric(ifelse(annotation_packing[.annotation] == 'squished', plot_vertical_parameters['annot_squished'], plot_vertical_parameters['annot']))
      .annot.text.steps = as.numeric(plot_vertical_parameters['annot_text_segment'])
      .y.span = basic_plot_parameters[[plotted_strand]][['track.vector']][.stranded.annotation]
      .y.limits = sort(ifelse(feature_names_above[[.strand]][.annotation], 1, -1)*c(0, .y.span))
      plot(0, 0, type='n', xlim=c(plot_start, plot_end), ylim=.y.limits, ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i')
      for (.feat.name in names(.subset.annotation)){
        .feat.annotation = .subset.annotation[[.feat.name]]
        if (!('blocks' %in% names(S4Vectors::mcols(.feat.annotation)))){
          S4Vectors::mcols(.feat.annotation)$blocks = split(IRanges::shift(IRanges::ranges(.feat.annotation), -IRanges::start(.feat.annotation)+1), as.factor(1:length(.feat.annotation)))
        }
        if (length(.feat.annotation) > 0){
          for (.pack.line in 1:length(.packing[[.feat.name]])){
            for (.annot.line in .packing[[.feat.name]][[.pack.line]]){
              .overall.annot.range = IRanges::ranges(.feat.annotation)[.annot.line]
              .y.vals = sort(ifelse(feature_names_above[[.strand]][.annotation], -1, 1)*c(.annot.steps*((-.pack.line+0.5)-0.25), .annot.steps*((-.pack.line+0.5)+0.25)))
              #@cat(.y.vals, '\n') #@cat
              .arrow.scaling = abs(diff(.y.vals))/.y.scaling  #@ replace abs(diff(.y.vals)) with .arrow.scaling
              .y.center = mean(.y.vals)
              if (is.null(annot_cols[[.annotation]])){
                if ('itemRgb' %in% colnames(S4Vectors::mcols(.feat.annotation))){
                  .annot.col = S4Vectors::mcols(.feat.annotation)$itemRgb[.annot.line]
                }else{
                  .annot.col = 'black'
                }
              }else{
                if (class(annot_cols[[.annotation]])=='list'){
                  .annot.col = annot_cols[[.annotation]][[S4Vectors::mcols(.feat.annotation)$score[.annot.line]]]
                }else{
                  .annot.col = annot_cols[[.annotation]]
                }
              }
              .exon.ranges = IRanges::shift(S4Vectors::mcols(.feat.annotation)$blocks[[.annot.line]], IRanges::start(.feat.annotation[.annot.line])-1)
              if (annotation_packing[.annotation] == 'expanded' | annotation_packing[.annotation] == 'squished'){
                if (length(.exon.ranges)==1){
                  if (IRanges::width(.exon.ranges)==IRanges::width(.overall.annot.range)){
                    if (S4Vectors::mcols(.feat.annotation)$intron.from.start[.annot.line] & S4Vectors::mcols(.feat.annotation)$intron.from.end[.annot.line]){
                      .exon.ranges = IRanges::IRanges()
                    }
                  }
                }
              }
              if (length(.exon.ranges) > 0){
                for (.n.exon in 1:length(.exon.ranges)){
                  .exon.range = .exon.ranges[.n.exon]
                  .exon.start = IRanges::start(.exon.range)
                  .exon.end = IRanges::end(.exon.range)
                  if (bin_size > 1){
                    #snap to nearest bin
                    .exon.start = ifelse(.exon.start==IRanges::start(.overall.annot.range), .exon.start, .coords[which(abs(IRanges::start(.exon.range)-.coords)==min(abs(IRanges::start(.exon.range)-.coords)))] - bin_size/2)
                    .exon.end = ifelse(.exon.end==IRanges::end(.overall.annot.range), .exon.end, .coords[which(abs(IRanges::end(.exon.range)-.coords)==min(abs(IRanges::end(.exon.range)-.coords)))] + bin_size/2)
                    while((.exon.end-.exon.start)/plot_width < 0.001){   ### make sure that annotation can be seen
                      .add.x = (as.integer(0.001*plot_width+.exon.start-.exon.end) + 1)/2
                      .exon.start = .exon.start - .add.x
                      .exon.end = .exon.end + .add.x
                    }
                  }
                  rect(xleft=.exon.start, xright=.exon.end, ybottom=.y.vals[1], ytop=.y.vals[2], col=.annot.col, border=NA  )
                  .n.arrows = ifelse(round(diff(c(.exon.start, .exon.end+1))/(8*.length.arrows)) > 0, 1, 0)
                  if (stranded & .n.arrows > 0){
                    .pos.arrow = mean(c(.exon.start, .exon.end))
                    segments(x0=.pos.arrow+(.direction.arrows*.arrow.scaling), y0=.y.vals[2], x1=.pos.arrow-(.direction.arrows*.arrow.scaling), y1=.y.center, col='white', lwd=.line.width/2, lend=2)
                    segments(x0=.pos.arrow+(.direction.arrows*.arrow.scaling), y0=.y.vals[1], x1=.pos.arrow-(.direction.arrows*.arrow.scaling), y1=.y.center, col='white', lwd=.line.width/2, lend=2)
                  }
                }
              }
              if (.include.introns){
                .intron.ranges = .overall.annot.range
                IRanges::start(.intron.ranges) = ifelse(S4Vectors::mcols(.feat.annotation)$intron.from.start[.annot.line], IRanges::start(plotted_region), IRanges::start(.intron.ranges))
                IRanges::end(.intron.ranges) = ifelse(S4Vectors::mcols(.feat.annotation)$intron.from.end[.annot.line], IRanges::end(plotted_region), IRanges::end(.intron.ranges))
                .intron.ranges = IRanges::setdiff(.intron.ranges, .exon.ranges)
                if (length(.intron.ranges) > 0){
                  for (.n.intron in 1:length(.intron.ranges)){
                    .intron.range = .intron.ranges[.n.intron]
                    #snap to nearest bin
                    .intron.start = .coords[which(abs(IRanges::start(.intron.range)-.coords)==min(abs(IRanges::start(.intron.range)-.coords)))] - bin_size/2
                    .intron.end = .coords[which(abs(IRanges::end(.intron.range)-.coords)==min(abs(IRanges::end(.intron.range)-.coords)))] + bin_size/2
                    while((.intron.end-.intron.start)/plot_width < 0.001){   ### make sure that annotation can be seen
                      .add.x = (as.integer(0.001*plot_width+.intron.start-.intron.end) + 1)/2
                      .intron.start = .intron.start - .add.x
                      .intron.end = .intron.end + .add.x
                    }
                    segments(x0=.intron.start, x1=.intron.end, y0=.y.center, lwd=.line.width/2, col=.annot.col, lend=1)
                    .n.arrows = ifelse(round(diff(c(.intron.start, .intron.end+1))/(4*.length.arrows)) > 1, 1, 0)
                    if (stranded & .n.arrows > 0){
                      .pos.arrow = mean(c(.intron.start, .intron.end))
                      segments(x0=.pos.arrow+(.direction.arrows*.arrow.scaling), y0=.y.vals[2], x1=.pos.arrow-(.direction.arrows*.arrow.scaling), y1=.y.center, col=.annot.col, lwd=.line.width/2, lend=1)
                      segments(x0=.pos.arrow+(.direction.arrows*.arrow.scaling), y0=.y.vals[1], x1=.pos.arrow-(.direction.arrows*.arrow.scaling), y1=.y.center, col=.annot.col, lwd=.line.width/2, lend=1)
                    }
                  }
                }
              }else{
                if (S4Vectors::mcols(.feat.annotation[.annot.line])$on.from.start){
                  .pos.arrow = IRanges::start(plotted_region)
                  if (sign(.direction.arrows)==-1){
                    triangle_xs = c(.pos.arrow, .pos.arrow, .pos.arrow-2*(.direction.arrows*.arrow.scaling))
                  }else{
                    triangle_xs = c(.pos.arrow+2*(.direction.arrows*.arrow.scaling), .pos.arrow+2*(.direction.arrows*.arrow.scaling), .pos.arrow)
                  }
                  polygon(x=triangle_xs, y=c(rev(.y.vals), .y.center), col ='yellow', border=NA)
                }
                if (S4Vectors::mcols(.feat.annotation[.annot.line])$on.from.end){
                  .pos.arrow = IRanges::end(plotted_region)
                  if (sign(.direction.arrows)==-1){
                    triangle_xs = c(.pos.arrow+2*(.direction.arrows*.arrow.scaling), .pos.arrow+2*(.direction.arrows*.arrow.scaling), .pos.arrow)
                  }else{
                    triangle_xs = c(.pos.arrow, .pos.arrow, .pos.arrow-2*(.direction.arrows*.arrow.scaling))
                  }
                  polygon(x=triangle_xs, y=c(rev(.y.vals), .y.center), col ='yellow', border=NA)
                }
              }
            }
          }
        }
      }
      if (incl_feature_names[.annotation]){
        .y0.text = basic_plot_parameters[[.strand]][['annot.heights']][[.annotation]]
        .feat.text.gr = final_feature_text_org[[.strand]][['names.gr.list']][[.annotation]]
        .text.packing.list = final_feature_text_org[[.strand]][['names.packing.list']][[.annotation]]
        if (incl_feature_brackets[.annotation]){
          if (annotation_packing[.annotation]=='collapsed'){
            .feat.bracket.gr = unlist(as(annot_info[[.strand]][[.annotation]][['collapsed']], 'GRangesList'))
            .bracket.packing.list = list(names(.feat.bracket.gr))
          }else{
            .feat.bracket.gr = .feat.text.gr
            IRanges::start(.feat.bracket.gr) = S4Vectors::mcols(.feat.text.gr)$feat.start
            IRanges::end(.feat.bracket.gr) = S4Vectors::mcols(.feat.text.gr)$feat.end
            .collapsed2.gr = unlist(annot_info[[.strand]][[.annotation]][['collapsed2']])
            names(.collapsed2.gr) = S4Vectors::mcols(.collapsed2.gr)$name
            .collapsed2.gr = .collapsed2.gr[names(.feat.bracket.gr)]
            .feat.bracket.gr$on.from.start =  S4Vectors::mcols(.collapsed2.gr)$on.from.start
            .feat.bracket.gr$on.from.end =  S4Vectors::mcols(.collapsed2.gr)$on.from.end
            .bracket.packing.list = .text.packing.list
          }
        }
        for (.n.text.line in 1:length(.text.packing.list)){
          for (.feature.name in .text.packing.list[[.n.text.line]]){
            .n.line = .n.text.line
            if (incl_feature_brackets[.annotation]){
              .n.line = 2*(.n.line-1) + 1
              .feature.gr = .feat.bracket.gr[.feature.name]
              .feat.start = IRanges::start(.feature.gr)
              .feat.end = IRanges::end(.feature.gr)
              .y.vals = sort(ifelse(feature_names_above[[.strand]][.annotation], -1, 1)*c(-.y0.text-.annot.text.steps*(.n.line-0.5-0.25), -.y0.text-.annot.text.steps*(.n.line-0.5+0.25)))
              .y.center = mean(.y.vals)
              segments(x0=.feat.start, x1=.feat.end, y0=.y.center, lwd=.line.width/4, col='gray30', lend=1)
              # arrow left-pointing start-of-annotation
              if (!S4Vectors::mcols(.feature.gr)$on.from.start){
                segments(x0=.feat.start, y0=.y.center, x1=.feat.start-ifelse(.strand=='-', -1, 1)*2*(.direction.arrows*.arrow.scaling), y1=.y.vals[2], col='gray30', lwd=.line.width/4, lend=1)
                segments(x0=.feat.start, y0=.y.center, x1=.feat.start-ifelse(.strand=='-', -1, 1)*2*(.direction.arrows*.arrow.scaling), y1=.y.vals[1], col='gray30', lwd=.line.width/4, lend=1)
              }
              # arrow right-pointing start-of-annotation
              if (!S4Vectors::mcols(.feature.gr)$on.from.end){
                segments(x0=.feat.end, y0=.y.center, x1=.feat.end+ifelse(.strand=='-', -1, 1)*2*(.direction.arrows*.arrow.scaling), y1=.y.vals[2], col='gray30', lwd=.line.width/4, lend=1)
                segments(x0=.feat.end, y0=.y.center, x1=.feat.end+ifelse(.strand=='-', -1, 1)*2*(.direction.arrows*.arrow.scaling), y1=.y.vals[1], col='gray30', lwd=.line.width/4, lend=1)
              }
              .n.line = .n.line + 1
            }
            .y.vals = sort(ifelse(feature_names_above[[.strand]][.annotation], -1, 1)*c(-.y0.text-.annot.text.steps*(.n.line-0.5-0.25), -.y0.text-.annot.text.steps*(.n.line-0.5+0.25)))
            .y.center = mean(.y.vals)
            text(x=S4Vectors::mcols(.feat.text.gr[.feature.name])$coord, y=.y.center, labels=.feature.name, adj=abs(ifelse(((!reverse_strand_direction & .strand=='-') | .strand=='+') , 0, 1) - S4Vectors::mcols(.feat.text.gr[.feature.name])$adj), col=font_colors['features'], cex=scaling_factor*feature_font_size/12, family=font_family)
          }
        }
      }
      if (incl_feature_shadings[.annotation]){
        .feature.shading.colors = c(adjustcolor(feature_shading_colors[1], alpha.f=feature_shading_alpha), adjustcolor(feature_shading_colors[2], alpha.f=feature_shading_alpha))
        if (annotation_packing[.annotation]=='collapsed'){
          .feat.shading.gr = annot_info[[.strand]][[.annotation]][['collapsed']]
        }else{
          .feat.shading.gr = annot_info[[.strand]][[.annotation]][['collapsed2']]
        }
        .n.feat = 0
        for (.feature.name in names(.feat.shading.gr)){
          for (.n.sub.feature in 1:length(.feat.shading.gr[[.feature.name]])){
            .n.feat = .n.feat+1
            .feature.gr = .feat.shading.gr[[.feature.name]][.n.sub.feature]
            .feat.start = IRanges::start(.feature.gr)
            .feat.end = IRanges::end(.feature.gr)
            .y.limits = sort(ifelse(feature_names_above[[.strand]][.annotation], 1, -1)*c(.annot.steps*0.25, .y.span))
            rect(xleft=.feat.start, xright=.feat.end, ybottom=.y.limits[1], ytop=.y.limits[2], col=.feature.shading.colors[.n.feat%%2+1], border=NA  )
          }
        }
      }
    }
    .x.min = -coords_tracks[1]*plot_width_parameters[['full.width.cm']]
    par(fig=c(0,coords_tracks[1],windows_height[.n.segment+1],windows_height[.n.segment]), mai=scaling_factor*c(0, 0, 0, 0), new=ifelse(.n.segment==1 & first_plot, F, T))
    plot(0, 0, type='n', xlim=c(.x.min,0), ylim=c(-1, 1), ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i')
    text(x=-annot_panel_dist - 1.2*std_letter_width*annotation_panel_font_size, y=0, labels=.annotation, adj=1, col=font_colors['annotation'], cex=scaling_factor*annotation_panel_font_size/12, family=font_family, font=2)
    if (stranded){
      text(x=-annot_panel_dist, y=0, labels=.strand, adj=0.5, col=font_colors['annotation'], cex=scaling_factor*annotation_panel_font_size/12, family=font_family, font=2)
    }
  }
}


#' Plot Segment
#'
#' @details
#' Internal function:
#'
PlotSegment = function(feature, plotted_region, plotted_strand, both_strands, plotting_segment, basic_plot_parameters, neg_vals_neg_strand, plot_width_parameters, plot_vertical_parameters, annot_info, panel_info, panels_list, panel_separators, separators_lwds, separators_colors, incl_first_panel, print_one_line_sample_names, replicate_names, header, header_font_sizes, scaling_factor, full_width_cm, genomic_scale_on_top, genomic_scale_font_size, reverse_strand_direction, bin_stats, dummy_plot, tracks, strands_alpha, intermingled_color,  unstranded_beds, annotation_packing, annotation_panel_font_size, incl_feature_names, feature_font_size, feature_names_above, final_feature_text_org, incl_feature_brackets, incl_feature_shadings, feature_shading_colors, feature_shading_alpha, annot_cols, group_autoscale, incl_track_scales, scientific_scale, scale_font_size, force_scale_list, log2transformed, colors, alternating_background, bgr_colors, bgr_alpha, font_colors, letter_widths, letter_heights, enhance_signals, first_plot, verbosity){
  .strand = ifelse(plotted_strand == '+' | plotted_strand == '+-', '+', '-')
  .plotted.region = plotted_region[[.strand]]
  .chrom = as.character(GenomeInfoDb::seqnames(.plotted.region))
  .plot.width = IRanges::width(.plotted.region)
  .plot.start = ifelse(.strand=='+' | !reverse_strand_direction, IRanges::start(.plotted.region), IRanges::end(.plotted.region))
  .plot.end = ifelse(.strand=='+' | !reverse_strand_direction, IRanges::end(.plotted.region), IRanges::start(.plotted.region))
  .font.family = 'sans'

  .windows.height=basic_plot_parameters[[plotted_strand]]$windows.height
  .coords.tracks=plot_width_parameters$coords.tracks
  .coords.scale=plot_width_parameters$coords.scale
  .bin.size=as.integer(basic_plot_parameters[[plotted_strand]]$bin.info[1])
  .panel.width=diff(plot_width_parameters$coords.panels)

  .segment.top = SegmentTop(plotting_segment, .strand, .windows.height, annot_info, dummy_plot, tracks, unstranded_beds, verbosity)
  if (.segment.top==1 & first_plot & verbosity > 0){
    cat(paste0('plotting ', ifelse(!is.null(feature), paste0(feature, '@'), ''), .chrom, ':', .plot.start, '-', .plot.end), '\n')
  }

  if (grepl('spacer', plotting_segment, fixed=TRUE)){
    PlotSpacer(.windows.height, plotting_segment, .coords.tracks[2], plotted_strand, neg_vals_neg_strand, panel_separators, separators_lwds, separators_colors, scaling_factor)
  }else if (plotting_segment=='header'){
    PlotHeader(.windows.height, .segment.top, .coords.tracks, full_width_cm, .plot.width, header, header_font_sizes, .chrom, both_strands, .strand, .plot.start, .plot.end, font_colors, .font.family, first_plot, scaling_factor)
  }else if (plotting_segment=='scale'){
    PlotScale(.windows.height, .segment.top, .coords.tracks, full_width_cm, genomic_scale_on_top, .plot.width, .plot.start, .plot.end, first_plot, font_colors, .font.family, genomic_scale_font_size, scaling_factor)
  }else if (plotting_segment=='annotations'){
    if (length(unstranded_beds) > 0){
      .unstranded.annot.info = lapply(annot_info, function(l) l[unstranded_beds] )
      .all.beds.names = names(basic_plot_parameters[[.strand]][["max.annot.lines"]])
      if (any(.all.beds.names %in% unstranded_beds)){
        .stranded.beds.names = .all.beds.names[-which(.all.beds.names %in% unstranded_beds)]
      }else{
        .stranded.beds.names = .all.beds.names
      }
      #cat(paste(.strand, paste(.stranded.beds.names, collapse=', ')), '\n')
      .stranded.annot.info = lapply(annot_info, function(l) l[.stranded.beds.names] )
    }else{
      .stranded.annot.info = annot_info
    }
    .stranded = TRUE
    PlotAnnotation(.stranded.annot.info, .stranded, annot_cols, annotation_packing, .plotted.region, plotted_strand, .strand, basic_plot_parameters, .plot.start, .plot.end, .plot.width, .bin.size, reverse_strand_direction, incl_feature_names, feature_names_above, incl_feature_brackets, incl_feature_shadings, feature_shading_colors, feature_shading_alpha, plot_width_parameters, plot_vertical_parameters, final_feature_text_org, .windows.height, feature_font_size, annotation_panel_font_size, annot_panel_dist, .coords.tracks, font_colors, .font.family, first_plot, scaling_factor, verbosity)
    if (plotted_strand=='+-'){
      .annots.indices = unlist(lapply(names(.stranded.annot.info[[.strand]]), grep, names(basic_plot_parameters[['+-']][['windows.height']]), fixed=TRUE))
      .space.index = setdiff(seq(min(.annots.indices), max(.annots.indices), 1), .annots.indices)
      if (length(.space.index) > 0){
        spacer_segment = names(basic_plot_parameters[['+-']][['windows.height']])[rev(.space.index)[1]] #@ rev(.space.index)[1] <- .space.index
        if (grepl('spacer', spacer_segment, fixed=TRUE)){
          PlotSpacer(.windows.height, spacer_segment, .coords.tracks[2], plotted_strand, neg_vals_neg_strand, panel_separators, separators_lwds, separators_colors, scaling_factor)
        }
      }
      PlotAnnotation(.stranded.annot.info, .stranded, annot_cols, annotation_packing, .plotted.region, plotted_strand, '-', basic_plot_parameters, .plot.start, .plot.end, .plot.width, .bin.size, reverse_strand_direction, incl_feature_names, feature_names_above, incl_feature_brackets, incl_feature_shadings, feature_shading_colors, feature_shading_alpha, plot_width_parameters, plot_vertical_parameters, final_feature_text_org, .windows.height, feature_font_size, annotation_panel_font_size, annot_panel_dist, .coords.tracks, font_colors, .font.family, first_plot, scaling_factor, verbosity)
    }
  }else if (plotting_segment=="unstranded-beds"){
    if (length(unstranded_beds) > 0){
      .unstranded.annot.info = lapply(annot_info, function(l) l[unstranded_beds] )
      .all.beds.names = names(basic_plot_parameters[[.strand]][["max.annot.lines"]])
      .stranded.beds.names = .all.beds.names[-which(.all.beds.names %in% unstranded_beds)]
      .stranded.annot.info = lapply(annot_info, function(l) l[.stranded.beds.names] )
    }else{
      .stranded.annot.info = annot_info
    }
    .stranded = FALSE
    PlotAnnotation(.unstranded.annot.info, .stranded, annot_cols, annotation_packing, .plotted.region, plotted_strand, .strand, basic_plot_parameters, .plot.start, .plot.end, .plot.width, .bin.size, reverse_strand_direction, incl_feature_names, feature_names_above, incl_feature_brackets, incl_feature_shadings, feature_shading_colors, feature_shading_alpha, plot_width_parameters, plot_vertical_parameters, final_feature_text_org, .windows.height, feature_font_size, annotation_panel_font_size, annot_panel_dist, .coords.tracks, font_colors, .font.family, first_plot, scaling_factor, verbosity)
  }else{
    if (!dummy_plot){
      .sample.subset = names(tracks[[.strand]][[plotting_segment]])
    }else{
      .sample.subset = tracks[[.strand]][[plotting_segment]]
    }
    .n.track = which(names(tracks[[.strand]])==plotting_segment)
    .vertical.slots = grep(paste0("^", plotting_segment), names(.windows.height))
    if (length(.vertical.slots)==0){ #@ ->
      .vertical.slots = grep(paste0(plotting_segment), names(.windows.height), fixed=TRUE)
    } #@ <-
    .first.plot = first_plot
    if (alternating_background){
      .bgr.colors = c(adjustcolor(bgr_colors[1], alpha.f=bgr_alpha), adjustcolor(bgr_colors[2], alpha.f=bgr_alpha))
      par(fig=c(.coords.tracks[1],.coords.tracks[2],.windows.height[max(.vertical.slots)],.windows.height[.segment.top]), mai=scaling_factor*c(0, 0, 0, 0), new=ifelse(.segment.top==1 & .first.plot, F, T))
      .first.plot = FALSE
      plot(0, 0, type='n', xlim=c(-1,1), ylim=c(-1, 1), ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i')
      rect(xleft=-1, xright=1, ybottom=-1, ytop=1, col=.bgr.colors[(.n.track %% 2)+1], border=NA)
    }
    PlotPanels(plotting_segment, .strand, panel_info, panels_list, panel_separators, separators_lwds, separators_colors, incl_first_panel, print_one_line_sample_names, replicate_names, plot_width_parameters, .windows.height, .vertical.slots, .segment.top, full_width_cm, font_colors, .font.family, colors, .first.plot, letter_heights, scaling_factor)
    .first.plot = FALSE
    .plot.mat = list()
    .plot.mat[[.strand]] = PlotMatrix(.plotted.region, basic_plot_parameters[[plotted_strand]], .plot.start, .plot.end, .plot.width, .bin.size, reverse_strand_direction, .sample.subset, dummy_plot, tracks[[.strand]], plotting_segment, bin_stats)
    if (plotted_strand=='+-' & plotting_segment %in% names(tracks[['-']])){
      .plot.mat[['-']] = PlotMatrix(plotted_region[['-']], basic_plot_parameters[[plotted_strand]], .plot.start, .plot.end, .plot.width, .bin.size, reverse_strand_direction, .sample.subset, dummy_plot, tracks[['-']], plotting_segment, bin_stats)
    }
    .y.par = structure(lapply(names(.plot.mat), function(.strand) YParameters(.plot.mat[[.strand]], plotting_segment, force_scale_list[[.strand]], group_autoscale)), names=names(.plot.mat))
    .bin.width = basic_plot_parameters[[plotted_strand]]$bin.info[2]
    PlotData(plotting_segment, .plot.mat, colors, strands_alpha, intermingled_color, .sample.subset, .windows.height, .coords.tracks, .coords.scale, .first.plot, neg_vals_neg_strand, plotted_strand, .y.par, .plot.start, .plot.end, .bin.width, group_autoscale, incl_track_scales, scientific_scale, scale_font_size, log2transformed, full_width_cm, font_colors, .font.family, scaling_factor, letter_widths, enhance_signals, scale_warning=NULL, verbosity)
  }
}
