# Default values and parameter templates

###### # INTERNAL FUNCTIONS # ######

#' Constants and Defaults
#'
#' @description Internal function
#' Container for Constants and Defaults
#'
#' @keywords internal
#'
#' @author SLA
#'
#' @return a named vector with constants and defaults (package-wide available arguments)
#'
#' @examples
#' constants_defaults = ConstantsDefaults()
#' line_width_scaling_factor = constants_defaults['line_width_scaling_factor']
#' points_per_cm = constants_defaults['points_per_cm']
#' arrow_constant = constants_defaults['arrow_constant']
#' cm_to_in = constants_defaults['cm_to_in']
#' std_letter_width = constants_defaults['std_letter_width']
#' std_letter_height = constants_defaults['std_letter_height']
#' min_font_size = constants_defaults['min_font_size']
#' annot_panel_dist = constants_defaults['annot_panel_dist']
#' 
ConstantsDefaults = function(){
  c(line_width_scaling_factor = 0.25/0.38,   # scaling factor to use for the line width (if multiplied with 1 it will give 0.5-pt line width in the pdf with default dimensions)
    points_per_cm = 28.3465,
    arrow_constant = 0.4,                    # used for scaling directional arrows in annotation with feature heights
    cm_to_in = 0.393701,					           # 1 cm = 0.393701 inches
    std_letter_width = 0.023,                # gives the width of the standard letter if multiplied by font size
    std_letter_height = 0.045,               # gives the height of the standard letter if multiplied by font size
    min_font_size = 4,                       # the minimum font size for all formats
    annot_panel_dist = 0.4 )                 # right margin for annotation title panel
}


#' Plot Vertical Parameters
#'
#' @description Internal function
#'
#' @keywords internal
#' 
#' @author SLA 
#'         
#' @return a named vector with default vertical proportions of various plotting segment types relative to 'seq' tracks
#'
#' @examples
#' plot_vertical_parameters = PlotVerticalParameters()
#' 
PlotVerticalParameters = function(){
  c('header'=2.2,             ## vertical units used for header segment
    'seq'=1,                  ## vertical units used for each sequencing track
    'scale'=0.8,              ## vertical units used for the genomic scale
    'line-spacer'=0.2,        ## vertical units used for spacers
    'empty-spacer'=0.2,       ## vertical units used for empty spacers (w/o possibility of horizontal lines)
    'thickline-spacer'=0.4,   ## vertical units used for annotation separator
    'annot'=0.8,              ## per annotation line...if annotation_packing is not squished
    'annot_squished'=0.5,     ## per annotation line...if annotation_packing is squished
    'annot_text_segment'=0.8) ## 
}


#' Default Plot Options
#'
#' @description Internal function
#'
#' @keywords internal
#' 
#' @author MS/SLA
#'
#' @return a named list of default plot options
#'
#' @examples
#' default_plot_options = DefaultPlotOptions()
#' 
DefaultPlotOptions = function(){
  list(plotting_segment_order=NULL,                               #(v)
       preloaded_tracks=NULL,                                     ## not in app
       output_tracks=FALSE,                                       ## not in app
       output_parameters=FALSE,                                   ## not in app
       input_parameters=NULL,                                     ## not in app
       both_strands=TRUE,                                         #(v)
       strands_intermingled=TRUE,                                 #(v)
       neg_vals_neg_strand=FALSE,                                 #(v)
       reverse_strand_direction=FALSE,                            #(v)
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


#' Default Annotation Options
#'
#' @description Internal function
#'
#' @keywords internal
#' 
#' @author MS/SLA
#'
#' @return a named list of default annotation-display options.
#'
#' @examples
#' default_annotation_options = DefaultAnnotationOptions()
#' 
DefaultAnnotationOptions = function() {
  list('incl_feature_names'=TRUE,                                 #(v)
       'feature_names_above'=FALSE,                               #(v)
       'incl_feature_brackets'=TRUE,                              #(v)
       'incl_feature_shadings'=TRUE,                              #(v)
       'annotation_packing'='collapsed2',                         #(v)
       'annot_cols'='black'                                       #(v)
  )
}


#' Default Parameters 
#'
#' @description Internal function
#'
#' @keywords internal
#' 
#' @author MS/SLA
#' 
#' @return a named list of default Dataset-specific options
#'
#' @examples
#' default_parameters = DefaultParameters()
#' 
DefaultParameters = function() {
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


