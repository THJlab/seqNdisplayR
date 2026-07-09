# Main entry point and session management

###### # PACKAGE FUNCTIONS # ######


#' seq'N'display
#'
#' @description Main plotting function
#'
#' @author SLA
#'
#' @param datasets A nested list of character vectors containing dataset names, subgroup names and corresponding sample names. This nested list contains the 'tree' structure of the data to be plotted. See example below.
#' @param colors Nested list of colors corresponding to the bigwigs nested list. Pick colors for the individual sample tracks (replicates for a given sample will get the same color). See example below.
#' @param bigwig_dirs Named character vector. For each dataset in datasets a directory where the bigwig files associated with each dataset is located. See example below.
#' @param bigwigs A nested list of bigwig files under each dataset. See example below.
#' @param parameters A list of parameters for customizing data transformation and processing. See example below.
#'
#' This argument allows you to customize various aspects of data processing and transformation
#' for different 'datasets' data. The 'parameters' list should have named elements where each
#' name corresponds to a 'dataset' and contains sub-elements for customization.
#'
#' The possible sub-elements within the 'parameters' list include:
#'
#' - \code{whichSamples}: A vector or list specifying which samples to include in the analysis.
#'
#' - \code{whichReps}: A vector or list specifying which replicates to include in the analysis.
#'
#' - \code{log2transform}: A logical value (TRUE/FALSE) indicating whether to perform log2 transformation on the data.
#'
#' - \code{pseudoCount}: A numeric value specifying the pseudo-count to be added to the data before transformation.
#'
#' - \code{batchCorrect}: A logical value (TRUE/FALSE) indicating whether batch correction should be applied.
#'
#' - \code{batch}: A vector or list specifying batch information for samples.
#'
#' - \code{negative_valued_bw}: A logical value (TRUE/FALSE) indicating whether negative-valued bigwig data should be considered as positive data.
#'
#' - \code{calcMean}: A logical value (TRUE/FALSE) indicating whether the mean of replicated samples should be calculated.
#'
#' - \code{negValsSet0}: A logical value (TRUE/FALSE) indicating whether negative values should be set to 0.
#'
#' The 'parameters' argument allows you to fine-tune the data processing for a specific 'dataset'
#' by providing custom settings for each 'dataset'. If 'parameters' is NULL (default),
#' the function uses default settings.
#' @param plotting_segment_order Specify the order of displayed vertical segments in the plot using the following segment-labels: 'header', 'scale', names of individual datasets, 'annotations', unstranded-beds, 'thickline-spacer', 'line-spacer' and 'empty-spacer'\. There should be perfect correspondance between datasets to display and the listed datasets. Default NULL which leads to a pre-determined order.
#' @param preloaded_tracks Void.
#' @param output_tracks Void.
#' @param output_parameters Void.
#' @param input_parameters Void.
#' @param both_strands A logical value (TRUE/FALSE) . Specify whether data should be displayed for both strands (when available). Default TRUE.
#' @param strands_intermingled A logical value (TRUE/FALSE) . Specify if both strand should be displayed as intermingled. Default TRUE. Ignored if both_strands=FALSE.
#' @param neg_vals_neg_strand A logical value (TRUE/FALSE) . Specify if reverse strand data should be represented as negative values. Automatically the case with 'strands_intermingled' display.
#' @param reverse_strand_direction A logical value (TRUE/FALSE) . If data is only plotted for the strand of interest (i.e. both_strands=FALSE), reverse strand loci can be horizontally mirrored (5'-left to 3'-right). This option substantially extends the plotting time. Default=FALSE.
#' @param alternating_background A logical value (TRUE/FALSE) . Should the background of the tracks alternate between datasets for easier discrimination? Colors to use can be specified by setting bgr_colors and bgr_alpha.
#' @param bgr_colors Provide a vector of two colors. Accepts colors by name and hex code, e.g. c('green','yellow') or c('#FF0000','#FF0042') or c('green','#FF0000').
#' @param bgr_alpha Opacity of background shading.
#' @param strands_alpha Percent opacity of the forward,reverse strand (100=full;0=blank). Default is c(100,100). If one number is provided, this will be used for both forward and reverse strand. Ignored if signals are enhanced (specified for individual datasets in parameters).
#' @param intermingled_color When the strands_Intermingled=TRUE, it may be beneficial to display data from the two strands with different colors. This can be done by changing the opacity and/or by choosing one of the options complementary, analogous_right and analogous_left.
#' @param feature The feature/locus name has to be present in one of the supplied annotations and match case. When entering feature/locus name and coordinates simultaneously, only the locus name will be considered.
#' @param locus The locus coordinates (e.g. c('chr1', '+', 87325400, 87351991). When entering feature/locus name and coordinates simultaneously, only the locus name will be considered.
#' @param extra_space Extra space up- and downstream of and relative to the selected genomic feature (0.1 = 10 percent). Only taken into account when locus/feature name is entered - ignored when genomic coordinates are entered.  Default c(1.5,1.5).
#' @param annots Represents annotations related to genomic data. It is a crucial input for the function and is used to customize the visualization of genomic features. It can either be a 'pre-loaded' annotation in GRanges format by use of the ReadInAnnotations function or a named character vector providing the full paths to indidual annotations that will then be loaded by the function. If using the same set of annotations for creating multiple plots the 'pre-loaded' format is recommended. See example below.
#' @param annotation_packing Set the annotation packing for each annotation. Options are: 'expanded', 'squished', 'collapsed' and 'collapsed2'. 'expanded' and 'squished' display the detailed structures of transcripts under a given feature either as fully expanded or squished. 'collapsed' collapses all overlapping features into one 'super exon' feature whereas 'collapsed2' only collapses features belonging to the same locus into one 'super exon' feature.
#' @param annot_cols Specify the color(s) used to visualize the annotated features (by color name or hex code). If set to NULL, the colors specified in the bed file will be used.
#' @param annot_panel_color Color of the titles of the annotation(s) depicted in the left panel (as color name or hex code).
#' @param annot_panel_font_size Font size of the titles of the annotation(s) depicted in the left panel. Will be determined automatically by default.
#' @param bin_start Center the bins around the given genomic position. Provide an integer value that lies within the plotted region. Per default the bin center will be at the 5'-end of the plotted region if it is defined by genomic coordinates and at the 5'-end of the locus if the plotted region is defined by locus name.
#' @param bin_size Integer value (>1). Default: 'auto';  the bin size will be automatically determined. The lower the value, the slower the plotting.
#' @param bins_per_cm Number of bins to display per centimeter. Only relevant if 'Bin Size' is automatically determined. Default 250 bins/cm.
#' @param track_width_cm Specify the width in centimeters for the sequencing track window of the plot (full plot width will be determined based on this value). Default 12 cm.
#' @param full_width_cm If track_width_cm is not specified (=NULL), you can specify the width in centimeters for the full plot. We recommend to set this argument to NULL to allow this to be determined automatically based on the specified track_width_cm and panels_max_width_cm. Default NULL.
#' @param full_height_cm Specify the plot height in centimeters. We recommend to set this argument to NULL to allow this to be determined automatically based on the number of tracks to display and the specified track_height_cm. Default NULL.
#' @param track_height_cm Height in centimeters of each sequencing track (full plot height will be influenced by this value). Positive numeric value. Default 0.3 cm. Recommended value between 0.2 and 1.0 cm.
#' @param title_field_height_cm Height in centimeters of the title field (full plot height will be influenced based on this value). Positive numeric value. Default 0.66 cm. Will be ignored if the field is set too small to fit the font size selected for the title.
#' @param genomic_scale_height_cm Height in centimeters of the genomic scale field (full plot height will be influenced based on this value). Positive numeric value. Default 0.24 cm.
#' @param annotation_height_cm Height in centimeters of each line in the annotation track (full plot height will be influenced based on this value). Positive numeric value. Default 0.24 cm.
#' @param spacer_height_cm Height in centimeters of each spacer line used in the plot (full plot height will be influenced based on this value).Positive numeric value. Default 0.06 cm.
#' @param panels_max_width_cm Maximum width in cm that can be occupied by the sample labels panel (to the left of tracks; 'auto' or a positive numeric value). Text truncation may occur if the value makes the panel too narrow.
#' @param margin_width_cm Specify the size in centimeters of the margins on each side of the sequencing tracks. 0.05 cm per default.
#' @param fixed_panel_width A logical value (TRUE/FALSE) . Specify if the tracks labels should mandatorily occupy all the space provided in panels_max_width_cm or if they can use less should it be possible. Ignored if panels_max_width_cm='auto'.
#' @param horizontal_panels_list List of boolean vectors indicating whether text in the individual subpanels in the 'sample overview panel' should be displayed horizontally (TRUE) or vertically (FALSE). The list should be provided in the following format: list('dataset1'=c(TRUE,FALSE,FALSE,TRUE), 'dataset2'=c(TRUE,TRUE), ...). If NULL, an automatic assignment based on available space will be performed. 
#' @param panel_font_sizes Font size(s) for panel text. Provide either one integer (applied to all panel text), two comma-separated integers (the first for the left-most panel, the second one for all subsequent panels), or X comma-separated integer where X corresponds to the largest number of subgroups (incl. dataset). Will be automatically assigned if argument is set to NULL.
#' @param panel_font_size_list List of font sizes for each dataset and subgroup in the following format: list('dataset1'=c(12,8,6,4), 'dataset2'=c(12,6,4), ...).
#' @param panel_text_colors Color(s) of the panel text (as name or hex code). Provide either one color (for all) or two comma-separated colors (for datasets and subgroups).
#' @param horizontal_spacers A logical value (TRUE/FALSE) . Specify if a white space (horizontal) should be left between sequencing datasets tracks.
#' @param panel_separators Specify if horizontal,vertical line-separators should be displayed in order to clearly separate panels. c(FALSE,TRUE) by default for horizontal and vertical, respectively. If one logical value is supplied it will automatically be applied to both. Horizontal line-separators will only be displayed if horizontal_spacers=TRUE. 
#' @param separators_lwds Weight of the line-separators. Provide either one weight or three comma-separated weight(s) to designate individual  weights for 'line-spacer', 'thickline-spacer', 'vertical-spacer', where the first two are horizontal spacers.
#' @param separators_colors Color(s) of the line-separators (as name or hex code). Provide either one color or three comma-separated colors to designate individual colors for 'line-spacer', 'thickline-spacer', 'vertical-spacer', where the first two are horizontal spacers.
#' @param incl_first_panel A logical value (TRUE/FALSE) . Should the left-most panels, which displays dataset names, be displayed? Can be omitted if all datasets consist of only one sample or if all samples are contained within one dataset.
#' @param print_one_line_sample_names Combine all sample 'subgroup' information in one panel - separated by points (.)  - instead of setting up multiple panels. 
#' @param replicate_names Prefix added before replicate numbers (e.g. rep, r). NULL will lead to display of individual replicates without separate naming. NA will lead to display of replicate numbers without a prefix. Ignored when the mean of replicates is calculated.
#' @param group_autoscale For each dataset, specify whether to 'group' auto-scale or just auto-scale for each individual track. Named boolean vector.
#' @param incl_track_scales A logical value (TRUE/FALSE) . Should tracks scales be displayed (to the left of tracks).
#' @param scientific_scale Should scientific number format be used for the tracks scale. Options: allow, all, none. Allow is the default.
#' @param force_scale Provide the maximum value for the data scale (y-axis) for each dataset. Either single or two comma-separated positive numeric values. If a single value is supplied, this scaling will be applied to both strands. NULL leads to auto-scaling. Will per default be determined based on maximum value within the dataset.
#' @param scale_font_size Font size of the data scales.
#' @param scale_panel_width_cm Width in cm allocated to the tracks scale ('auto' or a positive numeric value). Ignored if incl_track_scales=FALSE.
#' @param scale_font_color Color of the data scales (as color name or hex code).
#' @param header Specify a header to be used instead of the automatically generated header based on the name of the locus/feature. If genomic coordinates are used, the title panel will be excluded per default unless specified here.
#' @param suppress_header Exclude the 'Header Panel' at the top of the produced plot? This argument is ignored if a header is provided manually.
#' @param header_font_sizes Font size(s) in the header region (integer value(s) >4). One integer or three comma-separated integers for 'main title', 'genomic range (subtitle)' and 'scale', respectively. Will be determined automatically by default.
#' @param header_font_colors Text colors of the header region. One color or three comma-separated colors for 'main title', 'genomic range (subtitle)' and 'scale', respectively  (use color names or hex codes). Default: black,darkgray,black.
#' @param include_genomic_scale A logical value (TRUE/FALSE) . Should genomic scale be displayed.
#' @param genomic_scale_on_top Display genomic scale above tracks (otherwise it will be displayed below).
#' @param genomic_scale_font_size Font size of the genomic scale (integer value >4). NULL will lead to automatic determination. 
#' @param genomic_scale_font_color Color of the genomic scale.
#' @param incl_feature_names A logical value (TRUE/FALSE) . Display feature/locus names in the annotation panel.
#' @param feature_names_above A logical value (TRUE/FALSE). When TRUE, the annotation panel layout is vertically mirrored around the strand axis: transcripts, inline names, brackets and shadings all flip together so the layout sits above the strand line instead of below. When `feature_names_alternating = TRUE` and strands are not intermingled, the mirror is applied to one strand only, producing the classic head-to-head layout for stranded plots. Default FALSE.
#' @param feature_names_alternating A logical value (TRUE/FALSE) . Display reverse strand features as a miror of the forward strand. Will be ignored with 'Intermingled strands display'
#' @param feature_names_font_size Font size of the annotated feature name. Will be determined automatically by default.
#' @param incl_feature_brackets A logical value (TRUE/FALSE). Indicate the full range of each locus with a bracket; the gene name is drawn inside the bracket where width permits, otherwise placed above/below it via the c2 fallback. Only supported in 'expanded' and 'squished' annotation modes; ignored (with a warning) in 'collapsed' and 'collapsed2'.
#' @param incl_feature_shadings A logical value (TRUE/FALSE). When TRUE, each locus within the plotted region is given an alternating background colour spanning its transcript row(s) and the rendered gene name. Only supported in 'expanded' and 'squished' annotation modes; ignored (with a warning) in 'collapsed' and 'collapsed2'.
#' @param feature_shading_colors Shading colors. Provide two comma-separated colors (as color name or hex code).
#' @param feature_shading_alpha Shading opacity. Provide numeric value between 0 and 1. However, using a value  <0.2 is recommended.
#' @param center_of_mass A logical value (TRUE/FALSE). Only takes effect when `incl_feature_brackets = TRUE` for an expanded or squished annotation; otherwise inert. When brackets are on: (a) for genes whose name fits inside its bracket, the name is centred on the transcript-density-weighted center of mass instead of the bracket's geometric centre; (b) for genes whose name does not fit and is placed inline next to the bracket (c2 fallback), the COM determines which side (left or right of the bar) the name sits. Names placed above or below the bracket are not affected. Probably only makes sense when a long outlier transcript pulls the geometric centre away from where most transcripts cluster.
#' @param feature_names_font_color Text color of the annotated feature name (as color name or hex code).
#' @param dummy_plot A logical value (TRUE/FALSE) . Should a dummy plot (without sequencing data, for aestethic trials) be generated? This allows fast debugging of the Plot display parameters. Default FALSE.
#' @param pdf A logical value (TRUE/FALSE) . Whether to plot to pdf (TRUE) or on-screen (FALSE). Default FALSE
#' @param pdf_name Name of pdf file. Default NULL will lead to autogeneration of file name, which will include the bin size and header or feature name. If none of those are defined the file name will include the date.
#' @param pdf_dir Directory where the pdf file should be stored. Default './testplotting'.
#' @param scaling_factor Specify a scaling factor to apply for on-screen display - ignored when exporting to pdf (pdf=TRUE).
#' @param verbosity Level of information displayed in console.
#' @param interface 'R' or 'shiny' (determines whether output relates to variable names in R or shiny)
#' @param ... 
#'
#' @return A customized "genome-browser" plot
#' 
#' @import S4Vectors
#' @import GenomicRanges
#' @import IRanges
#' @importFrom BiocGenerics strand
#' 
#' @export
#'
#' @examples
#' datasets <- list(
#'   '3-seq' = list(
#'     '-PAP' = list(
#'       'total' = c("siCTRL", "siEXOSC3"),
#'       '4sU' = c("siCTRL", "siEXOSC3")
#'     ),
#'    '+PAP' = list(
#'      'total' = c("siCTRL", "siEXOSC3"),
#'      '4sU' = c("siCTRL", "siEXOSC3")
#'     )
#'   ),
#'   'TT-seq' = c("siCTRL", "siEXOSC3"),
#'   'RNA-seq' = c("siCTRL", "siEXOSC3"),
#'   'ChIP-seq' = "RNAPII"
#' )
#' 
#' colors <- list(
#'   '3-seq' = list(
#'     '-PAP' = list(
#'       'total' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E"),
#'       '4sU' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E")
#'     ),
#'     '+PAP' = list(
#'       'total' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E"),
#'       '4sU' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E")
#'     )
#'   ),
#'   'TT-seq' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E"),
#'   'RNA-seq' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E"),
#'   'ChIP-seq' = c("RNAPII" = "#2A3132")
#' )
#' 
#' bigwigs <- list(
#'   '+' = list(
#'     '3-seq' = list(
#'       '-PAP' = list(
#'         'total' = list(
#'           siCTRL = c(
#'             "siGFP_noPAP_in_batch1_plus.bw",
#'             "siGFP_noPAP_in_batch2_plus.bw",
#'             "siGFP_noPAP_in_batch3_plus.bw"
#'           ),
#'           siEXOSC3 = c(
#'             "siRRP40_noPAP_in_batch1_plus.bw",
#'             "siRRP40_noPAP_in_batch2_plus.bw",
#'             "siRRP40_noPAP_in_batch3_plus.bw"
#'           )
#'         ),
#'         '4sU' = list(
#'           siCTRL = c(
#'             "siGFP_noPAP_ip_batch1_plus.bw",
#'             "siGFP_noPAP_ip_batch3_plus.bw"
#'           ),
#'           siEXOSC3 = c(
#'             "siRRP40_noPAP_ip_batch1_plus.bw",
#'             "siRRP40_noPAP_ip_batch3_plus.bw"
#'           )
#'         )
#'       ),
#'       '+PAP' = list(
#'         'total' = list(
#'           siCTRL = c(
#'             "siGFP_xPAP_in_batch1_plus.bw",
#'             "siGFP_xPAP_in_batch2_plus.bw",
#'             "siGFP_xPAP_in_batch3_plus.bw"
#'           ),
#'           siEXOSC3 = c(
#'             "siRRP40_xPAP_in_batch1_plus.bw",
#'             "siGFP_xPAP_in_batch2_plus.bw",
#'             "siRRP40_xPAP_in_batch3_plus.bw"
#'           )
#'         ),
#'         '4sU' = list(
#'           siCTRL = c(
#'             "siGFP_xPAP_ip_batch1_plus.bw",
#'             "siGFP_xPAP_ip_batch3_plus.bw"
#'           ),
#'           siEXOSC3 = c(
#'             "siRRP40_xPAP_ip_batch1_plus.bw",
#'             "siRRP40_xPAP_ip_batch3_plus.bw"
#'           )
#'         )
#'       )
#'     ),
#'     'TT-seq' = list(
#'       siCTRL = c(
#'         "L_EGFP_rep1_tt_corr_ff_noJncReads_plus.bw",
#'         "L_EGFP_rep2_tt_corr_ff_noJncReads_plus.bw"
#'       ),
#'      siEXOSC3 = c(
#'         "L_RRP40_rep1_tt_corr_ff_noJncReads_plus.bw",
#'         "L_RRP40_rep2_tt_corr_ff_noJncReads_plus.bw"
#'       )
#'     ),
#'     'RNA-seq' = list(
#'       siCTRL = c(
#'         "T_EGFP_rep1_tt_corr_plus.bw",
#'         "T_EGFP_rep2_tt_corr_plus.bw"
#'       ),
#'       siEXOSC3 = c(
#'         "T_RRP40_rep1_tt_corr_plus.bw",
#'         "T_RRP40_rep2_tt_corr_plus.bw"
#'       )
#'     ),
#'     'ChIP-seq' = list(
#'       RNAPII = c(
#'         "GSM2642506_WIGfs_Hela-H9_WT_siFFL_Pol_II_N20_MA733_bin50_Scaled_BGSub_Hg38.bw",
#'         "GSM2642508_WIGfs_Hela-H9_WT_siFFL_Pol_II_N20_MA736_bin50_Scaled_BGSub_Hg38.bw"
#'       )
#'     )
#'   ),
#'   '-' = list(
#'     '3-seq' = list(
#'       '-PAP' = list(
#'         'total' = list(
#'           siCTRL = c(
#'             "siGFP_noPAP_in_batch1_minus.bw",
#'             "siGFP_noPAP_in_batch2_minus.bw",
#'             "siGFP_noPAP_in_batch3_minus.bw"
#'           ),
#'           siEXOSC3 = c(
#'             "siRRP40_noPAP_in_batch1_minus.bw",
#'             "siRRP40_noPAP_in_batch2_minus.bw",
#'             "siRRP40_noPAP_in_batch3_minus.bw"
#'           )
#'         ),
#'         '4sU' = list(
#'           siCTRL = c(
#'             "siGFP_noPAP_ip_batch1_minus.bw",
#'             "siGFP_noPAP_ip_batch3_minus.bw"
#'           ),
#'           siEXOSC3 = c(
#'             "siRRP40_noPAP_ip_batch1_minus.bw",
#'             "siRRP40_noPAP_ip_batch3_minus.bw"
#'           )
#'         )
#'       ),
#'       '+PAP' = list(
#'         'total' = list(
#'           siCTRL = c(
#'             "siGFP_xPAP_in_batch1_minus.bw",
#'             "siGFP_xPAP_in_batch2_minus.bw",
#'             "siGFP_xPAP_in_batch3_minus.bw"
#'           ),
#'           siEXOSC3 = c(
#'             "siRRP40_xPAP_in_batch1_minus.bw",
#'             "siGFP_xPAP_in_batch2_minus.bw",
#'             "siRRP40_xPAP_in_batch3_minus.bw"
#'           )
#'         ),
#'         '4sU' = list(
#'           siCTRL = c(
#'             "siGFP_xPAP_ip_batch1_minus.bw",
#'             "siGFP_xPAP_ip_batch3_minus.bw"
#'           ),
#'           siEXOSC3 = c(
#'             "siRRP40_xPAP_ip_batch1_minus.bw",
#'             "siRRP40_xPAP_ip_batch3_minus.bw"
#'           )
#'         )
#'       )
#'     ),
#'     'TT-seq' = list(
#'       siCTRL = c(
#'         "L_EGFP_rep1_tt_corr_ff_noJncReads_minus.bw",
#'         "L_EGFP_rep2_tt_corr_ff_noJncReads_minus.bw"
#'       ),
#'       siEXOSC3 = c(
#'         "L_RRP40_rep1_tt_corr_ff_noJncReads_minus.bw",
#'         "L_RRP40_rep2_tt_corr_ff_noJncReads_minus.bw"
#'       )
#'     ),
#'     'RNA-seq' = list(
#'       siCTRL = c(
#'         "T_EGFP_rep1_tt_corr_minus.bw",
#'         "T_EGFP_rep2_tt_corr_minus.bw"
#'       ),
#'       siEXOSC3 = c(
#'         "T_RRP40_rep1_tt_corr_minus.bw",
#'         "T_RRP40_rep2_tt_corr_minus.bw"
#'       )
#'     )
#'   )
#' )
#' 
#' bigwig_dirs <- c(
#'   '3-seq' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_3pseq/",
#'   'TT-seq' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_TTseq/",
#'   'RNA-seq' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_RNAseq/",
#'   'ChIP-seq' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_ChIPseq/"
#' )
#' 
#' parameters <- list(
#'   '3-seq' = list(
#'     whichSamples = NULL,
#'     bin_stats = "max",
#'     enhance_signals = TRUE,
#'     log2transform = FALSE,
#'     pseudoCount = 1,
#'     batchCorrect = TRUE,
#'     batch = c(1, 2, 3, 1, 2, 3, 1, 3, 1, 3, 1, 2, 3, 1, 2, 3, 1, 3, 1, 3),
#'     whichReps = NULL,
#'     negative_valued_bw = FALSE,
#'     calcMean = TRUE,
#'     negValsSet0 = TRUE,
#'     force_scale = c(NA, NA),
#'     group_autoscale = TRUE
#'   ),
#'   'ChIP-seq' = list(
#'     whichSamples = NULL,
#'     bin_stats = "mean",
#'     enhance_signals = TRUE,
#'     log2transform = FALSE,
#'     pseudoCount = 1,
#'     batchCorrect = FALSE,
#'     batch = NULL,
#'     whichReps = NULL,
#'     negative_valued_bw = FALSE,
#'     calcMean = TRUE,
#'     negValsSet0 = TRUE,
#'     force_scale = c(NA, NA),
#'     group_autoscale = TRUE
#'   ),
#'   'RNA-seq' = list(
#'     whichSamples = NULL,
#'     bin_stats = "mean",
#'     enhance_signals = FALSE,
#'     log2transform = FALSE,
#'     pseudoCount = 1,
#'     batchCorrect = TRUE,
#'     batch = c(1, 2, 1, 2),
#'     whichReps = NULL,
#'     negative_valued_bw = FALSE,
#'     calcMean = TRUE,
#'     negValsSet0 = TRUE,
#'     force_scale = c(NA, NA),
#'     group_autoscale = TRUE
#'   ),
#'   'TT-seq' = list(
#'     whichSamples = NULL,
#'     bin_stats = "mean",
#'     enhance_signals = FALSE,
#'     log2transform = FALSE,
#'     pseudoCount = 1,
#'     batchCorrect = TRUE,
#'     batch = c(1, 2, 2, 2),
#'     whichReps = NULL,
#'     negative_valued_bw = FALSE,
#'     calcMean = TRUE,
#'     negValsSet0 = TRUE,
#'     force_scale = c(NA, NA),
#'     group_autoscale = TRUE
#'   )
#' )
#' 
#' annotation_files <- c(
#'   'gencode v38' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/annotations/gencode.v38.annotation.bed",
#'   'in-house' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/annotations/HeLa_major_isoform_hg38_gc34.bed",
#'   'ChIP-peaks' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/annotations/RNAPII_ChIP_peaks.bed"
#' )
#' 
#' annots = ReadInAnnotations(annotation_files)
#' 
#' seqNdisplay(datasets, colors, bigwig_dirs, bigwigs, parameters, feature='LMO4', annots=annotation_files, incl_feature_names=c('ChIP-peaks'=FALSE, 'gencode v38'=TRUE, 'in-house'=TRUE), annot_cols=list('ChIP-peaks'='black', 'gencode v38'='black', 'in-house'=NULL))
#' 
#' seqNdisplay(datasets, colors, bigwig_dirs, bigwigs, parameters, feature='LMO4', annots=annots, incl_feature_names=c('ChIP-peaks'=FALSE, 'gencode v38'=TRUE, 'in-house'=TRUE), annot_cols=list('ChIP-peaks'='black', 'gencode v38'='black', 'in-house'=NULL))
#' 
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
    incl_feature_names=TRUE, feature_names_above=FALSE, feature_names_alternating=TRUE, feature_names_font_size=NULL, incl_feature_brackets=FALSE, incl_feature_shadings=FALSE, feature_shading_colors=c('steelblue', 'hotpink'), feature_shading_alpha=0.05, center_of_mass=FALSE, feature_names_font_color='black',
    dummy_plot=FALSE, pdf=FALSE, pdf_name=NULL, pdf_dir='./testplotting', scaling_factor=1, verbosity='normal', interface='R', ...){
  
  t1 = Sys.time()
  
  constants_defaults = ConstantsDefaults()
  cm_to_in = constants_defaults['cm_to_in'] #@ 2022-10-05
  std_letter_width = constants_defaults['std_letter_width'] #@ 2022-10-05
  std_letter_height = constants_defaults['std_letter_height'] #@ 2022-10-05
  min_font_size = constants_defaults['min_font_size'] #@ 2022-10-05
  #### -> check parameters part 1 - abort or auto correct
  .verbosity = structure(0:3, names=c('off', 'no warnings', 'normal', 'detailed'))[as.character(ScrutinizeExpandAndNameParameter(verbosity, 1, use_names=FALSE, default_value='normal', expect_standard=NULL, expect=c('off', 'no warnings', 'normal', 'detailed'), revert_to_default=TRUE, alt_par_name=NULL, verbosity=3))]
  .interface = as.character(ScrutinizeExpandAndNameParameter(interface, 1, use_names=FALSE, default_value='R', expect_standard=NULL, expect=c('R', 'shiny'), revert_to_default=TRUE, alt_par_name=NULL, verbosity=.verbosity))
  if (verbosity > 2){ .detailed.output = list() }
  #datasets, colors, bigwig_dirs, bigwigs, parameters,
  if (is.null(feature) & is.null(locus)){
    if (.verbosity > 0){ cat('ERROR: choose a genomic region for plotting by either assigning locus name or coordinates - aborting', '\n') }
    return()
  }
  bigwig_dirs = rapply(bigwig_dirs,
                       function(X) ifelse(grepl('/$', X), X, paste0(X, '/')),
                       classes = 'character', how = 'replace')
  #### -> check parameters part 1 --  numeric range checks (abort on fail)
  .abort_specs = list(
    #                   val                      min   max   name                       obligatory skip_null skip_auto
    list(val=track_width_cm,          min=3,    max=25,  name='track_width_cm',          obligatory=FALSE, skip_null=TRUE,  skip_auto=FALSE),
    list(val=full_width_cm,           min=5,    max=30,  name='full_width_cm',           obligatory=FALSE, skip_null=TRUE,  skip_auto=FALSE),
    list(val=full_height_cm,          min=5,    max=30,  name='full_height_cm',          obligatory=FALSE, skip_null=TRUE,  skip_auto=FALSE),
    list(val=track_height_cm,         min=0.2,  max=1,   name='track_height_cm',         obligatory=TRUE,  skip_null=TRUE,  skip_auto=FALSE),
    list(val=genomic_scale_height_cm, min=0.2,  max=0.5, name='genomic_scale_height_cm', obligatory=FALSE, skip_null=TRUE,  skip_auto=FALSE),
    list(val=annotation_height_cm,    min=0.2,  max=0.5, name='annotation_height_cm',    obligatory=FALSE, skip_null=TRUE,  skip_auto=FALSE),
    list(val=spacer_height_cm,        min=0.01, max=0.2, name='spacer_height_cm',        obligatory=FALSE, skip_null=TRUE,  skip_auto=FALSE),
    list(val=panels_max_width_cm,     min=0,    max=30,  name='panels_max_width_cm',     obligatory=FALSE, skip_null=TRUE,  skip_auto=TRUE),
    list(val=scale_panel_width_cm,    min=0,    max=1,   name='scale_panel_width_cm',    obligatory=FALSE, skip_null=TRUE,  skip_auto=TRUE)
  )
  for (.spec in .abort_specs) {
    .val <- .spec$val
    if (isTRUE(.spec$skip_null) && is.null(.val)) next
    if (isTRUE(.spec$skip_auto) && is.character(.val) && .val %in% c('auto', 'automatic')) next
    if (!EvaluateNumericValue(.val, positive_val=TRUE, min_val=.spec$min, max_val=.spec$max,
                              interval_obligatory=isTRUE(.spec$obligatory), turn_errors_to_warnings=FALSE,
                              alt_par_name=ParName(.spec$name, interface), .verbosity)) { return() }
  }
  # height mutual exclusivity check
  if (!is.null(full_height_cm) & !is.null(track_height_cm)){
    if (.verbosity > 1){
      cat(paste0('WARNING: ', ParName('track_height_cm', interface), ' = ', track_height_cm, ' and ', ParName('full_height_cm', interface), ' = ', full_height_cm,
                 '\n\t.) one of the arguments should be a positive numeric value and the other should be NULL',
                 '\n\t.) ', ParName('track_height_cm', interface), ' set to NULL'), '\n')
    }
    track_height_cm = NULL
  }else if (is.null(full_height_cm) & is.null(track_height_cm)){
    if (.verbosity > 0){ cat(' - both', ParName('track_height_cm', interface), 'and', ParName('full_height_cm', interface), 'are NULL - one of them has to be defined', '\n') }
    return()
  }
  if (scale_panel_width_cm!='auto'){
    if (.verbosity > 1) { cat('WARNING(s):\n - you are currently manually setting the width of the tracks scales, this can result in truncation of the scale that can go unnoticed. We strongly advise to leave this parameter as automatic setting or to control for absence of scale value truncation', '\n') }
  }
  if (!is.null(feature)) {
    if (!EvaluateNumericValue(extra_space, positive_val=TRUE, min_val=0, max_val=100, interval_obligatory=FALSE,
                              turn_errors_to_warnings=FALSE, alt_par_name=ParName('extra_space', interface), .verbosity)){ return() }
  }
  #### -> check parameters part 1 --  numeric range checks (fallback to default on fail)
  .fallback_specs = list(
    #                   val                      min   max    name                       obligatory default
    list(val=margin_width_cm,         min=0,    max=0.25, name='margin_width_cm',         obligatory=FALSE, default=0.05),
    list(val=panel_font_sizes,        min=4,    max=15,   name='panel_font_sizes',        obligatory=FALSE, default=NULL, skip_null=TRUE),
    list(val=scale_font_size,         min=4,    max=8,    name='scale_font_size',         obligatory=FALSE, default=NULL, skip_null=TRUE),
    list(val=feature_names_font_size, min=4,    max=12,   name='feature_names_font_size', obligatory=FALSE, default=NULL, skip_null=TRUE),
    list(val=bgr_alpha,               min=0,    max=0.25, name='bgr_alpha',               obligatory=TRUE,  default=0.2),
    list(val=feature_shading_alpha,   min=0,    max=0.25, name='feature_shading_alpha',   obligatory=TRUE,  default=0.05),
    list(val=scaling_factor,          min=0.5,  max=10,   name='scaling_factor',          obligatory=FALSE, default=1)
  )
  .env <- environment()
  for (.spec in .fallback_specs) {
    .val <- .spec$val
    if (isTRUE(.spec$skip_null) && is.null(.val)) next
    if (!EvaluateNumericValue(.val, positive_val=TRUE, min_val=.spec$min, max_val=.spec$max,
                              interval_obligatory=isTRUE(.spec$obligatory), turn_errors_to_warnings=TRUE,
                              alt_par_name=ParName(.spec$name, interface), .verbosity)) {
      assign(.spec$name, .spec$default, envir=.env)
      if (.verbosity > 1) cat(if (is.null(.spec$default)) 'NULL' else .spec$default, '\n')
    }
  }
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
    .incl.feature.names = ScrutinizeExpandAndNameParameter(incl_feature_names, .annotations, use_names=TRUE, default_value=TRUE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('incl_feature_names', interface), verbosity=.verbosity)
    .feature.names.above = ScrutinizeExpandAndNameParameter(feature_names_above, .annotations, use_names=TRUE, default_value=FALSE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('feature_names_above', interface), verbosity=.verbosity)
    if (!is.null(.feature.names.above)){
      if (feature_names_alternating & !strands_intermingled){
        .feature.names.above = list('+'=.feature.names.above, '-'=!.feature.names.above)
      }else{
        .feature.names.above = list('+'=.feature.names.above, '-'=.feature.names.above)
      }
    }
    .incl.feature.brackets = ScrutinizeExpandAndNameParameter(incl_feature_brackets, .annotations, use_names=TRUE, default_value=TRUE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('incl_feature_brackets', interface), verbosity=.verbosity)
    .incl.feature.shadings = ScrutinizeExpandAndNameParameter(incl_feature_shadings, .annotations, use_names=TRUE, default_value=TRUE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('incl_feature_shadings', interface), verbosity=.verbosity)
    .annotation.packing = ScrutinizeExpandAndNameParameter(annotation_packing, .annotations, use_names=TRUE, default_value='collapsed2', expect_standard=NULL, expect=c('expanded', 'squished', 'collapsed', 'collapsed2'), revert_to_default=TRUE, alt_par_name=ParName('annotation_packing', interface), verbosity=.verbosity)
    if (!is.null(annot_cols)){
      if (any(annot_cols=='NULL')){
        .replacement.col = ifelse('seashell4' %in% unlist(colors), ifelse('thistle3' %in% unlist(colors), 'slategray3', 'thistle3'), 'seashell4')
        annot_cols[names(which(annot_cols=='NULL'))] = .replacement.col
        .annot.cols = ScrutinizeExpandAndNameParameter(annot_cols, .annotations, use_names=TRUE, default_value='black', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('annot_cols', interface), verbosity=.verbosity)
        .null.index = which(.annot.cols==.replacement.col)
        .annot.cols = as.list(.annot.cols)
        .annot.cols[.null.index] = list(NULL)
      }else{
        .annot.cols = ScrutinizeExpandAndNameParameter(annot_cols, .annotations, use_names=TRUE, default_value='black', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('annot_cols', interface), verbosity=.verbosity)
        .annot.cols = as.list(.annot.cols)
      }
    }else{
      .annot.cols = ScrutinizeExpandAndNameParameter(annot_cols, .annotations, use_names=TRUE, default_value='black', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('annot_cols', interface), verbosity=.verbosity)
      .annot.cols = as.list(.annot.cols)
    }
    #### -> check parameters part 2 - abort and return NULL if any of the crucial parameters are NULL
    if ( any(sapply(list(.annotations,.incl.feature.names,.feature.names.above,.annot.cols,.incl.feature.brackets,.incl.feature.shadings,.annotation.packing), function(parameter) is.null(parameter))) ){ return() }
  }
  #### <- load all annotations
  #### -> check parameters part 3 - abort or auto correct
  .batch.correction = ScrutinizeExpandAndNameParameter(unlist(lapply(parameters, function(p) p$batchCorrect)), datasets, use_names=TRUE, default_value=FALSE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('batchCorrect', interface), verbosity=.verbosity)
  .log2.transform = ScrutinizeExpandAndNameParameter(unlist(lapply(parameters, function(p) p$log2transform)), datasets, use_names=TRUE, default_value=FALSE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('log2transform', interface), verbosity=.verbosity)
  .batch.log2 = structure(as.logical(.batch.correction + .log2.transform), names=names(.batch.correction))
  .pseudocounts = ScrutinizeExpandAndNameParameter(unlist(lapply(parameters, function(p) if(!is.null(p$pseudoCount)){p$pseudoCount}else{-1})), datasets, use_names=TRUE, default_value=1, expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('pseudoCount', interface), verbosity=.verbosity)
  for (.i in which(.batch.log2)){
    if (!EvaluateNumericValue(.pseudocounts[.i], positive_val=TRUE, min_val=0, max_val=1000000, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=paste(ParName('pseudoCount', interface), names(.pseudocounts[.i])), .verbosity)){
      parameters[[names(.pseudocounts[.i])]][['pseudoCount']] = 1
    }
  }
  bin_stats = sapply(names(datasets), function(dataset) parameters[[dataset]][['bin_stats']])
  .bin.stats = ScrutinizeExpandAndNameParameter(bin_stats, datasets, use_names=TRUE, default_value='mean', expect_standard=NULL, expect=c('mean', 'median', 'max'), revert_to_default=TRUE, alt_par_name=ParName('bin_stats', interface), verbosity=.verbosity)
  enhance_signals = sapply(names(datasets), function(dataset) parameters[[dataset]][['enhance_signals']])
  .enhance.signals = ScrutinizeExpandAndNameParameter(enhance_signals, datasets, use_names=TRUE, default_value=FALSE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('enhance_signals', interface), verbosity=.verbosity)
  .panel.separators = ScrutinizeExpandAndNameParameter(panel_separators, c('horizontal', 'vertical'), use_names=FALSE, default_value=FALSE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('panel_separators', interface), verbosity=.verbosity)
  .separators.lwds = ScrutinizeExpandAndNameParameter(separators_lwds, c('line-spacer', 'thickline-spacer', 'vertical-spacer'), use_names=FALSE, default_value=c(0.5, 1, 0.5), expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('separators_lwds', interface), verbosity=.verbosity)
  .separators.lwds = if (EvaluateNumericValue(.separators.lwds, positive_val=TRUE, min_val=0.5, max_val=10, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ParName('separators_lwds', interface), .verbosity)){.separators.lwds}else{ScrutinizeExpandAndNameParameter('dummy', c('line-spacer', 'thickline-spacer', 'vertical-spacer'), use_names=FALSE, default_value=c(0.5, 1, 0.5), expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('separators_lwds', interface), verbosity=.verbosity)}
  .separators.colors = ScrutinizeExpandAndNameParameter(separators_colors, c('line-spacer', 'thickline-spacer', 'vertical-spacer'), use_names=FALSE, default_value='black', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('separators_colors', interface), verbosity=.verbosity)
  .annot.panel.color = ScrutinizeExpandAndNameParameter(annot_panel_color, 'annotation', use_names=FALSE, default_value='steelblue', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('annot_panel_color', interface), verbosity=.verbosity)
  .panel.text.colors = ScrutinizeExpandAndNameParameter(panel_text_colors, c('panel_1st', 'panel'), use_names=FALSE, default_value=c('darkgreen', 'black'), expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('panel_text_colors', interface), verbosity=.verbosity)
  .scale.font.color = ScrutinizeExpandAndNameParameter(scale_font_color, 'scale', use_names=FALSE, default_value='darkred', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('scale_font_color', interface), verbosity=.verbosity)
  .header.font.colors = ScrutinizeExpandAndNameParameter(header_font_colors, c('header', 'subheader', 'genomic_scale'), use_names=FALSE, default_value=c('black', 'darkgray', 'black'), expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('header_font_colors', interface), verbosity=.verbosity)
  .genomic.scale.font.color = ScrutinizeExpandAndNameParameter(genomic_scale_font_color, 'genomic_axis', use_names=FALSE, default_value='black', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('genomic_scale_font_color', interface), verbosity=.verbosity)
  .feature.names.font.color = ScrutinizeExpandAndNameParameter(feature_names_font_color, 'features', use_names=FALSE, default_value='black', expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('feature_names_font_color', interface), verbosity=.verbosity)
  .feature.shading.colors = ScrutinizeExpandAndNameParameter(feature_shading_colors, 1:length(feature_shading_colors), use_names=FALSE, default_value=c('steelblue', 'hotpink'), expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('feature_shading_colors', interface), verbosity=.verbosity)
  .bgr.colors = as.character(ScrutinizeExpandAndNameParameter(bgr_colors, c('odd', 'even'), use_names=FALSE, default_value=c('#C1B49A', '#F1F1F2'), expect_standard='color', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('bgr_colors', interface), verbosity=.verbosity))
  .font.colors = structure(rep('black', 9), names=c('header', 'subheader', 'genomic_scale', 'genomic_axis', 'panel_1st', 'panel', 'scale', 'annotation', 'features'))
  .font.colors[c('annotation', names(.panel.text.colors), 'scale', names(.header.font.colors), 'genomic_axis', 'features')] = c(.annot.panel.color, .panel.text.colors, .scale.font.color, .header.font.colors, .genomic.scale.font.color, .feature.names.font.color)
  .intermingled.color = as.character(ScrutinizeExpandAndNameParameter(intermingled_color, 1, use_names=FALSE, default_value='same', expect_standard=NULL, expect=c('same', 'complementary', 'analogous_right', 'analogous_left'), revert_to_default=TRUE, alt_par_name=ParName('intermingled_color', interface), verbosity=.verbosity))
  .strands.alpha = ScrutinizeExpandAndNameParameter(strands_alpha, c('+', '-'), use_names=FALSE, default_value=c(100, 100), expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('strands_alpha', interface), verbosity=.verbosity)
  .strands.alpha = if(EvaluateNumericValue(.strands.alpha, positive_val=TRUE, min_val=20, max_val=100, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ParName('strands_alpha', interface), .verbosity)){.strands.alpha}else{ScrutinizeExpandAndNameParameter('dummy', c('+', '-'), use_names=FALSE, default_value=c(100, 100), expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('strands_alpha', interface), verbosity=0)}
  .title.field.height.cm = ScrutinizeExpandAndNameParameter(title_field_height_cm, 1, use_names=FALSE, default_value=0.66, expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('title_field_height_cm', interface), verbosity=.verbosity)
  .title.field.height.cm = if(EvaluateNumericValue(.title.field.height.cm, positive_val=TRUE, min_val=0.66, max_val=5, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ParName('title_field_height_cm', interface), .verbosity)){.title.field.height.cm}else{ScrutinizeExpandAndNameParameter('dummy', 1, use_names=FALSE, default_value=0.66, expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('title_field_height_cm', interface), verbosity=0)}
  if (!is.null(bin_start)){if (!EvaluateNumericValue(bin_start, positive_val=TRUE, min_val=1, max_val=1000000000, interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ParName('bin_start', interface), .verbosity)){ bin_start=NULL; if (.verbosity>1){cat('NULL', '\n')} }}
  .scientific.scale = as.character(ScrutinizeExpandAndNameParameter(scientific_scale, 1, use_names=FALSE, default_value='allow', expect_standard=NULL, expect=c('allow', 'all', 'none'), revert_to_default=TRUE, alt_par_name=ParName('scientific_scale', interface), verbosity=.verbosity))
  .group.autoscale = ScrutinizeExpandAndNameParameter(group_autoscale, datasets, use_names=TRUE, default_value=TRUE, expect_standard='logical', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('group_autoscale', interface), verbosity=.verbosity)
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
          # Also mirror bigwig_dirs so the path resolves to a real URL/dir
          # for unstranded datasets when only the - strand is being plotted.
          # Without this, UnpackSamples ends up with .dirs = NULL and the
          # full path collapses to just the filename, producing
          # "non-existing file" warnings.
          bigwig_dirs[['-']][[.unstranded.dataset]] = bigwig_dirs[['+']][[.unstranded.dataset]]
        }
      }
    }
  }
  #### <- strands display
  #### -> organize annotations in region
  if (!is.null(.annotations)){
    .annot.info = lapply(.plotted.region, function(.pr) OrganizeAnnotatedFeaturesInRegion(.pr, .annotations, .incl.feature.names, center_of_mass, .incl.feature.brackets, .annotation.packing))
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
  plot_vertical_parameters = PlotVerticalParameters() #@ 2022-10-05
  .plot.vertical.parameters = plot_vertical_parameters
  if (!is.null(track_height_cm)){ #@ ->
    plot_vertical_parameters = PlotVerticalParameters() #@ 2022-10-05
    .plot.vertical.parameters = UpdatePlotVerticalParameters(plot_vertical_parameters, track_height_cm, .title.field.height.cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm)
  }
  .estimated.plot.heights = AdjustEstimatedPlotHeights(structure(lapply(names(.plotted.region), function(.strand) EstimatePlotHeights(.annot.info[[.strand]], .incl.feature.names, .annotation.packing, .incl.feature.brackets, .plotting.segment.order[[.strand]], .tracks.listed[[.strand]], track_height_cm, full_height_cm, .stranded.beds[[.strand]], .plot.vertical.parameters, .verbosity, .interface)), names=names(.plotted.region)), .plot.vertical.parameters, full_height_cm, track_height_cm, .title.field.height.cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm)
  .est.track.height.cm.range = as.numeric(c(.estimated.plot.heights[[.strand]][['min.track.height.cm.est']], .estimated.plot.heights[[.strand]][['max.track.height.cm.est']]))
  if (any(.est.track.height.cm.range < 0.1)){
    if (.verbosity > 0){
      .height.pars = c(ParName('track_height_cm', interface), ParName('title_field_height_cm', interface), ParName('genomic_scale_height_cm', interface), ParName('annotation_height_cm', interface), ParName('spacer_height_cm', interface), ParName('full_height_cm', interface))
      .error.message = paste0('ERROR(s):\n - ', 'the Plot Display Parameters related to height are incompatible (', paste(.height.pars, collapse=', '), ') - aborting')
      cat(.error.message, '\n')
    }
    return()
  }
  .est.min.annot.height = min(unlist(lapply(names(.plotted.region), function(.strand) unlist(.estimated.plot.heights[[.strand]][['annot.heights.incl.text']]))))
  if (!is.null(annotation_height_cm) & is.null(track_height_cm)){
    #@.est.min.annot.height = annotation_height_cm*as.numeric(.est.min.annot.height/.plot.vertical.parameters['annot'])/min(.est.track.height.cm.range)
    .est.min.annot.height = annotation_height_cm*as.numeric(.est.min.annot.height/.plot.vertical.parameters['annot'])/max(.est.track.height.cm.range)
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
  #@ .rec.font.sizes = RecommendedFontSizes(max(.est.track.height.cm.range), .est.min.annot.height, .plot.vertical.parameters)
  .rec.font.sizes = RecommendedFontSizes(max(.est.track.height.cm.range), .est.min.annot.height, mean(.est.track.height.cm.range)*.plot.vertical.parameters)
  if (any(.rec.font.sizes < min_font_size)){
    .rec.font.sizes[which(.rec.font.sizes < min_font_size)] = min_font_size
  }
  if (is.null(header_font_sizes)){
    header_font_sizes = .rec.font.sizes[c('main', 'sub', 'scale')]
  }
  .header.font.sizes =  ScrutinizeExpandAndNameParameter(header_font_sizes, c('main', 'sub', 'scale'), use_names=FALSE, default_value=.rec.font.sizes[c('main', 'sub', 'scale')], expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('header_font_sizes', interface), verbosity=.verbosity)
  .header.font.sizes = if (EvaluateNumericValue(.header.font.sizes, positive_val=TRUE, min_val=4, max_val=c(24, 18, 18), interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ParName('header_font_sizes', interface), .verbosity)){.header.font.sizes}else{ScrutinizeExpandAndNameParameter('dummy', c('main', 'sub', 'scale'), use_names=FALSE, default_value=.rec.font.sizes[c('main', 'sub', 'scale')], expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('header_font_sizes', interface), verbosity=.verbosity)}
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
  .relative.annotation.height = structure(lapply(names(.plotted.region), function(.strand) RelativeAnnotationHeight(.annot.info[[.strand]], .estimated.plot.heights[[.strand]][['annot.heights']], .letter.heights, .incl.feature.names, .annotation.packing, .incl.feature.brackets, .stranded.beds[[.strand]])), names=names(.plotted.region))
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
  if (!EvaluateNumericValue(.full.width.cm, positive_val=TRUE, min_val=5, max_val=50, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ParName('full_width_cm', interface), .verbosity)){ return() }
  .scale.fontsize = .panel.info[[1]][['scale.fontsize']]
  if (!EvaluateNumericValue(.scale.fontsize, positive_val=TRUE, min_val=4, max_val=8, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ParName('scale_font_size', interface), .verbosity)){ return() }
  if (.scale.fontsize > .rec.font.sizes['signal_axis']){
    if (.verbosity > 1) { cat(paste0('WARNING(s):\n - parts of numbers on the data scale axis may hidden, because the font size appears to be too big for the panel - consider adjusting'), '\n') }
  }
  if (is.null(feature_names_font_size)){
    .feature.names.font.size = min(.feature.names.font.size, min(unlist(lapply(.panel.info[[1]][['panel.font.size.list']], min, na.rm=TRUE)), na.rm=TRUE))
  }
  if (!EvaluateNumericValue(.feature.names.font.size, positive_val=TRUE, min_val=4, max_val=12, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ParName('feature_names_font_size', interface), .verbosity)){ return() }
  if (is.null(annot_panel_font_size)){
    if (print_one_line_sample_names){
      .annot.panel.font.size = max(unlist(lapply(.panel.info[[1]][['panel.font.size.list']], function(x) mean(x[-1], na.rm=T))), na.rm=TRUE) #@
    }else{
      .annot.panel.font.size = round(max(unlist(lapply(.panel.info[[1]][['panel.font.size.list']], mean, na.rm=TRUE)), na.rm=TRUE), 0) #@
    }
  }else{
    .annot.panel.font.size = annot_panel_font_size
  }
  .annotation.panel.font.size = ScrutinizeExpandAndNameParameter(.annot.panel.font.size, 'annotation', use_names=FALSE, default_value=.rec.font.sizes['std'], expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('annot_panel_font_size', interface), verbosity=.verbosity)
  if (!EvaluateNumericValue(.annotation.panel.font.size, positive_val=TRUE, min_val=4, max_val=12, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ParName('annot_panel_font_size', interface), .verbosity)){ return() }
  if (!is.null(bin_size)){ if (bin_size!='auto' & bin_size!='automatic'){if (!EvaluateNumericValue(bin_size, positive_val=TRUE, min_val=1, max_val=10000000, interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ParName('bin_size', interface), .verbosity)){ bin_size='auto'; if (.verbosity>1){cat(NULL, '\n')} }}}
  if (!EvaluateNumericValue(bins_per_cm, positive_val=TRUE, min_val=50, max_val=1000, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ParName('bins_per_cm', interface), .verbosity)){ bins_per_cm=250; if (.verbosity>1){cat(bins_per_cm, '\n')} }
  #@.bins.per.cm = ScrutinizeExpandAndNameParameter(bins_per_cm, '1', use_names=FALSE, default_value=250, expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('bins_per_cm', interface), verbosity=.verbosity)
  .bins.per.cm = as.integer(if(EvaluateNumericValue(bins_per_cm, positive_val=TRUE, min_val=50, max_val=1000, interval_obligatory=TRUE, turn_errors_to_warnings=TRUE, alt_par_name=ParName('bins_per_cm', interface), .verbosity)){bins_per_cm}else{ScrutinizeExpandAndNameParameter(250, 1, use_names=FALSE, default_value=250, expect_standard='numeric', expect=NULL, revert_to_default=TRUE, alt_par_name=ParName('bins_per_cm', interface), verbosity=0)})
  .bin.size = GetBinSize(bin_size, IRanges::width(.plotted.region[[.strand]]), .plot.width.parameters[['tracks.width.cm']], .bins.per.cm, .verbosity)
  if (!EvaluateNumericValue(.bin.size, positive_val=TRUE, min_val=1, max_val=1000000, interval_obligatory=FALSE, turn_errors_to_warnings=FALSE, alt_par_name=ParName('bin_size', interface), .verbosity)){ return() }
  .fixed.plot.vertical.parameters = c('tracks'=!is.null(track_height_cm), 'header'=!is.null(.title.field.height.cm), 'scale'=!is.null(genomic_scale_height_cm), 'spacers'=!is.null(spacer_height_cm), 'annots'=!is.null(annotation_height_cm)) #@
  .vertical.parameters = c('tracks'=ifelse(!is.null(track_height_cm), track_height_cm, NA), 'header'=ifelse(!is.null(.title.field.height.cm),.title.field.height.cm, NA) , 'scale'=ifelse(!is.null(genomic_scale_height_cm), genomic_scale_height_cm, NA), 'spacers'=ifelse(!is.null(spacer_height_cm), spacer_height_cm, NA), 'annots'=ifelse(!is.null(annotation_height_cm), annotation_height_cm, NA)) #@ 2023-06-27
  if (!is.null(unlist(.annot.heights.incl.text))){
    .minimal.units = c('annots'=min(do.call('rbind', lapply(.annot.heights.incl.text, function(s) do.call('rbind', s)))[, .annot.panel.font.size]), 'tracks'= ifelse(all(.stranded.datasets) & strands_intermingled, 2, 1))
  }else{
    .minimal.units = c('annots'=NA, 'tracks'=ifelse(all(.stranded.datasets) & strands_intermingled, 2, 1))
  }
  #@.basic.plot.parameters = AlignBasicPlotParameters(structure(lapply(names(.plotted.region), function(.strand) BasicPlotParameters(.strand, .plotted.region, .feature.names.font.size, .plot.height.parameters, .plot.width.parameters, .full.width.cm, full_height_cm, track_height_cm, .plot.vertical.parameters, .bin.size, .bins.per.cm, .plotting.segment.order, .tracks.listed, .unstranded.beds)), names=names(.plotted.region)), both_strands, .strands.intermingled, .fixed.plot.vertical.parameters, .vertical.parameters, .minimal.units, full_height_cm) 
  .basic.plot.parameters = AlignBasicPlotParameters(structure(lapply(names(.plotted.region), function(.strand) BasicPlotParameters(.strand, .plotted.region, .feature.names.font.size, .plot.height.parameters, .plot.width.parameters, .full.width.cm, full_height_cm, track_height_cm, .plot.vertical.parameters, .bin.size, .bins.per.cm, .plotting.segment.order, .tracks.listed, .unstranded.beds)), names=names(.plotted.region)), both_strands, .strands.intermingled, .fixed.plot.vertical.parameters, .vertical.parameters, .minimal.units, full_height_cm, .annotation.packing)
  if (both_strands){
    if (.strands.intermingled){
      .plot.vertical.parameters = .basic.plot.parameters[['+-']][['plot.vertical.parameters']]
    }else{
      .plot.vertical.parameters = .basic.plot.parameters[['+']][['plot.vertical.parameters']]
    }
  }else{
    .plot.vertical.parameters = .basic.plot.parameters[[names(.plotted.region)]][['plot.vertical.parameters']]
  }
  if (is.null(genomic_scale_font_size)){
    .genomic.scale.font.size = .rec.font.sizes['genomic_axis']
  }else{
    .genomic.scale.font.size = genomic_scale_font_size
  }
  if (!EvaluateNumericValue(.genomic.scale.font.size, positive_val=TRUE, min_val=4, max_val=.rec.font.sizes['genomic_axis'], interval_obligatory=FALSE, turn_errors_to_warnings=TRUE, alt_par_name=ParName('genomic_scale_font_size', interface), .verbosity)){ return() }
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
    .track.height.cm = .basic.plot.parameters[[ifelse(.strands.intermingled, 3, 1)]][['track.height.cm']] #@ 2023-06-27 ifelse(.strands.intermingled, 3, 1) <- 1
    .plot.height.parameters = .plot.vertical.parameters * .track.height.cm #@ * .basic.plot.parameters[[ifelse(.strands.intermingled, 3, 1)]][['weight']] #@ 2023-06-27 added * .basic.plot.parameters[[ifelse(.strands.intermingled, 3, 1)]][['weight']] 
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
    .strand_label = function(s) if (interface == 'R') s else ifelse(s == '+', 'upper', 'lower')
    if (.strands.intermingled){
      .pso = c(.plotting.segment.order[['+']], 'scale')[c(rep(TRUE, length(.plotting.segment.order[['+']])), (!genomic_scale_on_top & include_genomic_scale))]
      .detailed.output.vector = c(.detailed.output.vector, paste0('\t', '.) ', .strand_label('+'), ': ', paste(.pso, collapse=',')))
    }else{
      if (both_strands){
        for (.s in c('+', '-')){
          .pso = .plotting.segment.order[[.s]]
          .detailed.output.vector = c(.detailed.output.vector, paste0('\t', '.) ', .strand_label(.s), ': ', paste(.pso, collapse=',')))
        }
      }else{
        .s = names(.plotting.segment.order)
        .pso = .plotting.segment.order[[.s]]
        .detailed.output.vector = c(.detailed.output.vector, paste0('\t', '.) ', .strand_label(.s), ': ', paste(.pso, collapse=',')))
      }
    }
    .detailed.output[[paste0('"', ParName('plotting_segment_order', interface), '"')]] = .detailed.output.vector
  }
  PreparePlottingInterface(plot_dim=c(.width.in, .height.in), pdf, pdf_name, pdf_dir, header, .bin.size, feature, .scaling.factor)
  # .pdf.name = PreparePlottingInterface(plot_dim=c(.width.in, .height.in), pdf, pdf_name, pdf_dir, header, .bin.size, feature, .scaling.factor) #@ 2022-10-26 added .pdf.name =
  ##### <- pdf or on-screen plotting?
  ##### -> plotting
  ## Batch all draw operations so the device only refreshes once at the end.
  ## Without this, multi-segment plots flicker (white-then-black mid-build)
  ## on interactive devices such as macOS Quartz. dev.hold() is a no-op on
  ## non-buffered devices (e.g. pdf), so it's safe to do unconditionally.
  ## Also reset par(bg) so any stale device colour from an earlier aborted
  ## plot can't bleed through the bg='transparent' panel draws downstream.
  if (!pdf) {
    par(bg = "white", fg = "black")
    dev.hold()
    on.exit(try(dev.flush(), silent = TRUE), add = TRUE)
  }
  .plotting.ready.segment.order = NumberingSpacers(.plotting.segment.order)
  if (.strands.intermingled){
    .plotted.strand = '+-'
    .first.plot = TRUE
    .plotting.segments = c(.plotting.ready.segment.order[['+']], 'scale')[c(rep(TRUE, length(.plotting.ready.segment.order[['+']])), (!genomic_scale_on_top & include_genomic_scale))]
    for (.plotting.segment in .plotting.segments){
      PlotSegment(feature, .plotted.region, .plotted.strand, both_strands, .plotting.segment, .basic.plot.parameters, .neg.vals.neg.strand, .plot.width.parameters, .plot.vertical.parameters, .annot.info, .panel.info, .panels.list, .panel.separators, .separators.lwds, .separators.colors, incl_first_panel, print_one_line_sample_names, replicate_names, header, .header.font.sizes, .scaling.factor, .full.width.cm, genomic_scale_on_top, .genomic.scale.font.size, reverse_strand_direction, .bin.stats, dummy_plot, .tracks, .strands.alpha, .intermingled.color, .unstranded.beds, .annotation.packing, .annotation.panel.font.size, .incl.feature.names, .feature.names.font.size, .feature.names.above, .incl.feature.brackets, .incl.feature.shadings, .feature.shading.colors, feature_shading_alpha, .annot.cols, .group.autoscale, incl_track_scales, .scientific.scale, .scale.fontsize, .force.scale.list, .log2.transform, colors, alternating_background, .bgr.colors, bgr_alpha, .font.colors, .letter.widths, .letter.heights, .enhance.signals, .first.plot, .verbosity)
    }
  }else{
    for (.plotted.strand in names(.plotting.ready.segment.order)){
      .first.plot = which(names(.plotting.ready.segment.order)==.plotted.strand)==1
      for (.plotting.segment in .plotting.ready.segment.order[[.plotted.strand]]){
        PlotSegment(feature, .plotted.region, .plotted.strand, both_strands, .plotting.segment, .basic.plot.parameters, .neg.vals.neg.strand, .plot.width.parameters, .plot.vertical.parameters, .annot.info, .panel.info, .panels.list, .panel.separators, .separators.lwds, .separators.colors, incl_first_panel, print_one_line_sample_names, replicate_names, header, .header.font.sizes, .scaling.factor, .full.width.cm, genomic_scale_on_top, .genomic.scale.font.size, reverse_strand_direction, .bin.stats, dummy_plot, .tracks, .strands.alpha, .intermingled.color, .unstranded.beds, .annotation.packing, .annotation.panel.font.size, .incl.feature.names, .feature.names.font.size, .feature.names.above, .incl.feature.brackets, .incl.feature.shadings, .feature.shading.colors, feature_shading_alpha, .annot.cols, .group.autoscale, incl_track_scales, .scientific.scale, .scale.fontsize, .force.scale.list, .log2.transform, colors, alternating_background, .bgr.colors, bgr_alpha, .font.colors, .letter.widths, .letter.heights, .enhance.signals, .first.plot, .verbosity)
      }
    }
  }
  ##### -> plotting
  if (!pdf) {
    ## macOS Quartz needs to be coaxed into repainting the window with the
    ## buffered content. dev.set(dev.cur()) is a known idiom that forces
    ## a window-invalidation event; the Sys.sleep gives the OS event loop
    ## a tick to process it before R returns to the REPL.
    try(dev.flush(), silent = TRUE)
    try(dev.set(dev.cur()), silent = TRUE)
    Sys.sleep(0.2)
  }
  if (pdf){
    suppressMessages( dev.off() )
    # if ( !grepl('.pdf$', .pdf.name) ){ #@ 2022-10-26 added ->
    #   file.rename(.pdf.name, paste0(.pdf.name, '.pdf'))
    # } #@ 2022-10-26 added <-
  }else{ 
    suppressMessages( dev.set(which=2) ) 
  }
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


#' seq'N'display'R Session
#'
#' @description Container for seqNdisplayR session information
#'
#' @author MS (minor additions by SLA)
#'
#' @param df an optional df, overrides all other options except annotations
#' @param samples samples object as used by seqNdisplay
#' @param colors colors object as used by seqNdisplay
#' @param bigwig_dirs bigwig_dirs object as used by seqNdisplay
#' @param bigwigs bigwigs object as used by seqNdisplay
#' @param parameters parameters object as used by seqNdisplay
#' @param annotations annotations object as used by seqNdisplay
#' @param options named list of other arguments used by seqNdisplay function
#' @param load_annotations load annotations as GRanges? default=FALSE
#'
#' @details seqNdisplayR session object holding above slots identical to parameters
#'   for seqNdisplay. If df is provided parses information from columns, colors,
#'   bigwig_file, bigwig_directory, dataset and subgroup_1, subgroup_2 etc. See
#'   Excel template sheet in
#'   \code{system.file('extdata','example_excel_template.xls',
#'   package='seqNdisplayR')} for more information. The df here is tidy, ie all
#'   "empty" slots are filled. Otherwise see \code{vignette(package='seqNdisplayR')}. 
#'   If options are not provided, adds default options to session object. 
#'   If load_annotations=TRUE will try to load annotations using \link[rtracklayer]{import}. 
#'   OBS: Currently only bed files are used correctly by seqNdisplayR.
#'
#' @return Object of class seqNdisplayRSession, which is a named list with slots samples, colors, bigwig_dirs, bigwigs, parameters and annotation_files and annots. 
#' Annots is either identical to annotation_files or a named list of loaded GRanges.
#' 
#' @import GenomicRanges
#' @importFrom rtracklayer import import.bed
#' 
#' @export
#'
#' @examples
#' xl_fname = system.file('extdata', 'sNdR_sample_example_elaborate.xlsx', package='seqNdisplayR')
#' session <- LoadExcel(xl_fname, load_annotations = T) # takes some time, since annotations are loaded...
#' feat = 'LMO4'
#' plot(session, feature=feat)
#' 
seqNdisplayRSession = function(df=NULL, samples=NULL, colors=NULL, bigwig_dirs=NULL, bigwigs=NULL, parameters=NULL, annotations=NULL, options=NULL, load_annotations=F) {
  if ( !missing(df) ) {
    for ( col in c('dataset', colnames(df)[grepl('^subgroup_', colnames(df))]) ){
      if ( any(grepl(';', df[[col]])) ) {
        cat('Note: Semicolons not allowed in dataset and subgroup names, will be exchanged to colon [":"] in ', col, '\n')
        df[[col]] = sub(';', ':', df[[col]])
      }
    }
    
    samples = GetSamples(df)
    colors = GetColors(df)
    bigwigs = GetBigwigs(df)
    bigwig_dirs = GetBigwigDirs(df)
    if ( missing(parameters) ) {
      parameters = lapply(names(samples), function(n) {x = DefaultParameters(); x})
      names(parameters) = names(samples)
    }

  } else if ( !is.null(bigwig_dirs) && is.character(bigwig_dirs) && !is.list(bigwig_dirs) && !is.null(bigwigs) ) {
    # Legacy input: flat named vector of per-dataset directories. Broadcast to the
    # new nested shape for backward compatibility with v1.x sessions.
    bigwig_dirs = LegacyBigwigDirsToNested(bigwig_dirs, bigwigs)
  }
  
  if ( missing(options) ) {
    options = DefaultPlotOptions()
    options = c(options, DefaultAnnotationOptions())
  }
  
  if ( load_annotations & !is.null(annotations)) {
    #@ .annots = lapply(annotations, function(anno) GenomicRanges::GRanges(rtracklayer::import.bed(anno))) #@ 2023-05-30
    .annots = lapply(annotations, function(anno) {
      tryCatch(
        {
          GenomicRanges::GRanges(suppressWarnings(rtracklayer::import.bed(anno)))
        },
        warning = function(w) NULL,
        error   = function(e) NULL   # local path missing, broken URL, etc.
      )
    })
    if ( any(sapply(.annots, function(x) is.null(x))) ){
      not_founds = names(.annots)[sapply(.annots, function(x) is.null(x))]
      founds = setdiff(names(.annots), not_founds)
      cat(paste('WARNING: the annotation(s) named', paste(paste0('"', not_founds, '"'), collapse=', '), 'could not be found or loading failed'), '\n')
      annotations[not_founds] = NULL
      options[c('incl_feature_names', 'feature_names_above', 'incl_feature_brackets', 'incl_feature_shadings', 'annotation_packing', 'annot_cols')] =
        lapply(options[c('incl_feature_names', 'feature_names_above', 'incl_feature_brackets', 'incl_feature_shadings', 'annotation_packing', 'annot_cols')], function(x) if (length(founds) > 0) {x[founds]}else{NULL})
      .annots[not_founds] = NULL
      # Collapse empty list to NULL so downstream is.null() checks behave
      # consistently. Without this, list() with no entries still passes
      # !is.null() but breaks code that expects either a populated list or
      # NULL.
      if (length(annotations) == 0L) annotations <- NULL
      if (length(.annots) == 0L)     .annots     <- NULL
    }
  } else {
    .annots = annotations
  }
  
  structure(
    c(list(
      samples = samples,
      colors = colors,
      bigwig_dirs = bigwig_dirs,
      bigwigs = bigwigs,
      parameters = parameters,
      annotation_files = annotations,
      annots = .annots),
      options
    ),
    class='seqNdisplayRSession'
  )
  
}


#' plot seq'N'display'R Session
#'
#' @description Container for seqNdisplayR session information
#'
#' @author MS (minor additions by SLA)
#'
#' @param session object of class seqNdisplayRSession
#' @param ... arguments passed to seqNdisplay. Should contain at least the argument feature or locus.
#'
#' @details see seqNdisplay for details. Session contains samples, colors, bigwigs, bigwig_dirs, parameters and annotation information.
#'
#' @return On-screen plot or pdf 
#' 
#' @export
#'
#' @examples
#' xl_fname = system.file('extdata', 'seqNdisplayR_sample_sheet_elaborate2.xlsx', package='seqNdisplayR')
#' session = LoadExcel(xl_fname, load_annotations =F)
#' class(session) # 'seqNdisplayRSession'
#' plot(session, feature='TAF1D')
#' 
plot.seqNdisplayRSession = function(session, ...){
  external_args = list(...)
  # use default args except if present in ellipsis (...) (first priority), or in session (2nd priority)
  from_dots = intersect(names(external_args), names(session))
  session[from_dots] = external_args[from_dots]
  # add external_args not in args ((ie add feature or locus!))
  only_dots = setdiff(names(external_args), names(session))
  session = c(session, external_args[only_dots])
  # add default for all for some reason missing
  default_args = DefaultPlotOptions()
  only_default = setdiff(names(default_args), names(session))
  session = c(session, default_args[only_default])
  # handle replicate_names (prefix)
  if (!is.null(session$replicate_names)){
    if (is.na(session$replicate_names)){ #@ session$replicate_names=='NA'
      session$replicate_names = ''
    }
  }
  if ( !('force_scale' %in% names(external_args)) ) {
    session$force_scale = NULL
  }
  # handle group_autoscale which is part of parameters but needs to passed differently to plot function
  if ( !('group_autoscale' %in% names(external_args)) ) {
    group_autoscale = unlist(lapply(session$parameters, function(para) {
      ga = ParseOption(para$group_autoscale)
      if (is.null(ga)) {
        NA
      }else{
        ga
      }
    }))
    names(group_autoscale) = names(session$parameters)
    session$group_autoscale = group_autoscale
  }
  session$parameters = lapply(session$parameters, function(x) x[!(names(x)=='group_autoscale')])
  # samples renamed to dataset for function call
  names(session) = sub('^samples$', 'datasets', names(session))
  do.call('seqNdisplay', session)
}


#' print seq'N'display'R Session
#'
#' @description Prints an overview over samples, colors and associated bigwigs in a seqNdisplayR Session.
#' Just a pretty overview over a session.
#'
#' @author MS
#'
#' @param session seqNdisplayRSession object
#' @param verbose print detailed information? default=FALSE
#' @param ... arguments 
#' 
#' @note ... arguments are samples, colors, bigwigs objects as used by seqNdisplayR and indent_size (string used for indent spacing of levels in the output)
#'
#' @details Convenience function for checking parsing of samples, colors and bigwigs.
#'
#' @return Print to R session.
#' 
#' @export
#'
#' @examples
#' xl_fname = system.file('extdata', 'seqNdisplayR_sample_sheet_elaborate2.xlsx', package='seqNdisplayR')
#' session = LoadExcel(xl_fname, load_annotations =F)
#' print(session)
#' print(session, verbose=T)
#' 
print.seqNdisplayRSession = function(session, verbose=FALSE, ...) {
  GlimpseSession(session$samples,
                 session$colors,
                 session$bigwigs,
                 ...)
  if ( verbose ) {
    cat('\nBigwig Paths:\n')
    print(session$bigwig_dirs)
    cat('\nBigwigs:\n')
    print(session$bigwigs)
    cat('\nParameters:\n')
    print(session$parameters)
    cat('\nAnnotations:\n')
    print(session$annotation_files)
    cat('\nOptions:\n')
    for ( opt in names(DefaultPlotOptions()) ) {
      print(session[opt])
    }
    for ( opt in names(DefaultAnnotationOptions()) ) {
      print(session[opt])
    }
  }
}


