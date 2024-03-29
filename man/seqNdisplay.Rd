% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seqNdisplayR.R
\name{seqNdisplay}
\alias{seqNdisplay}
\title{seq'N'display}
\usage{
seqNdisplay(
  datasets,
  colors,
  bigwig_dirs,
  bigwigs,
  parameters,
  plotting_segment_order = NULL,
  preloaded_tracks = NULL,
  output_tracks = FALSE,
  output_parameters = FALSE,
  input_parameters = NULL,
  both_strands = TRUE,
  strands_intermingled = TRUE,
  neg_vals_neg_strand = TRUE,
  reverse_strand_direction = FALSE,
  alternating_background = TRUE,
  bgr_colors = c("#C1B49A", "#F1F1F2"),
  bgr_alpha = 0.2,
  strands_alpha = c(100, 100),
  intermingled_color = "same",
  feature = NULL,
  locus = NULL,
  extra_space = c(1.5, 1.5),
  annots = NULL,
  annotation_packing = "collapsed2",
  annot_cols = NULL,
  annot_panel_color = "steelblue",
  annot_panel_font_size = NULL,
  bin_start = NULL,
  bin_size = "automatic",
  bins_per_cm = 250,
  track_width_cm = 10,
  full_width_cm = NULL,
  full_height_cm = NULL,
  track_height_cm = 0.3,
  title_field_height_cm = 0.66,
  genomic_scale_height_cm = 0.24,
  annotation_height_cm = 0.24,
  spacer_height_cm = 0.06,
  panels_max_width_cm = "auto",
  margin_width_cm = 0.05,
  fixed_panel_width = FALSE,
  horizontal_panels_list = NULL,
  panel_font_sizes = NULL,
  panel_font_size_list = NULL,
  panel_text_colors = c("darkgreen", "black"),
  horizontal_spacers = TRUE,
  panel_separators = c(FALSE, TRUE),
  separators_lwds = c(0.5, 1, 0.5),
  separators_colors = "black",
  incl_first_panel = TRUE,
  print_one_line_sample_names = FALSE,
  replicate_names = "rep",
  group_autoscale = TRUE,
  incl_track_scales = TRUE,
  scientific_scale = c("allow", "all", "none")[1],
  force_scale = NULL,
  scale_font_size = NULL,
  scale_panel_width_cm = "auto",
  scale_font_color = "darkred",
  header = NULL,
  suppress_header = FALSE,
  header_font_sizes = NULL,
  header_font_colors = c("black", "darkgray", "black"),
  include_genomic_scale = TRUE,
  genomic_scale_on_top = TRUE,
  genomic_scale_font_size = NULL,
  genomic_scale_font_color = "black",
  incl_feature_names = TRUE,
  feature_names_above = FALSE,
  feature_names_alternating = TRUE,
  feature_names_font_size = NULL,
  incl_feature_brackets = FALSE,
  incl_feature_shadings = FALSE,
  feature_shading_colors = c("steelblue", "hotpink"),
  feature_shading_alpha = 0.05,
  center_of_mass = FALSE,
  feature_names_font_color = "black",
  dummy_plot = FALSE,
  pdf = FALSE,
  pdf_name = NULL,
  pdf_dir = "./testplotting",
  scaling_factor = 1,
  verbosity = "normal",
  interface = "R",
  ...
)
}
\arguments{
\item{datasets}{A nested list of character vectors containing dataset names, subgroup names and corresponding sample names. This nested list contains the 'tree' structure of the data to be plotted. See example below.}

\item{colors}{Nested list of colors corresponding to the bigwigs nested list. Pick colors for the individual sample tracks (replicates for a given sample will get the same color). See example below.}

\item{bigwig_dirs}{Named character vector. For each dataset in datasets a directory where the bigwig files associated with each dataset is located. See example below.}

\item{bigwigs}{A nested list of bigwig files under each dataset. See example below.}

\item{parameters}{A list of parameters for customizing data transformation and processing. See example below.

This argument allows you to customize various aspects of data processing and transformation
for different 'datasets' data. The 'parameters' list should have named elements where each
name corresponds to a 'dataset' and contains sub-elements for customization.

The possible sub-elements within the 'parameters' list include:

- \code{whichSamples}: A vector or list specifying which samples to include in the analysis.

- \code{whichReps}: A vector or list specifying which replicates to include in the analysis.

- \code{log2transform}: A logical value (TRUE/FALSE) indicating whether to perform log2 transformation on the data.

- \code{pseudoCount}: A numeric value specifying the pseudo-count to be added to the data before transformation.

- \code{batchCorrect}: A logical value (TRUE/FALSE) indicating whether batch correction should be applied.

- \code{batch}: A vector or list specifying batch information for samples.

- \code{negative_valued_bw}: A logical value (TRUE/FALSE) indicating whether negative-valued bigwig data should be considered as positive data.

- \code{calcMean}: A logical value (TRUE/FALSE) indicating whether the mean of replicated samples should be calculated.

- \code{negValsSet0}: A logical value (TRUE/FALSE) indicating whether negative values should be set to 0.

The 'parameters' argument allows you to fine-tune the data processing for a specific 'dataset'
by providing custom settings for each 'dataset'. If 'parameters' is NULL (default),
the function uses default settings.}

\item{plotting_segment_order}{Specify the order of displayed vertical segments in the plot using the following segment-labels: 'header', 'scale', names of individual datasets, 'annotations', unstranded-beds, 'thickline-spacer', 'line-spacer' and 'empty-spacer'\. There should be perfect correspondance between datasets to display and the listed datasets. Default NULL which leads to a pre-determined order.}

\item{preloaded_tracks}{Void.}

\item{output_tracks}{Void.}

\item{output_parameters}{Void.}

\item{input_parameters}{Void.}

\item{both_strands}{A logical value (TRUE/FALSE) . Specify whether data should be displayed for both strands (when available). Default TRUE.}

\item{strands_intermingled}{A logical value (TRUE/FALSE) . Specify if both strand should be displayed as intermingled. Default TRUE. Ignored if both_strands=FALSE.}

\item{neg_vals_neg_strand}{A logical value (TRUE/FALSE) . Specify if reverse strand data should be represented as negative values. Automatically the case with 'strands_intermingled' display.}

\item{reverse_strand_direction}{A logical value (TRUE/FALSE) . If data is only plotted for the strand of interest (i.e. both_strands=FALSE), reverse strand loci can be horizontally mirrored (5'-left to 3'-right). This option substantially extends the plotting time. Default=FALSE.}

\item{alternating_background}{A logical value (TRUE/FALSE) . Should the background of the tracks alternate between datasets for easier discrimination? Colors to use can be specified by setting bgr_colors and bgr_alpha.}

\item{bgr_colors}{Provide a vector of two colors. Accepts colors by name and hex code, e.g. c('green','yellow') or c('#FF0000','#FF0042') or c('green','#FF0000').}

\item{bgr_alpha}{Opacity of background shading.}

\item{strands_alpha}{Percent opacity of the forward,reverse strand (100=full;0=blank). Default is c(100,100). If one number is provided, this will be used for both forward and reverse strand. Ignored if signals are enhanced (specified for individual datasets in parameters).}

\item{intermingled_color}{When the strands_Intermingled=TRUE, it may be beneficial to display data from the two strands with different colors. This can be done by changing the opacity and/or by choosing one of the options complementary, analogous_right and analogous_left.}

\item{feature}{The feature/locus name has to be present in one of the supplied annotations and match case. When entering feature/locus name and coordinates simultaneously, only the locus name will be considered.}

\item{locus}{The locus coordinates (e.g. chr1:+:87325400:87351991). When entering feature/locus name and coordinates simultaneously, only the locus name will be considered.}

\item{extra_space}{Extra space up- and downstream of and relative to the selected genomic feature (0.1 = 10 percent). Only taken into account when locus/feature name is entered - ignored when genomic coordinates are entered.  Default c(1.5,1.5).}

\item{annots}{Represents annotations related to genomic data. It is a crucial input for the function and is used to customize the visualization of genomic features. It can either be a 'pre-loaded' annotation in GRanges format by use of the ReadInAnnotations function or a named character vector providing the full paths to indidual annotations that will then be loaded by the function. If using the same set of annotations for creating multiple plots the 'pre-loaded' format is recommended. See example below.}

\item{annotation_packing}{Set the annotation packing for each annotation. Options are: ‘expanded’, ‘squished’, ‘collapsed’ and ‘collapsed2’. 'expanded' and 'squished' display the detailed structures of transcripts under a given feature either as fully expanded or squished. ’collapsed’ collapses all overlapping features into one 'super-exon' feature whereas 'collapsed2’ only collapses features belonging to the same locus into one 'super-exon' feature.}

\item{annot_cols}{Specify the color(s) used to visualize the annotated features (by color name or hex code). If set to NULL, the colors specified in the bed file will be used.}

\item{annot_panel_color}{Color of the titles of the annotation(s) depicted in the left panel (as color name or hex code).}

\item{annot_panel_font_size}{Font size of the titles of the annotation(s) depicted in the left panel. Will be determined automatically by default.}

\item{bin_start}{Center the bins around the given genomic position. Provide an integer value that lies within the plotted region. Per default the bin center will be at the 5'-end of the plotted region if it is defined by genomic coordinates and at the 5'-end of the locus if the plotted region is defined by locus name.}

\item{bin_size}{Integer value (>1). Default: 'auto';  the bin size will be automatically determined. The lower the value, the slower the plotting.}

\item{bins_per_cm}{Number of bins to display per centimeter. Only relevant if 'Bin Size' is automatically determined. Default 250 bins/cm.}

\item{track_width_cm}{Specify the width in centimeters for the sequencing track window of the plot (full plot width will be determined based on this value). Default 12 cm.}

\item{full_width_cm}{If track_width_cm is not specified (=NULL), you can specify the width in centimeters for the full plot. We recommend to set this argument to NULL to allow this to be determined automatically based on the specified track_width_cm and panels_max_width_cm. Default NULL.}

\item{full_height_cm}{Specify the plot height in centimeters. We recommend to set this argument to NULL to allow this to be determined automatically based on the number of tracks to display and the specified track_height_cm. Default NULL.}

\item{track_height_cm}{Height in centimeters of each sequencing track (full plot height will be influenced by this value). Positive numeric value. Default 0.3 cm. Recommended value between 0.2 and 1.0 cm.}

\item{title_field_height_cm}{Height in centimeters of the title field (full plot height will be influenced based on this value). Positive numeric value. Default 0.66 cm. Will be ignored if the field is set too small to fit the font size selected for the title.}

\item{genomic_scale_height_cm}{Height in centimeters of the genomic scale field (full plot height will be influenced based on this value). Positive numeric value. Default 0.24 cm.}

\item{annotation_height_cm}{Height in centimeters of each line in the annotation track (full plot height will be influenced based on this value). Positive numeric value. Default 0.24 cm.}

\item{spacer_height_cm}{Height in centimeters of each spacer line used in the plot (full plot height will be influenced based on this value).Positive numeric value. Default 0.06 cm.}

\item{panels_max_width_cm}{Maximum width in cm that can be occupied by the sample labels panel (to the left of tracks; 'auto' or a positive numeric value). Text truncation may occur if the value makes the panel too narrow.}

\item{margin_width_cm}{Specify the size in centimeters of the margins on each side of the sequencing tracks. 0.05 cm per default.}

\item{fixed_panel_width}{A logical value (TRUE/FALSE) . Specify if the tracks labels should mandatorily occupy all the space provided in panels_max_width_cm or if they can use less should it be possible. Ignored if panels_max_width_cm='auto'.}

\item{horizontal_panels_list}{List of boolean vectors indicating whether text in the individual subpanels in the 'sample overview panel' should be displayed horizontally (TRUE) or vertically (FALSE). The list should be provided in the following format: list('dataset1'=c(TRUE,FALSE,FALSE,TRUE), 'dataset2'=c(TRUE,TRUE), ...). If NULL, an automatic assignment based on available space will be performed.}

\item{panel_font_sizes}{Font size(s) for panel text. Provide either one integer (applied to all panel text), two comma-separated integers (the first for the left-most panel, the second one for all subsequent panels), or X comma-separated integer where X corresponds to the largest number of subgroups (incl. dataset). Will be automatically assigned if argument is set to NULL.}

\item{panel_font_size_list}{List of font sizes for each dataset and subgroup in the following format: list('dataset1'=c(12,8,6,4), 'dataset2'=c(12,6,4), ...).}

\item{panel_text_colors}{Color(s) of the panel text (as name or hex code). Provide either one color (for all) or two comma-separated colors (for datasets and subgroups).}

\item{horizontal_spacers}{A logical value (TRUE/FALSE) . Specify if a white space (horizontal) should be left between sequencing datasets tracks.}

\item{panel_separators}{Specify if horizontal,vertical line-separators should be displayed in order to clearly separate panels. c(FALSE,TRUE) by default for horizontal and vertical, respectively. If one logical value is supplied it will automatically be applied to both. Horizontal line-separators will only be displayed if horizontal_spacers=TRUE.}

\item{separators_lwds}{Weight of the line-separators. Provide either one weight or three comma-separated weight(s) to designate individual  weights for 'line-spacer', 'thickline-spacer', 'vertical-spacer', where the first two are horizontal spacers.}

\item{separators_colors}{Color(s) of the line-separators (as name or hex code). Provide either one color or three comma-separated colors to designate individual colors for 'line-spacer', 'thickline-spacer', 'vertical-spacer', where the first two are horizontal spacers.}

\item{incl_first_panel}{A logical value (TRUE/FALSE) . Should the left-most panels, which displays dataset names, be displayed? Can be omitted if all datasets consist of only one sample or if all samples are contained within one dataset.}

\item{print_one_line_sample_names}{Combine all sample 'subgroup' information in one panel - separated by points (.)  - instead of setting up multiple panels.}

\item{replicate_names}{Prefix added before replicate numbers (e.g. rep, r). NULL will lead to display of individual replicates without separate naming. NA will lead to display of replicate numbers without a prefix. Ignored when the mean of replicates is calculated.}

\item{group_autoscale}{For each dataset, specify whether to 'group' auto-scale or just auto-scale for each individual track. Named boolean vector.}

\item{incl_track_scales}{A logical value (TRUE/FALSE) . Should tracks scales be displayed (to the left of tracks).}

\item{scientific_scale}{Should scientific number format be used for the tracks scale. Options: allow, all, none. Allow is the default.}

\item{force_scale}{Provide the maximum value for the data scale (y-axis) for each dataset. Either single or two comma-separated positive numeric values. If a single value is supplied, this scaling will be applied to both strands. NULL leads to auto-scaling. Will per default be determined based on maximum value within the dataset.}

\item{scale_font_size}{Font size of the data scales.}

\item{scale_panel_width_cm}{Width in cm allocated to the tracks scale ('auto' or a positive numeric value). Ignored if incl_track_scales=FALSE.}

\item{scale_font_color}{Color of the data scales (as color name or hex code).}

\item{header}{Specify a header to be used instead of the automatically generated header based on the name of the locus/feature. If genomic coordinates are used, the title panel will be excluded per default unless specified here.}

\item{suppress_header}{Exclude the 'Header Panel' at the top of the produced plot? This argument is ignored if a header is provided manually.}

\item{header_font_sizes}{Font size(s) in the header region (integer value(s) >4). One integer or three comma-separated integers for 'main title', 'genomic range (subtitle)' and 'scale', respectively. Will be determined automatically by default.}

\item{header_font_colors}{Text colors of the header region. One color or three comma-separated colors for 'main title', 'genomic range (subtitle)' and 'scale', respectively  (use color names or hex codes). Default: black,darkgray,black.}

\item{include_genomic_scale}{A logical value (TRUE/FALSE) . Should genomic scale be displayed.}

\item{genomic_scale_on_top}{Display genomic scale above tracks (otherwise it will be displayed below).}

\item{genomic_scale_font_size}{Font size of the genomic scale (integer value >4). NULL will lead to automatic determination.}

\item{genomic_scale_font_color}{Color of the genomic scale.}

\item{incl_feature_names}{A logical value (TRUE/FALSE) . Display feature/locus names in the annotation panel.}

\item{feature_names_above}{A logical value (TRUE/FALSE) . If TRUE, feature names are displayed above features. If FALSE, feature names are displayed below features.}

\item{feature_names_alternating}{A logical value (TRUE/FALSE) . Display reverse strand features as a miror of the forward strand. Will be ignored with 'Intermingled strands display'}

\item{feature_names_font_size}{Font size of the annotated feature name. Will be determined automatically by default.}

\item{incl_feature_brackets}{A logical value (TRUE/FALSE) . Indicate with a bracket (a double-headed arrow) the full range of each locus within the plotted region. Makes most sense when displaying expanded or squished annotations.}

\item{incl_feature_shadings}{A logical value (TRUE/FALSE) . Individual loci within the plotted region can be highlighted with a shaded box of alternating colors. This may help distinguish the different loci within the plotted region. Shading colors and opacity can be specified below.}

\item{feature_shading_colors}{Shading colors. Provide two comma-separated colors (as color name or hex code).}

\item{feature_shading_alpha}{Shading opacity. Provide numeric value between 0 and 1. However, using a value  <0.2 is recommended.}

\item{center_of_mass}{A logical value (TRUE/FALSE) . Feature names will be centered based on the the transcript density within a given locus rather than the center. Probably only makes sense if there is a very long outlier transcript that moves the center away from the major transcript variants.}

\item{feature_names_font_color}{Text color of the annotated feature name (as color name or hex code).}

\item{dummy_plot}{A logical value (TRUE/FALSE) . Should a dummy plot (without sequencing data, for aestethic trials) be generated? This allows fast debugging of the Plot display parameters. Default FALSE.}

\item{pdf}{A logical value (TRUE/FALSE) . Whether to plot to pdf (TRUE) or on-screen (FALSE). Default FALSE}

\item{pdf_name}{Name of pdf file. Default NULL will lead to autogeneration of file name, which will include the bin size and header or feature name. If none of those are defined the file name will include the date.}

\item{pdf_dir}{Directory where the pdf file should be stored. Default './testplotting'.}

\item{scaling_factor}{Specify a scaling factor to apply for on-screen display - ignored when exporting to pdf (pdf=TRUE).}

\item{verbosity}{Level of information displayed in console.}

\item{interface}{'R' or 'shiny' (determines whether output relates to variable names in R or shiny)}

\item{...}{}
}
\value{
A customized "genome-browser" plot
}
\description{
Main plotting function
}
\examples{
datasets <- list(
  '3-seq' = list(
    '-PAP' = list(
      'total' = c("siCTRL", "siEXOSC3"),
      '4sU' = c("siCTRL", "siEXOSC3")
    ),
   '+PAP' = list(
     'total' = c("siCTRL", "siEXOSC3"),
     '4sU' = c("siCTRL", "siEXOSC3")
    )
  ),
  'TT-seq' = c("siCTRL", "siEXOSC3"),
  'RNA-seq' = c("siCTRL", "siEXOSC3"),
  'ChIP-seq' = "RNAPII"
)

colors <- list(
  '3-seq' = list(
    '-PAP' = list(
      'total' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E"),
      '4sU' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E")
    ),
    '+PAP' = list(
      'total' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E"),
      '4sU' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E")
    )
  ),
  'TT-seq' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E"),
  'RNA-seq' = c("siCTRL" = "#505160", "siEXOSC3" = "#68829E"),
  'ChIP-seq' = c("RNAPII" = "#2A3132")
)

bigwigs <- list(
  '+' = list(
    '3-seq' = list(
      '-PAP' = list(
        'total' = list(
          siCTRL = c(
            "siGFP_noPAP_in_batch1_plus.bw",
            "siGFP_noPAP_in_batch2_plus.bw",
            "siGFP_noPAP_in_batch3_plus.bw"
          ),
          siEXOSC3 = c(
            "siRRP40_noPAP_in_batch1_plus.bw",
            "siRRP40_noPAP_in_batch2_plus.bw",
            "siRRP40_noPAP_in_batch3_plus.bw"
          )
        ),
        '4sU' = list(
          siCTRL = c(
            "siGFP_noPAP_ip_batch1_plus.bw",
            "siGFP_noPAP_ip_batch3_plus.bw"
          ),
          siEXOSC3 = c(
            "siRRP40_noPAP_ip_batch1_plus.bw",
            "siRRP40_noPAP_ip_batch3_plus.bw"
          )
        )
      ),
      '+PAP' = list(
        'total' = list(
          siCTRL = c(
            "siGFP_xPAP_in_batch1_plus.bw",
            "siGFP_xPAP_in_batch2_plus.bw",
            "siGFP_xPAP_in_batch3_plus.bw"
          ),
          siEXOSC3 = c(
            "siRRP40_xPAP_in_batch1_plus.bw",
            "siGFP_xPAP_in_batch2_plus.bw",
            "siRRP40_xPAP_in_batch3_plus.bw"
          )
        ),
        '4sU' = list(
          siCTRL = c(
            "siGFP_xPAP_ip_batch1_plus.bw",
            "siGFP_xPAP_ip_batch3_plus.bw"
          ),
          siEXOSC3 = c(
            "siRRP40_xPAP_ip_batch1_plus.bw",
            "siRRP40_xPAP_ip_batch3_plus.bw"
          )
        )
      )
    ),
    'TT-seq' = list(
      siCTRL = c(
        "L_EGFP_rep1_tt_corr_ff_noJncReads_plus.bw",
        "L_EGFP_rep2_tt_corr_ff_noJncReads_plus.bw"
      ),
     siEXOSC3 = c(
        "L_RRP40_rep1_tt_corr_ff_noJncReads_plus.bw",
        "L_RRP40_rep2_tt_corr_ff_noJncReads_plus.bw"
      )
    ),
    'RNA-seq' = list(
      siCTRL = c(
        "T_EGFP_rep1_tt_corr_plus.bw",
        "T_EGFP_rep2_tt_corr_plus.bw"
      ),
      siEXOSC3 = c(
        "T_RRP40_rep1_tt_corr_plus.bw",
        "T_RRP40_rep2_tt_corr_plus.bw"
      )
    ),
    'ChIP-seq' = list(
      RNAPII = c(
        "GSM2642506_WIGfs_Hela-H9_WT_siFFL_Pol_II_N20_MA733_bin50_Scaled_BGSub_Hg38.bw",
        "GSM2642508_WIGfs_Hela-H9_WT_siFFL_Pol_II_N20_MA736_bin50_Scaled_BGSub_Hg38.bw"
      )
    )
  ),
  '-' = list(
    '3-seq' = list(
      '-PAP' = list(
        'total' = list(
          siCTRL = c(
            "siGFP_noPAP_in_batch1_minus.bw",
            "siGFP_noPAP_in_batch2_minus.bw",
            "siGFP_noPAP_in_batch3_minus.bw"
          ),
          siEXOSC3 = c(
            "siRRP40_noPAP_in_batch1_minus.bw",
            "siRRP40_noPAP_in_batch2_minus.bw",
            "siRRP40_noPAP_in_batch3_minus.bw"
          )
        ),
        '4sU' = list(
          siCTRL = c(
            "siGFP_noPAP_ip_batch1_minus.bw",
            "siGFP_noPAP_ip_batch3_minus.bw"
          ),
          siEXOSC3 = c(
            "siRRP40_noPAP_ip_batch1_minus.bw",
            "siRRP40_noPAP_ip_batch3_minus.bw"
          )
        )
      ),
      '+PAP' = list(
        'total' = list(
          siCTRL = c(
            "siGFP_xPAP_in_batch1_minus.bw",
            "siGFP_xPAP_in_batch2_minus.bw",
            "siGFP_xPAP_in_batch3_minus.bw"
          ),
          siEXOSC3 = c(
            "siRRP40_xPAP_in_batch1_minus.bw",
            "siGFP_xPAP_in_batch2_minus.bw",
            "siRRP40_xPAP_in_batch3_minus.bw"
          )
        ),
        '4sU' = list(
          siCTRL = c(
            "siGFP_xPAP_ip_batch1_minus.bw",
            "siGFP_xPAP_ip_batch3_minus.bw"
          ),
          siEXOSC3 = c(
            "siRRP40_xPAP_ip_batch1_minus.bw",
            "siRRP40_xPAP_ip_batch3_minus.bw"
          )
        )
      )
    ),
    'TT-seq' = list(
      siCTRL = c(
        "L_EGFP_rep1_tt_corr_ff_noJncReads_minus.bw",
        "L_EGFP_rep2_tt_corr_ff_noJncReads_minus.bw"
      ),
      siEXOSC3 = c(
        "L_RRP40_rep1_tt_corr_ff_noJncReads_minus.bw",
        "L_RRP40_rep2_tt_corr_ff_noJncReads_minus.bw"
      )
    ),
    'RNA-seq' = list(
      siCTRL = c(
        "T_EGFP_rep1_tt_corr_minus.bw",
        "T_EGFP_rep2_tt_corr_minus.bw"
      ),
      siEXOSC3 = c(
        "T_RRP40_rep1_tt_corr_minus.bw",
        "T_RRP40_rep2_tt_corr_minus.bw"
      )
    )
  )
)

bigwig_dirs <- c(
  '3-seq' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_3pseq/",
  'TT-seq' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_TTseq/",
  'RNA-seq' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_RNAseq/",
  'ChIP-seq' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_ChIPseq/"
)

parameters <- list(
  '3-seq' = list(
    whichSamples = NULL,
    bin_stats = "max",
    enhance_signals = TRUE,
    log2transform = FALSE,
    pseudoCount = 1,
    batchCorrect = TRUE,
    batch = c(1, 2, 3, 1, 2, 3, 1, 3, 1, 3, 1, 2, 3, 1, 2, 3, 1, 3, 1, 3),
    whichReps = NULL,
    negative_valued_bw = FALSE,
    calcMean = TRUE,
    negValsSet0 = TRUE,
    force_scale = c(NA, NA),
    group_autoscale = TRUE
  ),
  'ChIP-seq' = list(
    whichSamples = NULL,
    bin_stats = "mean",
    enhance_signals = TRUE,
    log2transform = FALSE,
    pseudoCount = 1,
    batchCorrect = FALSE,
    batch = NULL,
    whichReps = NULL,
    negative_valued_bw = FALSE,
    calcMean = TRUE,
    negValsSet0 = TRUE,
    force_scale = c(NA, NA),
    group_autoscale = TRUE
  ),
  'RNA-seq' = list(
    whichSamples = NULL,
    bin_stats = "mean",
    enhance_signals = FALSE,
    log2transform = FALSE,
    pseudoCount = 1,
    batchCorrect = TRUE,
    batch = c(1, 2, 1, 2),
    whichReps = NULL,
    negative_valued_bw = FALSE,
    calcMean = TRUE,
    negValsSet0 = TRUE,
    force_scale = c(NA, NA),
    group_autoscale = TRUE
  ),
  'TT-seq' = list(
    whichSamples = NULL,
    bin_stats = "mean",
    enhance_signals = FALSE,
    log2transform = FALSE,
    pseudoCount = 1,
    batchCorrect = TRUE,
    batch = c(1, 2, 2, 2),
    whichReps = NULL,
    negative_valued_bw = FALSE,
    calcMean = TRUE,
    negValsSet0 = TRUE,
    force_scale = c(NA, NA),
    group_autoscale = TRUE
  )
)

annotation_files <- c(
  'gencode v38' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/annotations/gencode.v38.annotation.bed",
  'in-house' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/annotations/HeLa_major_isoform_hg38_gc34.bed",
  'ChIP-peaks' = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/annotations/RNAPII_ChIP_peaks.bed"
)

annots = ReadInAnnotations(annotation_files)

seqNdisplay(datasets, colors, bigwig_dirs, bigwigs, parameters, feature='LMO4', annots=annotation_files, incl_feature_names=c('ChIP-peaks'=FALSE, 'gencode v38'=TRUE, 'in-house'=TRUE), annot_cols=list('ChIP-peaks'='black', 'gencode v38'='black', 'in-house'=NULL))

seqNdisplay(datasets, colors, bigwig_dirs, bigwigs, parameters, feature='LMO4', annots=annots, incl_feature_names=c('ChIP-peaks'=FALSE, 'gencode v38'=TRUE, 'in-house'=TRUE), annot_cols=list('ChIP-peaks'='black', 'gencode v38'='black', 'in-house'=NULL))

}
\author{
SLA
}
