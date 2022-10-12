###

# THE BUG


library(seqNdisplayR)
  
xl_fname = system.file('extdata', 'seqNdisplayR_sample_sheet_elaborate2.xlsx',
                       package='seqNdisplayR')

session = load_excel(xl_fname, load_annotations = T)

plot(session, feature='LMO4')

session[['parameters']][['3-seq']][['calcMean']] = FALSE

plot(session, feature='LMO4')


###
feature = 'LMO4' # session$feature # 'LMO4'  "NEAT1"  feat  'TAF1D'  NULL
locus = session$locus # NULL  c("chr11", "-", 93648282, 93866057)

datasets=session$samples
colors=session$colors
bigwig_dirs=session$bigwig_dirs
bigwigs=session$bigwigs
parameters=session$parameters

annots=session$annots

plotting_segment_order = session$plotting_segment_order #  NULL
preloaded_tracks = session$preloaded_tracks #  NULL  pre_loaded_tracks  TAF1D_tracks  LMO4_tracks  NULL
output_tracks = session$output_tracks #
output_parameters = session$output_parameters #  FALSE
input_parameters = session$input_parameters # NULL
both_strands = session$both_strands # TRUE
strands_intermingled = session$strands_intermingled # TRUE  FALSE
intermingled_color = session$intermingled_color  #
neg_vals_neg_strand = session$neg_vals_neg_strand # FALSE  TRUE
actual_strand_direction = session$actual_strand_direction # TRUE
alternating_background = session$alternating_background # TRUE
bgr_colors = session$bgr_colors # c('C1B49A', 'F1F1F2')
bgr_alpha = session$bgr_alpha # 0.2
strands_alpha = session$strands_alpha # c(100,100)
intermingled_color = session$intermingled_color
extra_space = session$extra_space # c(0.5,0.5)
annotation_packing = session$annotation_packing # 'collapsed2'  'collapsed'  'expanded'
annot_cols = session$annot_cols # NULL
annot_panel_color = session$annot_panel_color # 'steelblue'
annot_panel_font_size = session$annot_panel_font_size # NULL
bin_start = session$bin_start # NULL
bin_size = session$bin_size # 'auto'
bins_per_cm = session$bins_per_cm # 250
track_width_cm = session$track_width_cm # 12 NULL
full_width_cm = session$full_width_cm # NULL 15
full_height_cm = session$full_height_cm # NULL
track_height_cm = session$track_height_cm # 0.5
title_field_height_cm = session$title_field_height_cm # 0.66
genomic_scale_height_cm = session$genomic_scale_height_cm # 0.24
annotation_height_cm = session$annotation_height_cm # 0.24
spacer_height_cm = session$spacer_height_cm # 0.06
panels_max_width_cm = session$panels_max_width_cm # 'auto' 2
margin_width_cm = session$margin_width_cm # 0.05
fixed_panel_width = session$fixed_panel_width # FALSE
horizontal_panels_list = session$horizontal_panels_list # NULL
panel_font_sizes = session$panel_font_sizes # NULL
panel_font_size_list = session$panel_font_size_list # NULL
panel_text_colors = session$panel_text_colors # c('darkgreen', 'black')
horizontal_spacers = session$horizontal_spacers # TRUE
panel_separators = session$panel_separators # c(FALSE, TRUE)
separators_lwds = session$separators_lwds # c(0.5, 1, 0.5)
separators_colors = session$separators_colors # 'black'
incl_first_panel = session$incl_first_panel # TRUE
print_one_line_sample_names = session$print_one_line_sample_names # FALSE
replicate_names = session$replicate_names # 'rep'  NULL  ""
group_autoscale=FALSE # group_autoscale=session$group_autoscale # FALSE
incl_track_scales = session$incl_track_scales # TRUE
scientific_scale = session$scientific_scale # c('allow', 'all', 'none')[1]
force_scale = session$force_scale # NULL  list('+'20, 'RNA-seq'20, '3-seq'
scale_font_size = session$scale_font_size # NULL
scale_panel_width_cm = session$scale_panel_width_cm # 'auto' 0.6
scale_font_color = session$scale_font_color # 'darkred'
header = session$header # NULL
suppress_header = session$suppress_header # FALSE
header_font_sizes = session$header_font_sizes # NULL
header_font_colors = session$header_font_colors # c('black', 'darkgray', 'black')
include_genomic_scale = session$include_genomic_scale # TRUE
genomic_scale_on_top = session$genomic_scale_on_top # TRUE
genomic_scale_font_size = session$genomic_scale_font_size # NULL
genomic_scale_font_color = session$genomic_scale_font_color # 'black'
incl_feature_names = session$incl_feature_names # session$incl_feature_names
feature_names_above = session$feature_names_above # session$feature_names_above
feature_names_alternating = session$feature_names_alternating # TRUE
feature_names_font_size = session$feature_names_font_size # NULL
incl_feature_brackets = session$incl_feature_brackets # FALSE  session$incl_feature_brackets
incl_feature_shadings = session$incl_feature_shadings # FALSE  session$incl_feature_shadings
feature_shading_colors = session$feature_shading_colors # c('steelblue', 'hotpink')
feature_shading_alpha = session$feature_shading_alpha # 0.05
center_of_mass = session$center_of_mass # FALSE
feature_names_font_color = session$feature_names_font_color # 'black'
dummy_plot = session$dummy_plot # FALSE
pdf = session$pdf # FALSE
pdf_name = session$pdf_name # NULL
pdf_dir = session$pdf_dir # './testplotting'
scaling_factor = session$scaling_factor # 1
verbosity  = session$verbosity  #  'detailed'
interface = 'R' # session$interface # 'R'

source("/Users/au103725/OneDrive - Aarhus Universitet/THJ LAB/Projects/seqNdisplayR/seqNdisplayR/R/seqNdisplayR.R")

# seqNdisplayR function up until OrganizePanelsDimensions is used (~ l. 462)

# PREPARE ARGUMENTS FOR WORKING WITH FUNCTION

.strand = '+'
datasets	=	names(.tracks.listed[[.strand]])
min_word_length	=	.min.wordlength.left.panel
replicate_names	=	replicate_names
print_one_line_sample_names	=	print_one_line_sample_names
incl_first_panel	=	incl_first_panel
plot_height_parameters	=	.plot.height.parameters[[.strand]]
feature_names_font_size	=	.feature.names.font.size
font_size_range	=	which(!is.na(.letter.heights))
recommended_font_sizes	=	.rec.font.sizes
scale_font_size	=	scale_font_size
horizontal_panels_list	=	horizontal_panels_list
panel_font_size_list	=	.panel.font.size.list
panels_list	=	.panels.list[[.strand]]
plot_widths_cm	=	.plot.widths.cm
panel_separators	=	.panel.separators
both_strands=both_strands
strands_intermingled	=	.strands.intermingled
stranded_samples	=	.stranded.datasets
fixed_panel_width	=	fixed_panel_width



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
