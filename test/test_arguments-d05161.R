### seqNdisplayR
#library(seqNdisplayR)
library(RCurl)

## testing various things
if (FALSE){

#0 solved
session = load_excel("/Users/au103725/OneDrive - Aarhus Universitet/THJ LAB/Projects/seqNdisplayR/seqNdisplayR/inst/extdata/seqNdisplayR_sample_sheet_simple.xlsx", load_annotations = T)
print(session, verbose=TRUE)
plot(session, feature='LMO4', verbosity='detailed')


#1 solved
rtracklayer::import(.bw.file, which=GenomicRanges::GRanges(seqnames=.chrom, ranges=IRanges::IRanges(start=.chrom.start, end=.chrom.end)), as='NumericList')[[1]]

.bw.file = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_RNAseq/T_EGFP_rep1_tt_corr_plus.bw"
bw = rtracklayer::import(.bw.file, which=GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=87308012, end=87369379)))
seqinfo(bw)

# dealing with error of type: 'which' contains seqnames not known to BigWig file: chr1
.bw.file = "http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Human/THJ/2020_Polak_3pseq/siLUC_minus.bw"
bw = rtracklayer::import(.bw.file)
bw = rtracklayer::import(.bw.file, which=GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=87308012, end=87369379)))
seqinfo(bw)


.chrom = 'chr1'
.chrom.start = 87308012
.chrom.end = 87369379
.bw.file = "http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Human/THJ/2020_Polak_3pseq/siLUC_minus.bw"
bw = rtracklayer::import(.bw.file, which=GenomicRanges::GRanges(seqnames=.chrom, ranges=IRanges::IRanges(start=.chrom.start, end=.chrom.end)), as='NumericList')[[1]]
bw = tryCatch(
        expr = {
          rtracklayer::import(.bw.file, which=GenomicRanges::GRanges(seqnames=.chrom, ranges=IRanges::IRanges(start=.chrom.start, end=.chrom.end)), as='NumericList')[[1]]
        },
        error = function(e){
          .alt.chrom = strsplit(.chrom, split='chr')[[1]][2]
          rtracklayer::import(.bw.file, which=GenomicRanges::GRanges(seqnames=.alt.chrom, ranges=IRanges::IRanges(start=.chrom.start, end=.chrom.end)), as='NumericList')[[1]]
        },
        warning = function(w){
          .alt.chrom = strsplit(.chrom, split='chr')[[1]][2]
          rtracklayer::import(.bw.file, which=GenomicRanges::GRanges(seqnames=.alt.chrom, ranges=IRanges::IRanges(start=.chrom.start, end=.chrom.end)), as='NumericList')[[1]]
        }
      )


mm_bed = rtracklayer::import.bed("http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Mouse/Annotations/RefSeq/mm10_Refseq.bed")
mm_bed[mcols(mm_bed)$name==gname]
.chrom = 'chr1'
.chrom.start = 25067550
.chrom.end = 25830205
.bw.file = "http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Mouse/THJ/2019_Garland_ChIP-seq_RNAseq_ITC-seq_RIP-seq/ITC-seq/mm10/E14_1_U.minus.tpm.bw"
.bw.file = "http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Mouse/THJ/2019_Garland_ChIP-seq_RNAseq_ITC-seq_RIP-seq/ITC-seq/mm10/E14_1_B.minus.tpm.bw"
  #"http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Mouse/THJ/2019_Garland_ChIP-seq_RNAseq_ITC-seq_RIP-seq/ChIP-seq/mm10/b1.E14-WT-Input.up_to_2_times.shifted.TPM.bw"
bw = rtracklayer::import(.bw.file, which=GenomicRanges::GRanges(seqnames=.chrom, ranges=IRanges::IRanges(start=.chrom.start, end=.chrom.end)), as='NumericList')[[1]]
bw = rtracklayer::import(.bw.file)

# chr1:+:25067550:25830205


#2 solved
session = load_excel('/Users/au103725/Downloads/Iasillo.xlsx', load_annotations = T)
plot(session, feature='LMO4')

session = load_excel('/Users/au103725/Downloads/Iasillo2.xlsx', load_annotations = T)
plot(session, feature='LMO4')

session = load_excel('/Users/au103725/Downloads/Iasillo3.xlsx', load_annotations = T)
plot(session, feature='LMO4')



library(RCurl)
url.exists(.bw.file)
.bw.file = "http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Human/THJ/2016_Iasillo_ChIP-seq_RNAseq/ChIPseq/hg38/GSM2642491_WIGfs_Hela-H9_WT_siFFL_Input_CI623_bin50_Scaled_Hg38.bw"
bw = rtracklayer::import(.bw.file, which=GenomicRanges::GRanges(seqnames="chr1", ranges=IRanges::IRanges(start=87308012, end=87369379)))
seqinfo(bw)

#2b solved

session = load_excel('/Users/au103725/Downloads/Neat1_seqNdisplayR.xlsx', load_annotations = T)
## go to #generic
plot(session, feature='NEAT1')
session$parameters$CAGE$whichSamples = NA
session$parameters$`RNA-seq`$whichSamples = NA
session$parameters$`TT-seq`$whichSamples = NA
session$parameters$`chRNA-seq`$whichSamples = NA
session$parameters$eCLIP$whichSamples = NA
plot(session, feature='NEAT1')
session$parameters$FANTOM_CAGE$whichSamples = NA
session$parameters$Nutlin$whichSamples = NA
session$parameters$`CPSF73 ChIP`$whichSamples = NA
session$parameters$`CSTF64 ChIP`$whichSamples = NA
plot(session, feature='NEAT1')
session$parameters$`3'end-seq`$whichSamples = list('noPAP'=c('RRP40', 'RRP40xINTS11', 'RRP40xCPSF73'), 'xPAP'=c('RRP40', 'RRP40xINTS11', 'RRP40xARS2', 'RRP40xCPSF73'))
plot(session, feature='NEAT1')

session$parameters$`3'end-seq`$whichSamples = list('RRP40'=c('noPAP', 'xPAP'), 'RRP40xINTS11'=c('noPAP', 'xPAP'), 'RRP40xCPSF73'=c('noPAP', 'xPAP'), 'RRP40xARS2'=c('noPAP', 'xPAP'))
plot(session, feature='NEAT1')


#3 solved
session = load_excel('/Users/au103725/Downloads/small_seq_issue_excel.xlsx', load_annotations = T)
plot(session, feature='NEAT1')

plot(session, feature='NEAT1', both_strands=FALSE)

#4 solved
session = load_excel('/Users/au103725/Downloads/Argument_zero_data.xlsx', load_annotations = T)
plot(session, feature='NEAT1', panel_font_sizes=6, horizontal_panels_list=list('2015_Shiekhattar_pA+_RNA-seq'=c(F,T,T), '2020_Ruiz_pA+_RNA-seq'=c(F,T)))
## go to #generic

#5 solved
xl_fname = system.file('extdata', 'seqNdisplayR_sample_sheet_elaborate.xlsx', package='seqNdisplayR')
session = load_excel(xl_fname, load_annotations = T)

plot(session, feature='LMO4', extra_space=c(0.7,0.7), track_width_cm=10)
plot(session, feature='LMO4', extra_space=c(0.7,0.7), track_width_cm=10, print_one_line_sample_names=TRUE)

session$print_one_line_sample_names = TRUE

#6

xl_fname = '/Users/au103725/Dropbox/Lab stuff/Scripts_for_the_people/PAPER/Figures/shiny_app/Figure_S5/sNdR_Fig_S5A.xlsx'
session = load_excel(xl_fname, load_annotations = T)
plot(session, feature='LMO4', extra_space=c(0.7,0.7), track_width_cm=10)


#7

xl_fname = '/Users/au103725/Dropbox/Lab stuff/Scripts_for_the_people/PAPER/Figures/shiny_app/Figure_4/sNdR_Fig_4C.xlsx'
session = load_excel(xl_fname, load_annotations = T)
plot(session, feature='LMO4')


## go to #generic


}

xl_fname = system.file('extdata', 'seqNdisplayR_sample_sheet_simple.xlsx', package='seqNdisplayR')
session = load_excel(xl_fname, load_annotations = T)

session = load_excel('inst/extdata/seqNdisplayR_sample_sheet_simple.xlsx', load_annotations = T)

#generic
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
neg_vals_neg_strand = session$neg_vals_neg_strand # FALSE  TRUE
actual_strand_direction = session$actual_strand_direction # TRUE
alternating_background = session$alternating_background # TRUE
bgr_colors = session$bgr_colors # c('C1B49A', 'F1F1F2')
bgr_alpha = session$bgr_alpha # 0.2
strands_alpha = session$strands_alpha # c(100,100)
feature = 'LMO4' # session$feature # 'LMO4'  "NEAT1"  feat  'TAF1D'  NULL
locus = session$locus # NULL  c("chr11", "-", 93648282, 93866057)
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

### OrganizeAnnotatedFeaturesInRegion
.strand = '+'
plotted_region=.plotted.region[[.strand]]
annotations=.annotations

### AnnotatedFeaturesInRegion



###  FinalizePlottingSegmentOrder

plotting_segment_order=.plotting.segment.order
tracks_listed=.tracks.listed
both_strands=both_strands
include_genomic_scale=include_genomic_scale
genomic_scale_on_top=genomic_scale_on_top
any_stranded_beds=.any.stranded.beds
any_unstranded_beds=.any.unstranded.beds
strands_intermingled=.strands.intermingled



### EstimatePlotHeights

.strand = "-"

annot_info=.annot.info[[.strand]]
incl_feature_names=.incl.feature.names
annotation_packing=.annotation.packing
incl_feature_brackets=.incl.feature.brackets
plotting_segment_order=.plotting.segment.order[[.strand]]
tracks_listed=.tracks.listed[[.strand]]
track_height_cm=track_height_cm
full_height_cm=full_height_cm
stranded_beds=.stranded.beds[[.strand]]

### PlotHeightParameters

.strand = "+"

annot_info	=	.annot.info[[.strand]]
track_vector	=	.estimated.plot.heights[[.strand]][['track.vector']]
max_annot_lines	=	.estimated.plot.heights[[.strand]][['max.annot.lines']]
annot_heights	=	.estimated.plot.heights[[.strand]][['annot.heights']]
letter_widths	=	.letter.heights
incl_feature_names	=	.incl.feature.names
feature_text_org	=	.feature.text.org[[.strand]]
annotation_packing	=	.annotation.packing
incl_feature_brackets	=	.incl.feature.brackets
track_height_cm	=	track_height_cm
full_height_cm	=	full_height_cm
stranded_beds	=	.stranded.beds[[.strand]]

### BasicPlotParameters

.strand= "+"

plotted_strand	=	.strand
plotted_region	=	.plotted.region
feature_names_font_size	=	.feature.names.font.size
plot_height_parameters	=	.plot.height.parameters
plot_width_parameters	=	.plot.width.parameters
full_width_cm	=	.full.width.cm
full_height_cm 	=	full_height_cm
track_height_cm =	track_height_cm
plot_vertical_parameters	=	plot_vertical_parameters
bin_size	=	.bin.size
bins_per_cm	=	bins_per_cm
plotting_segment_order	=	.plotting.segment.order
tracks_listed	=	.tracks.listed
unstranded_beds	=	.unstranded.beds


### AlignBasicPlotParameters

basic_plot_parameters = structure(lapply(names(.plotted.region), function(.strand) BasicPlotParameters(.strand, .plotted.region, .feature.names.font.size, .plot.height.parameters, .plot.width.parameters, .full.width.cm, full_height_cm, track_height_cm, plot_vertical_parameters, .bin.size, bins_per_cm, .plotting.segment.order, .tracks.listed, .unstranded.beds)), names=names(.plotted.region))
both_strands = both_strands
strands_intermingled = .strands.intermingled

### OrganizePanelsDimensions

.strand = '+'

#x = OrganizePanelsDimensions(names(.tracks.listed[[.strand]]), .min.wordlength.left.panel, replicate_names, print_one_line_sample_names, incl_first_panel, .plot.height.parameters[[.strand]], .feature.names.font.size, which(!is.na(.letter.heights)), .rec.font.sizes, scale_font_size, .horizontal.panels.list, .panel.font.size.list, .panels.list[[.strand]], .plot.widths.cm, .panel.separators, .strand, both_strands, .strands.intermingled, .stranded.datasets, fixed_panel_width, .verbosity)

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

### PlotSegment

feature=feature
plotted_region=.plotted.region
plotted_strand=.plotted.strand
both_strands=both_strands
plotting_segment=.plotting.segment
basic_plot_parameters=.basic.plot.parameters
neg_vals_neg_strand=.neg.vals.neg.strand
plot_width_parameters=.plot.width.parameters
plot_vertical_parameters=plot_vertical_parameters
annot_info=.annot.info
panel_info=.panel.info
panels_list=.panels.list
panel_separators=.panel.separators
separators_lwds=.separators.lwds
separators_colors=.separators.colors
header=header
header_font_sizes=.header.font.sizes
scaling_factor=.scaling.factor
full_width_cm=.full.width.cm
genomic_scale_on_top=genomic_scale_on_top
genomic_scale_font_size=.genomic.scale.font.size
actual_strand_direction=actual_strand_direction
bin_stats=.bin.stats
dummy_plot=dummy_plot
tracks=.tracks
strands_alpha=.strands.alpha
annotation_packing=.annotation.packing
annotation_panel_font_size=.annotation.panel.font.size
incl_feature_names=.incl.feature.names
feature_font_size=.feature.names.font.size
feature_names_above=.feature.names.above
final_feature_text_org=.final.feature.text.org
incl_feature_brackets=.incl.feature.brackets
incl_feature_shadings=.incl.feature.shadings
feature_shading_colors=feature_shading_colors
feature_shading_alpha=feature_shading_alpha
annot_cols=annot_cols
incl_track_scales=incl_track_scales
scientific_scale=scientific_scale
scale_font_size=.scale.fontsize
force_scale=force_scale
colors=colors
alternating_background=alternating_background
bgr_colors=bgr_colors
bgr_alpha=bgr_alpha
font_colors=.font.colors
letter_widths=.letter.widths
letter_heights=.letter.heights
enhance_signals=.enhance.signals
first_plot=.first.plot


### PlotPanels

plotting_segment=plotting_segment
plotted_strand=.strand
panel_info=panel_info
panels_list=panels_list
panel_separators=panel_separators
separators_lwds=separators_lwds
separators_colors=separators_colors
incl_first_panel=incl_first_panel
print_one_line_sample_names=print_one_line_sample_names
replicate_names=replicate_names
plot_width_parameters=plot_width_parameters
windows_height=.windows.height
vertical_slots=.vertical.slots
segment_top=.segment.top
full_width_cm=full_width_cm
font_color=font_colors
font_family=.font.family
colors=colors
first_plot=.first.plot
letter_heights=letter_heights
scaling_factor=scaling_factor

### PlotAnnotation
annot_info	=	.unstranded.annot.info # annot_info
stranded=.stranded
annot_cols	=	annot_cols
annotation_packing	=	annotation_packing
plotted_region	=	.plotted.region
plotted_strand	=	plotted_strand
substrand	=	.strand
basic_plot_parameters	=	basic_plot_parameters
plot_start	=	.plot.start
plot_end	=	.plot.end
plot_width	=	.plot.width
bin_size	=	.bin.size
actual_strand_direction	=	actual_strand_direction
incl_feature_names	=	incl_feature_names
feature_names_above	=	feature_names_above
incl_feature_brackets	=	incl_feature_brackets
incl_feature_shadings	=	incl_feature_shadings
feature_shading_colors	=	feature_shading_colors
feature_shading_alpha	=	feature_shading_alpha
plot_width_parameters	=	plot_width_parameters
plot_vertical_parameters	=	plot_vertical_parameters
final_feature_text_org	=	final_feature_text_org
windows_height	=	.windows.height
feature_font_size	=	feature_font_size
annotation_panel_font_size	=	annotation_panel_font_size
annot_panel_dist	=	annot_panel_dist
coords_tracks	=	.coords.tracks
font_colors	=	font_colors
font_family	=	.font.family
first_plot	=	first_plot
scaling_factor	=	scaling_factor



### SegmentTop

plotting_segment=plotting_segment
plotted_strand=.strand
windows_height=.windows.height
annot_info=annot_info
dummy_plot=dummy_plot
tracks=tracks

### UnpackSamples

seqtype=seqtype
samples=samples
which_samples=.which.samples
which_reps=.which.reps
bigwig_list=bigwigs[[.strand]]
bigwig_dirs=bigwig_dirs



### LoadAndTransformDataForTrack

.strand = '+'
.seqtype = names(datasets)[1]

#.tracks.listed = list()
.tracks.listed[[.strand]][[.seqtype]] = LoadAndTransformDataForTrack(.seqtype, .plotted.region[[.strand]], datasets, bigwigs, bigwig_dirs, parameters, get_subsamples=FALSE, print_order=FALSE, .verbosity)

seqtype=.seqtype
plotted_region=.plotted.region[[.strand]]
samples=datasets
bigwigs=bigwigs
bigwig_dirs=bigwig_dirs
parameters=parameters
get_subsamples=FALSE
print_order=FALSE

### UnpackSamples

seqtype=.seqtype
samples=datasets
.param = parameters[[seqtype]]
which_samples = .param[['whichSamples']]
which_reps = .param[['whichReps']]
bigwig_list=bigwigs[[.strand]]
#bigwig_dirs
#verbosity

### SortUnlistedSampleNames

unlisted_which_sample=.unlisted.which.samples
unlisted_sample_names=.unlisted.sample.names
incl_rep=F



