# Annotation reading and processing

# Internal debug helper.  Tagged trace `message()` calls ([c2names], [exp-*],
# [bk-fallback], etc.) are routed through `.dbg()` so they stay silent by
# default.  Re-enable with `options(seqNdisplayR.debug = TRUE)` to bring back
# the full troubleshooting log, or set it back to FALSE / unset to silence.
.dbg <- function(...) {
  if (isTRUE(getOption("seqNdisplayR.debug", FALSE))) message(...)
}

#' Read In Annotations
#'
#' @description Internal function: 
#' Read in annotations as GRanges objects from bed files - stored in list format
#'
#' @keywords internal
#' 
#' @author SLA 
#'
#' @param annots named list of bed file names (incl. full path; can be stored both locally and on http)
#' @param verbosity indicated by an integer 0 to 3 referring to the levels 'off', 'no warnings', 'normal', and 'detailed'
#'
#' @return named list GRanges objects for each given bed file
#' 
#' @importFrom rtracklayer import import.bed
#' 
#' @export
#' 
#' @note bed files can be imported from http on both MacOS and Windows (not true for bigwig files on Windows)
#' 
#' @examples
#' annots_fnames = list('gencode v21'='http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/annotations/gencode.v21.annotation.nohosted.bed',
#'                      'in-house'='http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/annotations/HeLa_major_isoform_hg38_gc34.bed')
#' annots_listedGR = ReadInAnnotations(annots=annots_fnames, verbosity=3)               
#' 
ReadInAnnotations = function(annots, verbosity=3){
  if (verbosity > 0){ 
    cat('reading annotations', '\n') 
    cat(paste(names(annots), collapse='\t'), '\n')
  }
  if (!is.null(annots)){
    annotations = list()
    for (annot in names(annots)){
      annotations[[annot]] = suppressWarnings(rtracklayer::import(annots[[annot]], format='bed'))
    }
  }else{
    annotations = NULL
  }
  return(annotations)
}


#' Region GRanges
#'
#' @description Internal function: 
#' Convert feature name or locus to be plotted to a GRanges object
#'
#' @keywords internal
#' 
#' @author SLA 
#'
#' @param locus genomic coordinates, incl. strand, of locus (e.g., c("chr1", "+", 87297784, 87379607)
#' @param tracks_width width of plotted data tracks in cm
#' @param feature the name of a genomic locus (e.g., LMO4; the name needs to be present within the name column of the provided annotations, which will be searched in the given order).
#' @param annotations named list of GRanges objects representing annotations (produced by ReadInAnnotations)
#' @param bin_start Center the bins around the given genomic position. Provide an integer value that lies within the plotted region. Per default the bin center will be at the 5'-end of the plotted region if it is defined by genomic coordinates and at the 5'-end of the locus if the plotted region is defined by locus name
#' @param extra_space Extra space up- and downstream of and relative to the selected genomic feature (0.1 = 10 percent). Only taken into account when genomic locus (feature) name is entered - ignored when genomic coordinates are entered.  Numeric vector - default c(1.0,1.0).
#' @param verbosity indicated by an integer 0 to 3 referring to the levels 'off', 'no warnings', 'normal', and 'detailed'
#' @param interface 'R' or 'shiny'
#'
#' @return a GRanges object with user-supplied locus coordinates (two metadata columns included with bin_start and tracks_width)
#' 
#' @note bin_start indicates the coordinate from where binning is centered
#' @note tracks_width is the plotted width in cm
#' 
#' @import S4Vectors
#' @import IRanges
#' @import GenomicRanges
#' @importFrom BiocGenerics strand
#' @importFrom GenomeInfoDb seqnames
#'
#' @examples
#' annots_fnames = list('gencode v21'='http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/annotations/gencode.v21.annotation.nohosted.bed',
#'                      'in-house'='http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/annotations/HeLa_major_isoform_hg38_gc34.bed')
#' annots_listedGR = ReadInAnnotations(annots=annots_fnames, verbosity=3)       
#' locus_gr = RegionGRanges(locus=NULL, tracks_width=12, feature='LMO4', annotations=annots_listedGR, bin_start=NULL, extra_space=c(0.1,0.1), verbosity=3, interface='R')
#' locus_gr = RegionGRanges(locus=c('chr1', '+', 87326423, 87350968), tracks_width=12, feature=NULL, annotations=NULL, bin_start=NULL, extra_space=c(0.1,0.1), verbosity=3, interface='R')

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
    #@ if (is.null(bin_start)){}
    .bin.start = ifelse(.strand=='+', .chrom.start, .chrom.end)
    .gene.width = .chrom.end - .chrom.start + 1
    if (.gene.width > 0 & .gene.width < 100){
      .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', 'the supplied region is less than 100 bp wide (',  .gene.width, ') - adjusting to 100 bp (minimum width) based on center coordinate')
      .chrom.center = .chrom.start + as.integer(.gene.width/2)
      .chrom.start = .chrom.center - 49
      .chrom.end = .chrom.center + 50
    }else if (.gene.width <= 0){
      .messages[['errors']][[length(.messages[['errors']])+1]] = paste0(' - ', 'the supplied region is equal to or less than 0 bp wide (',  .gene.width, ') - aborting')
      PrintOutput(.messages, verbosity)
      return()
    }
    if(.strand=='-'){ extra_space = rev(extra_space) }
    .chrom.start = .chrom.start - as.integer(extra_space[1]*.gene.width)
    .chrom.end = .chrom.end + as.integer(extra_space[2]*.gene.width)
    if (!is.null(bin_start)){
      if (bin_start < .chrom.start | bin_start > .chrom.end){
        #.bin.start = ifelse(.strand=='+', .chrom.start, .chrom.end)
        .arg.name = ifelse(interface=='R', '"bin_start"', '"Bins Center"')
        .messages[['warnings']][[length(.messages[['warnings']])+1]] = paste0(' - ', 'the set ',  .arg.name, ' is outside of the plotted region - setting to ', .bin.start)
      }else{
        .bin.start = bin_start
      }
    }
    PrintOutput(.messages, verbosity)
    return(GenomicRanges::GRanges(.chrom, .strand, ranges=IRanges::IRanges(.chrom.start, .chrom.end), bin.start=.bin.start, tracks.width=tracks_width))
  }else{
    PrintOutput(.messages, verbosity)
    return()
  }
}

# ---- ParseLocus ----

#' 
#' Parse a genomic locus string into a standardised 4-element vector
#'
#' @description
#' Accepts many common coordinate notations and returns \code{c(chrom, strand, start, end)},
#' or \code{NULL} when the input cannot be parsed as genomic coordinates (e.g. a gene name).
#'
#' Supported formats (examples):
#' \itemize{
#'   \item \code{chr11:+:93700000:93740000}
#'   \item \code{chr11:93700000:93740000}
#'   \item \code{chr11:+:93700000-93740000}  (en-dash / em-dash range)
#'   \item \code{chr11 + 93700000 93740000}
#'   \item \code{chr11 93700000 93740000}
#' }
#' Comma thousands-separators are stripped; strand defaults to \code{"+"} when absent.
#'
#' @param x Character string to parse.
#' @return Character vector \code{c(chrom, strand, start, end)}, or \code{NULL}.
#' @keywords internal
ParseLocus <- function(x) {
  if (is.null(x) || nchar(trimws(x)) == 0) return(NULL)
  x <- trimws(x)

  # Normalise unicode dashes (en-dash U+2013, em-dash U+2014, minus sign U+2212) to ASCII hyphen
  x <- gsub("\u2013|\u2014|\u2212", "-", x)

  # Remove comma thousands-separators between digits
  x <- gsub("(?<=\\d),(?=\\d)", "", x, perl = TRUE)

  # Extract strand from :+: / :-: notation
  strand <- "+"
  if (grepl(":+:", x, fixed = TRUE)) {
    strand <- "+"
    x <- sub(":+:", ":", x, fixed = TRUE)
  } else if (grepl(":-:", x, fixed = TRUE)) {
    strand <- "-"
    x <- sub(":-:", ":", x, fixed = TRUE)
  } else {
    # Check for space-delimited strand token: "chr11 + 93700000 93740000"
    m <- regmatches(x, regexpr("\\s([+-])\\s", x))
    if (length(m) > 0) {
      strand <- gsub("\\s", "", m)
      x <- sub("\\s[+-]\\s", " ", x)
    }
  }

  # Hyphen between digits is a range separator --  convert to colon
  x <- gsub("(?<=\\d)-(?=\\d)", ":", x, perl = TRUE)

  # Collapse whitespace runs to colon
  x <- gsub("\\s+", ":", x)

  # Split and drop empty tokens
  parts <- strsplit(x, ":")[[1]]
  parts <- parts[nchar(parts) > 0]

  # Require exactly chrom + start + end
  if (length(parts) != 3L) return(NULL)

  start_val <- suppressWarnings(as.integer(parts[2]))
  end_val   <- suppressWarnings(as.integer(parts[3]))
  if (is.na(start_val) || is.na(end_val)) return(NULL)

  c(parts[1], strand, as.character(start_val), as.character(end_val))
}


#' Find Nonoverlapping Intervals
#'
#' @description Internal function: 
#' Find nonoverlapping intervals within a GRanges object for each interval (based on the SelfHits object obtained by findOverlaps function applied to the GRanges object beforehand)
#' - used together with "ConnectIVs" to organize the individual feature annotations, such that they take up as little space as possible
#'
#' @keywords internal
#' 
#' @author SLA 
#'
#' @param n_IV 
#' @param n_IVs 
#' @param overlaps 
#'
#' @return placeholder
#' 
#' @import S4Vectors
#'
#' @examples
#' NULL
#' 
FindNonoverlappingIVs = function(n_IV, n_IVs, overlaps){
  nonoverlapping.IVs = c(n_IV, setdiff(1:n_IVs, S4Vectors::subjectHits(overlaps)[which(S4Vectors::queryHits(overlaps)==n_IV)]))
  nonoverlapping.IVs = nonoverlapping.IVs[which(nonoverlapping.IVs >= n_IV)]
  return(nonoverlapping.IVs)
}


#' Connect Intervals
#'
#' @description Internal function: 
#' Connect non-overlapping intervals, such that the number of "lines" used in the annotation track is minimized
#' - used together with "FindNonoverlappingIVs" IVs to organize the individual feature annotations, such that they take up as little space as possible
#'
#' @keywords internal
#' 
#' @author SLA 
#'
#' @param n_IV 
#' @param nonoverlapping_IVs 
#' @param remaining_IVs 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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


#' Organize Overlapping Intervals
#'
#' @description Internal function: 
#' Overlapping feature intervals (transcripts/genes/transcription units) are organized in separate lines to allow "non-overlapping" plotting
#'
#' @keywords internal
#' 
#' @author SLA 
#'
#' @param annotation_gr 
#' @param gap 
#'
#' @return placeholder
#' 
#' @import GenomicRanges
#'
#' @examples
#' NULL
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
#' @description Internal function: 
#' This function is the first step in organizing the annotated features in the plotted region into various formats
#' Used together with "OrganizeOverlappingLoci" and "ConvertCollapsedFormat" to create a final organized version of the annotated features
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param plotted_region 
#' @param annotations 
#'
#' @return placeholder
#' 
#' @import IRanges
#' @import S4Vectors
#' @import GenomicRanges
#' @importFrom BiocGenerics strand
#'
#' @examples
#' NULL
#' 
AnnotatedFeaturesInRegion = function(plotted_region, annotations){
  .strand = as.character(BiocGenerics::strand(plotted_region))
  .plot.width = IRanges::width(plotted_region)
  .tracks.width = S4Vectors::mcols(plotted_region)$tracks.width
  # Cache plotted_region coordinates --  these are accessed dozens of times below
  .pr.start = IRanges::start(plotted_region)
  .pr.end = IRanges::end(plotted_region)
  .pr.ranges = IRanges::ranges(plotted_region)
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
      # Cache subset annotation metadata --  accessed repeatedly below
      .sa.mcols = S4Vectors::mcols(.subset.annotation)
      .sa.names = .sa.mcols$name
      ##### group based on feature name
      .feat.names = unique(.sa.names)
      ##### color - pick the most prevalent color in the collapsed set of transcripts
      .collapsed.cols = unlist(lapply(.feat.names, function(.feat.name) sort(.sa.mcols$itemRgb[.sa.names == .feat.name])[1]))
      ##### collapse .annotation based on feature name
      .collapsed.annot = unlist(as(lapply(.feat.names, function(.feat.name) {.ann=.subset.annotation[.sa.names == .feat.name]; return(GenomicRanges::GRanges(IRanges::subsetByOverlaps(range(.ann), plotted_region), name=.feat.name))}), 'GRangesList'))
      if (!is.null(.collapsed.cols)){
        S4Vectors::mcols(.collapsed.annot)$itemRgb = .collapsed.cols
      }else{
        S4Vectors::mcols(.collapsed.annot)$itemRgb = '#000000'
      }
      # are collapsed transcript models extending past plotted region
      S4Vectors::mcols(.collapsed.annot)$on.from.start = FALSE
      S4Vectors::mcols(.collapsed.annot)$on.from.end = FALSE
      
      #@ 2023-09-25 ->
      ## do we have repeated feature names at this stage??
      .collapsed.names = S4Vectors::mcols(.collapsed.annot)$name
      if (sum(duplicated(.collapsed.names)) > 0){
        S4Vectors::mcols(.collapsed.annot)$name = paste0(.collapsed.names, '#DUPNAME#', 1:length(.collapsed.annot))
        .ca.names.dup = S4Vectors::mcols(.collapsed.annot)$name
        for (.n.feat in 1:length(.collapsed.annot)){ 
          .o.feats = S4Vectors::queryHits(GenomicRanges::findOverlaps(.subset.annotation, .collapsed.annot[.n.feat], type='within'))
          .o.feats.final = .o.feats[unlist(sapply(.sa.names[.o.feats], function(name) grepl(paste0('^', name, '#DUPNAME#'), .ca.names.dup[.n.feat], fixed=FALSE) ))]
          .sa.names[.o.feats.final] = .ca.names.dup[.n.feat]
          S4Vectors::mcols(.subset.annotation[.o.feats.final])$name = .ca.names.dup[.n.feat]
        }
        .feat.names = unique(.sa.names)
      }
      #@ 2023-09-25 <-
      
      ##### check if feature regions overlap or not
      .is.disjoint = GenomicRanges::isDisjoint(.collapsed.annot)
      if (!.is.disjoint){
        .red.collapsed.annot = GenomicRanges::reduce(.collapsed.annot, with.revmap=TRUE)
        if (.strand == '-'){
          .red.collapsed.annot = .red.collapsed.annot[GenomicRanges::order(IRanges::end(.red.collapsed.annot), decreasing=TRUE)]
        }
        .revmap = S4Vectors::mcols(.red.collapsed.annot)$revmap
        # Cache collapsed.annot metadata for the revmap loop
        .ca.mcols = S4Vectors::mcols(.collapsed.annot)
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
          .collapsed.names.list[[i]] = .ca.mcols$name[ .revmap.IVs ]
          .collapsed.cols[i] = sort(.ca.mcols$itemRgb[ .revmap.IVs ])[1]
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
      # Cache starts/ends to avoid repeated S4 dispatch
      .ca.starts = IRanges::start(.collapsed.annot)
      .ca.ends = IRanges::end(.collapsed.annot)
      if (any(.ca.starts < .pr.start) | any(.ca.ends > .pr.end)){
        .ca.on.from.start = .ca.starts < .pr.start
        .ca.on.from.end = .ca.ends > .pr.end
        S4Vectors::mcols(.collapsed.annot)$on.from.start = .ca.on.from.start
        S4Vectors::mcols(.collapsed.annot)$on.from.end = .ca.on.from.end
        IRanges::start(.collapsed.annot)[.ca.on.from.start] = .pr.start
        IRanges::end(.collapsed.annot)[.ca.on.from.end] = .pr.end
      }
      .rca.starts = IRanges::start(.red.collapsed.annot)
      .rca.ends = IRanges::end(.red.collapsed.annot)
      if (any(.rca.starts < .pr.start) | any(.rca.ends > .pr.end)){
        .rca.on.from.start = .rca.starts < .pr.start
        .rca.on.from.end = .rca.ends > .pr.end
        S4Vectors::mcols(.red.collapsed.annot)$on.from.start = .rca.on.from.start
        S4Vectors::mcols(.red.collapsed.annot)$on.from.end = .rca.on.from.end
        IRanges::start(.red.collapsed.annot)[.rca.on.from.start] = .pr.start
        IRanges::end(.red.collapsed.annot)[.rca.on.from.end] = .pr.end
      }
      .feature.annotations = list()
      .annotation.lines = list()
      for (.feat.name in .feat.names){ 
        .feat.annot = .subset.annotation[.sa.names == .feat.name]
        .annotation.lines[[.feat.name]] = OrganizeOverlappingIVs(.feat.annot, .gap)
        # Cache per-feature coordinates to avoid S4 dispatch in sapply below
        .fa.starts = IRanges::start(.feat.annot)
        .fa.ends = IRanges::end(.feat.annot)
        .fa.n = length(.feat.annot)
        # are transcript models (exon-intron structure) interrupted
        .fa.on.from.start = .fa.starts < .pr.start
        .fa.on.from.end = .fa.ends > .pr.end
        S4Vectors::mcols(.feat.annot)$on.from.start = .fa.on.from.start
        S4Vectors::mcols(.feat.annot)$on.from.end = .fa.on.from.end
        .fa.mcols = S4Vectors::mcols(.feat.annot)
        if (!('blocks' %in% colnames(.fa.mcols))){
          .fa.widths = IRanges::width(.feat.annot)
          S4Vectors::mcols(.feat.annot)$blocks = IRanges::IRangesList(lapply(.fa.widths, function(w) IRanges::IRanges(start=1, end=w + 1)))
          .fa.mcols = S4Vectors::mcols(.feat.annot)
        }
        exons = .fa.mcols$blocks
        .exon.ranges = lapply(1:.fa.n, function(.n.trn) GenomicRanges::sort(IRanges::overlapsRanges(IRanges::IRanges(start=.fa.starts[.n.trn]-1 + IRanges::start(exons[[.n.trn]]), end=.fa.starts[.n.trn]-1 + (IRanges::end(exons[[.n.trn]]) - 1)), .pr.ranges), decreasing = FALSE) )
        S4Vectors::mcols(.feat.annot)$intron.from.start = sapply( 1:length(.exon.ranges), function(.n.trn)  if (length(.exon.ranges[[.n.trn]]) > 0){ ifelse( min(IRanges::start(.exon.ranges[[.n.trn]])) == .pr.start, FALSE, .fa.on.from.start[.n.trn]) }else{ TRUE } )
        S4Vectors::mcols(.feat.annot)$intron.from.end = sapply( 1:length(.exon.ranges), function(.n.trn) if (length(.exon.ranges[[.n.trn]]) > 0){ ifelse( max(IRanges::end(.exon.ranges[[.n.trn]])) == .pr.end, FALSE, .fa.on.from.end[.n.trn]) }else{ TRUE } )
        if (any(.fa.on.from.start) | any(.fa.on.from.end)){
          .trunc.starts = pmax(.fa.starts, .pr.start)
          .trunc.ends = pmin(.fa.ends, .pr.end)
          .new.starts = sapply( 1:length(.exon.ranges), function(.n.trn) ifelse(length(.exon.ranges[[.n.trn]]) > 0, min(IRanges::start(.exon.ranges[[.n.trn]])), .trunc.starts[.n.trn]) )
          .new.ends = sapply( 1:length(.exon.ranges), function(.n.trn) ifelse(length(.exon.ranges[[.n.trn]]) > 0, max(IRanges::end(.exon.ranges[[.n.trn]])), .trunc.ends[.n.trn]) )
          IRanges::start(.feat.annot) = .new.starts
          IRanges::end(.feat.annot) = .new.ends
          .exon.ranges = lapply( 1:length(.exon.ranges), function(.n.trn) {if (length(.exon.ranges[[.n.trn]]) > 0){IRanges::shift(.exon.ranges[[.n.trn]], shift=-min(IRanges::start(.exon.ranges[[.n.trn]]))+1 )}else{IRanges::shift(IRanges::ranges(.feat.annot)[.n.trn], -.new.starts[.n.trn]+1)}} )
          S4Vectors::mcols(.feat.annot)$blocks = as(.exon.ranges, 'IRangesList')
          if ("thick" %in% names(.fa.mcols)){
            .fa.thick = S4Vectors::mcols(.feat.annot)$thick
            .fa.starts.new = IRanges::start(.feat.annot)
            .fa.ends.new = IRanges::end(.feat.annot)
            .thick.outside = .fa.thick %outside% .pr.ranges
            if (any(.thick.outside)){
              .fa.thick[.thick.outside] = IRanges::IRanges(.fa.starts.new, width=0)[.thick.outside]
            }
            .thick.zero = IRanges::width(.fa.thick) == 0
            if (any(.thick.zero)){
              .fa.thick[.thick.zero] = IRanges::IRanges(.fa.starts.new, width=0)[.thick.zero]
            }
            .thick.pos = IRanges::width(.fa.thick) > 0
            if (any(.thick.pos)){
              .fa.thick[.thick.pos] = IRanges::overlapsRanges(.fa.thick[.thick.pos], .pr.ranges)
            }
            .thick.starts = pmax(IRanges::start(.fa.thick), .fa.starts.new)
            .thick.ends = pmin(IRanges::end(.fa.thick), .fa.ends.new)
            IRanges::start(.fa.thick) = .thick.starts
            IRanges::end(.fa.thick) = .thick.ends
            S4Vectors::mcols(.feat.annot)$thick = .fa.thick
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
#' @description Internal function: 
#' This function is the second step in organizing the annotated features in the plotted region into various formats
#' Used after AnnotatedFeaturesInRegion and before ConvertCollapsedFormat to create a final organized version of the annotated features
#'
#' @keywords internal
#' 
#' @author SLA 
#' 
#' @param annot_info 
#' 
#' @return placeholder
#' 
#' @import S4Vectors
#'
#' @examples
#' NULL
#' 
OrganizeOverlappingLoci = function(annot_info){
  for (.annot.name in names(annot_info)){
    annot_info[[.annot.name]][['packing2']] = NULL
    annot_info[[.annot.name]][['gene_row_offsets']] = NULL
    annot_info[[.annot.name]][['gene_trn_indices']] = NULL
    annot_info[[.annot.name]][['small_gene_row_overrides']] = NULL
    if (length(annot_info[[.annot.name]]) > 0){
      annot_info[[.annot.name]][['packing2']] = structure(lapply(rep(1, length(annot_info[[.annot.name]][['collapsed']])), list), names=S4Vectors::mcols(annot_info[[.annot.name]][['collapsed']])$name)
      .non.disjoint = !S4Vectors::mcols(annot_info[[.annot.name]][['collapsed']])$disjoint
      if (any(.non.disjoint)){
        .merged.feat.names.list = unique(S4Vectors::mcols(annot_info[[.annot.name]][['collapsed']])$collapsed.names[.non.disjoint])
        for (.n.nd in 1:length(.merged.feat.names.list)){
          .merged.feat.names = .merged.feat.names.list[[.n.nd]]
          .merged.feats.name = paste(.merged.feat.names, collapse=',')
          .subset.annotation = annot_info[[.annot.name]][['collapsed2']][S4Vectors::mcols(annot_info[[.annot.name]][['collapsed2']])$name %in% .merged.feat.names]
          .packing2.raw = OrganizeOverlappingIVs(.subset.annotation)
          # Reorder rows so the widest gene occupies row 1 (descending by max gene
          # width per row).  Indices within each row are unchanged --  they still point
          # to genomic-order positions in collapsed2 --  only the row ordering changes.
          .sa.widths = IRanges::width(.subset.annotation)
          .row.max.w = vapply(.packing2.raw, function(.ri) max(.sa.widths[.ri]), numeric(1L))
          annot_info[[.annot.name]][['packing2']][[.merged.feats.name]] = .packing2.raw[order(.row.max.w, decreasing = TRUE)]
          annot_info[[.annot.name]][['expanded']][[.merged.feats.name]] = unlist(annot_info[[.annot.name]][['expanded']][.merged.feat.names])
          .n.trn.per.feature = cumsum(unlist(lapply(annot_info[[.annot.name]][['expanded']][.merged.feat.names], length)))
          .merged.trn.numbers = structure(lapply(1:length(.n.trn.per.feature), function(n) {if (n==1){1:.n.trn.per.feature[n]}else{(.n.trn.per.feature[n-1]+1):.n.trn.per.feature[n]}}), names=.merged.feat.names)
          
          # --- Gene-level row packing for expanded mode ---
          # Instead of concatenating per-gene packings sequentially (which
          # gives each gene its own rows even when non-overlapping), we:
          # 1. Keep each gene's internal packing intact
          # 2. Compute gene-level row offsets by packing gene footprints
          #    using OrganizeOverlappingIVs on collapsed2 entries
          # This way non-overlapping genes (e.g. snoRNAs inside NOP56) can
          # share rows, while transcripts within each gene stay grouped.
          
          # Get each gene's internal row count
          .gene.heights = sapply(.merged.feat.names, function(g) length(annot_info[[.annot.name]][['packing']][[g]]))
          
          # Pack genes by their collapsed2 footprint to find which can share row blocks
          .gene.packing = OrganizeOverlappingIVs(.subset.annotation)
          # .gene.packing is a list of rows, each containing indices into .subset.annotation
          # genes in the same row don't overlap genomically
          
          # Compute row offset for each gene:
          # genes in the same gene-packing row share vertical space,
          # starting at the cumulative height of previous gene-packing rows
          .gene.row.offsets = structure(rep(0L, length(.merged.feat.names)), names=.merged.feat.names)
          .cumulative.row = 0L
          for (.gene.pack.row in .gene.packing) {
            # All genes in this row start at .cumulative.row
            .genes.in.row = S4Vectors::mcols(.subset.annotation)$name[.gene.pack.row]
            for (.g in .genes.in.row) {
              .gene.row.offsets[.g] = .cumulative.row
            }
            # The row block height is the max height of any gene in this row
            .row.block.height = max(.gene.heights[.genes.in.row])
            .cumulative.row = .cumulative.row + .row.block.height
          }
          # Store gene_row_offsets for this merged group
          annot_info[[.annot.name]][['gene_row_offsets']][[.merged.feats.name]] = .gene.row.offsets
          # Store gene-to-transcript-index mapping for inline name placement
          annot_info[[.annot.name]][['gene_trn_indices']][[.merged.feats.name]] = .merged.trn.numbers
          
          # Build packing using offsets instead of sequential concatenation
          annot_info[[.annot.name]][['packing']][[.merged.feats.name]] = list()
          # Total rows needed = cumulative.row (max offset + max height at last row)
          .total.rows = .cumulative.row
          # Initialize empty rows
          for (.r in 1:.total.rows) {
            annot_info[[.annot.name]][['packing']][[.merged.feats.name]][[.r]] = integer(0)
          }
          # Place each gene's transcripts at their offset position
          for (.merged.feat.name in .merged.feat.names){
            .offset = .gene.row.offsets[.merged.feat.name]
            .sub.packing = annot_info[[.annot.name]][['packing']][[.merged.feat.name]]
            .trn.numbers = .merged.trn.numbers[[.merged.feat.name]]
            for (.row.idx in seq_along(.sub.packing)) {
              .target.row = .offset + .row.idx
              .annot.lines = .trn.numbers[.sub.packing[[.row.idx]]]
              annot_info[[.annot.name]][['packing']][[.merged.feats.name]][[.target.row]] = 
                c(annot_info[[.annot.name]][['packing']][[.merged.feats.name]][[.target.row]], .annot.lines)
            }
            annot_info[[.annot.name]][['expanded']][[.merged.feat.name]] = NULL
            annot_info[[.annot.name]][['packing']][[.merged.feat.name]] = NULL
          }
          # Remove any empty rows (shouldn't happen but be safe)
          annot_info[[.annot.name]][['packing']][[.merged.feats.name]] = 
            annot_info[[.annot.name]][['packing']][[.merged.feats.name]][
              lengths(annot_info[[.annot.name]][['packing']][[.merged.feats.name]]) > 0]
        }
      }
      # For non-merged (standalone) genes, compute trivial gene_row_offsets and gene_trn_indices
      for (.feat.name in names(annot_info[[.annot.name]][['packing']])) {
        if (is.null(annot_info[[.annot.name]][['gene_row_offsets']][[.feat.name]])) {
          annot_info[[.annot.name]][['gene_row_offsets']][[.feat.name]] = structure(0L, names=.feat.name)
        }
        if (is.null(annot_info[[.annot.name]][['gene_trn_indices']][[.feat.name]])) {
          .n.trn = length(annot_info[[.annot.name]][['expanded']][[.feat.name]])
          annot_info[[.annot.name]][['gene_trn_indices']][[.feat.name]] = structure(list(seq_len(.n.trn)), names=.feat.name)
        }
      }
    }
  }
  return(annot_info)
}


#' Convert Collapsed Format
#'
#' @description Internal function: 
#' This function is the third step in organizing the annotated features in the plotted region into various formats
#' Used after AnnotatedFeaturesInRegion and OrganizeOverlappingLoci to create a final organized version of the annotated features
#' Converts the format of "collapsed" and "collapsed2" from GRanges to GRangesLists to make the overall formatting uniform; one GRanges object per 'feature name' (e.g. "TAF1D" and "RP11-178H8.7")
#'
#' @keywords internal
#'
#' @author SLA 
#'
#' @param annot_info 
#'
#' @return placeholder
#' 
#' @import S4Vectors
#' @import GenomicRanges
#' @importFrom BiocGenerics strand
#'
#' @examples
#' NULL
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
#' @description Internal function: 
#' Wrapper function that executes the three above functions
#'
#' @keywords internal
#' 
#' @author SLA 
#'
#' @param plotted_region 
#' @param annotations 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
OrganizeAnnotatedFeaturesInRegion = function(plotted_region, annotations, incl_feature_names = NULL, center_of_mass = FALSE, incl_feature_brackets = NULL, annotation_packing = NULL){
  # Cache lookup. The c2 ILP path inside ComputeInlineNamePlacements() can
  # take hundreds of ms for crowded regions; once Layer 1 (bigwig cache) is
  # in place this becomes the dominant cost on re-plot. Output is a
  # deterministic function of these arguments, so caching is safe.
  cache_key <- .annot_cache_key(plotted_region, annotations,
                                incl_feature_names, center_of_mass,
                                incl_feature_brackets, annotation_packing)
  if (!is.null(cache_key) &&
      exists(cache_key, envir = .annot_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .annot_cache, inherits = FALSE))
  }

  annot_info = AnnotatedFeaturesInRegion(plotted_region, annotations)
  annot_info = OrganizeOverlappingLoci(annot_info)
  annot_info = ComputeInlineNamePlacements(annot_info, plotted_region, incl_feature_names, center_of_mass, incl_feature_brackets, annotation_packing)
  annot_info = ConvertCollapsedFormat(annot_info)

  if (!is.null(cache_key)) assign(cache_key, annot_info, envir = .annot_cache)
  return(annot_info)
}


# ---- Annotation processing cache --------------------------------------------
#
# Caches the output of OrganizeAnnotatedFeaturesInRegion() so re-plotting the
# same region with the same annotation options doesn't re-run the c2 ILP
# layout solver. Key = fingerprint of (region, annotations, options).
# Annotation fingerprint uses length + sum(start) per annotation - cheap to
# compute and statistically distinguishing across different annotation sets.

.annot_cache <- new.env(parent = emptyenv())

#' @keywords internal
.annot_fingerprint_one <- function(gr) {
  if (is.null(gr)) return("0")
  if (inherits(gr, "GRanges")) {
    n <- length(gr)
    if (n == 0L) return("0")
    paste(n, sum(as.numeric(BiocGenerics::start(gr))), sep = ":")
  } else if (is.character(gr)) {
    # File path placeholder (un-loaded)
    gr
  } else {
    "?"
  }
}

#' @keywords internal
.annot_cache_key <- function(plotted_region, annotations,
                             incl_feature_names, center_of_mass,
                             incl_feature_brackets, annotation_packing) {
  if (is.null(annotations) || length(annotations) == 0L) return(NULL)
  # Region fingerprint
  reg_fp <- paste(as.character(GenomeInfoDb::seqnames(plotted_region))[1],
                  as.character(BiocGenerics::strand(plotted_region))[1],
                  BiocGenerics::start(plotted_region)[1],
                  BiocGenerics::end(plotted_region)[1],
                  sep = ":")
  # Annotation fingerprint
  ann_fp <- paste(names(annotations),
                  vapply(annotations, .annot_fingerprint_one, character(1)),
                  sep = "=", collapse = ";")
  # Option fingerprint
  opt_fp <- paste(
    paste(names(incl_feature_names),    incl_feature_names,    sep = "=", collapse = ","),
    as.character(center_of_mass),
    paste(names(incl_feature_brackets), incl_feature_brackets, sep = "=", collapse = ","),
    paste(names(annotation_packing),    annotation_packing,    sep = "=", collapse = ","),
    sep = "|"
  )
  paste("ANNO", reg_fp, ann_fp, opt_fp, sep = "##")
}

#' Clear the annotation-processing cache
#'
#' @description Empties the in-memory cache that stores the output of
#' \code{OrganizeAnnotatedFeaturesInRegion()}. Use when memory is tight or
#' to force a recompute (e.g. you mutated an annotation GRanges in place).
#'
#' @return Invisibly returns the number of entries removed.
#'
#' @export
#'
#' @examples
#' clear_annotation_cache()
clear_annotation_cache <- function() {
  n <- length(ls(.annot_cache, all.names = TRUE))
  rm(list = ls(.annot_cache, all.names = TRUE), envir = .annot_cache)
  invisible(n)
}

#' Number of entries currently in the annotation-processing cache
#'
#' @return Integer; entry count.
#'
#' @export
#'
#' @examples
#' annotation_cache_size()
annotation_cache_size <- function() {
  length(ls(.annot_cache, all.names = TRUE))
}


# ---- Stage timer (internal benchmark helper) -------------------------------
#
# Lightweight per-stage wall-time accumulator used by the ILP optimisation
# benchmark (tests/test_ilp_benchmark.R). Activated by setting
# options(seqNdisplayR.stage_timer = <env>) to an environment that the
# accumulators will write into. When the option is unset (the default), the
# tic / toc / record_cluster helpers are essentially no-ops (single
# `getOption` + early return), so the cost in production is negligible.
#
# Env layout after a run:
#   env$starts    -- named list, transient (current per-label start time)
#   env$totals    -- named list, accumulated seconds per label
#   env$counts    -- named list, call count per label
#   env$clusters  -- list of (N_genes, N_flat, time_s) per ILP cluster solve

#' @keywords internal
.tic <- function(label) {
  env <- getOption("seqNdisplayR.stage_timer", default = NULL)
  if (is.null(env)) return(invisible())
  if (!exists("starts", envir = env, inherits = FALSE))
    assign("starts", list(), envir = env)
  starts <- get("starts", envir = env)
  starts[[label]] <- Sys.time()
  assign("starts", starts, envir = env)
  invisible()
}

#' @keywords internal
.toc <- function(label) {
  env <- getOption("seqNdisplayR.stage_timer", default = NULL)
  if (is.null(env)) return(invisible())
  starts <- if (exists("starts", envir = env, inherits = FALSE))
    get("starts", envir = env) else list()
  start  <- starts[[label]]
  if (is.null(start)) return(invisible())
  elapsed <- as.numeric(Sys.time() - start, units = "secs")

  totals <- if (exists("totals", envir = env, inherits = FALSE))
    get("totals", envir = env) else list()
  totals[[label]] <- (if (is.null(totals[[label]])) 0 else totals[[label]]) + elapsed
  assign("totals", totals, envir = env)

  counts <- if (exists("counts", envir = env, inherits = FALSE))
    get("counts", envir = env) else list()
  counts[[label]] <- (if (is.null(counts[[label]])) 0L else counts[[label]]) + 1L
  assign("counts", counts, envir = env)
  invisible()
}

#' @keywords internal
.record_cluster <- function(N_genes, N_flat, time_s) {
  env <- getOption("seqNdisplayR.stage_timer", default = NULL)
  if (is.null(env)) return(invisible())
  cl <- if (exists("clusters", envir = env, inherits = FALSE))
    get("clusters", envir = env) else list()
  cl[[length(cl) + 1L]] <- list(N_genes = N_genes, N_flat = N_flat, time_s = time_s)
  assign("clusters", cl, envir = env)
  invisible()
}


#' Compute Inline Name Placements
#'
#' @description Internal function:
#' Pre-computes row assignments for gene names in expanded/squished mode.
#' Determines which row each gene name should appear on (beside transcripts
#' or below) so that height estimation is exact.
#' Must be called BEFORE ConvertCollapsedFormat (needs collapsed2 as GRanges).
#'
#' @keywords internal
#' @author SLA / Claude
#'
#' @param annot_info annotation info list from OrganizeOverlappingLoci
#' @param plotted_region GRanges of plotted region
#'
#' @return annot_info with added 'inline_name_placements' slot per annotation
#'
#' @import IRanges
#' @import S4Vectors
#'
ComputeInlineNamePlacements = function(annot_info, plotted_region, incl_feature_names = NULL, center_of_mass = FALSE, incl_feature_brackets = NULL, annotation_packing = NULL) {
  .pr.start = IRanges::start(plotted_region)
  .pr.end = IRanges::end(plotted_region)
  .pr.width = .pr.end - .pr.start
  .tracks.width.cm = as.numeric(S4Vectors::mcols(plotted_region)$tracks.width)
  .constants = ConstantsDefaults()
  .std.letter.width = .constants['std_letter_width']
  .feature.font.size = 7
  .bp.per.cm = .pr.width / .tracks.width.cm
  
  # --- Scoring helper ---
  # Returns list(score, penalty) for a candidate position
  # score = position preference, penalty = overlap in bp
  .score.candidate = function(.x, .adj, .row, .name.width.bp,
                               .gene.mid.row, .gene.first.row, .gene.last.row,
                               .gene.start, .gene.end, .name.gap,
                               .global.row.trn, .global.placed.rects,
                               .gene.com = NULL, .exclude.genes = NULL,
                               .gene.n.trn = 1L, .expanded.mode = FALSE) {
    .xl = .x - .adj * .name.width.bp
    .xr = .xl + .name.width.bp
    # Out of bounds = disqualifying (allow 10% name-width tolerance at each edge)
    .tol = .name.width.bp * 0.10
    if (.xl < .pr.start - .tol || .xr > .pr.end + .tol)
      return(list(score = -Inf, pref = -Inf, com_bonus = 0, penalty = Inf))

    # Calculate overlap penalty (bp).
    # Bar overlap (name drawn over any gene bar) is a hard constraint --  weighted
    # by a large factor so any bar-free candidate always beats a bar-overlapping one.
    # Name-on-name overlap is also penalised (1.0) --  it creates visual confusion.
    .bar.overlap = 0
    .name.overlap = 0
    .row.key = as.character(.row)
    if (.row.key %in% names(.global.row.trn)) {
      for (.trn in .global.row.trn[[.row.key]]) {
        if (!is.null(.exclude.genes) && !is.null(.trn$gene) && .trn$gene %in% .exclude.genes) next
        .bar.overlap = .bar.overlap + max(0, min(.xr, .trn$end) - max(.xl, .trn$start))
      }
    }
    for (.p in .global.placed.rects) {
      if (.p$row == .row) {
        .name.overlap = .name.overlap + max(0, min(.xr, .p$xr) - max(.xl, .p$xl))
      }
    }
    .total.overlap = .bar.overlap * 1e6 + .name.overlap
    
    # Position preference score.
    # is.right / is.left: name is FULLY outside the gene span.  The old
    # formulation (".x > .gene.end" and ".x - (1-adj)*nw < .gene.start")
    # incorrectly fired for adj=0 candidates whose bar actually lay inside
    # the gene (e.g. COM-shifted inline candidates at `com+gap`, or
    # edge-pinned fallbacks on the bar row), granting them pref=8 and then
    # placing the name on top of the gene's own bars.
    .is.right = .xl >= .gene.end
    .is.left  = .xr <= .gene.start
    .row.dist = abs(.row - .gene.mid.row)
    
    # Expanded-mode multi-transcript bump: for genes with n.trn >= 2 in expanded
    # mode, prefer CENTERED-below (10) and CENTERED-above (9) over inline (8).
    # The bump only applies to the TRUE centred candidate: adj=0.5 AND the name
    # is centred on the gene block (x within one letter-width of gene-centre).
    # Excludes COM-shifted adj=0.5 candidates so they don't tie with the centred
    # one and win by arbitrary order.
    .em.bump = isTRUE(.expanded.mode) && .gene.n.trn >= 2L
    .gene.centre = (.gene.start + .gene.end) / 2
    .is.true.centre = (.adj == 0.5) && abs(.x - .gene.centre) <= .name.gap
    if (.row == .gene.mid.row && (.is.right || .is.left)) {
      .pref = 8  # inline: right or left on the gene's mid row
    } else if (.row > .gene.last.row) {
      .rows.below = .row - .gene.last.row
      .pref = if (.em.bump && .is.true.centre && .rows.below == 1L) 10
              else 4 - 2 * (.rows.below - 1)
    } else if (.row >= .gene.first.row && .row <= .gene.last.row && (.is.right || .is.left)) {
      .pref = 3  # corner: inline on first/last row of a multi-row gene
    } else if (.row < .gene.first.row) {
      .rows.above = .gene.first.row - .row
      .pref = if (.em.bump && .is.true.centre && .rows.above == 1L) 9
              else 3 - 2 * (.rows.above - 1)
    } else {
      .pref = 3  # on gene row, not at edge (same as corners)
    }
    
    # Center-of-mass is no longer applied in standard scoring (any mode, brackets
    # OFF).  COM is repurposed as a bracket-section-only option --  applied directly
    # in the bracket-section path (inside-bracket x-coordinate, c2 inline-fallback
    # side selection).  The .gene.com parameter is retained here for signature
    # compatibility but no bonus is added.
    .com.bonus = 0

    return(list(score = .pref + .com.bonus, pref = .pref, com_bonus = .com.bonus, penalty = .total.overlap))
  }

  # --- Generate candidates for a gene ---
  .generate.candidates = function(.gene.start, .gene.end, .name.width.bp, .name.gap,
                                   .gene.mid.row, .gene.first.row, .gene.last.row,
                                   .total.packing.rows, .gene.com = NULL,
                                   .forced.side = NULL,
                                   .plot.start = NULL, .plot.end = NULL,
                                   .collapsed2 = FALSE) {
    # .forced.side: NULL/"any" = both sides; "left" = adj=1 only; "right" = adj=0 only.
    # .plot.start / .plot.end: when provided, boundary-pinned below/above candidates
    #   are added (expanded-mode path only).
    # .collapsed2: when TRUE, use the strict collapsed2 candidate set:
    #   1) inline-left, 2) inline-right,
    #   3) center one row below,
    #   4) left-aligned at +1/+2/+3 rows below,
    #   5) right-aligned at +1/+2/+3 rows below,
    #   6) center one row above.
    #   No COM-shifted candidates, no boundary-pinned fallbacks.
    .gene.center = (.gene.start + .gene.end) / 2
    if (is.null(.gene.com)) .gene.com = .gene.center
    .candidates = list()
    .add = function(.x, .adj, .row) {
      .candidates[[length(.candidates) + 1]] <<- list(x = .x, adj = .adj, row = .row)
    }

    if (.collapsed2) {
      # Edge-fallback helper: if a spec candidate would extend past the plotted
      # region (with the standard 10% name-width tolerance), add an edge-pinned
      # substitute on the same row so the gene is still placeable.  Returns
      # TRUE if the original candidate is in-bounds, FALSE if it was out and a
      # substitute was added (caller can then choose not to add the original).
      .tol = .name.width.bp * 0.10
      .add.with.fallback = function(.x, .adj, .row) {
        .xl = .x - .adj * .name.width.bp
        .xr = .xl + .name.width.bp
        .in.bounds = TRUE
        if (!is.null(.plot.start) && .xl < .plot.start - .tol) {
          .add(.plot.start, 0, .row)   # left-edge pinned substitute
          .in.bounds = FALSE
        }
        if (!is.null(.plot.end) && .xr > .plot.end + .tol) {
          .add(.plot.end, 1, .row)     # right-edge pinned substitute
          .in.bounds = FALSE
        }
        if (.in.bounds) .add(.x, .adj, .row)
      }
      # 1) inline-left  (name right edge one .name.gap to the left of bar start)
      if (is.null(.forced.side) || .forced.side == "left")
        .add.with.fallback(.gene.start - .name.gap, 1, .gene.mid.row)
      # 2) inline-right (name left edge one .name.gap to the right of bar end)
      if (is.null(.forced.side) || .forced.side == "right")
        .add.with.fallback(.gene.end + .name.gap, 0, .gene.mid.row)
      # 3) center, one row below (only when no forced side, to avoid ambiguity)
      .r.below1 = .gene.last.row + 1L
      if (is.null(.forced.side)) {
        .add.with.fallback(.gene.center, 0.5, .r.below1)
      }
      # 4) left-aligned at +1/+2/+3 rows below (name's left edge at bar start)
      # 5) right-aligned at +1/+2/+3 rows below (name's right edge at bar end)
      # forced.side is respected: "left" -> right-edge-at-bar-end (name extends left);
      #                           "right" -> left-edge-at-bar-start (name extends right).
      # When no forced side, both alignments are offered.
      for (.k in 1:3) {
        .r = .gene.last.row + .k
        if (is.null(.forced.side) || .forced.side == "right")
          .add.with.fallback(.gene.start, 0, .r)   # xl = gene.start, extends right
        if (is.null(.forced.side) || .forced.side == "left")
          .add.with.fallback(.gene.end,   1, .r)   # xr = gene.end,   extends left
      }
      # 6) center, left-aligned, and right-aligned one row above.
      # Row 0 (above row 1) is allowed: if a name lands there, a faux row is
      # added above the annotation and the y-limits are extended accordingly.
      # forced.side respected as above.
      .r.above1 = .gene.first.row - 1L
      if (is.null(.forced.side)) {
        .add.with.fallback(.gene.center, 0.5, .r.above1)
      }
      if (is.null(.forced.side) || .forced.side == "right")
        .add.with.fallback(.gene.start, 0, .r.above1)   # left-edge above, extends right
      if (is.null(.forced.side) || .forced.side == "left")
        .add.with.fallback(.gene.end,   1, .r.above1)   # right-edge above, extends left
      return(.candidates)
    }

    # ----- expanded-mode (legacy) candidate generation -----
    # Right/left of gene on gene rows --  restricted by .forced.side when set.
    # COM-shifted side candidates are no longer generated here (COM is bracket-
    # section only).
    for (.r in unique(c(.gene.mid.row, .gene.last.row, .gene.first.row))) {
      if (is.null(.forced.side) || .forced.side == "right")
        .add(.gene.end + .name.gap, 0, .r)      # right of gene end
      if (is.null(.forced.side) || .forced.side == "left")
        .add(.gene.start - .name.gap, 1, .r)     # left of gene start
    }
    # All other gene rows (only when both sides are permitted)
    if (is.null(.forced.side)) {
      for (.r in setdiff(seq(.gene.first.row, .gene.last.row), c(.gene.mid.row, .gene.last.row, .gene.first.row))) {
        .add(.gene.end + .name.gap, 0, .r)
        .add(.gene.start - .name.gap, 1, .r)
      }
    }
    # Below gene --  cover all rows from gene.last.row+1 through the total
    # packing depth (includes hosted small-gene rows) plus 3 extra rows.
    # The 1e6 bar-overlap penalty in scoring ensures rows with bars are
    # avoided; this range guarantees we always find a bar-free row.
    .below.max = max(.gene.last.row + 3L, .total.packing.rows + 1L)
    for (.r in (.gene.last.row + 1L):.below.max) {
      .add(.gene.center, 0.5, .r)             # centered below bar
      .add(.gene.start,  0,   .r)             # left-aligned at bar start
      .add(.gene.start,  1,   .r)             # right-aligned at bar start --  points to bar
      .add(.gene.end - .name.width.bp, 0, .r) # right-aligned from gene end
      # Boundary-pinned candidates (only when plot limits are known)
      if (!is.null(.plot.start)) .add(.plot.start, 0, .r)           # left edge pinned to plot left
      if (!is.null(.plot.end))   .add(.plot.end,   1, .r)           # right edge pinned to plot right
    }
    # Above gene (up to 2 rows above)
    for (.offset in 1:2) {
      .r = .gene.first.row - .offset
      if (.r >= 1) {
        .add(.gene.center, 0.5, .r)
        .add(.gene.start,  0,   .r)
        .add(.gene.start,  1,   .r)             # right-aligned at bar start
        .add(.gene.end - .name.width.bp, 0, .r)
        # Boundary-pinned candidates (only when plot limits are known)
        if (!is.null(.plot.start)) .add(.plot.start, 0, .r)
        if (!is.null(.plot.end))   .add(.plot.end,   1, .r)
      }
    }
    return(.candidates)
  }

  # ===== Stage helpers for collapsed2 pipeline (Stages A-E) =====
  # These local functions implement the pure stages of the redesigned pipeline.
  # Each takes explicit inputs and returns explicit outputs --  no side-effects on
  # annot_info until the final Commit at the end of the collapsed2 loop.

  # --- Stage A: Collect gene info ---
  # Returns: named list gene_name -> list(start, end, com, nw, ng, feat.name)
  .collect.gene.info = function(annot_info, annot_name, c2, c2_names_vec,
                                    feature_font_size, std_letter_width, bp_per_cm,
                                    gene_trn_indices = NULL, gene_row_offsets = NULL,
                                    is_collapsed_mode = FALSE) {
    gene_info = list()
    # Collapsed mode: each packing2 key is a super-merged locus.  Treat it as
    # ONE gene whose name is the merged label (possibly comma-joined) and whose
    # span comes from the [['collapsed']] super-locus GRanges --  NOT a comma
    # split into sub-features.  This produces one label per super-locus.
    if (isTRUE(is_collapsed_mode)) {
      .super = annot_info[[annot_name]][['collapsed']]
      .super.names = if (!is.null(.super)) as.character(S4Vectors::mcols(.super)$name) else character(0)
      for (.fn in names(annot_info[[annot_name]][['packing2']])) {
        .idx = which(.super.names == .fn)
        if (length(.idx) == 0) next
        .gs = min(IRanges::start(.super[.idx]))
        .ge = max(IRanges::end(.super[.idx]))
        .com = (.gs + .ge) / 2
        .dname = strsplit(.fn, '#DUPNAME#')[[1]][1]
        .entry = list(
          start    = .gs, end = .ge, com = .com,
          nw       = nchar(.dname) * feature_font_size * std_letter_width * bp_per_cm,
          ng       = feature_font_size * std_letter_width * bp_per_cm,
          feat.name = .fn
        )
        gene_info[[.fn]] = .entry
      }
      return(gene_info)
    }
    for (.fn in names(annot_info[[annot_name]][['packing2']])) {
      .genes = if (!grepl(",", .fn) || .fn %in% c2_names_vec)
        .fn else strsplit(.fn, ",")[[1]]
      for (.g in .genes) {
        .idx = which(c2_names_vec == .g)
        if (length(.idx) == 0) next
        .gs = min(IRanges::start(c2[.idx]))
        .ge = max(IRanges::end(c2[.idx]))
        .com = (.gs + .ge) / 2
        .exp = annot_info[[annot_name]][['expanded']]
        if (!is.null(.exp)) {
          .exp.gr = NULL
          if (.g %in% names(.exp)) {
            .exp.gr = .exp[[.g]]
          } else if (.fn %in% names(.exp)) {
            .tim = annot_info[[annot_name]][['gene_trn_indices']][[.fn]]
            if (!is.null(.tim) && .g %in% names(.tim)) {
              .ti = .tim[[.g]]
              .me = .exp[[.fn]]
              if (length(.ti) > 0 && max(.ti) <= length(.me)) .exp.gr = .me[.ti]
            }
          }
          if (!is.null(.exp.gr) && length(.exp.gr) > 0) {
            .es = IRanges::start(.exp.gr); .ee = IRanges::end(.exp.gr)
            .ew = .ee - .es + 1
            .com = sum((.es + .ee) / 2 * .ew) / sum(.ew)
          }
        }
        .dname = strsplit(.g, '#DUPNAME#')[[1]][1]
        .entry = list(
          start    = .gs, end = .ge, com = .com,
          nw       = nchar(.dname) * feature_font_size * std_letter_width * bp_per_cm,
          ng       = feature_font_size * std_letter_width * bp_per_cm,
          feat.name = .fn
        )
        # Expanded-mode fields (transcript indices, row offsets)
        if (!is.null(gene_trn_indices)) {
          .tim = gene_trn_indices[[.fn]]
          .trn.idx = if (!is.null(.tim) && .g %in% names(.tim)) .tim[[.g]] else integer(0)
          .entry$trn.indices = .trn.idx
          .entry$n.trn = length(.trn.idx)
        }
        if (!is.null(gene_row_offsets)) {
          .grp.off = gene_row_offsets[[.fn]]
          .entry$row.offset = if (!is.null(.grp.off) && .g %in% names(.grp.off)) .grp.off[.g] else 0L
        }
        gene_info[[.g]] = .entry
      }
    }
    gene_info
  }

  # --- Stage B: Identify host-enclosed structure ---
  # Returns: list(host_map = gene -> c(all ancestor names),
  #               enclosed_map = host -> c(direct-child names only))
  # enclosed_map is direct-children-only: a gene appears only in the enclosed_map
  # of its *smallest* containing host (its immediate parent).  This prevents
  # nested hosts (e.g. SCARNA9 inside CEP295) from each claiming the same
  # grandchild genes and producing conflicting pair assignments in Stage C.
  # Purely genomic --  no dependency on display rows.
  .find.hosts = function(gene_info) {
    .tic("stage_B")
    host_map     = structure(vector("list", length(gene_info)), names = names(gene_info))
    enclosed_map = list()
    for (.g in names(gene_info)) {
      .gi = gene_info[[.g]]
      .all.hosts = character(0)
      for (.h in names(gene_info)) {
        if (.h == .g) next
        .gh = gene_info[[.h]]
        if (.gh$start <= .gi$start && .gh$end >= .gi$end) {
          .all.hosts = c(.all.hosts, .h)
        }
      }
      host_map[[.g]] = .all.hosts
      # Direct parent = smallest containing gene (narrowest span).
      # Only add this gene to its direct parent's enclosed_map.
      if (length(.all.hosts) > 0L) {
        .spans = sapply(.all.hosts, function(.h) gene_info[[.h]]$end - gene_info[[.h]]$start)
        .direct.parent = .all.hosts[which.min(.spans)]
        if (is.null(enclosed_map[[.direct.parent]])) enclosed_map[[.direct.parent]] = character(0)
        enclosed_map[[.direct.parent]] = c(enclosed_map[[.direct.parent]], .g)
      }
    }
    .toc("stage_B")
    list(host_map = host_map, enclosed_map = enclosed_map)
  }

  # --- Stage C: Pair co-enclosed genes ---
  # Returns: list(forced_side = gene -> "left"/"right", pair_row_offsets = gene -> integer)
  # Purely genomic --  no dependency on display rows.
  # Note: pairing is only *applied* in Stage D if all enclosed genes share the same
  # initial bar row (matching current Step 4c behaviour for near-identical output).
  .pair.enclosed = function(gene_info, enclosed_map, pr_start, pr_end) {
    .tic("stage_C")
    forced_side      = list()
    pair_row_offsets = list()

    # Process hosts outermost-first (largest genomic span first) so that a
    # nested host's pair_row_offset is already known when we process its children.
    .host.order = names(enclosed_map)
    if (length(.host.order) > 1L) {
      .host.spans = sapply(.host.order, function(.h) {
        .gh = gene_info[[.h]]
        if (is.null(.gh)) 0L else .gh$end - .gh$start
      })
      .host.order = .host.order[order(.host.spans, decreasing = TRUE)]
    }

    for (.host in .host.order) {
      .enc = unique(enclosed_map[[.host]])
      if (length(.enc) < 1L) next
      .enc.sorted = .enc[order(sapply(.enc, function(.g) gene_info[[.g]]$start))]

      # Base offset for this host's enclosed genes.
      # If the host is itself enclosed (has a pair_row_offset assigned by its
      # own parent), its children start one row below it:
      #   base = host_offset + 1
      # For an outermost host (not enclosed), base = 0.
      .host.base = if (!is.null(pair_row_offsets[[.host]])) pair_row_offsets[[.host]] + 1L else 0L

      .i = 1L; .cur.offset = .host.base; .assignments = list()

      # Block-shift logic: applies only to outermost hosts (host.base == 0).
      # If the host's name cannot fit inline (blocked by plot bounds or a
      # neighbouring bar), the ILP will place it at +1-below.  In that case,
      # shift the entire enclosed block down by one row so that +1-below is
      # free for the host name: enclosed genes start pairing from offset 1
      # rather than offset 0.  When inline IS feasible the host stays on row 1
      # and enclosed genes pack straight from offset 0.
      # Nested hosts (host.base > 0) are already given a row by their parent;
      # their children start from host_offset+1 regardless.
      if (.host.base == 0L) {
        .gh = gene_info[[.host]]
        .host.inline.ok = FALSE
        if (!is.null(.gh)) {
          .ng_h = .gh$ng; .nw_h = .gh$nw; .tol_h = .nw_h * 0.10
          .ri.xl = .gh$end   + .ng_h; .ri.xr = .ri.xl + .nw_h
          .li.xr = .gh$start - .ng_h; .li.xl = .li.xr - .nw_h
          .ri.ok = .ri.xr <= pr_end   + .tol_h
          .li.ok = .li.xl >= pr_start - .tol_h
          for (.other in names(gene_info)) {
            if (.other == .host) next
            .go = gene_info[[.other]]
            if (.ri.ok && .ri.xr > .go$start && .ri.xl < .go$end) .ri.ok = FALSE
            if (.li.ok && .li.xr > .go$start && .li.xl < .go$end) .li.ok = FALSE
            if (!.ri.ok && !.li.ok) break
          }
          .host.inline.ok = .ri.ok || .li.ok
        }
        if (!.host.inline.ok) {
          # Shift: leave offset 0 (row+1) free for the host name;
          # pair all enclosed genes from offset 1 (row+2) onward.
          .cur.offset = 1L
        }
      }
      while (.i <= length(.enc.sorted)) {
        .g1 = .enc.sorted[.i]; .gi1 = gene_info[[.g1]]
        # Always pair consecutive enclosed genes L/R regardless of inline
        # feasibility.  forced.side now governs all candidate types (inline,
        # below, above, row-0), so pairing just means "left gene's name goes
        # left-side, right gene's name goes right-side" --  the ILP finds the
        # best actual position within those constraints.  The old inline-
        # feasibility check (.g1.L.ok && .g2.R.ok) was too conservative and
        # broke pairing for genes with long names at wide zoom levels.
        if (.i < length(.enc.sorted)) {
          .g2 = .enc.sorted[.i + 1L]; .gi2 = gene_info[[.g2]]
          # Don't pair features that overlap genomically --  they'd draw on top
          # of each other.  Use a minimal 1-bp gap so features with exactly
          # adjacent boundaries also get separate rows.
          .gap.bp = 1L
          .overlaps = .gi1$end + .gap.bp > .gi2$start && .gi2$end + .gap.bp > .gi1$start
          if (!.overlaps) {
            .assignments[[length(.assignments) + 1L]] =
              list(g = .g1, offset = .cur.offset, side = "left")
            .assignments[[length(.assignments) + 1L]] =
              list(g = .g2, offset = .cur.offset, side = "right")
            .i = .i + 2L
          } else {
            # Overlapping: put .g1 solo, try .g2 in next iteration
            .g1.L.ok = (.gi1$start - .gi1$ng - .gi1$nw) >= pr_start - .gi1$nw * 0.10
            .g1.R.ok = (.gi1$end   + .gi1$ng + .gi1$nw) <= pr_end   + .gi1$nw * 0.10
            .side = if (.g1.L.ok) "left" else if (.g1.R.ok) "right" else "left"
            .assignments[[length(.assignments) + 1L]] =
              list(g = .g1, offset = .cur.offset, side = .side)
            .i = .i + 1L
          }
        } else {
          # Odd gene out --  solo.  Prefer left if it fits inline, else right,
          # else left as fallback (below-left is always available).
          .g1.L.ok = (.gi1$start - .gi1$ng - .gi1$nw) >= pr_start - .gi1$nw * 0.10
          .g1.R.ok = (.gi1$end   + .gi1$ng + .gi1$nw) <= pr_end   + .gi1$nw * 0.10
          .side = if (.g1.L.ok) "left" else if (.g1.R.ok) "right" else "left"
          .assignments[[length(.assignments) + 1L]] =
            list(g = .g1, offset = .cur.offset, side = .side)
          .i = .i + 1L
        }
        .cur.offset = .cur.offset + 1L
      }
      for (.a in .assignments) {
        forced_side[[.a$g]]      = .a$side
        pair_row_offsets[[.a$g]] = .a$offset
      }
    }

    # --- Post-process: fix cross-level same-offset collisions ---
    # Genes from different nesting depths can end up at the same pair_row_offset
    # (e.g., a solo child of CEP295 and a solo child of SCARNA9 both at offset 2).
    # Post-process: fix cross-level same-offset collisions.
    # Genes from different nesting depths can end up at the same pair_row_offset
    # (e.g., a solo child of CEP295 and a solo child of SCARNA9 both at offset 2).
    # Sort all genes at each offset by genomic start and reassign L/R in order.
    # For each gene assigned "left", verify that inline-left actually fits within
    # the plotted region (xl = gene.start - ng - nw >= pr_start - nw*0.10).
    # If it doesn't fit, leave that gene's side unconstrained (NULL) so the ILP
    # can use either side --  it will place the label below on whichever side clears
    # its neighbour's bar, avoiding the "edge-pinned to left of plot" artifact.
    .all.offsets = unique(unlist(pair_row_offsets))
    for (.off in .all.offsets) {
      .genes.at.off = names(pair_row_offsets)[
        sapply(names(pair_row_offsets), function(.g) pair_row_offsets[[.g]] == .off)]
      if (length(.genes.at.off) < 2L) next
      .sorted = .genes.at.off[order(sapply(.genes.at.off,
                                           function(.g) gene_info[[.g]]$start))]
      for (.k in seq_along(.sorted)) {
        .g = .sorted[.k]
        .gi = gene_info[[.g]]
        if ((.k %% 2L) == 1L) {
          # Would be "left" --  check inline-left feasibility.
          # Inline-left: xr = gene.start - ng, xl = gene.start - ng - nw
          .xl.left = .gi$start - .gi$ng - .gi$nw
          if (.xl.left >= pr_start - .gi$nw * 0.10) {
            forced_side[[.g]] = "left"
          } else {
            # Name too long for left placement; leave unconstrained.
            forced_side[[.g]] = NULL
          }
        } else {
          forced_side[[.g]] = "right"
        }
      }
    }

    .toc("stage_C")
    list(forced_side = forced_side, pair_row_offsets = pair_row_offsets)
  }

  # --- Stage D: Single-pass bar packing with pixel gap ---
  # Re-packs each collapsed2 group with a 1-pixel bar gap (requires bp_per_cm).
  # Applies pair_row_offsets from Stage C for same-row enclosed gene groups.
  # Returns: list(bar_rows = gene -> integer row, packing2_display = group -> list of row vecs,
  #               applied_genes = character vector of genes that had offsets applied)
  .c2.pack.bars = function(annot_info, annot_name, gene_info, pair_row_offsets,
                            enclosed_map, c2, c2_names_vec, bp_per_cm,
                            is_collapsed_mode = FALSE) {
    .tic("stage_D")
    gap_bp = ceiling(bp_per_cm / 28.35)  # 1 pixel at 72 dpi
    bar_rows        = list()
    # Initialise from packing2 so singleton entries (individual gene keys) are
    # preserved --  Stage D only rewrites multi-gene group entries.
    packing2_display = annot_info[[annot_name]][['packing2']]
    if (is.null(packing2_display)) packing2_display = list()
    applied_genes   = character(0)

    # Collapsed mode: each packing2 key is one super-locus drawn on row 1.
    # Skip the multi-gene re-packing branch entirely so super-merged loci
    # (e.g. "TAF1D,RP11-178H8.7") stay as one bar instead of stacking N rows.
    # Force packing2_display[[fn]] = list(1L) --  OrganizeOverlappingLoci can have
    # populated packing2 with multiple sub-feature rows for super-merged loci,
    # which would otherwise inflate max.display.row and add blank rows.
    if (isTRUE(is_collapsed_mode)) {
      for (.fn in names(annot_info[[annot_name]][['packing2']])) {
        if (.fn %in% names(gene_info)) {
          bar_rows[[.fn]] = 1L
          packing2_display[[.fn]] = list(1L)
        }
      }
      return(list(bar_rows = bar_rows, packing2_display = packing2_display,
                  applied_genes = applied_genes))
    }

    for (.fn in names(annot_info[[annot_name]][['packing2']])) {
      .genes = if (!grepl(",", .fn) || .fn %in% c2_names_vec)
        .fn else strsplit(.fn, ",")[[1]]
      .valid = .genes[.genes %in% names(gene_info)]
      .sub.idx = which(c2_names_vec %in% .valid)
      .sub.names = c2_names_vec[.sub.idx]

      if (length(.valid) == 0L) {
        # No valid genes --  leave the packing2 entry untouched (already in packing2_display)
        next
      }

      # Singleton groups: bar row is trivially 1; no repacking needed.
      # Keep the packing2 entry as-is (already copied into packing2_display).
      if (length(.valid) == 1L) {
        .g = .valid[1L]
        bar_rows[[.g]] = 1L
        next
      }

      # Multi-gene groups: re-pack with pixel gap.
      .sub.ann = c2[.sub.idx]
      .raw.pk = OrganizeOverlappingIVs(.sub.ann, gap = gap_bp)
      # Reorder rows: widest gene on row 1
      .sa.w = IRanges::width(.sub.ann)
      .row.w = vapply(.raw.pk, function(.ri) max(.sa.w[.ri]), numeric(1L))
      .raw.pk = .raw.pk[order(.row.w, decreasing = TRUE)]

      # Read initial rows from repacked result
      .init.rows = integer(length(.sub.names))
      for (.r in seq_along(.raw.pk)) {
        for (.idx in .raw.pk[[.r]]) {
          if (.idx < 1L || .idx > length(.sub.names)) {
            .dbg("[c2names]   WARNING StageD: index ", .idx, " out of range 1:",
                    length(.sub.names), " for group '", .fn, "'")
            next
          }
          .init.rows[.idx] = .r
        }
      }

      # Apply pair_row_offsets for any enclosed genes in this group.
      # Pairing is purely genomic (Stage C) --  no same-row check needed.
      # Base row = max row of non-enclosed genes + 1.  This handles the case
      # where pixel-gap repacking spreads tightly-clustered enclosed features
      # across different initial rows, which was causing the old same-row guard
      # to silently skip pairing.
      .final.rows = .init.rows
      .any.changed = FALSE
      .enc.in.group = .sub.names[.sub.names %in% names(pair_row_offsets)]
      if (length(.enc.in.group) > 0L) {
        .non.enc.sub = setdiff(.sub.names, .enc.in.group)
        .non.enc.rows = if (length(.non.enc.sub) > 0L) {
          .r = .init.rows[match(.non.enc.sub, .sub.names)]
          .r[.r > 0L]
        } else integer(0)
        .base = if (length(.non.enc.rows) > 0L) max(.non.enc.rows) + 1L else 2L
        for (.g in .enc.in.group) {
          .gi = match(.g, .sub.names)
          .final.rows[.gi] = as.integer(.base + pair_row_offsets[[.g]])
          applied_genes = c(applied_genes, .g)
          .any.changed = TRUE
        }
        # Log pairing result
        .hosts.here = names(enclosed_map)[sapply(names(enclosed_map),
          function(.h) length(intersect(enclosed_map[[.h]], .enc.in.group)) > 0L)]
        for (.host in .hosts.here) {
          .enc.sorted = .enc.in.group[order(sapply(.enc.in.group,
            function(.g) gene_info[[.g]]$start))]
          .pair.info = paste(sapply(.enc.sorted, function(.g) {
            .fi = match(.g, .sub.names)
            .side = if (!is.null(.c2.forced.side[[.g]])) paste0("(", toupper(substr(.c2.forced.side[[.g]], 1, 1)), ")") else ""
            paste0(.g, "->row", .final.rows[.fi], .side)
          }), collapse = " ")
          .dbg("[c2names]   paired ", length(.enc.sorted),
                  " enclosed genes of host '", .host, "': ", .pair.info)
        }
      }

      # Build packing2_display and assign bar_rows
      .new.pk = list()
      for (.idx in seq_along(.sub.names)) {
        .g = .sub.names[.idx]
        if (.init.rows[.idx] == 0L) next
        bar_rows[[.g]] = .final.rows[.idx]
        while (length(.new.pk) < .final.rows[.idx])
          .new.pk[[length(.new.pk) + 1L]] = integer(0)
        .new.pk[[.final.rows[.idx]]] = c(.new.pk[[.final.rows[.idx]]], .idx)
      }
      packing2_display[[.fn]] = .new.pk

      if (.any.changed) {
        .pk.summary = paste(sapply(seq_along(.new.pk), function(.r)
          paste0("row", .r, ":[", paste(.new.pk[[.r]], collapse = ","), "]")
        ), collapse = " ")
        .dbg("[c2names]   packing2_display rebuilt for '", .fn, "': ", .pk.summary)
      }
    }
    .toc("stage_D")
    list(bar_rows = bar_rows, packing2_display = packing2_display,
         applied_genes = applied_genes)
  }

  # --- Stage E: Build obstacle map (once) ---
  # Returns: global_row_bars (row_key -> list of list(start, end, gene))
  .build.obstacle.map = function(gene_info, bar_rows) {
    global_row_bars = list()
    for (.g in names(gene_info)) {
      .dr = bar_rows[[.g]]
      if (is.null(.dr)) next
      .gi = gene_info[[.g]]
      .rk = as.character(.dr)
      if (is.null(global_row_bars[[.rk]])) global_row_bars[[.rk]] = list()
      global_row_bars[[.rk]][[length(global_row_bars[[.rk]]) + 1L]] = list(
        start = .gi$start, end = .gi$end, gene = .g
      )
    }
    global_row_bars
  }

  # --- Shared ILP solver for name placement ---
  # Clusters genes by x-span overlap, builds ompr MIP per cluster, returns placements.
  # flat_genes: character vector of gene names (sorted left-to-right)
  # all_candidates: named list gene -> list of candidate lists (each with $score, $valid, $xmin, $xmax, $row, $x, $adj)
  .solve.name.placement.ilp = function(flat_genes, all_candidates) {
    .tic("ilp_total")
    if (length(flat_genes) == 0L) { .toc("ilp_total"); return(list()) }

    for (.ilp.pkg in c("ompr", "ompr.roi", "ROI.plugin.glpk")) {
      if (!requireNamespace(.ilp.pkg, quietly = TRUE))
        stop("Package '", .ilp.pkg, "' is required for ILP name placement. ",
             "Install with: install.packages('", .ilp.pkg, "')")
    }

    # Flatten all (gene, candidate) pairs to a 1-D index
    .flat.pairs = list()
    .gf.start   = integer(length(flat_genes))
    .gf.end     = integer(length(flat_genes))
    .n.flat     = 0L
    for (.fg.i in seq_along(flat_genes)) {
      .g = flat_genes[.fg.i]
      .gf.start[.fg.i] = .n.flat + 1L
      for (.k in seq_along(all_candidates[[.g]])) {
        .n.flat = .n.flat + 1L
        .cc = all_candidates[[.g]][[.k]]
        .flat.pairs[[.n.flat]] = list(
          fg.i  = .fg.i,  k     = .k,
          score = .cc$score, valid = isTRUE(.cc$valid),
          xmin  = .cc$xmin,  xmax  = .cc$xmax,
          row   = .cc$row
        )
      }
      .gf.end[.fg.i] = .n.flat
    }
    .N.flat      = .n.flat
    .flat.scores = vapply(.flat.pairs, `[[`, numeric(1L), "score")

    # Cluster genes whose candidate x-spans overlap or touch
    .gxmins = vapply(flat_genes, function(.g)
      min(vapply(all_candidates[[.g]], `[[`, numeric(1L), "xmin")), numeric(1L))
    .gxmaxs = vapply(flat_genes, function(.g)
      max(vapply(all_candidates[[.g]], `[[`, numeric(1L), "xmax")), numeric(1L))
    .go = order(.gxmins)
    .clusters = list(); .cl.cur = integer(0); .cl.xmax = -Inf
    for (.ci in .go) {
      if (.gxmins[.ci] > .cl.xmax && length(.cl.cur) > 0) {
        .clusters[[length(.clusters) + 1L]] = .cl.cur
        .cl.cur = .ci; .cl.xmax = .gxmaxs[.ci]
      } else {
        .cl.cur = c(.cl.cur, .ci); .cl.xmax = max(.cl.xmax, .gxmaxs[.ci])
      }
    }
    if (length(.cl.cur) > 0) .clusters[[length(.clusters) + 1L]] = .cl.cur

    .ilp.placements = list()

    for (.cl.gi in .clusters) {
      .cluster_t0 <- Sys.time()
      .cl.ns = unlist(lapply(.cl.gi, function(.i) .gf.start[.i]:.gf.end[.i]))
      .cl.N  = length(.cl.ns)
      .remap = setNames(seq_len(.cl.N), .cl.ns)
      .cl.fp = .flat.pairs[.cl.ns]
      cl_sc  = .flat.scores[.cl.ns]
      cl_sc[!is.finite(cl_sc)] = -1e9

      .cl.G  = length(.cl.gi)

      ## ---- Fast path: single-gene cluster with unique max-score ----
      ## When a cluster contains exactly one gene AND there is exactly one
      ## maximum-score valid candidate (no ties), bypassing the ILP gives
      ## a placement IDENTICAL to the ILP solution. The lambda-row-penalty
      ## is constant for a single-gene single-row choice, and a unique max
      ## leaves nothing for the solver to tie-break.
      ## When the max is tied across multiple equally-optimal candidates,
      ## we fall through to the full ILP so the chosen placement matches
      ## the original behaviour byte-for-byte (the GLPK tie-break is
      ## solver-internal and not generally reproducible by a plain
      ## which.max in R).
      ## Set options(seqNdisplayR.disable_fast_ilp = TRUE) to force the
      ## ILP path for comparison/debugging.
      if (.cl.G == 1L &&
          !isTRUE(getOption("seqNdisplayR.disable_fast_ilp", FALSE))) {
        .valid <- vapply(.cl.fp, function(.p) isTRUE(.p$valid), logical(1L))
        .sc.fp <- cl_sc
        .sc.fp[!.valid] <- -Inf
        if (any(is.finite(.sc.fp))) {
          .max.sc <- max(.sc.fp)
          .n.ties <- sum(.sc.fp == .max.sc)
          if (.n.ties == 1L) {
            .best.k <- which.max(.sc.fp)
            .g.name <- flat_genes[.cl.gi[1L]]
            .oc     <- all_candidates[[.g.name]][[.cl.fp[[.best.k]]$k]]
            .ilp.placements[[.g.name]] <- list(x = .oc$x, adj = .oc$adj, row = .oc$row)
            .record_cluster(.cl.G, .cl.N, as.numeric(Sys.time() - .cluster_t0, units = "secs"))
            next
          }
          # else: fall through to the ILP for deterministic tie-break.
        } else {
          # No valid candidate at all -> nothing to place.
          .record_cluster(.cl.G, .cl.N, as.numeric(Sys.time() - .cluster_t0, units = "secs"))
          next
        }
      }

      .cl.gs = vapply(.cl.gi, function(.i) .remap[as.character(.gf.start[.i])], integer(1L))
      .cl.ge = vapply(.cl.gi, function(.i) .remap[as.character(.gf.end[.i])],   integer(1L))

      .cl.rows = sort(unique(vapply(.cl.fp, `[[`, numeric(1L), "row")))
      .cl.R    = length(.cl.rows)
      .cl.Ri   = setNames(seq_len(.cl.R), .cl.rows)

      # Pre-compute conflicting pairs
      .tic("ilp_conflicts")
      .cl.cf = list()
      for (.n1 in seq_len(.cl.N - 1L)) {
        p1 = .cl.fp[[.n1]]
        if (!p1$valid) next
        for (.n2 in (.n1 + 1L):.cl.N) {
          p2 = .cl.fp[[.n2]]
          if (!p2$valid || p1$fg.i == p2$fg.i) next
          if (p1$row != p2$row) next
          if (p1$xmax <= p2$xmin || p2$xmax <= p1$xmin) next
          .cl.cf[[length(.cl.cf) + 1L]] = c(.n1, .n2)
        }
      }
      .toc("ilp_conflicts")

      .lam = 0.01 / max(1L, .cl.R)

      # ---- Cluster LP build/solve --------------------------------------
      # Two backends share the same input shape and return the same shape:
      #   list(status, objval, xn_chosen)
      # where xn_chosen = sorted integer indices into 1:.cl.N of the
      # candidates picked by the solver.
      # Default: direct sparse-matrix ROI construction (5-20x faster on
      # large clusters because it bypasses the ompr add_constraint DSL).
      # Toggles:
      #   options(seqNdisplayR.use_legacy_ilp = TRUE)
      #     -> use ompr build path verbatim (rollback).
      #   options(seqNdisplayR.verify_ilp = TRUE)
      #     -> solve with BOTH, assert objective equality, log any
      #        tied-objective tie-break differences via .dbg().

      .solve_legacy <- function() {
        .m = ompr::MIPModel()
        .m = ompr::add_variable(.m, xn[n], n = 1:.cl.N, type = "binary")
        .m = ompr::add_variable(.m, yr[r], r = 1:.cl.R, type = "binary")
        for (.i in seq_len(.cl.G)) {
          .a = .cl.gs[.i]; .b = .cl.ge[.i]
          .m = ompr::add_constraint(.m, ompr::sum_expr(xn[n], n = .a:.b) == 1)
        }
        for (.n in seq_len(.cl.N)) {
          if (!.cl.fp[[.n]]$valid)
            .m = ompr::add_constraint(.m, xn[.n] == 0)
        }
        for (.n in seq_len(.cl.N)) {
          .r = .cl.Ri[as.character(.cl.fp[[.n]]$row)]
          .m = ompr::add_constraint(.m, xn[.n] <= yr[.r])
        }
        for (.cf in .cl.cf) {
          .m = ompr::add_constraint(.m, xn[.cf[1L]] + xn[.cf[2L]] <= 1)
        }
        .m = ompr::set_objective(
          .m,
          ompr::sum_expr(xn[n] * cl_sc[n], n = 1:.cl.N) -
            .lam * ompr::sum_expr(yr[r], r = 1:.cl.R),
          "max"
        )
        .res = ompr::solve_model(.m, ompr.roi::with_ROI(solver = "glpk", verbose = FALSE))
        .st  = ompr::solver_status(.res)
        if (!.st %in% c("optimal", "success"))
          return(list(status = .st, objval = NA_real_, xn_chosen = integer(0)))
        .sol = ompr::get_solution(.res, xn[n])
        list(status    = .st,
             objval    = ompr::objective_value(.res),
             xn_chosen = sort(as.integer(.sol$n[.sol$value > 0.5])))
      }

      .solve_direct <- function() {
        blocks  <- list()
        row_off <- 0L

        # Block 1: gene equalities -- sum_{n in [a,b]} xn[n] == 1
        gw     <- .cl.ge - .cl.gs + 1L
        g_rows <- rep(seq_len(.cl.G), gw)
        g_cols <- unlist(lapply(seq_len(.cl.G), function(g) .cl.gs[g]:.cl.ge[g]))
        blocks[["genes"]] <- list(
          i = g_rows + row_off, j = g_cols, v = rep(1.0, length(g_rows)),
          dir = rep("==", .cl.G), rhs = rep(1.0, .cl.G)
        )
        row_off <- row_off + .cl.G

        # Block 2: invalid candidate equalities -- xn[n] == 0
        .invalid <- which(!vapply(.cl.fp, function(p) isTRUE(p$valid), logical(1L)))
        if (length(.invalid)) {
          blocks[["invalid"]] <- list(
            i = seq_along(.invalid) + row_off, j = .invalid,
            v = rep(1.0, length(.invalid)),
            dir = rep("==", length(.invalid)), rhs = rep(0.0, length(.invalid))
          )
          row_off <- row_off + length(.invalid)
        }

        # Block 3: row-linking -- xn[n] - yr[Ri[row(n)]] <= 0
        r_yr <- .cl.N + vapply(.cl.fp, function(p)
          as.integer(.cl.Ri[as.character(p$row)]), integer(1L))
        link_rows <- seq_len(.cl.N) + row_off
        blocks[["link"]] <- list(
          i = c(link_rows, link_rows),
          j = c(seq_len(.cl.N), r_yr),
          v = c(rep(1.0, .cl.N), rep(-1.0, .cl.N)),
          dir = rep("<=", .cl.N), rhs = rep(0.0, .cl.N)
        )
        row_off <- row_off + .cl.N

        # Block 4: conflict pairs -- xn[a] + xn[b] <= 1
        if (length(.cl.cf)) {
          cfm <- do.call(rbind, .cl.cf)
          cf_rows <- seq_len(nrow(cfm)) + row_off
          blocks[["conflict"]] <- list(
            i = c(cf_rows, cf_rows),
            j = c(cfm[, 1L], cfm[, 2L]),
            v = rep(1.0, 2L * nrow(cfm)),
            dir = rep("<=", nrow(cfm)), rhs = rep(1.0, nrow(cfm))
          )
          row_off <- row_off + nrow(cfm)
        }

        n_rows <- row_off
        n_vars <- .cl.N + .cl.R
        i_all   <- as.integer(unlist(lapply(blocks, `[[`, "i")))
        j_all   <- as.integer(unlist(lapply(blocks, `[[`, "j")))
        v_all   <- as.numeric(unlist(lapply(blocks, `[[`, "v")))
        dir_all <- unlist(lapply(blocks, `[[`, "dir"))
        rhs_all <- as.numeric(unlist(lapply(blocks, `[[`, "rhs")))

        .tic("ilp_build_direct")
        A <- slam::simple_triplet_matrix(
          i = i_all, j = j_all, v = v_all,
          nrow = n_rows, ncol = n_vars
        )
        constr <- ROI::L_constraint(A, dir = dir_all, rhs = rhs_all)
        obj    <- ROI::L_objective(c(cl_sc, rep(-.lam, .cl.R)))
        op     <- ROI::OP(objective = obj, constraints = constr,
                          types = rep("B", n_vars), maximum = TRUE)
        .toc("ilp_build_direct")
        .tic("ilp_solve_glpk")
        res    <- ROI::ROI_solve(op, solver = "glpk")
        .toc("ilp_solve_glpk")
        if (!isTRUE(res$status$code == 0L)) {
          return(list(status = paste0("code=", res$status$code),
                      objval = NA_real_, xn_chosen = integer(0)))
        }
        sol <- ROI::solution(res)
        list(status    = "optimal",
             objval    = as.numeric(res$objval),
             xn_chosen = sort(which(sol[seq_len(.cl.N)] > 0.5)))
      }

      .use_legacy <- isTRUE(getOption("seqNdisplayR.use_legacy_ilp", FALSE))
      .verify     <- isTRUE(getOption("seqNdisplayR.verify_ilp",     FALSE))

      if (.verify) {
        .leg <- .solve_legacy()
        .dir <- .solve_direct()
        .vt_env <- getOption("seqNdisplayR.stage_timer", default = NULL)
        .objval_match <- isTRUE(all.equal(.leg$objval, .dir$objval, tolerance = 1e-6))
        .chosen_match <- identical(.leg$xn_chosen, .dir$xn_chosen)
        if (!.objval_match) {
          message("[verify-ilp] OBJVAL MISMATCH: legacy=", .leg$objval,
                  " direct=", .dir$objval, " (G=", .cl.G, " N=", .cl.N, ")")
        } else if (!.chosen_match) {
          message("[verify-ilp] tied tie-break differs (G=", .cl.G,
                  " N=", .cl.N, ") legacy=[",
                  paste(.leg$xn_chosen, collapse = ","), "] direct=[",
                  paste(.dir$xn_chosen, collapse = ","), "]")
        }
        if (!is.null(.vt_env)) {
          .vc <- if (exists("verify_counts", envir = .vt_env, inherits = FALSE))
            get("verify_counts", envir = .vt_env) else list(n = 0L, obj_mm = 0L, tie_diff = 0L)
          .vc$n <- .vc$n + 1L
          if (!.objval_match)      .vc$obj_mm  <- .vc$obj_mm  + 1L
          else if (!.chosen_match) .vc$tie_diff <- .vc$tie_diff + 1L
          assign("verify_counts", .vc, envir = .vt_env)
        }
        .solved <- if (.use_legacy) .leg else .dir
      } else if (.use_legacy) {
        .solved <- .solve_legacy()
      } else {
        .solved <- .solve_direct()
      }

      if (!.solved$status %in% c("optimal", "success")) {
        .dbg("[ilp] cluster failed (", .solved$status, ") - skipping")
        .record_cluster(.cl.G, .cl.N, as.numeric(Sys.time() - .cluster_t0, units = "secs"))
        next
      }

      for (.i in seq_len(.cl.G)) {
        .g.name = flat_genes[.cl.gi[.i]]
        .a = .cl.gs[.i]; .b = .cl.ge[.i]
        .chosen = .solved$xn_chosen[.solved$xn_chosen >= .a & .solved$xn_chosen <= .b]
        if (length(.chosen) == 0L) next
        .fp.ch = .cl.fp[[.chosen[1L]]]
        .oc    = all_candidates[[.g.name]][[.fp.ch$k]]
        .ilp.placements[[.g.name]] = list(x = .oc$x, adj = .oc$adj, row = .oc$row)
      }
      .record_cluster(.cl.G, .cl.N, as.numeric(Sys.time() - .cluster_t0, units = "secs"))
    }

    .toc("ilp_total")
    .ilp.placements
  }

  for (.annot.name in names(annot_info)) {
    annot_info[[.annot.name]][['inline_name_placements']] = NULL
    if (is.null(annot_info[[.annot.name]][['packing']])) next
    if (is.null(annot_info[[.annot.name]][['expanded']])) next
    # Skip annotations with names turned off (same logic as c2 block at ~line 2775)
    if (!is.null(incl_feature_names) && isFALSE(incl_feature_names[.annot.name])) next
    
    .placements = list()
    .c2 = annot_info[[.annot.name]][['collapsed2']]
    if (is.null(.c2)) next
    .c2.names = S4Vectors::mcols(.c2)$name
    .gene.row.offsets.all = annot_info[[.annot.name]][['gene_row_offsets']]

    # Debug: dump all packing group names for this annotation
    .dbg.pk.names = names(annot_info[[.annot.name]][['packing']])
    .dbg("[exp-debug] annotation='", .annot.name, "' packing groups: [",
            paste(sapply(.dbg.pk.names, function(.n) {
              .ntrn = length(annot_info[[.annot.name]][['expanded']][[.n]])
              paste0(.n, "(", .ntrn, "trn)")
            }), collapse=", "), "]")

    # Insert visual separator rows between adjacent packing rows whose
    # LARGE-gene (n.trn >= 2) sets differ. The separator is an empty
    # integer(0) row. Modifies annot_info[[annot]][['packing']] in place.
    for (.fn.sep in names(annot_info[[.annot.name]][['packing']])) {
      .pk.sep = annot_info[[.annot.name]][['packing']][[.fn.sep]]
      .tim.sep = annot_info[[.annot.name]][['gene_trn_indices']][[.fn.sep]]
      if (is.null(.tim.sep) || length(.pk.sep) < 2) next
      # Compute n.trn per gene for this feature group
      .gnt.sep = sapply(.tim.sep, length)
      # Per-row large-gene set
      .row.lg = lapply(.pk.sep, function(.row.trns) {
        if (length(.row.trns) == 0) return(character(0))
        out = character(0)
        for (.gn in names(.tim.sep)) {
          if (.gnt.sep[[.gn]] < 2L) next
          if (any(.row.trns %in% .tim.sep[[.gn]])) out = c(out, .gn)
        }
        out
      })
      .new.pk.sep = list()
      for (.i.sep in seq_along(.pk.sep)) {
        .new.pk.sep[[length(.new.pk.sep) + 1]] = .pk.sep[[.i.sep]]
        if (.i.sep < length(.pk.sep)) {
          .curr.lg = .row.lg[[.i.sep]]; .next.lg = .row.lg[[.i.sep + 1]]
          # Insert separator only if both rows have a large gene AND the two
          # large-gene sets are DISJOINT (no shared gene). A gene shared
          # across the boundary visually connects the rows; no separator needed.
          if (length(.curr.lg) > 0 && length(.next.lg) > 0 &&
              length(intersect(.curr.lg, .next.lg)) == 0) {
            .new.pk.sep[[length(.new.pk.sep) + 1]] = integer(0)
            .dbg("[exp-sep] feat='", .fn.sep, "' separator inserted between row ",
                    .i.sep, " (", paste(.curr.lg, collapse=","),
                    ") and row ", .i.sep + 1L, " (", paste(.next.lg, collapse=","), ")")
          }
        }
      }
      annot_info[[.annot.name]][['packing']][[.fn.sep]] = .new.pk.sep
    }

    # When brackets are ON for this annotation, all names live in the bracket
    # section (built by the c2 loop further down).  Skip the expanded inline-name
    # pipeline entirely --  Phase 0 / Phase 1 / Phase 2 / B-cleanup are not needed.
    # Transcript rendering is unaffected (separator-row insertion above already ran).
    .brackets.on = !is.null(incl_feature_brackets) &&
                   isTRUE(incl_feature_brackets[.annot.name])
    if (.brackets.on) {
      .dbg("[exp-brackets] annotation='", .annot.name,
              "' brackets ON - skipping expanded inline-name pipeline")
      next
    }

    # Build FULL global gene-block obstacle map.
    # Each gene's transcript group is treated as an impenetrable block:
    #   xmin = min(all transcript starts), xmax = max(all transcript ends),
    #   spanning every row from first_row to last_row.
    # Names from other genes cannot be placed inside a gene's block.
    .global.row.trn = list()  # row (as string) -> list of (start, end, gene)
    for (.fn in names(annot_info[[.annot.name]][['packing']])) {
      .pk = annot_info[[.annot.name]][['packing']][[.fn]]
      .eg = annot_info[[.annot.name]][['expanded']][[.fn]]
      if (is.null(.eg) || length(.eg) == 0) next
      .es = IRanges::start(.eg)
      .ee = IRanges::end(.eg)
      .tim = annot_info[[.annot.name]][['gene_trn_indices']][[.fn]]

      # Collect per-gene blocks: (gene_name -> list(xmin, xmax, first_row, last_row))
      # Use collapsed2 ranges for xmin/xmax (full gene span) instead of expanded
      # transcript coordinates (which may be truncated to exon boundaries).
      .gene.blocks = list()
      if (!is.null(.tim)) {
        for (.gn in names(.tim)) {
          .ti = .tim[[.gn]]
          # Get full gene range from collapsed2 entries
          .c2.idx = which(.c2.names == .gn)
          if (length(.c2.idx) > 0) {
            .xmin = min(IRanges::start(.c2[.c2.idx]))
            .xmax = max(IRanges::end(.c2[.c2.idx]))
          } else {
            # Fallback to expanded coordinates
            .xmin = min(.es[.ti])
            .xmax = max(.ee[.ti])
          }
          .grow = integer(0)
          for (.r in seq_along(.pk)) {
            if (any(.pk[[.r]] %in% .ti)) .grow = c(.grow, .r)
          }
          if (length(.grow) > 0) {
            .gene.blocks[[.gn]] = list(xmin = .xmin, xmax = .xmax,
                                       first = min(.grow), last = max(.grow))
          }
        }
      } else {
        # Single-gene group --  use collapsed2 range
        .c2.idx = which(.c2.names == .fn)
        .grow = which(sapply(.pk, function(.r) length(.r) > 0))
        if (length(.grow) > 0) {
          if (length(.c2.idx) > 0) {
            .gene.blocks[[.fn]] = list(xmin = min(IRanges::start(.c2[.c2.idx])),
                                       xmax = max(IRanges::end(.c2[.c2.idx])),
                                       first = min(.grow), last = max(.grow))
          } else {
            .gene.blocks[[.fn]] = list(xmin = min(.es), xmax = max(.ee),
                                       first = min(.grow), last = max(.grow))
          }
        }
      }

      # Add each gene block to every row it spans
      for (.gn in names(.gene.blocks)) {
        .blk = .gene.blocks[[.gn]]
        for (.r in .blk$first:.blk$last) {
          .rk = as.character(.r)
          if (is.null(.global.row.trn[[.rk]])) .global.row.trn[[.rk]] = list()
          .global.row.trn[[.rk]][[length(.global.row.trn[[.rk]]) + 1]] = list(
            start = .blk$xmin, end = .blk$xmax, gene = .gn
          )
        }
      }
    }
    .global.placed.rects = list()

    # Debug: dump obstacle map contents
    for (.dbg.rk in sort(names(.global.row.trn))) {
      .dbg.entries = sapply(.global.row.trn[[.dbg.rk]], function(.e)
        paste0(.e$gene, "[", round(.e$start), "-", round(.e$end), "]"))
      .dbg("[exp-obsmap] row=", .dbg.rk, " entries: ", paste(.dbg.entries, collapse=", "))
    }

    # N(total): global row count from packing ALL top-level features in this annotation.
    # Used to decide whether enclosed features should be spread (fit within N_total)
    # or compacted collapsed2-style (exceed N_total).
    .N.total = max(sapply(annot_info[[.annot.name]][['packing']], length))

    # Process gene groups in order: groups containing large genes first, then small-gene-only groups.
    # This ensures large gene transcripts are in the global map before small gene placement.
    .all.feat.names = names(annot_info[[.annot.name]][['packing']])
    .feat.has.large = sapply(.all.feat.names, function(.fn) {
      .eg = annot_info[[.annot.name]][['expanded']][[.fn]]
      if (is.null(.eg) || length(.eg) == 0) return(FALSE)
      .tim = annot_info[[.annot.name]][['gene_trn_indices']][[.fn]]
      if (is.null(.tim)) return(length(.eg) >= 3)
      any(sapply(.tim, length) >= 3)
    })
    .feat.order = c(.all.feat.names[.feat.has.large], .all.feat.names[!.feat.has.large])
    
    for (.feat.name in .feat.order) {
      .packing = annot_info[[.annot.name]][['packing']][[.feat.name]]
      .trn.idx.map = annot_info[[.annot.name]][['gene_trn_indices']][[.feat.name]]
      .expanded.gr = annot_info[[.annot.name]][['expanded']][[.feat.name]]
      if (is.null(.expanded.gr) || length(.expanded.gr) == 0) next
      
      .fa.starts = IRanges::start(.expanded.gr)
      .fa.ends = IRanges::end(.expanded.gr)
      # Cut-intron flags: when a transcript crosses the plot border, .fa.starts/
      # .fa.ends is reset to the first/last visible exon (annotations.R ~line 492),
      # but the visual rendering draws a cut-intron line extending to the plot
      # border. For occupancy, treat that visual extension as occupied.
      .fa.mcols = S4Vectors::mcols(.expanded.gr)
      .fa.intron.start = if ('intron.from.start' %in% names(.fa.mcols))
                           as.logical(.fa.mcols$intron.from.start) else logical(length(.fa.starts))
      .fa.intron.end   = if ('intron.from.end' %in% names(.fa.mcols))
                           as.logical(.fa.mcols$intron.from.end)   else logical(length(.fa.ends))
      .fa.intron.start[is.na(.fa.intron.start)] = FALSE
      .fa.intron.end[is.na(.fa.intron.end)]     = FALSE
      .total.packing.rows = length(.packing)
      
      if (grepl(",", .feat.name)) {
        .genes = strsplit(.feat.name, ",")[[1]]
      } else {
        .genes = .feat.name
      }
      
      .grp.offsets = .gene.row.offsets.all[[.feat.name]]
      
      # Collect gene info
      .gene.info = list()
      for (.g in .genes) {
        .idx = which(.c2.names == .g)
        if (length(.idx) == 0) next
        .trn.idx = if (!is.null(.trn.idx.map) && .g %in% names(.trn.idx.map)) .trn.idx.map[[.g]] else integer(0)
        .gene.info[[.g]] = list(
          start = min(IRanges::start(.c2[.idx])),
          end = max(IRanges::end(.c2[.idx])),
          trn.indices = .trn.idx,
          n.trn = length(.trn.idx),
          row.offset = if (!is.null(.grp.offsets) && .g %in% names(.grp.offsets)) .grp.offsets[.g] else 0L
        )
      }
      
      # Separate into large (>=3 transcripts) and small (1-2 transcripts) genes
      .large.genes = names(which(sapply(.gene.info, `[[`, 'n.trn') >= 3))
      .small.genes = names(which(sapply(.gene.info, `[[`, 'n.trn') < 3))
      # Sort each group left-to-right
      if (length(.large.genes) > 1) .large.genes = .large.genes[order(sapply(.gene.info[.large.genes], `[[`, 'start'))]
      if (length(.small.genes) > 1) .small.genes = .small.genes[order(sapply(.gene.info[.small.genes], `[[`, 'start'))]
      
      .placed.rects = list()
      .feat.placements = list()
      
      # Debug: log feature group contents
      .dbg("[exp-grp] feat='", .feat.name, "' large=[",
              paste(.large.genes, collapse=", "), "] small=[",
              paste(.small.genes, collapse=", "), "]")
      for (.g in names(.gene.info)) {
        .gi = .gene.info[[.g]]
        .dbg("[exp-grp]   '", .g, "' n.trn=", .gi$n.trn,
                " start=", .gi$start, " end=", .gi$end,
                " offset=", .gi$row.offset)
      }

      # Pre-compute host-enclosed relationships for the whole group.
      # Used in Phase 1 to exclude enclosed genes' blocks from the obstacle map
      # when placing a host gene's name (the enclosed bars will be moved by Phase 2).
      .pre.host.struct = .find.hosts(.gene.info)
      .pre.enclosed.map = .pre.host.struct$enclosed_map  # host -> direct children
      # Build full descendant map: host -> ALL descendants (including through nested hosts)
      .pre.all.descendants = list()
      for (.h in names(.pre.enclosed.map)) {
        .all.desc = names(which(sapply(.pre.host.struct$host_map, function(.ancestors) .h %in% .ancestors)))
        .pre.all.descendants[[.h]] = .all.desc
      }

      # ===== ENCOMPASSMENT HELPERS (used by Phase 0 tuck and any later checks) =====
      # Helper: run the iterative encompassment scan for a forced starting side
      # ("left" or "right") at +row1. Returns the run (list of {row, interval}).
      # Closes over per-feature-group variables (.packing, .fa.starts, etc.).
      .encompass.scan = function(.scan.rows, .forced.side, .gi.h, .blk.s.h, .blk.e.h, .nw.h, .g.h) {
        .tunnel = NULL
        .run = list()
        for (.r in .scan.rows) {
          .occ = list()
          .trn.self = intersect(.packing[[.r]], .gi.h$trn.indices)
          for (.idx in .trn.self) {
            .ts = if (.fa.intron.start[.idx]) .pr.start else .fa.starts[.idx]
            .te = if (.fa.intron.end[.idx])   .pr.end   else .fa.ends[.idx]
            .xs.o = max(.ts, .blk.s.h)
            .xe.o = min(.te, .blk.e.h)
            if (.xe.o > .xs.o)
              .occ[[length(.occ) + 1]] = list(xs = .xs.o, xe = .xe.o)
          }
          .rk.h = as.character(.r)
          if (.rk.h %in% names(.global.row.trn)) {
            for (.trn in .global.row.trn[[.rk.h]]) {
              if (!is.null(.trn$gene) && .trn$gene == .g.h) next
              .xs.o = max(.trn$start, .blk.s.h)
              .xe.o = min(.trn$end,   .blk.e.h)
              if (.xe.o > .xs.o)
                .occ[[length(.occ) + 1]] = list(xs = .xs.o, xe = .xe.o)
            }
          }
          if (length(.occ) == 0) {
            .frees = list(list(xs = .blk.s.h, xe = .blk.e.h))
          } else {
            .occ = .occ[order(sapply(.occ, function(.o) .o$xs))]
            .mrg = list(.occ[[1]])
            if (length(.occ) >= 2) {
              for (.i in 2:length(.occ)) {
                .last = .mrg[[length(.mrg)]]
                if (.occ[[.i]]$xs <= .last$xe) {
                  .mrg[[length(.mrg)]]$xe = max(.last$xe, .occ[[.i]]$xe)
                } else {
                  .mrg[[length(.mrg) + 1]] = .occ[[.i]]
                }
              }
            }
            .frees = list(); .prev = .blk.s.h
            for (.m in .mrg) {
              if (.m$xs > .prev) .frees[[length(.frees) + 1]] = list(xs = .prev, xe = .m$xs)
              .prev = max(.prev, .m$xe)
            }
            if (.prev < .blk.e.h) .frees[[length(.frees) + 1]] = list(xs = .prev, xe = .blk.e.h)
          }
          if (is.null(.tunnel)) {
            .border.fi = NULL
            for (.fi in .frees) {
              if ((.fi$xe - .fi$xs) < .nw.h) next
              if (.forced.side == "left" && .fi$xs == .blk.s.h &&
                  (is.null(.border.fi) || (.fi$xe - .fi$xs) > (.border.fi$xe - .border.fi$xs)))
                .border.fi = .fi
              if (.forced.side == "right" && .fi$xe == .blk.e.h &&
                  (is.null(.border.fi) || (.fi$xe - .fi$xs) > (.border.fi$xe - .border.fi$xs)))
                .border.fi = .fi
            }
            if (is.null(.border.fi)) break
            .tunnel = list(side = .forced.side, xs = .border.fi$xs, xe = .border.fi$xe)
          } else {
            .new.tun = NULL
            for (.fi in .frees) {
              if (.tunnel$side == "left") {
                if (.fi$xs == .blk.s.h) {
                  .eff.xe = min(.fi$xe, .tunnel$xe)
                  .eff.w  = .eff.xe - .blk.s.h
                  if (.eff.w >= .nw.h &&
                      (is.null(.new.tun) || .eff.w > (.new.tun$xe - .new.tun$xs)))
                    .new.tun = list(side = "left", xs = .blk.s.h, xe = .eff.xe)
                }
              } else {
                if (.fi$xe == .blk.e.h) {
                  .eff.xs = max(.fi$xs, .tunnel$xs)
                  .eff.w  = .blk.e.h - .eff.xs
                  if (.eff.w >= .nw.h &&
                      (is.null(.new.tun) || .eff.w > (.new.tun$xe - .new.tun$xs)))
                    .new.tun = list(side = "right", xs = .eff.xs, xe = .blk.e.h)
                }
              }
            }
            if (is.null(.new.tun)) break
            .tunnel = .new.tun
          }
          .run[[length(.run) + 1]] = list(row = .r,
                                          interval = list(xs = .tunnel$xs, xe = .tunnel$xe))
        }
        return(.run)
      }
      # Free-space COM target corner = mirror of transcript COM around block centre.
      .com.target.corner = function(.gi.h, .blk.s.h, .blk.e.h, .fr.h, .lr.h) {
        if (length(.gi.h$trn.indices) == 0) return(list(side = "left", dir = "bu"))
        .widths = .fa.ends[.gi.h$trn.indices] - .fa.starts[.gi.h$trn.indices]
        .mids.x = (.fa.starts[.gi.h$trn.indices] + .fa.ends[.gi.h$trn.indices]) / 2
        .com.x  = if (sum(.widths) > 0) sum(.mids.x * .widths) / sum(.widths) else mean(.mids.x)
        .center.x = (.blk.s.h + .blk.e.h) / 2
        .target.h = if (.com.x >= .center.x) "left" else "right"
        .trn.rows = sapply(.gi.h$trn.indices, function(.idx) {
          for (.r in seq_along(.packing)) if (.idx %in% .packing[[.r]]) return(.r)
          return(NA_integer_)
        })
        .valid = !is.na(.trn.rows)
        .com.y = if (any(.valid) && sum(.widths[.valid]) > 0)
                   sum(.trn.rows[.valid] * .widths[.valid]) / sum(.widths[.valid])
                 else if (any(.valid)) mean(.trn.rows[.valid])
                 else (.fr.h + .lr.h) / 2
        .center.y = (.fr.h + .lr.h) / 2
        .target.v = if (.com.y >= .center.y) "td" else "bu"
        list(side = .target.h, dir = .target.v)
      }
      # Pick best run: longest wins; tie -> corner alignment with free-space COM.
      .pick.best.runs = function(.runs.list, .gi.h, .blk.s.h, .blk.e.h, .fr.h, .lr.h) {
        .lengths = sapply(.runs.list, function(.r) length(.r$run))
        if (max(.lengths) == 0) return(list())
        .best.len = max(.lengths)
        .candidates = .runs.list[.lengths == .best.len]
        if (length(.candidates) == 1) return(.candidates[[1]]$run)
        .target = .com.target.corner(.gi.h, .blk.s.h, .blk.e.h, .fr.h, .lr.h)
        .scores = sapply(.candidates, function(.c) {
          .h.match = if (!is.null(.c$side)) as.integer(.c$side == .target$side) else 0L
          .v.match = if (!is.null(.c$dir))  as.integer(.c$dir  == .target$dir)  else 0L
          .h.match + .v.match
        })
        .candidates[[which.max(.scores)]]$run
      }

      # ===== PHASE 0: Pre-placement encompassment tuck =====
      # For multi-transcript genes (n.trn >= 2 with >=2 packing rows), try to
      # tuck the name inside the gene block via the iterative encompassment scan
      # before any other placement runs. Tucked names act as fixed obstacles for
      # Phase 1, Phase 2, and B-cleanup, freeing the gene's flanks for adjacent
      # genes. Always runs (not gated by inline_name_cleanup).
      .fixed.placements = character(0)
      .tuck.cands = list()
      for (.g in names(.gene.info)) {
        .gi = .gene.info[[.g]]
        if (is.null(.gi$n.trn) || .gi$n.trn < 2L) next
        if (length(.gi$trn.indices) == 0) next
        .grows = integer(0)
        for (.r in seq_along(.packing))
          if (any(.packing[[.r]] %in% .gi$trn.indices)) .grows = c(.grows, .r)
        if (length(.grows) < 2) next
        .tuck.cands[[.g]] = list(gi = .gi, rows = .grows)
      }
      # Process largest first (most packing rows) so they claim space before smaller ones.
      .tuck.order = if (length(.tuck.cands) > 0)
                      names(.tuck.cands)[order(
                        -sapply(.tuck.cands, function(.c) length(.c$rows)))]
                    else character(0)
      for (.g in .tuck.order) {
        .gi.t = .tuck.cands[[.g]]$gi
        .grows.t = .tuck.cands[[.g]]$rows
        .fr.t = min(.grows.t); .lr.t = max(.grows.t)
        .blk.s.t = .gi.t$start; .blk.e.t = .gi.t$end
        .dn.t = strsplit(.g, '#DUPNAME#')[[1]][1]
        .ng.t = .feature.font.size * .std.letter.width * .bp.per.cm   # one letter width
        .nw.t = nchar(.dn.t) * .ng.t                                   # name width (actual)
        # For tucking, require an extra one-letter pad on each side so the
        # name doesn't sit flush against transcripts. The actual placed rect
        # still uses .nw.t --  the pad just tightens the fit check inside the scan.
        .nw.t.fit = .nw.t + 2 * .ng.t
        .scan.bu = seq(.lr.t, .fr.t, by = -1L)
        .scan.td = seq(.fr.t, .lr.t, by =  1L)
        .runs.t = list(
          list(run = .encompass.scan(.scan.bu, "left",  .gi.t, .blk.s.t, .blk.e.t, .nw.t.fit, .g),
               dir = "bu", side = "left"),
          list(run = .encompass.scan(.scan.bu, "right", .gi.t, .blk.s.t, .blk.e.t, .nw.t.fit, .g),
               dir = "bu", side = "right"),
          list(run = .encompass.scan(.scan.td, "left",  .gi.t, .blk.s.t, .blk.e.t, .nw.t.fit, .g),
               dir = "td", side = "left"),
          list(run = .encompass.scan(.scan.td, "right", .gi.t, .blk.s.t, .blk.e.t, .nw.t.fit, .g),
               dir = "td", side = "right")
        )
        .run.t = .pick.best.runs(.runs.t, .gi.t, .blk.s.t, .blk.e.t, .fr.t, .lr.t)
        if (length(.run.t) == 0) next
        .N.t = length(.run.t)
        # Row choice within the run: 1 + ceil(N/2) - (N mod 2). For odd N this
        # equals ceil(N/2) (true middle); for even N it's one deeper, biasing the
        # placement toward the inner end of the tunnel.
        .row.idx = 1L + as.integer(ceiling(.N.t / 2)) - as.integer(.N.t %% 2)
        .chosen.t = .run.t[[.row.idx]]
        .deepest.t = .run.t[[.N.t]]$interval
        .new.x.t = (.deepest.t$xs + .deepest.t$xe) / 2
        .feat.placements[[.g]] = list(x = .new.x.t, adj = 0.5, row = .chosen.t$row)
        .xl.t = .new.x.t - 0.5 * .nw.t
        .global.placed.rects[[length(.global.placed.rects) + 1]] =
          list(xl = .xl.t, xr = .xl.t + .nw.t, row = .chosen.t$row)
        .fixed.placements = c(.fixed.placements, .g)
        .dbg("[exp-tuck] '", .g, "' tucked row=", .chosen.t$row,
                " x=", round(.new.x.t), " (N=", .N.t, ")")
      }

      # ===== PHASE 1: Place large genes (fixed transcript positions) =====
      for (.g in .large.genes) {
        if (.g %in% .fixed.placements) next  # already tucked in Phase 0
        .gi = .gene.info[[.g]]
        .display.name = strsplit(.g, '#DUPNAME#')[[1]][1]
        .name.width.bp = nchar(.display.name) * .feature.font.size * .std.letter.width * .bp.per.cm
        .name.gap = .feature.font.size * .std.letter.width * .bp.per.cm
        
        .gene.rows = integer(0)
        for (.r in seq_along(.packing)) {
          if (any(.packing[[.r]] %in% .gi$trn.indices)) .gene.rows = c(.gene.rows, .r)
        }
        if (length(.gene.rows) == 0) next
        .gene.mid.row = .gene.rows[ceiling(length(.gene.rows) / 2)]
        .gene.first.row = min(.gene.rows)
        .gene.last.row = max(.gene.rows)
        
        # Compute center-of-mass from transcript parts within the plotted region
        .vis.start = max(.gi$start, .pr.start)
        .vis.end = min(.gi$end, .pr.end)
        .gene.com = (.vis.start + .vis.end) / 2
        if (length(.gi$trn.indices) > 0) {
          .trn.starts = pmax(.fa.starts[.gi$trn.indices], .pr.start)
          .trn.ends = pmin(.fa.ends[.gi$trn.indices], .pr.end)
          .visible = .trn.ends > .trn.starts
          if (any(.visible)) {
            .trn.mids = (.trn.starts[.visible] + .trn.ends[.visible]) / 2
            .trn.widths = .trn.ends[.visible] - .trn.starts[.visible] + 1
            .gene.com = sum(.trn.mids * .trn.widths) / sum(.trn.widths)
          }
        }
        
        .candidates = .generate.candidates(.gi$start, .gi$end, .name.width.bp, .name.gap,
                                            .gene.mid.row, .gene.first.row, .gene.last.row,
                                            .total.packing.rows, .gene.com)
        
        # Score all candidates; initial default is NULL (no placement yet)
        # Enclosed genes keep their natural packing positions (SPREAD mode),
        # so their bars are real obstacles --  do not exclude them.
        .p1.row.trn = .global.row.trn
        .best.score = -Inf
        .best.penalty = Inf
        .best.cand = NULL
        for (.cand in .candidates) {
          .result = .score.candidate(.cand$x, .cand$adj, .cand$row, .name.width.bp,
                                      .gene.mid.row, .gene.first.row, .gene.last.row,
                                      .gi$start, .gi$end, .name.gap,
                                      .p1.row.trn, .global.placed.rects, .gene.com,
                                      .gene.n.trn = .gi$n.trn, .expanded.mode = TRUE)
          if (is.infinite(.result$score) && .result$score < 0) next  # hard out-of-bounds
          # Prefer: lowest penalty first, then highest score
          if (is.null(.best.cand) ||
              .result$penalty < .best.penalty ||
              (.result$penalty == .best.penalty && .result$score > .best.score)) {
            .best.penalty = .result$penalty
            .best.score = .result$score
            .best.cand = .cand
          }
        }
        # Absolute fallback: all scored candidates are out-of-bounds (gene at plot edge
        # with a name wider than the available margin). Try right side, then left, then
        # pin right edge to pr.end so the label is at least partially visible.
        if (is.null(.best.cand)) {
          .right.fits = .gi$end + .name.gap + .name.width.bp <= .pr.end + .name.width.bp * 0.10
          .left.fits  = .gi$start - .name.gap - .name.width.bp >= .pr.start - .name.width.bp * 0.10
          if (.right.fits) {
            .best.cand = list(x = .gi$end + .name.gap, adj = 0, row = .gene.mid.row)
          } else if (.left.fits) {
            .best.cand = list(x = .gi$start - .name.gap, adj = 1, row = .gene.mid.row)
          } else {
            # Name too wide for either margin --  pin right edge at plot right boundary
            .best.cand = list(x = .pr.end, adj = 1, row = .gene.mid.row)
          }
        }
        
        # Debug: log Phase 1 placement --  show obstacles at chosen row
        if (!is.null(.best.cand)) {
          .dbg.xl = .best.cand$x - .best.cand$adj * .name.width.bp
          .dbg.xr = .dbg.xl + .name.width.bp
          .dbg.rk = as.character(.best.cand$row)
          .dbg.obs = character(0)
          if (.dbg.rk %in% names(.global.row.trn)) {
            for (.trn in .global.row.trn[[.dbg.rk]]) {
              .ov = max(0, min(.dbg.xr, .trn$end) - max(.dbg.xl, .trn$start))
              if (.ov > 0) .dbg.obs = c(.dbg.obs, paste0(.trn$gene, "[", round(.trn$start), "-", round(.trn$end), " ov=", round(.ov), "]"))
            }
          }
          .dbg("[exp-P1-obs] '", .g, "' name.xl=", round(.dbg.xl), " name.xr=", round(.dbg.xr),
                  " row=", .best.cand$row,
                  if (length(.dbg.obs) > 0) paste0(" obstacles: ", paste(.dbg.obs, collapse=", ")) else " no obstacles")
        }
        .n.scored = sum(sapply(.candidates, function(.c) TRUE))
        .n.oob = .n.scored - sum(sapply(.candidates, function(.c) {
          .r = .score.candidate(.c$x, .c$adj, .c$row, .name.width.bp,
                                .gene.mid.row, .gene.first.row, .gene.last.row,
                                .gi$start, .gi$end, .name.gap,
                                .global.row.trn, .global.placed.rects, .gene.com,
                                .gene.n.trn = .gi$n.trn, .expanded.mode = TRUE)
          !(is.infinite(.r$score) && .r$score < 0)
        }))
        .dbg("[exp-P1] '", .g, "' n.trn=", .gi$n.trn,
                " rows=", .gene.first.row, "-", .gene.last.row,
                " cands=", .n.scored, " oob=", .n.oob,
                " penalty=", round(.best.penalty, 1),
                " -> row=", .best.cand$row, " adj=", .best.cand$adj,
                " x=", round(.best.cand$x))

        .feat.placements[[.g]] = list(x = .best.cand$x, adj = .best.cand$adj, row = .best.cand$row)
        .xl = .best.cand$x - .best.cand$adj * .name.width.bp
        .global.placed.rects[[length(.global.placed.rects) + 1]] = list(xl = .xl, xr = .xl + .name.width.bp, row = .best.cand$row)
      }
      
      # ===== PHASE 2: Place small genes via ILP (shared with collapsed2) =====
      # Small genes (1-2 transcripts) are drawn as collapsed2-style single bars
      # with L/R pairing below their host gene.  Host detection runs on ALL genes
      # (large + small) so that multi-transcript hosts like NOP56 are recognized.
      if (length(.small.genes) > 0) {
        # Build gene info for ALL genes in this group (host detection needs large genes)
        .all.gene.info = list()
        for (.g in .genes) {
          .gi = .gene.info[[.g]]
          if (is.null(.gi)) next
          .display.name = strsplit(.g, '#DUPNAME#')[[1]][1]
          .nw = nchar(.display.name) * .feature.font.size * .std.letter.width * .bp.per.cm
          .ng = .feature.font.size * .std.letter.width * .bp.per.cm
          .com = (.gi$start + .gi$end) / 2
          if (length(.gi$trn.indices) > 0 && max(.gi$trn.indices) <= length(.fa.starts)) {
            .trn.mids = (.fa.starts[.gi$trn.indices] + .fa.ends[.gi$trn.indices]) / 2
            .trn.widths = .fa.ends[.gi$trn.indices] - .fa.starts[.gi$trn.indices] + 1
            .com = sum(.trn.mids * .trn.widths) / sum(.trn.widths)
          }
          .all.gene.info[[.g]] = list(start = .gi$start, end = .gi$end, com = .com,
                                       nw = .nw, ng = .ng, feat.name = .feat.name)
        }

        # Host-enclosed detection on ALL genes (large hosts + small enclosed)
        .exp.b.struct = .find.hosts(.all.gene.info)
        .exp.host.map = .exp.b.struct$host_map
        .exp.enclosed.map = .exp.b.struct$enclosed_map

        # Pair enclosed genes -> forced_side, pair_row_offsets
        .exp.c.struct = .pair.enclosed(.all.gene.info, .exp.enclosed.map,
                                            .pr.start, .pr.end)
        .exp.forced.side = .exp.c.struct$forced_side
        .exp.pair.offsets = .exp.c.struct$pair_row_offsets

        # Adaptive row overrides: all enclosed features are moved to rows below
        # their host. COMPACT (N(host)+M > N(total)): paired L/R via .pair.enclosed().
        # SPREAD (fits within N(total)): one feature per row.
        # See docs/nomenclature.md for terminology.

        # Per-host: row span N(host), base row, and total descendant count M
        .host.row.spans = list()
        .host.enclosed.count = list()
        for (.h in names(.exp.enclosed.map)) {
          .host.gi = .gene.info[[.h]]
          if (is.null(.host.gi)) next
          .host.rows = integer(0)
          for (.r in seq_along(.packing)) {
            if (any(.packing[[.r]] %in% .host.gi$trn.indices)) .host.rows = c(.host.rows, .r)
          }
          .host.row.spans[[.h]] = list(rows = .host.rows,
                                        N = length(.host.rows),
                                        base = if (length(.host.rows) > 0) max(.host.rows) else 1L)
          # Count ALL enclosed features under this host (including nested descendants)
          .all.desc = names(which(sapply(.exp.host.map, function(.ancestors) .h %in% .ancestors)))
          .host.enclosed.count[[.h]] = sum(.all.desc %in% .small.genes)
        }

        # Decide per host: compact if N(host) + M > N(total)
        # Nested hosts inherit COMPACT from any ancestor that is COMPACT.
        .host.decision = list()  # host -> "COMPACT" or "SPREAD"
        # Process outermost hosts first (largest span first)
        .host.order = names(.host.row.spans)
        if (length(.host.order) > 1) {
          .host.spans.vec = sapply(.host.order, function(.h) .all.gene.info[[.h]]$end - .all.gene.info[[.h]]$start)
          .host.order = .host.order[order(-.host.spans.vec)]
        }
        for (.h in .host.order) {
          # Check if any ancestor host is COMPACT -> inherit
          .h.ancestors = .exp.host.map[[.h]]
          .ancestor.compact = any(sapply(.h.ancestors, function(.a) {
            !is.null(.host.decision[[.a]]) && .host.decision[[.a]] == "COMPACT"
          }))
          if (.ancestor.compact) {
            .host.decision[[.h]] = "COMPACT"
          } else {
            .M = .host.enclosed.count[[.h]]
            .N.host = .host.row.spans[[.h]]$N
            .host.decision[[.h]] = if (.N.host + .M > .N.total) "COMPACT" else "SPREAD"
          }
        }

        # Debug: log adaptive decision
        for (.h in .host.order) {
          .M = .host.enclosed.count[[.h]]
          .N.host = .host.row.spans[[.h]]$N
          .dbg("[exp-phase2] host '", .h, "': N(host)=", .N.host, " M(enclosed)=", .M,
                  " N(total)=", .N.total, " N(host)+M=", .N.host + .M,
                  " -> ", .host.decision[[.h]])
        }

        # Compute row overrides for ALL enclosed small genes.
        # Base row = max(host's last packing row, host's name row from Phase 1)
        # so enclosed features don't overlap the host's name.
        .exp.row.overrides = list()

        # Process per outermost host (direct-children only via enclosed_map)
        for (.h in .host.order) {
          # Only process top-level hosts here (skip nested hosts --  their
          # enclosed features are counted under the outermost host)
          .h.ancestors = .exp.host.map[[.h]]
          .is.nested = any(.h.ancestors %in% names(.host.row.spans))
          if (.is.nested) next

          # All descendants of this host (including through nested hosts)
          .all.desc = names(which(sapply(.exp.host.map, function(.ancestors) .h %in% .ancestors)))
          .all.desc = intersect(.all.desc, .small.genes)
          if (length(.all.desc) == 0) next

          # Base row: account for host name placement from Phase 1, plus any
          # large-gene hosts that are themselves enclosed within .h (e.g. SNHG4
          # inside MATR3.v1). Without this, small-gene rows would land inside
          # the nested large host's packing rows.
          .host.name.row = if (!is.null(.feat.placements[[.h]])) .feat.placements[[.h]]$row else 0L
          .base = max(.host.row.spans[[.h]]$base, .host.name.row)
          for (.nh in names(.host.row.spans)) {
            if (.nh == .h) next
            .nh.anc = .exp.host.map[[.nh]]
            if (is.null(.nh.anc) || !(.h %in% .nh.anc)) next
            .nh.name.row = if (!is.null(.feat.placements[[.nh]])) .feat.placements[[.nh]]$row else 0L
            .base = max(.base, .host.row.spans[[.nh]]$base, .nh.name.row)
          }

          if (.host.decision[[.h]] == "COMPACT") {
            # Use pair_row_offsets from .pair.enclosed()
            for (.g in .all.desc) {
              if (!is.null(.exp.pair.offsets[[.g]])) {
                .exp.row.overrides[[.g]] = .base + 1L + .exp.pair.offsets[[.g]]
              }
            }
          } else {
            # SPREAD: one feature per row, sorted by genomic start
            .desc.sorted = .all.desc[order(sapply(.all.desc, function(.g) .all.gene.info[[.g]]$start))]
            for (.i in seq_along(.desc.sorted)) {
              .exp.row.overrides[[.desc.sorted[.i]]] = .base + .i
            }
          }
        }

        # Clear forced_side for SPREAD enclosed genes
        for (.g in names(.exp.forced.side)) {
          .hosts = .exp.host.map[[.g]]
          if (length(.hosts) > 0) {
            .spans = sapply(.hosts, function(.h) .all.gene.info[[.h]]$end - .all.gene.info[[.h]]$start)
            .direct.parent = .hosts[which.min(.spans)]
            if (!is.null(.host.decision[[.direct.parent]]) &&
                .host.decision[[.direct.parent]] == "SPREAD") {
              .exp.forced.side[[.g]] = NULL
            }
          }
        }

        # Update total packing rows to include override rows
        if (length(.exp.row.overrides) > 0) {
          .total.packing.rows = max(.total.packing.rows, max(unlist(.exp.row.overrides)))
        }

        # Update global obstacle map: remove overridden transcripts from their
        # original rows and add them at their new override rows.
        for (.g in names(.exp.row.overrides)) {
          .gi = .gene.info[[.g]]
          if (is.null(.gi) || length(.gi$trn.indices) == 0) next
          # Remove from original rows
          for (.rk in names(.global.row.trn)) {
            .global.row.trn[[.rk]] = Filter(function(.b) .b$gene != .g, .global.row.trn[[.rk]])
          }
          # Add at new row
          .new.row = .exp.row.overrides[[.g]]
          .rk = as.character(.new.row)
          if (is.null(.global.row.trn[[.rk]])) .global.row.trn[[.rk]] = list()
          .global.row.trn[[.rk]][[length(.global.row.trn[[.rk]]) + 1]] = list(
            start = .gi$start, end = .gi$end, gene = .g
          )
        }

        # Build small gene info with override display rows
        .small.gene.info = list()
        for (.g in .small.genes) {
          if (.g %in% .fixed.placements) next  # already tucked in Phase 0
          .gi = .gene.info[[.g]]
          .display.name = strsplit(.g, '#DUPNAME#')[[1]][1]
          .nw = nchar(.display.name) * .feature.font.size * .std.letter.width * .bp.per.cm
          .ng = .feature.font.size * .std.letter.width * .bp.per.cm

          # Packing rows and display row
          if (.g %in% names(.exp.row.overrides)) {
            .dr = .exp.row.overrides[[.g]]
            .first.row = .dr
            .last.row = .dr
          } else {
            .gene.rows = integer(0)
            for (.r in seq_along(.packing)) {
              if (any(.packing[[.r]] %in% .gi$trn.indices)) .gene.rows = c(.gene.rows, .r)
            }
            if (length(.gene.rows) == 0) next
            .dr = .gene.rows[ceiling(length(.gene.rows) / 2)]
            .first.row = min(.gene.rows)
            .last.row = max(.gene.rows)
          }

          # Center-of-mass: only consider transcript parts within the plotted region
          .vis.start = max(.gi$start, .pr.start)
          .vis.end = min(.gi$end, .pr.end)
          .com = (.vis.start + .vis.end) / 2
          if (length(.gi$trn.indices) > 0) {
            # Clip each transcript to the visible region
            .trn.starts = pmax(.fa.starts[.gi$trn.indices], .pr.start)
            .trn.ends = pmin(.fa.ends[.gi$trn.indices], .pr.end)
            .visible = .trn.ends > .trn.starts  # only transcripts with visible extent
            if (any(.visible)) {
              .trn.mids = (.trn.starts[.visible] + .trn.ends[.visible]) / 2
              .trn.widths = .trn.ends[.visible] - .trn.starts[.visible] + 1
              .com = sum(.trn.mids * .trn.widths) / sum(.trn.widths)
            }
          }

          .small.gene.info[[.g]] = list(
            start = .gi$start, end = .gi$end, com = .com,
            nw = .nw, ng = .ng, feat.name = .feat.name,
            display.row = .dr, first.row = .first.row, last.row = .last.row
          )
        }

        if (length(.small.gene.info) > 0) {

          # Sort small genes left-to-right
          .exp.flat.genes = names(.small.gene.info)
          if (length(.exp.flat.genes) > 1) {
            .exp.flat.genes = .exp.flat.genes[order(sapply(.small.gene.info[.exp.flat.genes], `[[`, 'start'))]
          }

          # Generate and score candidates for each small gene
          .exp.all.candidates = list()
          for (.g in .exp.flat.genes) {
            .sgi = .small.gene.info[[.g]]
            .dr = .sgi$display.row

            .exp.all.candidates[[.g]] = .generate.candidates(
              .sgi$start, .sgi$end, .sgi$nw, .sgi$ng,
              .dr, .sgi$first.row, .sgi$last.row, .total.packing.rows, .sgi$com,
              .forced.side = .exp.forced.side[[.g]],
              .plot.start = .pr.start, .plot.end = .pr.end,
              .collapsed2 = TRUE
            )

            # Enclosed genes keep their natural packing positions (SPREAD mode),
            # so the host's blocks are real obstacles --  do not exclude them.
            # The gene itself is excluded via .exclude.genes in the scorer.
            .gene.row.bars = .global.row.trn

            for (i in seq_along(.exp.all.candidates[[.g]])) {
              .cand = .exp.all.candidates[[.g]][[i]]

              .res = .score.candidate(
                .cand$x, .cand$adj, .cand$row, .sgi$nw,
                .dr, .sgi$first.row, .sgi$last.row, .sgi$start, .sgi$end, .sgi$ng,
                .gene.row.bars, .global.placed.rects,
                .sgi$com, .exclude.genes = .g,
                .gene.n.trn = .gene.info[[.g]]$n.trn, .expanded.mode = TRUE
              )

              .text.xl = .cand$x - .cand$adj * .sgi$nw
              .text.xr = .text.xl + .sgi$nw
              .self.ov = if (.cand$row == .dr)
                max(0, min(.text.xr, .sgi$end) - max(.text.xl, .sgi$start)) else 0

              # Distance from nearest gene edge (not display row) for below/above
              if (.cand$row > .sgi$last.row) {
                .row.dist = .cand$row - .sgi$last.row
              } else if (.cand$row < .sgi$first.row) {
                .row.dist = .sgi$first.row - .cand$row
              } else {
                .row.dist = abs(.cand$row - .dr)
              }
              if (.row.dist > 0) {
                .name.center.x = .text.xl + .sgi$nw / 2
                .bar.center = (.sgi$start + .sgi$end) / 2
                .row.height.bp = 0.8 * .bp.per.cm
                .x.dist.norm = abs(.name.center.x - .bar.center) / .row.height.bp
                .row.extra = .row.dist - 1
                .dist.penalty = 4 * .row.extra^2 + 0.1 * .x.dist.norm^2
                .effective.score = .res$pref
              } else {
                .dist.penalty = 0
                .effective.score = .res$pref
              }
              .adj.penalty = .res$penalty + .dist.penalty + .self.ov * 1e6

              .cand$score = .effective.score - .adj.penalty
              .cand$valid = !(is.infinite(.res$score) && .res$score < 0)
              .cand$xmin = .text.xl
              .cand$xmax = .text.xr
              .cand$ymin = .cand$row - 0.5
              .cand$ymax = .cand$row + 0.5

              .exp.all.candidates[[.g]][[i]] = .cand
            }
          }

          # Debug: log Phase 2 candidate summary per gene
          for (.g in .exp.flat.genes) {
            .cands = .exp.all.candidates[[.g]]
            .n.valid = sum(sapply(.cands, `[[`, 'valid'))
            .scores = sapply(.cands, `[[`, 'score')
            .best.i = which.max(.scores)
            .bc = .cands[[.best.i]]
            .sgi = .small.gene.info[[.g]]
            .dbg("[exp-P2] '", .g, "' n.trn=", .gene.info[[.g]]$n.trn,
                    " dr=", .sgi$display.row, " rows=", .sgi$first.row, "-", .sgi$last.row,
                    " valid=", .n.valid, "/", length(.cands),
                    " best: row=", .bc$row, " adj=", .bc$adj,
                    " score=", round(.bc$score, 1),
                    if (!is.null(.exp.host.map[[.g]]) && length(.exp.host.map[[.g]]) > 0)
                      paste0(" hosts=", paste(.exp.host.map[[.g]], collapse=",")) else "")
          }

          # Solve ILP globally for all small genes in this feature group
          .ilp.placements = .solve.name.placement.ilp(.exp.flat.genes, .exp.all.candidates)

          # Record placements
          for (.g in .exp.flat.genes) {
            .pl = .ilp.placements[[.g]]
            if (is.null(.pl)) {
              .dbg("[exp-P2] '", .g, "' -> NO PLACEMENT")
            } else {
              .dbg("[exp-P2] '", .g, "' -> row=", .pl$row, " adj=", .pl$adj, " x=", round(.pl$x))
            }
            if (!is.null(.pl)) {
              .feat.placements[[.g]] = .pl
              .nw = .small.gene.info[[.g]]$nw
              .xl = .pl$x - .pl$adj * .nw
              .global.placed.rects[[length(.global.placed.rects) + 1]] = list(
                xl = .xl, xr = .xl + .nw, row = .pl$row
              )
            }
          }

          # ========== POST-PLACEMENT CLEANUP PASS ==========
          # Always-on: Step 1 inline snap + B1-B4 adjusted-score sweep.

          # --- Step 1: Inline snap --  move inline names to nearest transcript ---
          # If a name is placed inline (adj != 0.5, on a row with the gene's own
          # transcripts), snap x to the nearest transcript end/start rather than
          # the gene block boundary. Applies to both large and small genes.
          for (.g in names(.feat.placements)) {
            .pl = .feat.placements[[.g]]
            if (is.null(.pl) || .pl$adj == 0.5) next
            .gi = .gene.info[[.g]]
            if (is.null(.gi) || length(.gi$trn.indices) == 0) next
            if (is.null(.pl$row) || .pl$row < 1L || .pl$row > length(.packing)) next
            .trn.on.row = intersect(.packing[[.pl$row]], .gi$trn.indices)
            if (length(.trn.on.row) == 0) next  # name not on a transcript row
            .ng = .feature.font.size * .std.letter.width * .bp.per.cm
            if (.pl$adj == 0) {
              # Right side: snap to rightmost transcript end + gap
              .trn.end.max = max(.fa.ends[.trn.on.row])
              if (.trn.end.max < .gi$end - 0.5 * .ng)
                .feat.placements[[.g]]$x = .trn.end.max + .ng
            } else {
              # Left side: snap to leftmost transcript start - gap
              .trn.start.min = min(.fa.starts[.trn.on.row])
              if (.trn.start.min > .gi$start + 0.5 * .ng)
                .feat.placements[[.g]]$x = .trn.start.min - .ng
            }
          }

          # ========== CLEANUP PASS B: iterative score-driven sweep ==========
          # adj_score = adj_pref + com_bonus_always
          #   adj_pref  = pref from .score.candidate(), with above-by-1 bumped 3->4
          #   com_bonus_always = COM bonus computed unconditionally (not gated by center_of_mass)
          # Threshold: adj_score < 7 flags suboptimal placements for re-evaluation.

          # Helper: compute adj_score for any (pl, geometry) combination.
          # Excludes self's bar from obstacle map (via .exclude.genes) AND self's name
          # rect from .global.placed.rects --  otherwise every already-placed name would be
          # penalized for overlapping itself, producing bogus very-negative scores.
          #
          # Center-bonus rewards alignment with the bar's geometric midpoint only.
          # COM is no longer used in standard scoring (bracket-section path only),
          # so the .com argument is accepted but unused.
          .center.bonus = function(.nc, .com, .start, .end) {
            .w = max(1, .end - .start)
            .gc = (.start + .end) / 2
            max(0, 3 * (1 - abs(.nc - .gc) / .w))
          }
          .adj.score.of = function(.g, .pl, .nw, .ng, .mid.row, .first.row, .last.row,
                                   .start, .end, .com) {
            if (is.null(.pl)) return(-Inf)
            .xl.self = .pl$x - .pl$adj * .nw
            .xr.self = .xl.self + .nw
            .rects.noself = Filter(
              function(.r) !(abs(.r$xl - .xl.self) < 1e-9 &&
                             abs(.r$xr - .xr.self) < 1e-9 &&
                             .r$row == .pl$row),
              .global.placed.rects
            )
            .res = .score.candidate(
              .pl$x, .pl$adj, .pl$row, .nw,
              .mid.row, .first.row, .last.row,
              .start, .end, .ng,
              .global.row.trn, .rects.noself, .com,
              .exclude.genes = .g,
              .gene.n.trn = if (!is.null(.gene.info[[.g]])) .gene.info[[.g]]$n.trn else 1L,
              .expanded.mode = TRUE
            )
            if (is.infinite(.res$score) && .res$score < 0) return(-Inf)
            .nc  = (.pl$x - .pl$adj * .nw) + .nw / 2
            .cob = .center.bonus(.nc, .com, .start, .end)
            .ap  = .res$pref + if (.first.row - .pl$row == 1L) 1L else 0L
            .ap + .cob - .res$penalty * 1e6
          }

          # ---- B1: Compute adj_scores for all small-gene placements ----
          # Clip gene extents to plot so "center" reflects visible extent, not
          # off-screen genomic span --  matters for genes cut by the plot boundary.
          .sgi.adj.scores = list()
          for (.g in names(.small.gene.info)) {
            .sgi = .small.gene.info[[.g]]
            .vis.s = max(.sgi$start, .pr.start)
            .vis.e = min(.sgi$end, .pr.end)
            .sgi.adj.scores[[.g]] = .adj.score.of(
              .g, .feat.placements[[.g]], .sgi$nw, .sgi$ng,
              .sgi$display.row, .sgi$first.row, .sgi$last.row,
              .vis.s, .vis.e, .sgi$com
            )
          }

          # ---- B2: Compute geometry + adj_scores for large-gene placements ----
          .lgi.geom       = list()
          .lgi.adj.scores = list()
          for (.g in .large.genes) {
            .gi = .gene.info[[.g]]
            .pl = .feat.placements[[.g]]
            if (is.null(.pl)) next
            .g.rows = integer(0)
            for (.r in seq_along(.packing))
              if (any(.packing[[.r]] %in% .gi$trn.indices)) .g.rows = c(.g.rows, .r)
            if (length(.g.rows) == 0) next
            .dn  = strsplit(.g, '#DUPNAME#')[[1]][1]
            .nw  = nchar(.dn) * .feature.font.size * .std.letter.width * .bp.per.cm
            .ng  = .feature.font.size * .std.letter.width * .bp.per.cm
            .mid.row   = .g.rows[ceiling(length(.g.rows) / 2)]
            .first.row = min(.g.rows)
            .last.row  = max(.g.rows)
            .com = (.gi$start + .gi$end) / 2
            if (length(.gi$trn.indices) > 0 && max(.gi$trn.indices) <= length(.fa.starts)) {
              .ts = pmax(.fa.starts[.gi$trn.indices], .pr.start)
              .te = pmin(.fa.ends[.gi$trn.indices], .pr.end)
              .vis = .te > .ts
              if (any(.vis)) {
                .tm = (.ts[.vis] + .te[.vis]) / 2
                .tw = .te[.vis] - .ts[.vis] + 1
                .com = sum(.tm * .tw) / sum(.tw)
              }
            }
            # Clip gene extents to plot region so candidate generation and COM
            # bonus use visible extent, not genomic span.
            .vis.s = max(.gi$start, .pr.start)
            .vis.e = min(.gi$end, .pr.end)
            .lgi.geom[[.g]] = list(nw=.nw, ng=.ng, mid.row=.mid.row,
                                   first.row=.first.row, last.row=.last.row,
                                   start=.vis.s, end=.vis.e, com=.com)
            .lgi.adj.scores[[.g]] = .adj.score.of(
              .g, .pl, .nw, .ng, .mid.row, .first.row, .last.row,
              .vis.s, .vis.e, .com
            )
          }

          # ---- B3: 1-transcript SPREAD bar+name sweep ----
          # Score all reachable (bar_row, name_candidate) combinations without modifying
          # the obstacle map --  .exclude.genes=.g removes self-penalization regardless of
          # which bar row is being tried. Commit the obstacle map change only on acceptance.
          # Genes in .exp.row.overrides came from either COMPACT pairing (must stay
          # paired; exclude from sweep) or SPREAD one-per-row (eligible --  the conflict
          # check prevents them from landing on another block).
          .compact.overridden.b = character(0)
          if (length(.host.decision) > 0) {
            for (.h in names(.host.decision)) {
              if (identical(.host.decision[[.h]], "COMPACT")) {
                .desc.b = names(which(sapply(.exp.host.map, function(.a) .h %in% .a)))
                .compact.overridden.b = c(.compact.overridden.b, intersect(.desc.b, .small.genes))
              }
            }
            .compact.overridden.b = unique(.compact.overridden.b)
          }
          .spread.genes.b = setdiff(.small.genes, .compact.overridden.b)
          .sweep.genes = Filter(function(.g) {
            s = .sgi.adj.scores[[.g]]
            !is.null(s) && is.finite(s) && s < 7 &&
            !is.null(.gene.info[[.g]]) && .gene.info[[.g]]$n.trn == 1L &&
            !(.g %in% .fixed.placements)
          }, .spread.genes.b)

          if (length(.sweep.genes) > 0) {
            .sweep.genes = .sweep.genes[
              order(sapply(.sweep.genes, function(.g) .sgi.adj.scores[[.g]]))]

            for (.g in .sweep.genes) {
              .sgi = .small.gene.info[[.g]]
              .gi  = .gene.info[[.g]]
              .pl  = .feat.placements[[.g]]
              if (is.null(.pl)) next
              .cur.as = .sgi.adj.scores[[.g]]
              # Visible (clipped) gene extent for candidate generation and scoring.
              .vis.s = max(.sgi$start, .pr.start)
              .vis.e = min(.sgi$end, .pr.end)

              .c2.idx     = which(.c2.names == .g)
              .block.xmin = if (length(.c2.idx) > 0) min(IRanges::start(.c2[.c2.idx])) else .sgi$start
              .block.xmax = if (length(.c2.idx) > 0) max(IRanges::end(.c2[.c2.idx]))   else .sgi$end

              # Build conflict-free bar rows: current + sweep down up to +5 + sweep up to -5
              .bar.rows = .sgi$display.row
              for (.dr in seq_len(5)) {
                .r = .sgi$display.row + .dr
                .rk = as.character(.r)
                .ok = TRUE
                if (!is.null(.global.row.trn[[.rk]]))
                  for (.blk in .global.row.trn[[.rk]])
                    if (.blk$gene != .g && .blk$start < .block.xmax && .blk$end > .block.xmin)
                      { .ok = FALSE; break }
                if (!.ok) break
                .bar.rows = c(.bar.rows, .r)
              }
              for (.dr in seq_len(5)) {
                .r = .sgi$display.row - .dr
                if (.r < 1) break
                .rk = as.character(.r)
                .ok = TRUE
                if (!is.null(.global.row.trn[[.rk]]))
                  for (.blk in .global.row.trn[[.rk]])
                    if (.blk$gene != .g && .blk$start < .block.xmax && .blk$end > .block.xmin)
                      { .ok = FALSE; break }
                if (!.ok) break
                .bar.rows = c(.bar.rows, .r)
              }

              # Temporarily remove old placed name rect while scoring
              .xl.old = .pl$x - .pl$adj * .sgi$nw
              .xr.old = .xl.old + .sgi$nw
              .global.placed.rects = Filter(
                function(.r) !(.r$xl == .xl.old && .r$xr == .xr.old && .r$row == .pl$row),
                .global.placed.rects
              )

              .best.bar = .sgi$display.row
              .best.nc  = .pl
              .best.as  = .cur.as

              for (.br in .bar.rows) {
                .cands = .generate.candidates(
                  .vis.s, .vis.e, .sgi$nw, .sgi$ng,
                  .br, .br, .br,
                  max(.total.packing.rows, .br),
                  .sgi$com,
                  .forced.side = .exp.forced.side[[.g]],
                  .plot.start = .pr.start, .plot.end = .pr.end,
                  .collapsed2 = TRUE
                )
                for (.c in .cands) {
                  .res = .score.candidate(
                    .c$x, .c$adj, .c$row, .sgi$nw,
                    .br, .br, .br,
                    .vis.s, .vis.e, .sgi$ng,
                    .global.row.trn, .global.placed.rects, .sgi$com,
                    .exclude.genes = .g,
                    .gene.n.trn = .gene.info[[.g]]$n.trn, .expanded.mode = TRUE
                  )
                  if (is.infinite(.res$score) && .res$score < 0) next
                  if (.res$penalty > 0) next  # reject bar or name overlap
                  .nc  = (.c$x - .c$adj * .sgi$nw) + .sgi$nw / 2
                  .cob = .center.bonus(.nc, .sgi$com, .vis.s, .vis.e)
                  .ra  = .br - .c$row
                  .ap  = .res$pref + if (.ra == 1L) 1L else 0L
                  .as  = .ap + .cob
                  if (.as > .best.as) { .best.as = .as; .best.bar = .br; .best.nc = .c }
                }
              }

              if (.best.as <= .cur.as) {
                # No improvement --  restore old placed rect and move on
                .global.placed.rects[[length(.global.placed.rects) + 1]] =
                  list(xl = .xl.old, xr = .xr.old, row = .pl$row)
                next
              }

              # Accept: commit obstacle map + packing if bar moved
              if (.best.bar != .sgi$display.row) {
                .old.rk = as.character(.sgi$display.row)
                .new.rk = as.character(.best.bar)
                .trn.idx = .gi$trn.indices
                # Packing --  pad list if .best.bar > current length, then guard NULL
                while (length(.packing) < .best.bar) {
                  .packing[[length(.packing) + 1]] = integer(0)
                }
                if (!is.null(.packing[[.sgi$display.row]]))
                  .packing[[.sgi$display.row]] = setdiff(.packing[[.sgi$display.row]], .trn.idx)
                if (is.null(.packing[[.best.bar]])) .packing[[.best.bar]] = integer(0)
                .packing[[.best.bar]] = union(.packing[[.best.bar]], .trn.idx)
                # Obstacle map
                if (!is.null(.global.row.trn[[.old.rk]])) {
                  .global.row.trn[[.old.rk]] = Filter(
                    function(.b) .b$gene != .g, .global.row.trn[[.old.rk]])
                  if (length(.global.row.trn[[.old.rk]]) == 0) .global.row.trn[[.old.rk]] = NULL
                }
                if (is.null(.global.row.trn[[.new.rk]])) .global.row.trn[[.new.rk]] = list()
                .global.row.trn[[.new.rk]][[length(.global.row.trn[[.new.rk]]) + 1]] =
                  list(start = .block.xmin, end = .block.xmax, gene = .g)
                # small.gene.info + plotting override
                .small.gene.info[[.g]]$display.row = .best.bar
                .small.gene.info[[.g]]$first.row   = .best.bar
                .small.gene.info[[.g]]$last.row     = .best.bar
                .exp.row.overrides[[.g]] = .best.bar
                .total.packing.rows = max(.total.packing.rows, .best.bar)
              }
              .feat.placements[[.g]] = list(x=.best.nc$x, adj=.best.nc$adj, row=.best.nc$row)
              .xl.new = .best.nc$x - .best.nc$adj * .sgi$nw
              .global.placed.rects[[length(.global.placed.rects) + 1]] =
                list(xl=.xl.new, xr=.xl.new + .sgi$nw, row=.best.nc$row)
              .dbg("[exp-cleanup] '", .g, "' bar ", .sgi$display.row, "->", .best.bar,
                      " name row=", .best.nc$row, " adj=", .best.nc$adj,
                      " score ", round(.cur.as, 2), "->", round(.best.as, 2))
            }
          }

          # ---- B4: Large gene name re-placement (bars fixed, names may improve) ----
          .large.rescore = Filter(function(.g) {
            s = .lgi.adj.scores[[.g]]
            !is.null(s) && is.finite(s) && s < 7 &&
            !(.g %in% .fixed.placements)
          }, .large.genes)

          if (length(.large.rescore) > 0) {
            .large.rescore = .large.rescore[
              order(sapply(.large.rescore, function(.g) .lgi.adj.scores[[.g]]))]

            for (.g in .large.rescore) {
              .geom = .lgi.geom[[.g]]
              if (is.null(.geom)) next
              .pl = .feat.placements[[.g]]
              if (is.null(.pl)) next
              .cur.as = .lgi.adj.scores[[.g]]

              .xl.old = .pl$x - .pl$adj * .geom$nw
              .xr.old = .xl.old + .geom$nw
              .global.placed.rects = Filter(
                function(.r) !(.r$xl == .xl.old && .r$xr == .xr.old && .r$row == .pl$row),
                .global.placed.rects
              )

              .cands = .generate.candidates(
                .geom$start, .geom$end, .geom$nw, .geom$ng,
                .geom$mid.row, .geom$first.row, .geom$last.row,
                .total.packing.rows, .geom$com,
                .plot.start = .pr.start, .plot.end = .pr.end
              )
              .best = NULL; .best.as = .cur.as
              .dbg("[exp-cleanup-dbg] '", .g, "' cur.as=", round(.cur.as, 2),
                      " vis=[", round(.geom$start), "-", round(.geom$end),
                      "] com=", round(.geom$com),
                      " rows=", .geom$first.row, "-", .geom$last.row,
                      " nw=", round(.geom$nw), " n.cands=", length(.cands))
              for (.c in .cands) {
                # No .exclude.genes here: large genes have multi-row bars, and
                # a candidate whose row equals the bar row must still be
                # penalised for overlapping that bar.  (Self-name overlap is
                # handled by filtering .global.placed.rects above.)
                .res = .score.candidate(
                  .c$x, .c$adj, .c$row, .geom$nw,
                  .geom$mid.row, .geom$first.row, .geom$last.row,
                  .geom$start, .geom$end, .geom$ng,
                  .global.row.trn, .global.placed.rects, .geom$com,
                  .gene.n.trn = .gene.info[[.g]]$n.trn, .expanded.mode = TRUE
                )
                .xl.c = .c$x - .c$adj * .geom$nw
                .xr.c = .xl.c + .geom$nw
                .nc  = .xl.c + .geom$nw / 2
                .cob = .center.bonus(.nc, .geom$com, .geom$start, .geom$end)
                .ra  = .geom$first.row - .c$row
                .ap  = .res$pref + if (.ra == 1L) 1L else 0L
                .as  = .ap + .cob
                .oob = is.infinite(.res$score) && .res$score < 0
                .dbg("[exp-cleanup-dbg]   cand x=", round(.c$x), " adj=", .c$adj,
                        " row=", .c$row, " xl=", round(.xl.c), " xr=", round(.xr.c),
                        " pref=", .res$pref, " cob=", round(.cob, 2),
                        " pen=", round(.res$penalty, 1),
                        " as=", round(.as, 2),
                        if (.oob) " [OOB]" else if (.res$penalty > 0) " [REJ-pen]" else "")
                if (.oob) next
                if (.res$penalty > 0) next
                if (.as > .best.as) { .best = .c; .best.as = .as }
              }

              if (is.null(.best)) {
                .global.placed.rects[[length(.global.placed.rects) + 1]] =
                  list(xl=.xl.old, xr=.xr.old, row=.pl$row)
                next
              }
              .feat.placements[[.g]] = list(x=.best$x, adj=.best$adj, row=.best$row)
              .xl.new = .best$x - .best$adj * .geom$nw
              .global.placed.rects[[length(.global.placed.rects) + 1]] =
                list(xl=.xl.new, xr=.xl.new + .geom$nw, row=.best$row)
              .dbg("[exp-cleanup] '", .g, "' name re-placed row=", .best$row,
                      " adj=", .best$adj, " score ", round(.cur.as, 2), "->", round(.best.as, 2))
            }
          }


          # Store row overrides --  plotting.R moves transcripts to these rows
          if (length(.exp.row.overrides) > 0) {
            if (is.null(annot_info[[.annot.name]][['small_gene_row_overrides']])) {
              annot_info[[.annot.name]][['small_gene_row_overrides']] = list()
            }
            annot_info[[.annot.name]][['small_gene_row_overrides']][[.feat.name]] = .exp.row.overrides
          }
        }
      }


      .placements[[.feat.name]] = .feat.placements
    }
    annot_info[[.annot.name]][['inline_name_placements']] = .placements
    # Compute max/min row used across all placements and overrides for height calculation
    .max.name.row = 0L
    .min.name.row = 1L
    .max.packing.row = 0L
    for (.feat.name in names(.placements)) {
      .max.packing.row = max(.max.packing.row, length(annot_info[[.annot.name]][['packing']][[.feat.name]]))
      for (.p in .placements[[.feat.name]]) {
        .max.name.row = max(.max.name.row, .p$row)
        .min.name.row = min(.min.name.row, .p$row)
      }
      # Also account for override rows (bars placed beyond packing)
      .overrides = annot_info[[.annot.name]][['small_gene_row_overrides']][[.feat.name]]
      if (!is.null(.overrides)) {
        .max.name.row = max(.max.name.row, max(unlist(.overrides)))
      }
    }
    annot_info[[.annot.name]][['inline_name_max_row']] = .max.name.row
    annot_info[[.annot.name]][['inline_name_extra_rows']] = max(0L, .max.name.row - .max.packing.row)
    annot_info[[.annot.name]][['inline_name_above_rows']] = max(0L, 1L - .min.name.row)
  }  # end expanded/squished inline name placement loop

  # ===== COLLAPSED2: name-aware inline name placement =====
  # Separate loop --  independent of the expanded-mode early exits above.
  # packing2_display is always equal to packing2 (bar-only footprints).
  # Name placement uses the scored candidate approach (Steps 5-7).
  for (.annot.name in names(annot_info)) {
    annot_info[[.annot.name]][['c2_inline_name_placements']] = NULL
    annot_info[[.annot.name]][['c2_inline_name_extra_rows']] = 0L
    annot_info[[.annot.name]][['c2_bracket_inside_names']] = NULL
    annot_info[[.annot.name]][['packing2_display']] = annot_info[[.annot.name]][['packing2']]
    # When feature names are not displayed (e.g. ChIP peaks), packing2_display = packing2
    # is the correct display packing and no name placement is needed --  skip all computation.
    # isFALSE handles NA (annotation not in vector) as "names displayed" --  the
    # safe default.  Only skip when explicitly set to FALSE (e.g. ChIP peaks).
    .names.displayed = is.null(incl_feature_names) || !isFALSE(incl_feature_names[.annot.name])
    if (!.names.displayed) next
    .c2 = annot_info[[.annot.name]][['collapsed2']]
    if (!is.null(annot_info[[.annot.name]][['packing2']]) && !is.null(.c2)) {
      .dbg("[c2names] annotation: ", .annot.name,
              "  |  n.packing2.groups: ", length(annot_info[[.annot.name]][['packing2']]),
              "  |  n.collapsed2.genes: ", length(.c2))
      .c2.placements = list()
      .c2.names.vec = S4Vectors::mcols(.c2)$name

      # Wrap Stages A-E in tryCatch so pre-placement errors are diagnosed too.
      tryCatch({

      # --- Stage A: Collect gene info ---
      .dbg("[c2names]   stage A...")
      .is.collapsed.mode = !is.null(annotation_packing) &&
        identical(unname(annotation_packing[.annot.name]), 'collapsed')
      .all.c2.gene.info = .collect.gene.info(
        annot_info, .annot.name, .c2, .c2.names.vec,
        .feature.font.size, .std.letter.width, .bp.per.cm,
        is_collapsed_mode = .is.collapsed.mode
      )

      # --- Stage B: Identify host-enclosed structure ---
      # host_map:     gene -> c(host_names)    --  used by Stage G (host exclusions)
      # enclosed_map: host -> c(enclosed_names) --  used by Stage C and Stage D
      .dbg("[c2names]   stage B...")
      .b.struct           = .find.hosts(.all.c2.gene.info)
      .c2.exclude.from.obs = .b.struct$host_map     # kept for compatibility with Stage G
      .c2.enclosed.map    = .b.struct$enclosed_map

      # --- Stage C: Pair co-enclosed genes ---
      # Returns forced_side and pair_row_offsets (relative to base row, applied in Stage D).
      .dbg("[c2names]   stage C...")
      .c.struct        = .pair.enclosed(.all.c2.gene.info, .c2.enclosed.map,
                                            .pr.start, .pr.end)
      .c2.forced.side  = .c.struct$forced_side
      .c2.pair.offsets = .c.struct$pair_row_offsets

      # --- Stage D: Single-pass bar packing with pixel gap ---
      # Repacks each collapsed2 group with 1-pixel gap (instead of gap=-1 from
      # OrganizeOverlappingLoci), applies pair_row_offsets for co-enclosed groups,
      # and produces the final packing2_display.
      .dbg("[c2names]   stage D...")
      .d.struct = .c2.pack.bars(
        annot_info, .annot.name, .all.c2.gene.info, .c2.pair.offsets,
        .c2.enclosed.map, .c2, .c2.names.vec, .bp.per.cm,
        is_collapsed_mode = .is.collapsed.mode
      )
      # Write final bar rows back into gene_info (Stages F-H read display.row from here)
      for (.g in names(.d.struct$bar_rows)) {
        if (!is.null(.all.c2.gene.info[[.g]]))
          .all.c2.gene.info[[.g]]$display.row = .d.struct$bar_rows[[.g]]
      }
      # Clear forced_side for any gene where Stage D did not apply offsets
      # (genes in groups that failed the same-row check keep unrestricted placement)
      for (.g in names(.c2.forced.side)) {
        if (!(.g %in% .d.struct$applied_genes)) .c2.forced.side[[.g]] = NULL
      }
      # Commit packing2_display (Stage D output replaces the packing2 copy set above)
      annot_info[[.annot.name]][['packing2_display']] = .d.struct$packing2_display

      # --- Stage E: Build obstacle map (once, from Stage D bar rows) ---
      .dbg("[c2names]   stage E...")
      .c2.global.row.bars   = .build.obstacle.map(.all.c2.gene.info, .d.struct$bar_rows)
      .c2.global.placed.rects = list()

      }, error = function(.e) {  # end tryCatch Stages A-E
        .dbg("[c2names] ERROR in Stages A-E, annotation '", .annot.name, "': ", conditionMessage(.e))
        .calls = sys.calls()
        .dbg("[c2names] Call stack:\n", paste(sapply(seq_along(.calls), function(.i) paste0("  ", .i, ": ", deparse(.calls[[.i]])[1])), collapse = "\n"))
        stop(.e)
      })

      # --- Steps 5-7: candidate generation, greedy placement, refinement ---
      # Wrapped in tryCatch so any error prints annotation/gene context before propagating.
      .c2.steps.result = tryCatch({

      # --- Step 5: Sort groups left-to-right; pre-compute candidates once per gene ---
      .c2.feat.names = names(annot_info[[.annot.name]][['packing2_display']])
      .group.starts = sapply(.c2.feat.names, function(.fn) {
        .genes = if (!grepl(",", .fn) || .fn %in% .c2.names.vec) .fn else strsplit(.fn, ",")[[1]]
        .idxs = which(.c2.names.vec %in% .genes)
        if (length(.idxs) == 0) return(Inf)
        min(IRanges::start(.c2[.idxs]))
      })
      .c2.feat.names = .c2.feat.names[order(.group.starts)]
      .max.display.rows = max(0L, lengths(annot_info[[.annot.name]][['packing2_display']]))

      # --- Bracket-section: identify genes whose names fit inside their bracket ---
      # When brackets are ON for this annotation, every gene whose collapsed2 span
      # is wide enough to hold the name with one letter-width pad on each side AND
      # at least one letter-width of bracket-line stubs (combined) gets its name
      # drawn directly on the bracket row, with the bracket line broken around the
      # text.  Threshold: bracket_width >= name_width + 3*letter_width.  Gap each
      # side of the name = 1*letter_width.  Such genes are removed from the c2
      # ILP pool below --  the bracket-section drawing in plotting.R handles them.
      # COM (when center_of_mass=TRUE) drives the inside x-coordinate; otherwise
      # the bracket geometric centre is used.  The COM is clipped so the name (and
      # its 1-letter pad) still fits inside the bracket span.
      # Brackets are only supported in expanded / squished modes; for
      # collapsed / collapsed2 we treat brackets as OFF here so no inside-bracket
      # names are produced and all names go through the standard c2 ILP.
      .brackets.on.c2 = !is.null(incl_feature_brackets) &&
                        isTRUE(incl_feature_brackets[.annot.name]) &&
                        !is.null(annotation_packing) &&
                        annotation_packing[.annot.name] %in% c('expanded', 'squished')
      .c2.bracket.inside = list()
      if (.brackets.on.c2) {
        for (.c2.fn.bk in .c2.feat.names) {
          .c2.bracket.inside[[.c2.fn.bk]] = list()
          # Collapsed mode treats each packing2 key as ONE merged gene (no comma
          # split); other modes split the merged feature name into sub-features.
          .c2.genes.bk = if (.is.collapsed.mode) {
            .c2.fn.bk
          } else if (!grepl(",", .c2.fn.bk) || .c2.fn.bk %in% .c2.names.vec) {
            .c2.fn.bk
          } else {
            strsplit(.c2.fn.bk, ",")[[1]]
          }
          for (.g.bk in .c2.genes.bk) {
            .gi.bk = .all.c2.gene.info[[.g.bk]]
            if (is.null(.gi.bk) || is.null(.gi.bk$display.row)) next
            .nw.bk  = .gi.bk$nw
            .lw.bk  = .gi.bk$ng
            .xs.bk  = .gi.bk$start
            .xe.bk  = .gi.bk$end
            if ((.xe.bk - .xs.bk) >= (.nw.bk + 3 * .lw.bk)) {
              .x.bk = if (isTRUE(center_of_mass) && !is.null(.gi.bk$com))
                .gi.bk$com else (.xs.bk + .xe.bk) / 2
              # Clip so the name + 1-letter pad still fits inside the bracket span.
              .x.lo = .xs.bk + .nw.bk / 2 + .lw.bk
              .x.hi = .xe.bk - .nw.bk / 2 - .lw.bk
              .x.bk = max(.x.lo, min(.x.hi, .x.bk))
              .c2.bracket.inside[[.c2.fn.bk]][[.g.bk]] = list(
                row      = .gi.bk$display.row,
                x.center = .x.bk,
                gap.xs   = .x.bk - .nw.bk / 2 - .lw.bk,
                gap.xe   = .x.bk + .nw.bk / 2 + .lw.bk
              )
              .dbg("[c2-bracket] '", .g.bk, "' inside row=", .gi.bk$display.row,
                      " x=", round(.x.bk),
                      " (com=", isTRUE(center_of_mass) && !is.null(.gi.bk$com), ")")
            }
          }
        }
      }
      annot_info[[.annot.name]][['c2_bracket_inside_names']] = .c2.bracket.inside

      .c2.all.candidates = list()
      .c2.gene.to.feat  = list()
      .c2.groups.genes  = list()

      for (.c2.feat.name in .c2.feat.names) {
        # Collapsed mode treats each packing2 key as ONE merged gene (no split).
        .c2.genes = if (.is.collapsed.mode) {
          .c2.feat.name
        } else if (!grepl(",", .c2.feat.name) || .c2.feat.name %in% .c2.names.vec) {
          .c2.feat.name
        } else {
          strsplit(.c2.feat.name, ",")[[1]]
        }
        .valid.genes = .c2.genes[.c2.genes %in% names(.all.c2.gene.info)]
        .valid.genes = .valid.genes[!sapply(.valid.genes, function(.g) is.null(.all.c2.gene.info[[.g]]$display.row))]
        # Brackets ON: drop genes whose names fit inside their bracket --  those are
        # rendered directly by the bracket-section drawing, not the c2 ILP.
        if (.brackets.on.c2 && length(.c2.bracket.inside[[.c2.feat.name]]) > 0) {
          .valid.genes = setdiff(.valid.genes, names(.c2.bracket.inside[[.c2.feat.name]]))
        }
        if (length(.valid.genes) > 1) {
          .valid.genes = .valid.genes[order(sapply(.all.c2.gene.info[.valid.genes], `[[`, 'start'))]
        }
        .c2.groups.genes[[.c2.feat.name]] = .valid.genes
        for (.g in .valid.genes) {
          .gi = .all.c2.gene.info[[.g]]
          .dr = .gi$display.row
          # No forced.side: the ILP handles all name-bar and name-name conflicts
          # globally.  For co-enclosed genes the 1e6 bar-overlap penalty (from sibling
          # bars that are no longer excluded --  see Step 4b) naturally pushes each
          # gene's name to whichever side does not cross a sibling's bar.
          .c2.all.candidates[[.g]] = .generate.candidates(
            .gi$start, .gi$end, .gi$nw, .gi$ng,
            .dr, .dr, .dr, .max.display.rows, .gi$com,
            .forced.side = if (!is.null(.c2.forced.side[[.g]])) .c2.forced.side[[.g]] else NULL,
            .plot.start = .pr.start, .plot.end = .pr.end,
            .collapsed2 = TRUE
          )
          .c2.gene.to.feat[[.g]] = .c2.feat.name
        }
      }
      .c2.flat.genes = unlist(.c2.groups.genes, use.names = FALSE)
      # --- Precompute score for each candidate (required for ILP) ---
      for (.g in names(.c2.all.candidates)) {
        .gi = .all.c2.gene.info[[.g]]
        .dr = .gi$display.row
        
        # Host bars are excluded from the obstacle map on the gene's own display row
        # (see Step 4b).  Host bars are always at a different display row (they
        # contain the enclosed gene and are thus packed to separate rows), so this
        # exclusion is effectively a no-op for typical cases.  On other rows the
        # full obstacle map is used so names cannot stray outside their row.
        .excl = if (!is.null(.c2.exclude.from.obs[[.g]])) .c2.exclude.from.obs[[.g]] else character(0)
        .gene.row.bars = structure(lapply(names(.c2.global.row.bars), function(.rk) {
          .bars = .c2.global.row.bars[[.rk]]
          .row.excl = if (as.integer(.rk) == .dr) .excl else character(0)
          Filter(function(.b) .b$gene == .g || !(.b$gene %in% .row.excl), .bars)
        }), names = names(.c2.global.row.bars))
        
        for (i in seq_along(.c2.all.candidates[[.g]])) {
          .cand = .c2.all.candidates[[.g]][[i]]
          
          .res = .score.candidate(
            .cand$x, .cand$adj, .cand$row, .gi$nw,
            .dr, .dr, .dr, .gi$start, .gi$end, .gi$ng,
            .gene.row.bars, list(),   # IMPORTANT: no placed rects yet
            .gi$com, .exclude.genes = .g
          )
          
          .text.xl = .cand$x - .cand$adj * .gi$nw
          .text.xr = .text.xl + .gi$nw
          .self.ov = if (.cand$row == .dr)
            max(0, min(.text.xr, .gi$end) - max(.text.xl, .gi$start)) else 0

          .row.dist = abs(.cand$row - .dr)
          if (.row.dist > 0) {
            # Below/above candidate (collapsed2): Euclidean distance metric using
            # the bar's GEOMETRIC center (NOT center-of-mass).  This ensures that
            # left-aligned and right-aligned candidates at the same row receive
            # exactly equal distance penalties (their name centers are mirror
            # images of the bar center), and that center-aligned has zero
            # horizontal displacement.  COM is deliberately not used in
            # collapsed2 mode --  see expanded-mode pipeline for COM-based scoring.
            .name.center.x  = .text.xl + .gi$nw / 2
            .bar.center     = (.gi$start + .gi$end) / 2
            .row.height.bp  = 0.8 * .bp.per.cm   # 0.8 cm = standard collapsed2 row height
            .x.dist.norm    = abs(.name.center.x - .bar.center) / .row.height.bp
            .row.extra      = .row.dist - 1       # 0 for +1, 1 for +2, 2 for +3
            .dist.penalty   = 4 * .row.extra^2 + 0.1 * .x.dist.norm^2
            .effective.score = .res$pref          # raw preference, no com_bonus
          } else {
            # Inline candidate (collapsed2): no distance penalty.
            # Both inline-left and inline-right share pref=8 and tie-break on
            # name-bar / name-name penalty alone --  except in the bracket-section
            # path with center_of_mass=TRUE, where COM determines preferred side
            # (F5/B2): adj=0 wins when COM > geometric centre, adj=1 when COM <.
            .dist.penalty    = 0
            .effective.score = .res$pref
            if (.brackets.on.c2 && isTRUE(center_of_mass) && !is.null(.gi$com)) {
              .gc.cm = (.gi$start + .gi$end) / 2
              .com.tol = 0.10 * .gi$nw
              if (abs(.gi$com - .gc.cm) > .com.tol) {
                .com.side = if (.gi$com > .gc.cm) 0 else 1
                if (.cand$adj == .com.side) .effective.score = .effective.score + 0.5
              }
            }
          }
          .adj.penalty = .res$penalty + .dist.penalty + .self.ov * 1e6

          # FINAL SCORE for ILP (maximize)
          .cand$score = .effective.score - .adj.penalty
          # mark invalid if out of bounds
          .cand$valid = !(is.infinite(.res$score) && .res$score < 0)
          # Geometry fields required by clustering and overlap detection
          .cand$xmin = .text.xl
          .cand$xmax = .text.xr
          .cand$ymin = .cand$row - 0.5
          .cand$ymax = .cand$row + 0.5

          .c2.all.candidates[[.g]][[i]] = .cand
        }
      }
      if (length(.c2.flat.genes) > 1) {
        .c2.flat.genes = .c2.flat.genes[order(sapply(.all.c2.gene.info[.c2.flat.genes], `[[`, 'start'))]
      }

      # --- Steps 6-7: ILP global name placement (shared solver) ---
      .ilp.placements = .solve.name.placement.ilp(.c2.flat.genes, .c2.all.candidates)

      # Populate .c2.placements from ILP results
      .c2.placements = list()
      for (.c2.feat.name in .c2.feat.names) {
        .c2.placements[[.c2.feat.name]] = list()
        for (.g in .c2.groups.genes[[.c2.feat.name]]) {
          .pl = .ilp.placements[[.g]]
          if (!is.null(.pl)) .c2.placements[[.c2.feat.name]][[.g]] = .pl
        }
      }
      
      # --- Safety check: ensure every gene in packing2_display has a placement ---
      # Genes can be missing if display.row was never set (name/packing2 key mismatch)
      # or if candidate scoring filtered them out. Add a fallback placement so that
      # the inline drawing loop can always find all expected names.
      # Inside-bracket genes (brackets ON path) are intentionally absent --  their
      # names are rendered directly by the bracket-section drawing.
      for (.c2.fn in .c2.feat.names) {
        .c2.genes = if (.is.collapsed.mode) {
          .c2.fn
        } else if (!grepl(",", .c2.fn) || .c2.fn %in% .c2.names.vec) {
          .c2.fn
        } else {
          strsplit(.c2.fn, ",")[[1]]
        }
        .c2.genes = .c2.genes[.c2.genes %in% names(.all.c2.gene.info)]
        if (.brackets.on.c2 && length(.c2.bracket.inside[[.c2.fn]]) > 0) {
          .c2.genes = setdiff(.c2.genes, names(.c2.bracket.inside[[.c2.fn]]))
        }
        if (is.null(.c2.placements[[.c2.fn]])) .c2.placements[[.c2.fn]] = list()
        for (.g in .c2.genes) {
          if (!is.null(.c2.placements[[.c2.fn]][[.g]])) next  # already placed
          .gi = .all.c2.gene.info[[.g]]
          .dr = if (!is.null(.gi$display.row)) .gi$display.row else 1L
          # Right side within bounds? Otherwise left side. Last resort: pin to right edge.
          if (.gi$end + .gi$ng + .gi$nw <= .pr.end + .gi$nw * 0.10) {
            .fb = list(x = .gi$end + .gi$ng, adj = 0, row = .dr)
          } else if (.gi$start - .gi$ng - .gi$nw >= .pr.start - .gi$nw * 0.10) {
            .fb = list(x = .gi$start - .gi$ng, adj = 1, row = .dr)
          } else {
            .fb = list(x = .pr.end, adj = 1, row = .dr)
          }
          .c2.placements[[.c2.fn]][[.g]] = .fb
        }
      }

      .c2.placements  # return value from tryCatch block

      }, error = function(.e) {
        .dbg("[c2names] ERROR in annotation '", .annot.name, "': ", conditionMessage(.e))
        stop(.e)
      })  # end tryCatch Steps 5-7

      annot_info[[.annot.name]][['c2_inline_name_placements']] = .c2.placements

      # --- Post-ILP: move bars to match name rows (single-gene groups only) ---
      # When the ILP places a gene's name on a different row than its bar, move the
      # bar to the name row so feature and label are visually co-located --  but ONLY
      # when inline placement at that row is also feasible.  Bar move and name inline
      # upgrade are treated as an atomic operation: if inline cannot be achieved, the
      # bar stays where it is (moving without inline would leave a centered name on
      # top of the moved bar, creating a visual name-on-bar overlap).
      # Guard: only for single-gene groups --  multi-gene (merged/paired) groups have
      # their bar positions fixed by Stage C/D pairing and must not be touched.
      # Also skipped for collapsed mode: every super-locus has a fixed bar on row 1
      # (the union span) and the bar must not move regardless of where the name
      # lands.
      if (.is.collapsed.mode) {
        # Skip the post-ILP bar move for collapsed mode entirely.
      } else {
      # Build a quick lookup: row -> list of (xl, xr) for every placed name at that row.
      .placed.name.ranges = list()
      for (.c2fn2 in names(.c2.placements)) {
        .c2pl2 = .c2.placements[[.c2fn2]]
        for (.g2 in names(.c2pl2)) {
          .pl2 = .c2pl2[[.g2]]
          .gi2 = .all.c2.gene.info[[.g2]]
          if (is.null(.gi2)) next
          .xl2 = .pl2$x - .pl2$adj * .gi2$nw
          .xr2 = .xl2 + .gi2$nw
          .rk2 = as.character(.pl2$row)
          if (is.null(.placed.name.ranges[[.rk2]])) .placed.name.ranges[[.rk2]] = list()
          .placed.name.ranges[[.rk2]][[length(.placed.name.ranges[[.rk2]]) + 1L]] =
            list(xl = .xl2, xr = .xr2, gene = .g2)
        }
      }
      for (.c2.feat.name in names(annot_info[[.annot.name]][['packing2_display']])) {
        .c2.genes.here = if (.is.collapsed.mode) {
          .c2.feat.name
        } else if (!grepl(",", .c2.feat.name) || .c2.feat.name %in% .c2.names.vec) {
          .c2.feat.name
        } else {
          strsplit(.c2.feat.name, ",")[[1]]
        }
        # Skip merged groups (more than one gene) --  their layout is managed by pairing.
        # In collapsed mode every key is treated as one merged gene so this never fires.
        if (length(.c2.genes.here) > 1L) next
        .subset.names.here = .c2.names.vec[.c2.names.vec %in% .c2.genes.here]
        .c2pl.here = .c2.placements[[.c2.feat.name]]
        if (is.null(.c2pl.here)) next
        .pk2 = annot_info[[.annot.name]][['packing2_display']][[.c2.feat.name]]
        .pk2.changed = FALSE
        for (.g.here in names(.c2pl.here)) {
          .pl.here = .c2pl.here[[.g.here]]
          .name.row.here = .pl.here$row
          .gi.here = .all.c2.gene.info[[.g.here]]
          if (is.null(.gi.here)) next
          .gene.idx.here = which(.subset.names.here == .g.here)
          if (length(.gene.idx.here) == 0L) next
          .bar.row.here = NA_integer_
          for (.r.here in seq_along(.pk2)) {
            if (.gene.idx.here %in% .pk2[[.r.here]]) { .bar.row.here = .r.here; break }
          }
          if (is.na(.bar.row.here) || .bar.row.here == .name.row.here || .name.row.here < 1L) next
          # Bar move + inline upgrade are treated as an atomic operation:
          # the bar only moves if inline placement at the target row is also feasible.
          # Moving a bar without securing inline placement would leave the name
          # at its original "below" (centered) position on the same row as the moved
          # bar, creating a visual name-on-bar overlap.
          #
          # Step 1: Check inline viability at target row (right-of-bar preferred).
          .target.rk = as.character(.name.row.here)
          .ng.h = .gi.here$ng; .nw.h = .gi.here$nw; .tol.h = .nw.h * 0.10
          .ri.xl = .gi.here$end + .ng.h; .ri.xr = .ri.xl + .nw.h
          .li.xr = .gi.here$start - .ng.h; .li.xl = .li.xr - .nw.h
          .ri.ok = (.ri.xl >= .pr.start - .tol.h && .ri.xr <= .pr.end + .tol.h)
          .li.ok = (.li.xl >= .pr.start - .tol.h && .li.xr <= .pr.end + .tol.h)
          for (.nr in if (!is.null(.placed.name.ranges[[.target.rk]])) .placed.name.ranges[[.target.rk]] else list()) {
            if (.nr$gene == .g.here) next
            if (.ri.ok && .ri.xr > .nr$xl && .ri.xl < .nr$xr) .ri.ok = FALSE
            if (.li.ok && .li.xr > .nr$xl && .li.xl < .nr$xr) .li.ok = FALSE
          }
          for (.tb in if (!is.null(.c2.global.row.bars[[.target.rk]])) .c2.global.row.bars[[.target.rk]] else list()) {
            if (.tb$gene == .g.here) next
            if (.ri.ok && .ri.xr > .tb$start && .ri.xl < .tb$end) .ri.ok = FALSE
            if (.li.ok && .li.xr > .tb$start && .li.xl < .tb$end) .li.ok = FALSE
          }
          if (!.ri.ok && !.li.ok) next  # no inline space at target row --  skip bar move
          #
          # Step 2: Check bar-move conflicts at target row.
          # A: would the moved bar overlap another gene's placed name?
          # B: would the moved bar overlap another gene's bar?
          .conflict = FALSE
          if (!is.null(.placed.name.ranges[[.target.rk]])) {
            for (.nr in .placed.name.ranges[[.target.rk]]) {
              if (.nr$gene == .g.here) next
              if (.gi.here$end > .nr$xl && .gi.here$start < .nr$xr) {
                .conflict = TRUE; break
              }
            }
          }
          if (!.conflict && !is.null(.c2.global.row.bars[[.target.rk]])) {
            for (.tb in .c2.global.row.bars[[.target.rk]]) {
              if (.tb$gene == .g.here) next
              if (.gi.here$end > .tb$start && .gi.here$start < .tb$end) {
                .conflict = TRUE; break
              }
            }
          }
          if (.conflict) next
          #
          # Step 3: Commit --  move bar and upgrade name to inline atomically.
          .pk2[[.bar.row.here]] = setdiff(.pk2[[.bar.row.here]], .gene.idx.here)
          while (length(.pk2) < .name.row.here) .pk2[[length(.pk2) + 1L]] = integer(0)
          .pk2[[.name.row.here]] = c(.pk2[[.name.row.here]], .gene.idx.here)
          .pk2.changed = TRUE
          .new.pl = if (.ri.ok)
            list(x = .gi.here$end + .ng.h, adj = 0, row = .name.row.here)
          else
            list(x = .gi.here$start - .ng.h, adj = 1, row = .name.row.here)
          .c2.placements[[.c2.feat.name]][[.g.here]] = .new.pl
          # Refresh .placed.name.ranges so subsequent iterations see the new inline position
          .placed.name.ranges[[.target.rk]] = Filter(
            function(.nr) .nr$gene != .g.here,
            if (!is.null(.placed.name.ranges[[.target.rk]])) .placed.name.ranges[[.target.rk]] else list()
          )
          .new.xl = .new.pl$x - .new.pl$adj * .nw.h
          .placed.name.ranges[[.target.rk]][[length(.placed.name.ranges[[.target.rk]]) + 1L]] =
            list(xl = .new.xl, xr = .new.xl + .nw.h, gene = .g.here)
        }
        if (.pk2.changed) annot_info[[.annot.name]][['packing2_display']][[.c2.feat.name]] = .pk2
      }
      }  # end if/else (.is.collapsed.mode skip post-ILP bar move)
      # Persist any name placement updates made during inline re-placement above
      annot_info[[.annot.name]][['c2_inline_name_placements']] = .c2.placements

      .c2.max.name.row = 0L
      .c2.min.name.row = Inf
      .c2.max.display.row = max(0L, lengths(annot_info[[.annot.name]][['packing2_display']]))
      for (.fp in .c2.placements) {
        for (.p in .fp) {
          .c2.max.name.row = max(.c2.max.name.row, .p$row)
          .c2.min.name.row = min(.c2.min.name.row, .p$row)
        }
      }
      if (is.infinite(.c2.min.name.row)) .c2.min.name.row = 1L
      .c2.above.rows = max(0L, 1L - as.integer(.c2.min.name.row))
      annot_info[[.annot.name]][['c2_inline_name_extra_rows']] = max(0L, .c2.max.name.row - .c2.max.display.row)
      annot_info[[.annot.name]][['c2_inline_name_above_rows']] = .c2.above.rows
      .dbg("[c2names]   done: max.display.row=", .c2.max.display.row,
              "  max.name.row=", .c2.max.name.row,
              "  extra.rows=", annot_info[[.annot.name]][['c2_inline_name_extra_rows']],
              "  above.rows=", .c2.above.rows,
              "  n.placements=", sum(sapply(.c2.placements, length)))
      # Detailed dump of every placement for debugging
      for (.fn.dbg in names(.c2.placements)) {
        for (.gn.dbg in names(.c2.placements[[.fn.dbg]])) {
          .pl.dbg = .c2.placements[[.fn.dbg]][[.gn.dbg]]
          .dbg("[c2names-place] feat='", .fn.dbg, "' gene='", .gn.dbg,
                  "' row=", .pl.dbg$row, " adj=", .pl.dbg$adj,
                  " x=", round(.pl.dbg$x))
        }
      }
      # And what got tucked inside its bracket
      .ins.dbg = annot_info[[.annot.name]][['c2_bracket_inside_names']]
      if (!is.null(.ins.dbg)) {
        for (.fn.dbg in names(.ins.dbg)) {
          for (.gn.dbg in names(.ins.dbg[[.fn.dbg]])) {
            .ip = .ins.dbg[[.fn.dbg]][[.gn.dbg]]
            .dbg("[c2names-inside] feat='", .fn.dbg, "' gene='", .gn.dbg,
                    "' row=", .ip$row, " x.center=", round(.ip$x.center))
          }
        }
      }
    }
  }
  return(annot_info)
}


#' RelativeAnnotationHeight
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param annot_info 
#' @param annot_heights 
#' @param letter_heights
#' @param incl_feature_names
#' @param annotation_packing
#' @param incl_feature_brackets
#' @param stranded_beds 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
RelativeAnnotationHeight = function(annot_info, annot_heights, letter_heights, incl_feature_names, annotation_packing, incl_feature_brackets, stranded_beds){
  .annot.heights.incl.text = list()
  if (!is.null(annot_info)){
    if (any(stranded_beds)){
      for (.annot in names(annot_info)[stranded_beds]){
        # `.bracket.lines` was historically derived from the legacy
        # OrganizeAnnotationText pipeline; with that pipeline removed, the
        # length is taken directly from the per-font-size letter_heights.
        .bracket.lines = rep(1, length(letter_heights))
        # For expanded/squished modes, names are normally drawn inline.  When
        # brackets are ON, the inline pipeline is skipped and names live in a
        # bracket section below the transcripts (see ComputeInlineNamePlacements).
        # The bracket section adds length(packing2_display) bracket rows plus the
        # c2 fallback above/below rows.
        if (annotation_packing[.annot] == 'expanded' || annotation_packing[.annot] == 'squished') {
          # The downstream `PlotHeightParameters` multiplies this output by
          # `annot` uniformly regardless of mode, but plotting renders squished
          # rows at `annot_squished`.  To keep the panel height in sync with
          # rendering (no extra empty space at the bottom), pre-scale the
          # squished output by the squished/expanded ratio (0.625 by default).
          .max.lines = if (!is.null(annot_info[[.annot]][['packing']])) max(1L, max(lengths(annot_info[[.annot]][['packing']]))) else 1L
          .brackets.on = isTRUE(incl_feature_brackets[.annot])
          if (.brackets.on) {
            .pk2.rows = if (!is.null(annot_info[[.annot]][['packing2_display']])) max(0L, lengths(annot_info[[.annot]][['packing2_display']])) else 0L
            .extra.rows = if (!is.null(annot_info[[.annot]][['c2_inline_name_extra_rows']])) annot_info[[.annot]][['c2_inline_name_extra_rows']] else 0L
            .above.rows = if (!is.null(annot_info[[.annot]][['c2_inline_name_above_rows']])) annot_info[[.annot]][['c2_inline_name_above_rows']] else 0L
            .total.rows = .max.lines + .pk2.rows + .extra.rows + .above.rows
          } else {
            .extra.rows = if (!is.null(annot_info[[.annot]][['inline_name_extra_rows']])) annot_info[[.annot]][['inline_name_extra_rows']] else 0L
            .above.rows = if (!is.null(annot_info[[.annot]][['inline_name_above_rows']])) annot_info[[.annot]][['inline_name_above_rows']] else 0L
            .total.rows = .max.lines + .extra.rows + .above.rows
          }
          .mode.scale = if (annotation_packing[.annot] == 'squished') 0.625 else 1
          .annot.heights.incl.text[[.annot]] = rep(.total.rows * .mode.scale, length(.bracket.lines))
        } else if (annotation_packing[.annot] %in% c('collapsed', 'collapsed2') &&
                   !is.null(annot_info[[.annot]][['c2_inline_name_placements']])) {
          # collapsed/collapsed2 transcript rows == packing2_display bracket rows;
          # add c2 fallback above/below rows (inside-bracket names share a row
          # with the bracket, so they don't add height).
          # Output is in ROW units to keep panel-height in sync with rendering:
          # PlotHeightParameters multiplies by `annot`, matching EstimatePlotHeights'
          # `(.max.lines + extras + above) * annot` formula.  Otherwise the panel
          # reserves more physical space than the data y-range covers, leaving an
          # empty "blank row" at the bottom.
          .max.lines.c2 = if (!is.null(annot_info[[.annot]][['packing2_display']])) max(1L, max(lengths(annot_info[[.annot]][['packing2_display']]))) else 1L
          .extra.rows = if (!is.null(annot_info[[.annot]][['c2_inline_name_extra_rows']])) annot_info[[.annot]][['c2_inline_name_extra_rows']] else 0L
          .above.rows = if (!is.null(annot_info[[.annot]][['c2_inline_name_above_rows']])) annot_info[[.annot]][['c2_inline_name_above_rows']] else 0L
          .total.rows = .max.lines.c2 + .extra.rows + .above.rows
          .annot.heights.incl.text[[.annot]] = rep(.total.rows, length(.bracket.lines))
        } else {
          .annot.heights.incl.text[[.annot]] = annot_heights[[.annot]] + ifelse(incl_feature_names[.annot], 1, 0) * (.bracket.lines + ifelse(incl_feature_brackets[.annot], 1, 0) * .bracket.lines)
        }
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


