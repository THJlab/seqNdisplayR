BuildScrutinizePlotSegmentOrder(plotting_segment_order, .plotted.region, datasets, .plotted.samples, header, include_genomic_scale, genomic_scale_on_top, incl_annot=!is.null(annots), horizontal_spacers, .tracks.listed, both_strands, .any.stranded.beds, .any.unstranded.beds, .strands.intermingled)

BuildScrutinizePlotSegmentOrder = function(plotting_segment_order, plotted_region, datasets, plotted_samples, header, include_genomic_scale, genomic_scale_on_top, incl_annot, horizontal_spacers, tracks_listed, both_strands, any_stranded_beds, any_unstranded_beds, strands_intermingled){
  .plotting.segment.order = NULL
  if (!is.null(plotting_segment_order)){
    if (is.list(plotting_segment_order)){
      if (identical(names(plotting_segment_order), names(plotted_region))){
        plotting_segment_order_temp = list()
        for (.plot.strand in names(plotting_segment_order)){
          if (.plot.strand == '+'){
            plotting_segment_order_temp_vector = ScrutinizeExpandAndNameParameter(plotting_segment_order[['+']], plotting_segment_order[['+']], use_names=FALSE, default_value=NULL, expect_standard=NULL, expect=c("header", "scale", "empty-spacer", "thickline-spacer", "line-spacer", "annotations", "unstranded-beds", names(datasets)))
          }else if (.plot.strand == '-'){
            plotting_segment_order_temp_vector = ScrutinizeExpandAndNameParameter(plotting_segment_order[['-']], plotting_segment_order[['-']], use_names=FALSE, default_value=NULL, expect_standard=NULL, expect=c("header", "scale", "empty-spacer", "thickline-spacer", "line-spacer", "annotations", "unstranded-beds", names(datasets)))
          }
          plotting_segment_order_temp[[.plot.strand]] = as.character(plotting_segment_order_temp_vector)
        }
        if ( any(sapply(plotting_segment_order_temp, function(parameter) length(parameter)==0)) ){ return() }
        .plotting.segment.order = plotting_segment_order_temp
      }else{
        cat('ERROR: names of plotting_segment_order list has to match to the plotted strands - aborting', '\n')
        cat(paste('names:', paste(names(plotted_region), collapse=' ')), '\n')
        return()
      }
    }else{
      plotting_segment_order_temp_vector = as.character(ScrutinizeExpandAndNameParameter(plotting_segment_order, plotting_segment_order, use_names=FALSE, default_value=NULL, expect_standard=NULL, expect=c("header", "scale", "empty-spacer", "thickline-spacer", "line-spacer", "annotations", "unstranded-beds", names(datasets))))
      if (length(plotting_segment_order_temp_vector)==0){ return() }
      .plotting.segment.order = plotting_segment_order_temp_vector
    }
  }
  .plotting.segment.order = PlottingSegmentOrder(.plotting.segment.order, plotted_samples, header, include_genomic_scale, genomic_scale_on_top, incl_annot=!is.null(annots), horizontal_spacers)
  .plotting.segment.order = FinalizePlottingSegmentOrder(.plotting.segment.order, tracks_listed, both_strands, include_genomic_scale, genomic_scale_on_top, any_stranded_beds, any_unstranded_beds, strands_intermingled)
  .segment.summation = list()
  for (.strand in names(.plotting.segment.order)){
    .segment.summation[[.strand]] = sapply(c("header", "scale", "empty-spacer", "thickline-spacer", "line-spacer", "annotations", "unstranded-beds",names(datasets)), function(dataset_name) length((which(.plotting.segment.order[[.strand]]==dataset_name))))
    if (any(.segment.summation[[.strand]][c("header", "scale", "annotations", "unstranded-beds",names(datasets))] > 1)){
      cat(paste0('ERROR: there can at maximum be one of the values "', paste(c("header", "scale", "annotations", "unstranded-beds",names(datasets)), collapse='", "'), '" in plotting_segment_order list - aborting'), '\n')
      cat(paste0('"', paste(c("header", "scale", "annotations", "unstranded-beds",names(datasets)), collapse='", "')[which(.segment.summation[[.strand]][c("header", "scale", "annotations", "unstranded-beds",names(datasets))] > 1)], '" represented more than once'), '\n')
      return()
    }
  }
  if (length(.segment.summation) == 2){
    .segment.summation.total = rowSums(as.data.frame(.segment.summation))
    if (.segment.summation.total[['header']] == 1){
      if (.segment.summation[['-']][['header']] == 1){
        cat('ERROR: the "header" segment is placed in the minus strand plotting segments, it should be placed in the plus strand plotting segments - aborting', '\n')
        return()
      }
    }
    if (.segment.summation.total[['annotations']] > 0){
      if (.segment.summation.total[['annotations']] != 2){
        cat('ERROR: if annotations should displayed the "annotations" segments should be present under plotting segments for both strands - aborting', '\n')
        cat(paste('"annotations" segment only present under' ,ifelse(.segment.summation[['+']][['annotations']]==1, 'plus', 'minus'), 'strand'), '\n')
        return()
      }
    }
  }
  return(.plotting.segment.order)
}
