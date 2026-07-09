# Plotting functions --  drawing to device

#' Prepare Plotting Interface
#'
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA
#'
#' @param plot_dim 
#' @param pdf 
#' @param pdf_name 
#' @param pdf_dir 
#' @param header 
#' @param bin_size 
#' @param feature 
#' @param scaling_factor 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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
    .pdf.name = paste0(.pdf.name) #@ 2022-10-26 removed the .pdf because of issues with shiny download # , '.pdf'
    if (!dir.exists(.pdf.dir)){
      dir.create(.pdf.dir, recursive=TRUE)
    }
    pdf(paste0(.pdf.dir, '/', .pdf.name), width=.full.width.in, height=.full.height.in)
    return(paste0(.pdf.dir, '/', .pdf.name)) #@ 2022-10-26 added
  }else{
    ## bg='white' explicitly so macOS Quartz can't open the window with the
    ## system dark-mode background.
    dev.new(width=scaling_factor*.full.width.in,
            height=scaling_factor*.full.height.in,
            noRStudioGD=TRUE,
            bg="white") ## opens a new plotting window with the indicated dimensions
    return() #@ 2022-10-26 added
  }
}


#' Plot Header
#'
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param windows_height 
#' @param n_segment 
#' @param coords_tracks 
#' @param full_width_cm 
#' @param plot_width 
#' @param header 
#' @param header_font_sizes 
#' @param chrom 
#' @param both_strands 
#' @param plotted_strand 
#' @param plot_start 
#' @param plot_end 
#' @param font_colors 
#' @param font_family 
#' @param first_plot 
#' @param scaling_factor 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
PlotHeader = function(windows_height, n_segment, coords_tracks, full_width_cm,  plot_width, header, header_font_sizes, chrom, both_strands, plotted_strand, plot_start, plot_end, font_colors, font_family, first_plot, scaling_factor){
  constants_defaults = ConstantsDefaults()
  line_width_scaling_factor = constants_defaults['line_width_scaling_factor'] #@ 2022-10-05
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
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param chrom_coord 
#' @param font_size 
#' @param coords_tracks 
#' @param full_width_cm 
#' @param plot_width 
#' @param plot_start 
#' @param plot_end 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
CoordsOfGenomicScale = function(chrom_coord, font_size, coords_tracks, full_width_cm, plot_width, plot_start, plot_end){
  constants_defaults = ConstantsDefaults()
  std_letter_width = constants_defaults['std_letter_width'] #@ 2022-10-05
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
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param windows_height 
#' @param n_segment 
#' @param coords_tracks 
#' @param full_width_cm 
#' @param genomic_scale_on_top 
#' @param plot_width 
#' @param plot_start 
#' @param plot_end 
#' @param first_plot 
#' @param font_color 
#' @param font_family 
#' @param genomic_scale_font_size 
#' @param scaling_factor 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
PlotScale = function(windows_height, n_segment, coords_tracks, full_width_cm, genomic_scale_on_top, plot_width, plot_start, plot_end, first_plot, font_color, font_family, genomic_scale_font_size, scaling_factor){
  constants_defaults = ConstantsDefaults()
  line_width_scaling_factor = constants_defaults['line_width_scaling_factor'] #@ 2022-10-05
  std_letter_width = constants_defaults['std_letter_width'] #@ 2022-10-05
  par(fig=c(coords_tracks[1],coords_tracks[2],windows_height[n_segment+1],windows_height[n_segment]), mai=scaling_factor*c(0, 0, 0, 0), new=ifelse(n_segment==1 & first_plot, F, T))
  plot(0, 0, type='n', xlim=c(plot_start, plot_end), ylim=c(-1,1), ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i')
  abline(h=ifelse(genomic_scale_on_top, -1, 1)*0.8, col=font_color['genomic_axis'], lwd=scaling_factor*line_width_scaling_factor*0.5, lend=1)
  .tickmarks = as.integer(axTicks(side=1))
  .full.width.coord.letters.cm = sum(nchar(format(.tickmarks, nsmall=0))) * (std_letter_width*genomic_scale_font_size)
  .tracks.width.cm = diff(coords_tracks)*full_width_cm
  if (.full.width.coord.letters.cm > .tracks.width.cm){
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
  } 
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
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param windows_height 
#' @param spacer_segment 
#' @param right_border 
#' @param plotted_strand 
#' @param neg_vals_neg_strand 
#' @param panel_separators 
#' @param separators_lwds 
#' @param separators_colors 
#' @param scaling_factor 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
PlotSpacer = function(windows_height, spacer_segment, right_border, plotted_strand, neg_vals_neg_strand, panel_separators, separators_lwds, separators_colors, scaling_factor){
  constants_defaults = ConstantsDefaults()
  line_width_scaling_factor = constants_defaults['line_width_scaling_factor'] #@ 2022-10-05
  .left.border = 1 - right_border
  # Defensive: bail out cleanly if the spacer name isn't in windows_height
  # (otherwise par(fig=c(..., numeric(0))) raises "wrong length"). Diagnostic
  # gated on seqNdisplayR.debug -- a missing spacer here is usually a
  # trailing-spacer-cleanup artefact, not a real bug, and silently skipping
  # is the correct behaviour.
  .spacer.idx <- which(names(windows_height) == spacer_segment)
  if (length(.spacer.idx) == 0L || .spacer.idx == 1L) {
    if (isTRUE(getOption("seqNdisplayR.debug", FALSE))) {
      message("[seqNdisplayR debug] PlotSpacer: '", spacer_segment,
              "' not found in windows_height (skipping spacer).")
    }
    return(invisible(NULL))
  }
  par(fig=c(.left.border,right_border,windows_height[spacer_segment],windows_height[.spacer.idx - 1L]), mai=scaling_factor*c(0, 0, 0, 0), new=TRUE)
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
#' @description Internal function: 
#' Sample info at the left side of the plots  
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param plotting_segment 
#' @param plotted_strand 
#' @param panel_info 
#' @param panels_list 
#' @param panel_separators 
#' @param separators_lwds 
#' @param separators_colors 
#' @param incl_first_panel 
#' @param print_one_line_sample_names 
#' @param replicate_names 
#' @param plot_width_parameters 
#' @param windows_height 
#' @param vertical_slots 
#' @param segment_top 
#' @param full_width_cm 
#' @param font_color 
#' @param font_family 
#' @param colors 
#' @param first_plot 
#' @param letter_heights 
#' @param scaling_factor 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
PlotPanels = function(plotting_segment, plotted_strand, panel_info, panels_list, panel_separators, separators_lwds, separators_colors, incl_first_panel, print_one_line_sample_names, replicate_names, plot_width_parameters, windows_height, vertical_slots, segment_top, full_width_cm, font_color, font_family, colors, first_plot, letter_heights, scaling_factor){
  constants_defaults = ConstantsDefaults()
  line_width_scaling_factor = constants_defaults['line_width_scaling_factor'] #@ 2022-10-05
  std_letter_width = constants_defaults['std_letter_width'] #@ 2022-10-05
  .panel.width.list = panel_info[[plotted_strand]]$panel.width.list
  .n.panels = length(.panel.width.list[[plotting_segment]])
  .n.panel.separators = ifelse(incl_first_panel & !print_one_line_sample_names, as.integer(.n.panels/2), as.integer((.n.panels-1)/2))
  .panels = ifelse(incl_first_panel & !print_one_line_sample_names, 1, 2):.n.panels
  .separators = c()
  if (.n.panel.separators>0){
    .separators = seq(ifelse(incl_first_panel | print_one_line_sample_names, 2, 3), .n.panels, by=2) 
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
      .panel = which(.panels==.n.panel) + ifelse(incl_first_panel & !print_one_line_sample_names, 0, 1) 
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
        .horizontal = as.logical(.horizontal.panels.list[[plotting_segment]][.panel]) #@ 2023-12-18
        if (print_one_line_sample_names){ 
          .horizontal = TRUE
        } 
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
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param plotted_region 
#' @param basic_plot_parameters 
#' @param plot_start 
#' @param plot_end 
#' @param plot_width 
#' @param bin_size 
#' @param reverse_strand_direction 
#' @param sample_subset 
#' @param dummy_plot 
#' @param tracks 
#' @param plotting_segment 
#' @param bin_stats 
#'
#' @return placeholder
#' 
#' @import S4Vectors
#' @import IRanges
#' @importFrom BiocGenerics strand
#'
#' @examples
#' NULL
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
          .rev.score = rev(.score)
          if (bin_stats[plotting_segment]=='mean'){
            .plot.mat[,.seq.sample] = sapply((-.n.bins.before+1):.n.bins.after, function(.n.bin) mean(.rev.score[abs(.bin.start - plot_start) + ((.n.bin-1)*bin_size+1):(.n.bin*bin_size)]))
          }else if (bin_stats[plotting_segment]=='median'){
            .plot.mat[,.seq.sample] = sapply((-.n.bins.before+1):.n.bins.after, function(.n.bin) median(.rev.score[abs(.bin.start - plot_start) + ((.n.bin-1)*bin_size+1):(.n.bin*bin_size)]))
          }else if (bin_stats[plotting_segment]=='max'){
            .plot.mat[,.seq.sample] = sapply((-.n.bins.before+1):.n.bins.after, function(.n.bin) max(.rev.score[abs(.bin.start - plot_start) + ((.n.bin-1)*bin_size+1):(.n.bin*bin_size)]))
          }
        }
      }
    }
  }
  return(.plot.mat)
}


#' Plot Data
#'
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param plotting_segment 
#' @param plot_mat 
#' @param colors 
#' @param strands_alpha 
#' @param intermingled_color 
#' @param sample_subset 
#' @param windows_height 
#' @param coords_tracks 
#' @param coords_scale 
#' @param first_plot 
#' @param neg_vals_neg_strand 
#' @param plotted_strand 
#' @param y_par 
#' @param plot_start 
#' @param plot_end 
#' @param bin_width 
#' @param group_autoscale 
#' @param incl_track_scales 
#' @param scientific_scale 
#' @param scale_font_size 
#' @param log2transformed 
#' @param full_width_cm 
#' @param font_colors 
#' @param font_family 
#' @param scaling_factor 
#' @param letter_widths 
#' @param enhance_signals 
#' @param scale_warning 
#' @param verbosity 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
PlotData = function(plotting_segment, plot_mat, colors, strands_alpha, intermingled_color, sample_subset, windows_height, coords_tracks, coords_scale, first_plot, neg_vals_neg_strand, plotted_strand, y_par, plot_start, plot_end, bin_width, group_autoscale, incl_track_scales, scientific_scale, scale_font_size, log2transformed, full_width_cm, font_colors, font_family, scaling_factor, letter_widths, enhance_signals, scale_warning=NULL, verbosity){
  constants_defaults = ConstantsDefaults()
  line_width_scaling_factor = constants_defaults['line_width_scaling_factor'] #@ 2022-10-05
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
      .y.min = -as.numeric(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['-']][['max']])), y_par[['-']][['max']], y_par[['-']][['max']][.seq.sample])) 
      .y.limits = structure(c(-1.5, 1.5), names=c('min', 'max')) 
      .y.val['+'] = as.numeric(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['+']][['val']])), y_par[['+']][['val']], y_par[['+']][['val']][.seq.sample])) 
      .y.val['-'] = as.numeric(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['-']][['val']])), y_par[['-']][['val']], y_par[['-']][['val']][.seq.sample])) 
      .n.decimals['+'] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['+']][['dec']])), y_par[['+']][['dec']], y_par[['+']][['dec']][.seq.sample])) 
      .n.decimals['-'] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['-']][['dec']])), y_par[['-']][['dec']], y_par[['-']][['dec']][.seq.sample])) 
      .scientific =  as.logical(ifelse(scientific_scale=='all', TRUE, ifelse(scientific_scale=='none', FALSE, (ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['+']][['sci']])), y_par[['+']][['sci']], y_par[['+']][['sci']][.seq.sample]) | ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['-']][['sci']])), y_par[['-']][['sci']], y_par[['-']][['sci']][.seq.sample]))))) 
      .y.exp['+'] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['+']][['exp']])), y_par[['+']][['exp']], y_par[['+']][['exp']][.seq.sample])) 
      .y.exp['-'] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[['-']][['exp']])), y_par[['-']][['exp']], y_par[['-']][['exp']][.seq.sample])) 
      .plot.mat = plot_mat[['+']] / .y.val['+']
    }else{
      .y.max = as.numeric(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[[.plotted.strand]][['max']])), y_par[[.plotted.strand]][['max']], y_par[[.plotted.strand]][['max']][.seq.sample])) 
      .y.min = 0
      .y.limits = structure(sort(ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*c(0, 1.5)), names=c('min', 'max'))
      .y.val[.plotted.strand] = as.numeric(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[[.plotted.strand]][['val']])), y_par[[.plotted.strand]][['val']], y_par[[.plotted.strand]][['val']][.seq.sample])) 
      .n.decimals[.plotted.strand] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[[.plotted.strand]][['dec']])), y_par[[.plotted.strand]][['dec']], y_par[[.plotted.strand]][['dec']][.seq.sample])) 
      .scientific = as.logical(ifelse(scientific_scale=='all', TRUE, ifelse(scientific_scale=='none', FALSE, ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[[.plotted.strand]][['sci']])), y_par[[.plotted.strand]][['sci']], y_par[[.plotted.strand]][['sci']][.seq.sample]))))
      .y.exp[.plotted.strand] = as.integer(ifelse((group_autoscale[plotting_segment] | 'forced' %in% names(y_par[[.plotted.strand]][['exp']])), y_par[[.plotted.strand]][['exp']], y_par[[.plotted.strand]][['exp']][.seq.sample])) 
      .plot.mat = plot_mat[[.plotted.strand]] / .y.val[.plotted.strand]
    }
    .max.val.abs = 1.2 #@ 2024-03-26 The adjusted scale goes from 0 to 1.5, where do you want the absolute max to be. With autoscale it is at 1.
    if (any(.plot.mat > .max.val.abs)){ .plot.mat[which(.plot.mat > .max.val.abs)] = .max.val.abs } #@ 2024-03-26
    plot(0, 0, type='n', xlim=c(plot_start, plot_end), ylim=.y.limits, ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i')
    .adj.colors = structure(unlist(lapply(c('+', '-'), function(.strand) {.adj.rgb = (255 - (ifelse(!.enhance, strands_alpha[.strand], 100)/100)*(255 - as.vector(col2rgb(.base.cols[.base.seq.sample]))))/255; .adj.color = rgb(.adj.rgb[1], .adj.rgb[2], .adj.rgb[3]); return(.adj.color)})), names=c('+', '-'))
    if (.plotted.strand=='+-'){
      lines(as.integer(rownames(.plot.mat)), ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*.plot.mat[,.seq.sample], type='h', lend=1,
            lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col=.adj.colors['+']) 
      if (any(.plot.mat[,.seq.sample] == .max.val.abs)){ #@ 2024-03-26 1.5 -> .max.val.abs
        saturated_indices = which(.plot.mat[,.seq.sample] == .max.val.abs) #@ 2024-03-26 1.5 -> .max.val.abs
        .samp.color = .adj.colors['+']
        .sat.color = ChangeColorLightness(.samp.color, 0.25)
        segments(as.integer(rownames(.plot.mat))[saturated_indices], ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*rep(.max.val.abs - 0.1, length(saturated_indices)), #@ 2024-03-26 1.4 -> .max.val.abs - 0.1
                 as.integer(rownames(.plot.mat))[saturated_indices], ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*rep(.max.val.abs, length(saturated_indices)), #@ 2024-03-26 1.5 -> .max.val.abs
                 lend=1, lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col=.sat.color)
        segments(as.integer(rownames(.plot.mat))[saturated_indices], ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*rep(.max.val.abs - 0.15, length(saturated_indices)), #@ 2024-03-26 1.35 -> .max.val.abs - 0.15
                 as.integer(rownames(.plot.mat))[saturated_indices], ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*rep(.max.val.abs - 0.1, length(saturated_indices)), #@ 2024-03-26 1.4 -> .max.val.abs - 0.1
                 lend=1, lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col='white')
      }
      .plot.mat = plot_mat[['-']] / .y.val['-']
      if (any(.plot.mat > .max.val.abs)){ .plot.mat[which(.plot.mat > .max.val.abs)] = .max.val.abs } #@ 2024-03-26 1.5 -> .max.val.abs
      if (intermingled_color!='same'){
        if (intermingled_color=='complementary'){
          .adj.colors['-'] = sapply(.adj.colors['-'], function(c) ConvertColor(c, phi=180))
        }else if (intermingled_color=='analogous_right'){
          .adj.colors['-'] = sapply(.adj.colors['-'], function(c) ConvertColor(c, phi=30))
        }else if (intermingled_color=='analogous_left'){
          .adj.colors['-'] = sapply(.adj.colors['-'], function(c) ConvertColor(c, phi=-30))
        }
      }
      lines(as.integer(rownames(.plot.mat)), -1*.plot.mat[,.seq.sample], type='h', lend=1,
            lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col=.adj.colors['-'])
      if (any(.plot.mat[,.seq.sample] == .max.val.abs)){ #@ 2024-03-26 1.5 -> .max.val.abs
        saturated_indices = which(.plot.mat[,.seq.sample] == .max.val.abs) #@ 2024-03-26 1.5 -> .max.val.abs
        .samp.color = .adj.colors['-']
        .sat.color = ChangeColorLightness(.samp.color, 0.25)
        segments(as.integer(rownames(.plot.mat))[saturated_indices], -1*rep(.max.val.abs - 0.1, length(saturated_indices)), #@ 2024-03-26 1.4 -> .max.val.abs - 0.1
                 as.integer(rownames(.plot.mat))[saturated_indices], -1*rep(.max.val.abs, length(saturated_indices)), #@ 2024-03-26 1.5 -> .max.val.abs - 0.1
                 lend=1, lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col=.sat.color)  
        segments(as.integer(rownames(.plot.mat))[saturated_indices], -1*rep(.max.val.abs - 0.15, length(saturated_indices)), #@ 2024-03-26 1.35 -> .max.val.abs - 0.15
                 as.integer(rownames(.plot.mat))[saturated_indices], -1*rep(.max.val.abs - 0.1, length(saturated_indices)), #@ 2024-03-26 1.4 -> .max.val.abs - 0.1
                 lend=1, lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col='white')
      }
      abline(h=0, col='whitesmoke', lwd=scaling_factor*line_width_scaling_factor*0.5, lend=1) 
    }else{
      lines(as.integer(rownames(.plot.mat)), ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*.plot.mat[,.seq.sample], type='h', lend=1,
            lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col=.adj.colors[.plotted.strand])
      if (any(.plot.mat[,.seq.sample] == .max.val.abs)){ #@ 2024-03-26 1.5 -> .max.val.abs
        saturated_indices = which(.plot.mat[,.seq.sample] == .max.val.abs) #@ 2024-03-26 1.5 -> .max.val.abs
        .samp.color = .adj.colors[.plotted.strand]
        .sat.color = ChangeColorLightness(.samp.color, 0.25)
        segments(as.integer(rownames(.plot.mat))[saturated_indices], ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*rep(.max.val.abs - 0.1, length(saturated_indices)), #@ 2024-03-26 1.4 -> .max.val.abs - 0.1
                 as.integer(rownames(.plot.mat))[saturated_indices], ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*rep(.max.val.abs, length(saturated_indices)), #@ 2024-03-26 1.5 -> .max.val.abs
                 lend=1, lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col=.sat.color) 
        segments(as.integer(rownames(.plot.mat))[saturated_indices], ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*rep(.max.val.abs - 0.15, length(saturated_indices)), #@ 2024-03-26 1.35 -> .max.val.abs - 0.15
                 as.integer(rownames(.plot.mat))[saturated_indices], ifelse(neg_vals_neg_strand & .plotted.strand=='-', -1, 1)*rep(.max.val.abs - 0.1, length(saturated_indices)), #@ 2024-03-26 1.4 -> .max.val.abs - 0.1
                 lend=1, lwd=ifelse(.enhance, 5, 1)*scaling_factor*bin_width, col='white') 
      }
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
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param plot_mat 
#' @param plotting_segment 
#' @param force_scale_list 
#' @param group_autoscale 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
YParameters = function(plot_mat, plotting_segment, force_scale_list, group_autoscale){
  if (!is.null(force_scale_list)){
    if (is.null(force_scale_list[[plotting_segment]]) | is.na(force_scale_list[[plotting_segment]])){
      if (as.logical(group_autoscale[plotting_segment])){
        .y.val = structure(max(plot_mat, na.rm=T), names='max')
      }else{
        .y.val = apply(plot_mat, 2, max, na.rm=T) 
      }
    }else{
      .y.val = structure(as.numeric(ifelse(is.na(force_scale_list[plotting_segment]), max(plot_mat, na.rm=T), force_scale_list[plotting_segment])), names='forced')
    }
  }else{
    if (as.logical(group_autoscale[plotting_segment])){
      .y.val = structure(max(plot_mat, na.rm=T), names='max')
    }else{
      .y.val = apply(plot_mat, 2, max, na.rm=T) 
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
  .y.val = round(.y.val, .n.decimals+1)  #@ 2024-11-12 trying to change rounding 
  .final.exponent = as.integer(log10(.y.val)) + ifelse(.y.val < 1, -1, 0)
  .y.max = 1.5*.y.val
  return(list('max'=.y.max, 'val'=.y.val, 'dec'=.n.decimals, 'sci'=.scientific, 'exp'=.final.exponent))
}


#' Segment Top
#'
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param plotting_segment 
#' @param plotted_strand 
#' @param windows_height 
#' @param annot_info 
#' @param dummy_plot 
#' @param tracks 
#' @param unstranded_beds_names 
#' @param verbosity 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param annot_info 
#' @param stranded 
#' @param annot_cols 
#' @param annotation_packing 
#' @param plotted_region 
#' @param plotted_strand 
#' @param substrand 
#' @param basic_plot_parameters 
#' @param plot_start 
#' @param plot_end 
#' @param plot_width 
#' @param bin_size 
#' @param reverse_strand_direction 
#' @param incl_feature_names 
#' @param feature_names_above 
#' @param incl_feature_brackets 
#' @param incl_feature_shadings 
#' @param feature_shading_colors 
#' @param feature_shading_alpha 
#' @param plot_width_parameters
#' @param plot_vertical_parameters
#' @param windows_height
#' @param feature_font_size
#' @param annotation_panel_font_size 
#' @param annot_panel_dist 
#' @param coords_tracks 
#' @param font_colors 
#' @param font_family 
#' @param first_plot 
#' @param scaling_factor 
#' @param verbosity 
#'
#' @return placeholder
#' 
#' @import S4Vectors
#' @import IRanges
#'
#' @examples
#' NULL
#' 
PlotAnnotation = function(annot_info, stranded, annot_cols, annotation_packing, plotted_region, plotted_strand, substrand, basic_plot_parameters, plot_start, plot_end, plot_width, bin_size, reverse_strand_direction, incl_feature_names, feature_names_above, incl_feature_brackets, incl_feature_shadings, feature_shading_colors, feature_shading_alpha, plot_width_parameters, plot_vertical_parameters, windows_height, feature_font_size, annotation_panel_font_size, annot_panel_dist=0.4, coords_tracks, font_colors, font_family, first_plot, scaling_factor, verbosity){
  constants_defaults = ConstantsDefaults()
  line_width_scaling_factor = constants_defaults['line_width_scaling_factor'] #@ 2022-10-05
  arrow_constant = constants_defaults['arrow_constant'] #@ 2022-10-05
  std_letter_width = constants_defaults['std_letter_width'] #@ 2022-10-05
  annot_panel_dist = constants_defaults['annot_panel_dist'] #@ 2022-10-05
  .strand = substrand # ifelse(plotted_strand == '+' | plotted_strand == '+-', '+', '-')
  # Cache plotted_region accessors --  used throughout the inner loops
  .pr.start = IRanges::start(plotted_region)
  .pr.end = IRanges::end(plotted_region)
  .pr.width = IRanges::width(plotted_region)
  .bin.start = S4Vectors::mcols(plotted_region)$bin.start
  .bin.width = basic_plot_parameters[[plotted_strand]]$bin.info[2]
  .n.bins.before = as.integer(abs(.bin.start - plot_start)/bin_size)
  .n.bins.after = as.integer(abs(plot_end - .bin.start)/bin_size)
  .coords = sapply((-.n.bins.before+1):.n.bins.after, function(.n.bin) mean(.bin.start + ifelse(.strand=='+' | !reverse_strand_direction, 1, -1)*c((.n.bin-1)*bin_size, .n.bin*bin_size-1)))
  .coords.per.mm = .pr.width/(plot_width_parameters$tracks.width.cm*10)
  .length.arrows = 0.363*.coords.per.mm/arrow_constant
  .direction.arrows = ifelse(.strand=='+', -1, +1)*.length.arrows
  .line.width = 4*scaling_factor*line_width_scaling_factor
  .y.scaling = as.numeric(plot_vertical_parameters['annot']/0.8) 
  for (.annotation in names(annot_info[[.strand]])){
    if (stranded){
      .stranded.annotation = paste0(.annotation, .strand)
    }else{
      .stranded.annotation = .annotation
    }
    if (verbosity > 0){ cat(paste('plotting', .stranded.annotation, 'annotation'), '\n') }
    .n.segment = which(names(windows_height)==.stranded.annotation)-1
    if (length(.n.segment) == 0L || .n.segment + 1 > length(windows_height)) {
      # The stranded annotation name isn't in windows_height. Skip this
      # iteration cleanly. Diagnostic gated on seqNdisplayR.debug.
      if (isTRUE(getOption("seqNdisplayR.debug", FALSE))) {
        message("[seqNdisplayR debug] PlotAnnotation: '", .stranded.annotation,
                "' not found in windows_height (skipping band).")
      }
      next
    }
    par(fig=c(coords_tracks[1],coords_tracks[2],windows_height[.n.segment+1],windows_height[.n.segment]), mai=scaling_factor*c(0, 0, 0, 0), new=ifelse(.n.segment==1 & first_plot, F, T))
    if (length(annot_info[[.strand]][[.annotation]]) > 0){
      .include.introns = FALSE
      if (annotation_packing[.annotation] == 'expanded' | annotation_packing[.annotation] == 'squished'){
        .subset.annotation = annot_info[[.strand]][[.annotation]][['expanded']]
        .packing = annot_info[[.strand]][[.annotation]][['packing']]
        .include.introns = TRUE
      }else if (annotation_packing[.annotation] == 'collapsed'){
        .subset.annotation = annot_info[[.strand]][[.annotation]][['collapsed']]
        .packing = structure(lapply(rep(1, length(.subset.annotation)), list), names=names(.subset.annotation))
      }else if (annotation_packing[.annotation] == 'collapsed2'){
        .subset.annotation = annot_info[[.strand]][[.annotation]][['collapsed2']]
        .packing = if (!is.null(annot_info[[.strand]][[.annotation]][['packing2_display']])) annot_info[[.strand]][[.annotation]][['packing2_display']] else annot_info[[.strand]][[.annotation]][['packing2']]
      }
      .annot.steps = as.numeric(ifelse(annotation_packing[.annotation] == 'squished', plot_vertical_parameters['annot_squished'], plot_vertical_parameters['annot']))
      .annot.text.steps = as.numeric(plot_vertical_parameters['annot_text_segment'])
      .y.span = basic_plot_parameters[[plotted_strand]][['track.vector']][.stranded.annotation]
      # In brackets-ON expanded/squished mode, the entire bracket section
      # (including c2 above-rows) sits BELOW the transcript area --  the full
      # y.span is used below y=0, with no top extension.  In collapsed2 mode
      # (with or without brackets), c2 above-rows still extend the y-axis above
      # y=0 as in the legacy layout.
      .brackets.below = isTRUE(incl_feature_brackets[.annotation]) &&
        (annotation_packing[.annotation] == 'expanded' ||
         annotation_packing[.annotation] == 'squished')
      if (.brackets.below) {
        .c2.above.rows = 0L
      } else if (annotation_packing[.annotation] %in% c('collapsed', 'collapsed2') && !is.null(annot_info[[.strand]][[.annotation]][['c2_inline_name_above_rows']])) {
        .c2.above.rows = annot_info[[.strand]][[.annotation]][['c2_inline_name_above_rows']]
      } else if ((annotation_packing[.annotation] == 'expanded' || annotation_packing[.annotation] == 'squished') && !is.null(annot_info[[.strand]][[.annotation]][['inline_name_above_rows']])) {
        .c2.above.rows = annot_info[[.strand]][[.annotation]][['inline_name_above_rows']]
      } else {
        .c2.above.rows = 0L
      }
      .y.limits = sort(ifelse(feature_names_above[[.strand]][.annotation], 1, -1)*c(-.c2.above.rows * .annot.steps, .y.span - .c2.above.rows * .annot.steps))
      .is.squished = annotation_packing[.annotation] == 'squished'
      plot(0, 0, type='n', xlim=c(plot_start, plot_end), ylim=.y.limits, ann=FALSE, axes=FALSE, bg='transparent', bty='n', xaxs='i', yaxs ='i')
      for (.feat.name in names(.subset.annotation)){
        .feat.annotation = .subset.annotation[[.feat.name]]
        # Handle small gene row overrides --  move transcripts to new rows in packing
        # so they are drawn normally (expanded style) at override positions.
        .row.overrides = annot_info[[.strand]][[.annotation]][['small_gene_row_overrides']][[.feat.name]]
        if (!is.null(.row.overrides) && .include.introns) {
          .trn.idx.map = annot_info[[.strand]][[.annotation]][['gene_trn_indices']][[.feat.name]]
          for (.override.gene in names(.row.overrides)) {
            .new.row = .row.overrides[[.override.gene]]
            .trn.indices = if (!is.null(.trn.idx.map) && .override.gene %in% names(.trn.idx.map)) .trn.idx.map[[.override.gene]] else integer(0)
            if (length(.trn.indices) == 0) next
            # Remove transcripts from their original packing rows
            for (.r in seq_along(.packing[[.feat.name]])) {
              .packing[[.feat.name]][[.r]] = setdiff(.packing[[.feat.name]][[.r]], .trn.indices)
            }
            # Ensure packing has enough rows
            while (length(.packing[[.feat.name]]) < .new.row) {
              .packing[[.feat.name]][[length(.packing[[.feat.name]]) + 1]] = integer(0)
            }
            # Add transcripts at new row --  drawn normally by the expanded drawing loop
            .packing[[.feat.name]][[.new.row]] = c(.packing[[.feat.name]][[.new.row]], .trn.indices)
          }
        }
        # Cache per-feature mcols and coordinates --  accessed repeatedly per annotation line
        .fa.mcols = S4Vectors::mcols(.feat.annotation)
        .fa.starts = IRanges::start(.feat.annotation)
        .fa.ranges = IRanges::ranges(.feat.annotation)
        if (!('blocks' %in% names(.fa.mcols))){
          S4Vectors::mcols(.feat.annotation)$blocks = split(IRanges::shift(.fa.ranges, -.fa.starts+1), as.factor(1:length(.feat.annotation)))
          .fa.mcols = S4Vectors::mcols(.feat.annotation)
        }
        .fa.has.itemRgb = 'itemRgb' %in% colnames(.fa.mcols)
        .include.utrs = ('thick' %in% names(.fa.mcols)) #@ --> ORF plotting if available 2023-06-05
        if (length(.feat.annotation) > 0){
          for (.pack.line in 1:length(.packing[[.feat.name]])){
            for (.annot.line in .packing[[.feat.name]][[.pack.line]]){
              .overall.annot.range = .fa.ranges[.annot.line]
              .y.vals = sort(ifelse(feature_names_above[[.strand]][.annotation], -1, 1)*c(.annot.steps*((-.pack.line+0.5)-0.25), .annot.steps*((-.pack.line+0.5)+0.25)))
              .arrow.scaling = abs(diff(.y.vals))/.y.scaling  
              .y.center = mean(.y.vals)
              if (is.null(annot_cols[[.annotation]])){
                if (.fa.has.itemRgb){
                  .annot.col = .fa.mcols$itemRgb[.annot.line]
                }else{
                  .annot.col = 'black'
                }
              }else{
                if (class(annot_cols[[.annotation]])=='list'){
                  .annot.col = annot_cols[[.annotation]][[.fa.mcols$score[.annot.line]]]
                }else{
                  .annot.col = annot_cols[[.annotation]]
                }
              }
              .exon.ranges = IRanges::shift(.fa.mcols$blocks[[.annot.line]], .fa.starts[.annot.line]-1)
              if (annotation_packing[.annotation] == 'expanded' | .is.squished){
                if (length(.exon.ranges)==1){
                  if (IRanges::width(.exon.ranges)==IRanges::width(.overall.annot.range)){
                    if (.fa.mcols$intron.from.start[.annot.line] & .fa.mcols$intron.from.end[.annot.line]){
                      .exon.ranges = IRanges::IRanges()
                    }
                  }
                }
                .introns.to.plot = (length(.exon.ranges) > 1 | .fa.mcols$intron.from.start[.annot.line] | .fa.mcols$intron.from.end[.annot.line]) #@ 2023-06-20
              }else{
                .introns.to.plot = length(.exon.ranges) > 1  #@ 2023-06-20
              }
              if (length(.exon.ranges) > 0){
                if (bin_size > 1){
                  .oar.start = IRanges::start(.overall.annot.range)
                  .oar.end = IRanges::end(.overall.annot.range)
                  .exon.starts = sapply( IRanges::start(.exon.ranges), function(s) ifelse(s==.oar.start, s, .coords[which(abs(s - .coords)==min(abs(s - .coords)))] - bin_size/2 ))
                  .exon.ends = sapply( IRanges::end(.exon.ranges), function(e) ifelse(e==.oar.end, e, .coords[which(abs(e - .coords)==min(abs(e - .coords)))] + bin_size/2 ))
                } else {
                  .exon.starts = IRanges::start(.exon.ranges)
                  .exon.ends = IRanges::end(.exon.ranges)
                }
                # Min-width expansion: ensure single-position / sub-pixel features
                # render as a visible bar.  Applies for any bin_size.
                if (any((.exon.ends - .exon.starts)/plot_width < 0.001)){
                  .n.exons = which((.exon.ends - .exon.starts)/plot_width < 0.001)
                  for (.n.exon in .n.exons){
                    while((.exon.ends[.n.exon ]-.exon.starts[.n.exon])/plot_width < 0.001){   ### make sure that annotation can be seen
                      .add.x = (as.integer(0.001*plot_width+.exon.starts[.n.exon]-.exon.ends[.n.exon]) + 1)/2
                      .exon.starts[.n.exon] = .exon.starts[.n.exon] - .add.x
                      .exon.ends[.n.exon] = .exon.ends[.n.exon] + .add.x
                    }
                  }
                }
                .exon.ranges = IRanges::IRanges(.exon.starts, .exon.ends)
                if (.include.introns & .introns.to.plot){
                  .intron.ranges = .overall.annot.range
                  IRanges::start(.intron.ranges) = ifelse(.fa.mcols$intron.from.start[.annot.line], .pr.start, IRanges::start(.intron.ranges))
                  IRanges::end(.intron.ranges) = ifelse(.fa.mcols$intron.from.end[.annot.line], .pr.end, IRanges::end(.intron.ranges))
                  .intron.ranges = IRanges::setdiff(.intron.ranges, .exon.ranges)
                  if (length(.intron.ranges) > 0){
                    .global.intron.start = min(IRanges::start(.intron.ranges)) - 1
                    .global.intron.end = max(IRanges::end(.intron.ranges)) + 1
                    segments(x0=.global.intron.start, x1=.global.intron.end, y0=.y.center, lwd=.line.width/ifelse(.is.squished, 4, 2), col=.annot.col, lend=1)
                    .n.arrows = sapply(IRanges::width(.intron.ranges), function(i) ifelse(round(i/(4*.length.arrows)) > 1, 1, 0))
                    if (stranded & any(.n.arrows==1)){
                      .i.arrows = which(.n.arrows==1)
                      for (.i.arrow in .i.arrows){
                        .pos.arrow = mean(c(IRanges::start(.intron.ranges)[.i.arrow], IRanges::end(.intron.ranges)[.i.arrow]))
                        .arrow.x = c(.pos.arrow+(.direction.arrows*.arrow.scaling), .pos.arrow-(.direction.arrows*.arrow.scaling), .pos.arrow+(.direction.arrows*.arrow.scaling))
                        .arrow.y = c(.y.vals[2], .y.center, .y.vals[1])
                        lines(.arrow.x, .arrow.y, col=.annot.col, lwd=.line.width/ifelse(.is.squished, 6, 4), lend=1)
                      }
                    }
                  }
                }else{
                  .intron.ranges = IRanges::IRanges()
                }
                #@ --> ORF plotting if available 2023-06-05
                if (.include.utrs){
                  .y.vals.thin = sort(ifelse(feature_names_above[[.strand]][.annotation], -1, 1)*c(.annot.steps*((-.pack.line+0.5)-0.15), .annot.steps*((-.pack.line+0.5)+0.15)))
                  .thick.range = .fa.mcols$thick[.annot.line]
                  if (width(.thick.range) > 2){
                    .thick.range2 = resize(.thick.range, width=width(.thick.range)-2, fix='center')
                  }else{
                    .thick.range2 = IRanges::IRanges()
                  }
                  .thin.ranges = IRanges::setdiff(.exon.ranges, .thick.range2)
                  .exon.ranges = IRanges::setdiff(.thick.range, .intron.ranges)
                  if (length(.thin.ranges) > 0){
                    .thin.starts = IRanges::start(.thin.ranges)
                    .thin.ends = IRanges::end(.thin.ranges)
                    for (.n.thin.exon in 1:length(.thin.ranges)){
                      .thin.exon.start = .thin.starts[.n.thin.exon]
                      .thin.exon.end = .thin.ends[.n.thin.exon]
                      rect(xleft=.thin.exon.start, xright=.thin.exon.end, ybottom=.y.vals.thin[1], ytop=.y.vals.thin[2], col=.annot.col, border=NA)
                      #@ -> add arrows to thin exons 2023-06-21
                      .n.arrows = ifelse(round(diff(c(.thin.exon.start, .thin.exon.end+1))/(8*.length.arrows)) > 0, 1, 0)
                      if (stranded & .n.arrows > 0){
                        .pos.arrow = mean(c(.thin.exon.start, .thin.exon.end))
                        .arrow.x = c(.pos.arrow+(.direction.arrows*.arrow.scaling), .pos.arrow-(.direction.arrows*.arrow.scaling), .pos.arrow+(.direction.arrows*.arrow.scaling))
                        .arrow.y = c(.y.vals.thin[2], .y.center, .y.vals.thin[1])
                        lines(.arrow.x, .arrow.y, col='white', lwd=.line.width/ifelse(.is.squished, 6, 3), lend=2)
                      }
                      #@ <- add arrows to thin exons 2023-06-21
                    }
                  }
                }
                if (length(.exon.ranges) > 0){
                #@ <-- 
                  .exon.starts.vec = IRanges::start(.exon.ranges)
                  .exon.ends.vec = IRanges::end(.exon.ranges)
                  for (.n.exon in 1:length(.exon.ranges)){
                    .exon.start = .exon.starts.vec[.n.exon]
                    .exon.end = .exon.ends.vec[.n.exon]
                    rect(xleft=.exon.start, xright=.exon.end, ybottom=.y.vals[1], ytop=.y.vals[2], col=.annot.col, border=NA)
                    .n.arrows = ifelse(round(diff(c(.exon.start, .exon.end+1))/(8*.length.arrows)) > 0, 1, 0)
                    if (stranded & .n.arrows > 0){
                      .pos.arrow = mean(c(.exon.start, .exon.end))
                      .arrow.x = c(.pos.arrow+(.direction.arrows*.arrow.scaling), .pos.arrow-(.direction.arrows*.arrow.scaling), .pos.arrow+(.direction.arrows*.arrow.scaling))
                      .arrow.y = c(.y.vals[2], .y.center, .y.vals[1])
                      lines(.arrow.x, .arrow.y, col='white', lwd=.line.width/ifelse(.is.squished, 4, 2), lend=2)
                    }
                  }
                }
              }else{
                if (.include.introns & .introns.to.plot){
                  .intron.ranges = .overall.annot.range
                  IRanges::start(.intron.ranges) = ifelse(.fa.mcols$intron.from.start[.annot.line], .pr.start, IRanges::start(.intron.ranges))
                  IRanges::end(.intron.ranges) = ifelse(.fa.mcols$intron.from.end[.annot.line], .pr.end, IRanges::end(.intron.ranges))
                  if (length(.intron.ranges) > 0){
                    .global.intron.start = min(IRanges::start(.intron.ranges)) - 1
                    .global.intron.end = max(IRanges::end(.intron.ranges)) + 1
                    segments(x0=.global.intron.start, x1=.global.intron.end, y0=.y.center, lwd=.line.width/ifelse(.is.squished, 4, 2), col=.annot.col, lend=1)
                    .n.arrows = sapply(IRanges::width(.intron.ranges), function(i) ifelse(round(i/(4*.length.arrows)) > 1, 1, 0))
                    if (stranded & any(.n.arrows==1)){
                      .i.arrows = which(.n.arrows==1)
                      for (.i.arrow in .i.arrows){
                        .pos.arrow = mean(c(IRanges::start(.intron.ranges)[.i.arrow], IRanges::end(.intron.ranges)[.i.arrow]))
                        .arrow.x = c(.pos.arrow+(.direction.arrows*.arrow.scaling), .pos.arrow-(.direction.arrows*.arrow.scaling), .pos.arrow+(.direction.arrows*.arrow.scaling))
                        .arrow.y = c(.y.vals[2], .y.center, .y.vals[1])
                        lines(.arrow.x, .arrow.y, col=.annot.col, lwd=.line.width/ifelse(.is.squished, 6, 4), lend=1)
                      }
                    }
                  }
                }
              }
              if (!(.include.introns & .introns.to.plot)){ #@ 2023-06-20 !.include.introns 
                .on.from.start = .fa.mcols$on.from.start[.annot.line]
                .on.from.end = .fa.mcols$on.from.end[.annot.line]
                if (.on.from.start){ #@ 2023-06-20
                  .pos.arrow = .pr.start
                  if (sign(.direction.arrows)==-1){
                    triangle_xs = c(.pos.arrow, .pos.arrow, .pos.arrow-2*(.direction.arrows*.arrow.scaling))
                  }else{
                    triangle_xs = c(.pos.arrow+2*(.direction.arrows*.arrow.scaling), .pos.arrow+2*(.direction.arrows*.arrow.scaling), .pos.arrow)
                  }
                  polygon(x=triangle_xs, y=c(rev(.y.vals), .y.center), col ='yellow', border=NA)
                }
                if (.on.from.end){ #@ 2023-06-20
                  .pos.arrow = .pr.end
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
        # (Overridden small gene transcripts are drawn by the normal expanded loop
        # above --  they were moved in the packing, not drawn separately.)
      }
      # --- Inline gene name drawing for expanded/squished modes ---
      # Uses pre-computed placements from ComputeInlineNamePlacements().
      # When brackets are ON for this annotation, the inline pipeline was skipped
      # --  names live in the bracket section drawn further below.
      if (isTRUE(incl_feature_names[.annotation]) && .include.introns &&
          !isTRUE(incl_feature_brackets[.annotation])) {
        .name.placements = annot_info[[.strand]][[.annotation]][['inline_name_placements']]
        if (!is.null(.name.placements)) {
          for (.feat.name in names(.name.placements)) {
            for (.gene.name in names(.name.placements[[.feat.name]])) {
              .np = .name.placements[[.feat.name]][[.gene.name]]
              .display.name = strsplit(.gene.name, '#DUPNAME#')[[1]][1]
              .name.row = .np$row
              .name.y.vals = sort(ifelse(feature_names_above[[.strand]][.annotation], -1, 1)*c(.annot.steps*((-.name.row+0.5)-0.25), .annot.steps*((-.name.row+0.5)+0.25)))
              .name.y = mean(.name.y.vals)
              text(x = .np$x, y = .name.y, labels = .display.name,
                   adj = .np$adj, col = font_colors['features'],
                   cex = scaling_factor * feature_font_size / 12 * ifelse(.is.squished, 0.8, 1),
                   family = font_family, font = 3)
            }
          }
        }
      }
      # --- Feature names for collapsed/collapsed2 modes ---
      if (isTRUE(incl_feature_names[.annotation]) && !.include.introns){
        # Check if collapsed/collapsed2 inline placements are available
        .c2.placements = annot_info[[.strand]][[.annotation]][['c2_inline_name_placements']]
        if (annotation_packing[.annotation] %in% c('collapsed', 'collapsed2')) {
          # collapsed/collapsed2 use the c2 ILP inline placements; NULL means no
          # features on this strand. When brackets are ON, names live in the
          # bracket section drawn further below --  skip the inline draw here.
          if (!is.null(.c2.placements) && !isTRUE(incl_feature_brackets[.annotation])) {
            for (.feat.name in names(.c2.placements)) {
              for (.gene.name in names(.c2.placements[[.feat.name]])) {
                .np = .c2.placements[[.feat.name]][[.gene.name]]
                .display.name = strsplit(.gene.name, '#DUPNAME#')[[1]][1]
                .name.row = .np$row
                .name.y.vals = sort(ifelse(feature_names_above[[.strand]][.annotation], -1, 1)*c(.annot.steps*((-.name.row+0.5)-0.25), .annot.steps*((-.name.row+0.5)+0.25)))
                .name.y = mean(.name.y.vals)
                text(x = .np$x, y = .name.y, labels = .display.name,
                     adj = .np$adj, col = font_colors['features'],
                     cex = scaling_factor * feature_font_size / 12 * ifelse(.is.squished, 0.8, 1),
                     family = font_family, font = 3)
              }
            }
          }
        }
      }
      # --- Bracket section drawing (brackets ON for expanded/squished only) ---
      # Renders a bracket per gene at its packing2_display row.  Names that fit
      # inside the bracket are drawn in a gap in the bracket line; the rest are
      # drawn at their c2 placement (above/below/inline of the bracket).
      # Brackets are not supported in collapsed / collapsed2 modes --  warn and skip.
      if (isTRUE(incl_feature_brackets[.annotation]) &&
          isTRUE(incl_feature_names[.annotation]) &&
          annotation_packing[.annotation] %in% c('collapsed', 'collapsed2')) {
        warning("[brackets] '", .annotation, "/", .strand,
                "': feature brackets are not supported in '",
                annotation_packing[.annotation],
                "' mode and have been skipped.", call. = FALSE)
      }
      if (isTRUE(incl_feature_brackets[.annotation]) &&
          isTRUE(incl_feature_names[.annotation]) &&
          annotation_packing[.annotation] %in% c('expanded', 'squished')) {
        .bracket.inside = annot_info[[.strand]][[.annotation]][['c2_bracket_inside_names']]
        .c2.placements.bk = annot_info[[.strand]][[.annotation]][['c2_inline_name_placements']]
        .pk2.disp = annot_info[[.strand]][[.annotation]][['packing2_display']]
        # Mode-specific row offset: in expanded/squished, the bracket section sits
        # below the transcript area, with `c2_inline_name_above_rows` rows of gap
        # between the last transcript and the first bracket row so c2 above-row
        # fallback names land in that gap.  In collapsed2 the brackets share
        # rows with the c2 bars (drawn earlier in the loop), so no offset.
        # NB: .packing is keyed by feature name; the visual transcript-row count
        # is max(sapply(.packing, length)), not length(.packing).
        if (.include.introns && length(.packing) > 0) {
          .above.bk = if (!is.null(annot_info[[.strand]][[.annotation]][['c2_inline_name_above_rows']]))
            annot_info[[.strand]][[.annotation]][['c2_inline_name_above_rows']] else 0L
          .trn.rows = max(sapply(.packing, length))
          .row.offset = .trn.rows + .above.bk
        } else {
          .row.offset = 0L
        }
        .bk.y.center = function(.row) {
          .y.vals = sort(ifelse(feature_names_above[[.strand]][.annotation], -1, 1) *
                         c(.annot.steps * ((-(.row.offset + .row) + 0.5) - 0.25),
                           .annot.steps * ((-(.row.offset + .row) + 0.5) + 0.25)))
          list(y.center = mean(.y.vals), y.vals = .y.vals)
        }
        .bk.arrow.scaling = .annot.steps * 0.5 / .y.scaling
        # 1) Bracket lines + arrows.
        #    expanded/squished: bracket section sits below the transcript area;
        #      iterate packing2_display + collapsed2 sub-features.
        #    collapsed: ONE bracket per super-locus on row 1, span derived from
        #      [['collapsed']] (post-ConvertCollapsedFormat GRangesList).
        #    Brackets are only supported in expanded/squished --  this entire
        #    block runs only when annotation_packing is one of those.
        if (.include.introns && !is.null(.pk2.disp)) {
          .c2.grl.bk = annot_info[[.strand]][[.annotation]][['collapsed2']]
          for (.fn.bk in names(.pk2.disp)) {
            .pk2.rows.bk = .pk2.disp[[.fn.bk]]
            .feat.gr = .c2.grl.bk[[.fn.bk]]
            if (is.null(.feat.gr) || length(.feat.gr) == 0) next
            .feat.gnames = S4Vectors::mcols(.feat.gr)$name
            .feat.starts = IRanges::start(.feat.gr)
            .feat.ends = IRanges::end(.feat.gr)
            .feat.on.start = S4Vectors::mcols(.feat.gr)$on.from.start
            .feat.on.end = S4Vectors::mcols(.feat.gr)$on.from.end
            for (.row.idx.bk in seq_along(.pk2.rows.bk)) {
              .row.gene.idxs = .pk2.rows.bk[[.row.idx.bk]]
              if (length(.row.gene.idxs) == 0) next
              .yp = .bk.y.center(.row.idx.bk)
              for (.gi.idx in .row.gene.idxs) {
                if (.gi.idx < 1 || .gi.idx > length(.feat.gr)) next
                .gn.bk = .feat.gnames[.gi.idx]
                if (is.na(.gn.bk)) next
                # Aggregate across all c2 entries with this gene name (in this
                # feature group) so a gene split into chunks gets one bracket
                # spanning min(start)..max(end).
                .gi.same = which(.feat.gnames == .gn.bk)
                .gs.bk = min(.feat.starts[.gi.same])
                .ge.bk = max(.feat.ends[.gi.same])
                .on.start.bk = any(.feat.on.start[.gi.same])
                .on.end.bk   = any(.feat.on.end[.gi.same])
                .inside = if (!is.null(.bracket.inside) && !is.null(.bracket.inside[[.fn.bk]]))
                  .bracket.inside[[.fn.bk]][[.gn.bk]] else NULL
                if (!is.null(.inside)) {
                  segments(x0=.gs.bk, x1=.inside$gap.xs, y0=.yp$y.center, lwd=.line.width/4, col='gray30', lend=1)
                  segments(x0=.inside$gap.xe, x1=.ge.bk, y0=.yp$y.center, lwd=.line.width/4, col='gray30', lend=1)
                } else {
                  segments(x0=.gs.bk, x1=.ge.bk, y0=.yp$y.center, lwd=.line.width/4, col='gray30', lend=1)
                }
                # Vertical ticks mark non-clipped bracket ends --  the default
                # bracket end style.  V-shaped arrows are kept as an opt-in
                # fallback (toggle .use.bracket.arrows = TRUE).  When enabled,
                # arrows are drawn on top of the ticks only when the bracket is
                # wide enough; the threshold scales with how many ends actually
                # need an arrow (a clipped end gets neither tick nor arrow).
                .use.bracket.arrows = FALSE
                .arrow.ext = 2 * .length.arrows * .bk.arrow.scaling
                .draw.start = !.on.start.bk
                .draw.end   = !.on.end.bk
                .n.arrows = as.integer(.draw.start) + as.integer(.draw.end)
                .min.width = if (.n.arrows == 2L) 2.2 * .arrow.ext
                             else if (.n.arrows == 1L) 1.1 * .arrow.ext
                             else 0
                .draw.arrows = .use.bracket.arrows && (.ge.bk - .gs.bk) >= .min.width
                if (.draw.start) {
                  segments(x0=.gs.bk, x1=.gs.bk, y0=.yp$y.vals[1], y1=.yp$y.vals[2], col='gray30', lwd=.line.width/4, lend=1)
                  if (.draw.arrows) {
                    segments(x0=.gs.bk, y0=.yp$y.center, x1=.gs.bk - ifelse(.strand=='-', -1, 1)*2*(.direction.arrows*.bk.arrow.scaling), y1=.yp$y.vals[2], col='gray30', lwd=.line.width/4, lend=1)
                    segments(x0=.gs.bk, y0=.yp$y.center, x1=.gs.bk - ifelse(.strand=='-', -1, 1)*2*(.direction.arrows*.bk.arrow.scaling), y1=.yp$y.vals[1], col='gray30', lwd=.line.width/4, lend=1)
                  }
                }
                if (.draw.end) {
                  segments(x0=.ge.bk, x1=.ge.bk, y0=.yp$y.vals[1], y1=.yp$y.vals[2], col='gray30', lwd=.line.width/4, lend=1)
                  if (.draw.arrows) {
                    segments(x0=.ge.bk, y0=.yp$y.center, x1=.ge.bk + ifelse(.strand=='-', -1, 1)*2*(.direction.arrows*.bk.arrow.scaling), y1=.yp$y.vals[2], col='gray30', lwd=.line.width/4, lend=1)
                    segments(x0=.ge.bk, y0=.yp$y.center, x1=.ge.bk + ifelse(.strand=='-', -1, 1)*2*(.direction.arrows*.bk.arrow.scaling), y1=.yp$y.vals[1], col='gray30', lwd=.line.width/4, lend=1)
                  }
                }
              }
            }
          }
        }
        # 2) Inside-bracket names (any mode).
        if (!is.null(.bracket.inside)) {
          for (.fn.bk in names(.bracket.inside)) {
            for (.gn.bk in names(.bracket.inside[[.fn.bk]])) {
              .inside = .bracket.inside[[.fn.bk]][[.gn.bk]]
              .display.name = strsplit(.gn.bk, '#DUPNAME#')[[1]][1]
              .yp = .bk.y.center(.inside$row)
              text(x = .inside$x.center, y = .yp$y.center, labels = .display.name,
                   adj = 0.5, col = font_colors['features'],
                   cex = scaling_factor * feature_font_size / 12 * ifelse(.is.squished, 0.8, 1),
                   family = font_family, font = 3)
            }
          }
        }
        # 3) Fallback c2 placements (above / below / inline of the bracket).
        if (!is.null(.c2.placements.bk)) {
          for (.fn.bk in names(.c2.placements.bk)) {
            for (.gn.bk in names(.c2.placements.bk[[.fn.bk]])) {
              .np = .c2.placements.bk[[.fn.bk]][[.gn.bk]]
              .display.name = strsplit(.gn.bk, '#DUPNAME#')[[1]][1]
              .yp = .bk.y.center(.np$row)
              .dbg("[bk-fallback] '", .gn.bk, "' c2.row=", .np$row,
                      " adj=", .np$adj, " x=", round(.np$x),
                      " abs.row=", .row.offset + .np$row,
                      " y=", round(.yp$y.center, 3))
              text(x = .np$x, y = .yp$y.center, labels = .display.name,
                   adj = .np$adj, col = font_colors['features'],
                   cex = scaling_factor * feature_font_size / 12 * ifelse(.is.squished, 0.8, 1),
                   family = font_family, font = 3)
            }
          }
        }
      }
      if (incl_feature_shadings[.annotation] &&
          annotation_packing[.annotation] %in% c('collapsed', 'collapsed2')) {
        warning("[shading] '", .annotation, "/", .strand,
                "': locus background shading is not supported in '",
                annotation_packing[.annotation],
                "' mode and has been skipped.", call. = FALSE)
      }
      if (incl_feature_shadings[.annotation] &&
          annotation_packing[.annotation] %in% c('expanded', 'squished')){
        .feature.shading.colors = c(adjustcolor(feature_shading_colors[1], alpha.f=feature_shading_alpha), adjustcolor(feature_shading_colors[2], alpha.f=feature_shading_alpha))
        .feat.shading.gr = annot_info[[.strand]][[.annotation]][['collapsed2']]
        # bp/char for the current font, used to compute name x ranges
        .sh.tracks.width.cm = plot_width_parameters[['tracks.width.cm']]
        .sh.bases.per.cm = (plot_end - plot_start + 1) / .sh.tracks.width.cm
        .sh.bp.per.char = feature_font_size * std_letter_width * .sh.bases.per.cm
        .sh.inline    = annot_info[[.strand]][[.annotation]][['inline_name_placements']]
        .sh.c2inline  = annot_info[[.strand]][[.annotation]][['c2_inline_name_placements']]
        .sh.bk.inside = annot_info[[.strand]][[.annotation]][['c2_bracket_inside_names']]
        # Pre-compute brackets-on offset (for absolute row numbers in the
        # bracket section under expanded/squished + brackets ON).
        .sh.brackets.below = isTRUE(incl_feature_brackets[.annotation]) &&
          (annotation_packing[.annotation] == 'expanded' || annotation_packing[.annotation] == 'squished')
        .sh.row.offset = if (.sh.brackets.below) {
          .sh.above.bk = if (!is.null(annot_info[[.strand]][[.annotation]][['c2_inline_name_above_rows']]))
            annot_info[[.strand]][[.annotation]][['c2_inline_name_above_rows']] else 0L
          .sh.trn.rows = if (length(.packing) > 0) max(sapply(.packing, length)) else 0L
          .sh.trn.rows + .sh.above.bk
        } else 0L
        # Helper: extend [xs, xe] to include the rendered name's x range.
        .sh.extend.x = function(.xs, .xe, .pl, .nw) {
          if (is.null(.pl)) return(c(.xs, .xe))
          .xl = .pl$x - .pl$adj * .nw
          .xr.name = .xl + .nw
          c(min(.xs, .xl), max(.xe, .xr.name))
        }
        # Helper: rows used by a gene's transcripts (for non-collapsed modes).
        # Uses the LOCAL `.packing` variable, which has small_gene_row_overrides
        # already applied by the transcript drawing block above --  so small
        # enclosed genes resolve to the row where they're actually drawn.
        .sh.gene.rows = function(.feat.key, .gene) {
          if (annotation_packing[.annotation] == 'collapsed') return(1L)
          if (annotation_packing[.annotation] == 'collapsed2') {
            .feat.gr.k = annot_info[[.strand]][[.annotation]][['collapsed2']][[.feat.key]]
            if (is.null(.feat.gr.k)) return(integer(0))
            .gi = which(S4Vectors::mcols(.feat.gr.k)$name == .gene)
            if (length(.gi) == 0) return(integer(0))
            .pk2 = .packing[[.feat.key]]
            if (is.null(.pk2)) return(integer(0))
            .out = integer(0)
            for (.r in seq_along(.pk2)) if (any(.pk2[[.r]] %in% .gi)) .out = c(.out, .r)
            return(.out)
          }
          # expanded / squished
          .tim = annot_info[[.strand]][[.annotation]][['gene_trn_indices']][[.feat.key]]
          .ti = if (!is.null(.tim) && .gene %in% names(.tim)) .tim[[.gene]] else integer(0)
          .pk = .packing[[.feat.key]]
          if (is.null(.pk) || length(.ti) == 0) return(integer(0))
          .out = integer(0)
          for (.r in seq_along(.pk)) if (any(.pk[[.r]] %in% .ti)) .out = c(.out, .r)
          .out
        }
        # Helper: name placement record + its absolute display row.
        .sh.name.row = function(.feat.key, .gene) {
          if (!is.null(.sh.inline) && !is.null(.sh.inline[[.feat.key]]) && !is.null(.sh.inline[[.feat.key]][[.gene]]))
            return(list(pl = .sh.inline[[.feat.key]][[.gene]], abs.row = .sh.inline[[.feat.key]][[.gene]]$row))
          if (!is.null(.sh.c2inline) && !is.null(.sh.c2inline[[.feat.key]]) && !is.null(.sh.c2inline[[.feat.key]][[.gene]])) {
            .p = .sh.c2inline[[.feat.key]][[.gene]]
            return(list(pl = .p, abs.row = if (.sh.brackets.below) .sh.row.offset + .p$row else .p$row))
          }
          if (!is.null(.sh.bk.inside) && !is.null(.sh.bk.inside[[.feat.key]]) && !is.null(.sh.bk.inside[[.feat.key]][[.gene]])) {
            .ib = .sh.bk.inside[[.feat.key]][[.gene]]
            return(list(pl = list(x = .ib$x.center, adj = 0.5),
                        abs.row = if (.sh.brackets.below) .sh.row.offset + .ib$row else .ib$row))
          }
          NULL
        }
        .n.feat = 0
        for (.feature.name in names(.feat.shading.gr)){
          for (.n.sub.feature in 1:length(.feat.shading.gr[[.feature.name]])){
            .n.feat = .n.feat+1
            .feature.gr = .feat.shading.gr[[.feature.name]][.n.sub.feature]
            .feat.start = IRanges::start(.feature.gr)
            .feat.end = IRanges::end(.feature.gr)
            .sh.genes = if (annotation_packing[.annotation] == 'collapsed') {
              .feature.name
            } else {
              .nm = S4Vectors::mcols(.feature.gr)$name
              if (is.null(.nm)) .feature.name else as.character(.nm)
            }
            # Collect rows used by the locus (transcripts/bars only --  name rows
            # are intentionally NOT added to the y range so all small genes
            # render with consistent rect heights regardless of where their
            # name lands). The name's x range is still folded into the rect so
            # names extending past the gene boundary sit inside the band.
            .rows.abs = integer(0)
            for (.g in .sh.genes) {
              .gr.rows = .sh.gene.rows(.feature.name, .g)
              if (length(.gr.rows) > 0) .rows.abs = c(.rows.abs, .gr.rows)
              .nrec = .sh.name.row(.feature.name, .g)
              if (!is.null(.nrec)) {
                .nw = nchar(strsplit(.g, '#DUPNAME#')[[1]][1]) * .sh.bp.per.char
                .ext = .sh.extend.x(.feat.start, .feat.end, .nrec$pl, .nw)
                .feat.start = .ext[1]; .feat.end = .ext[2]
              }
            }
            # Determine y range.  Default (unresolved row info): full panel.
            # Otherwise: just the rows actually used by this locus (collapsed
            # mode resolves to row 1 via .sh.gene.rows).
            if (length(.rows.abs) == 0) {
              .y.limits = sort(ifelse(feature_names_above[[.strand]][.annotation], 1, -1)*c(.annot.steps*0.25, .y.span))
            } else {
              .min.r = min(.rows.abs); .max.r = max(.rows.abs)
              # Same y-sign convention as the transcript drawing at line 951:
              # multiplier = -1 when names_above (rows mirror to positive y),
              # +1 otherwise (rows go negative below the strand axis).
              # The rect almost fills its row(s) (offsets +/-0.45 instead of
              # +/-0.25) so that single-gene rows are taller than their text.
              .sign = ifelse(feature_names_above[[.strand]][.annotation], -1, 1)
              .y1 = .sign * .annot.steps * (-(.min.r) + 0.95)
              .y2 = .sign * .annot.steps * (-(.max.r) + 0.05)
              .y.limits = sort(c(.y1, .y2))
            }
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
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA 
#'
#' @param feature 
#' @param plotted_region 
#' @param plotted_strand 
#' @param both_strands 
#' @param plotting_segment 
#' @param basic_plot_parameters 
#' @param neg_vals_neg_strand 
#' @param plot_width_parameters 
#' @param plot_vertical_parameters 
#' @param annot_info 
#' @param panel_info 
#' @param panels_list 
#' @param panel_separators 
#' @param separators_lwds 
#' @param separators_colors 
#' @param incl_first_panel 
#' @param print_one_line_sample_names 
#' @param replicate_names 
#' @param header 
#' @param header_font_sizes 
#' @param scaling_factor 
#' @param full_width_cm 
#' @param genomic_scale_on_top 
#' @param genomic_scale_font_size 
#' @param reverse_strand_direction 
#' @param bin_stats 
#' @param dummy_plot 
#' @param tracks 
#' @param strands_alpha 
#' @param intermingled_color 
#' @param unstranded_beds 
#' @param annotation_packing 
#' @param annotation_panel_font_size 
#' @param incl_feature_names 
#' @param feature_font_size
#' @param feature_names_above
#' @param incl_feature_brackets
#' @param incl_feature_shadings 
#' @param feature_shading_colors 
#' @param feature_shading_alpha 
#' @param annot_cols 
#' @param group_autoscale 
#' @param incl_track_scales 
#' @param scientific_scale 
#' @param scale_font_size 
#' @param force_scale_list 
#' @param log2transformed 
#' @param colors 
#' @param alternating_background 
#' @param bgr_colors 
#' @param bgr_alpha 
#' @param font_colors 
#' @param letter_widths 
#' @param letter_heights 
#' @param enhance_signals 
#' @param first_plot 
#' @param verbosity 
#'
#' @return placeholder
#' 
#' @import IRanges
#' @importFrom GenomeInfoDb seqnames
#'
#' @examples
#' NULL
#' 
PlotSegment = function(feature, plotted_region, plotted_strand, both_strands, plotting_segment, basic_plot_parameters, neg_vals_neg_strand, plot_width_parameters, plot_vertical_parameters, annot_info, panel_info, panels_list, panel_separators, separators_lwds, separators_colors, incl_first_panel, print_one_line_sample_names, replicate_names, header, header_font_sizes, scaling_factor, full_width_cm, genomic_scale_on_top, genomic_scale_font_size, reverse_strand_direction, bin_stats, dummy_plot, tracks, strands_alpha, intermingled_color,  unstranded_beds, annotation_packing, annotation_panel_font_size, incl_feature_names, feature_font_size, feature_names_above, incl_feature_brackets, incl_feature_shadings, feature_shading_colors, feature_shading_alpha, annot_cols, group_autoscale, incl_track_scales, scientific_scale, scale_font_size, force_scale_list, log2transformed, colors, alternating_background, bgr_colors, bgr_alpha, font_colors, letter_widths, letter_heights, enhance_signals, first_plot, verbosity){
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
  if (length(.segment.top) == 0L) {
    # SegmentTop couldn't resolve the segment to a position in windows_height
    # (which() returned integer(0)). This is expected when the segment iteration
    # loop hits a segment whose corresponding entry was dropped by the
    # trailing-spacer cleanup in AlignBasicPlotParameters -- e.g. a trailing
    # empty-spacer that the package strips from the rendered layout but that
    # the plotting_segment_order vector still references. Skip the
    # "plotting ..." header cat so we don't propagate "argument is of length
    # zero" from the length-0 == 1 comparison below; downstream PlotSpacer /
    # PlotAnnotation will short-circuit on the same lookup. Diagnostic gated
    # on seqNdisplayR.debug to avoid noisy Shiny toasts in normal use.
    if (isTRUE(getOption("seqNdisplayR.debug", FALSE))) {
      message("[seqNdisplayR debug] SegmentTop returned integer(0) for ",
              "plotting_segment='", plotting_segment,
              "' (.strand='", .strand, "', plotted_strand='", plotted_strand, "').")
    }
  } else if (.segment.top==1 & first_plot & verbosity > 0){
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
    PlotAnnotation(.stranded.annot.info, .stranded, annot_cols, annotation_packing, .plotted.region, plotted_strand, .strand, basic_plot_parameters, .plot.start, .plot.end, .plot.width, .bin.size, reverse_strand_direction, incl_feature_names, feature_names_above, incl_feature_brackets, incl_feature_shadings, feature_shading_colors, feature_shading_alpha, plot_width_parameters, plot_vertical_parameters, .windows.height, feature_font_size, annotation_panel_font_size, annot_panel_dist, .coords.tracks, font_colors, .font.family, first_plot, scaling_factor, verbosity)
    if (plotted_strand=='+-'){
      .annots.indices = unlist(lapply(names(.stranded.annot.info[[.strand]]), grep, names(basic_plot_parameters[['+-']][['windows.height']]), fixed=TRUE))
      .space.index = setdiff(seq(min(.annots.indices), max(.annots.indices), 1), .annots.indices)
      if (length(.space.index) > 0){
        spacer_segment = names(basic_plot_parameters[['+-']][['windows.height']])[rev(.space.index)[1]] 
        if (grepl('spacer', spacer_segment, fixed=TRUE)){
          PlotSpacer(.windows.height, spacer_segment, .coords.tracks[2], plotted_strand, neg_vals_neg_strand, panel_separators, separators_lwds, separators_colors, scaling_factor)
        }
      }
      PlotAnnotation(.stranded.annot.info, .stranded, annot_cols, annotation_packing, .plotted.region, plotted_strand, '-', basic_plot_parameters, .plot.start, .plot.end, .plot.width, .bin.size, reverse_strand_direction, incl_feature_names, feature_names_above, incl_feature_brackets, incl_feature_shadings, feature_shading_colors, feature_shading_alpha, plot_width_parameters, plot_vertical_parameters, .windows.height, feature_font_size, annotation_panel_font_size, annot_panel_dist, .coords.tracks, font_colors, .font.family, first_plot, scaling_factor, verbosity)
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
    PlotAnnotation(.unstranded.annot.info, .stranded, annot_cols, annotation_packing, .plotted.region, plotted_strand, .strand, basic_plot_parameters, .plot.start, .plot.end, .plot.width, .bin.size, reverse_strand_direction, incl_feature_names, feature_names_above, incl_feature_brackets, incl_feature_shadings, feature_shading_colors, feature_shading_alpha, plot_width_parameters, plot_vertical_parameters, .windows.height, feature_font_size, annotation_panel_font_size, annot_panel_dist, .coords.tracks, font_colors, .font.family, first_plot, scaling_factor, verbosity)
  }else{
    if (!dummy_plot){
      .sample.subset = names(tracks[[.strand]][[plotting_segment]])
    }else{
      .sample.subset = tracks[[.strand]][[plotting_segment]]
    }
    .n.track = which(names(tracks[[.strand]])==plotting_segment)
    .vertical.slots = grep(paste0("^", plotting_segment), names(.windows.height))
    if (length(.vertical.slots)==0){ 
      .vertical.slots = grep(paste0(plotting_segment), names(.windows.height), fixed=TRUE)
    } 
    .first.plot = first_plot
    if (alternating_background){
      .bgr.colors = c(adjustcolor(bgr_colors[1], alpha.f=bgr_alpha), adjustcolor(bgr_colors[2], alpha.f=bgr_alpha))
      .new.flag <- if (length(.segment.top) == 0L) TRUE else !(.segment.top == 1 & .first.plot)
      par(fig=c(.coords.tracks[1],.coords.tracks[2],.windows.height[max(.vertical.slots)],.windows.height[.segment.top]), mai=scaling_factor*c(0, 0, 0, 0), new=.new.flag)
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


