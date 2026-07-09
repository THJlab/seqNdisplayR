# Layout computation --  widths, heights, panels, segments

#' Plot Widths
#'
#' @description Internal function: 
#' Make sense of the supplied width arguments
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param panels_max_width_cm maximum width (in cm) of the combined panels plotted to the left of the data tracks ('auto'/'automatic' or positive numeric value) - 'auto' is only accepted if full_width_cm=NULL
#' @param scale_panel_width_cm width (in cm) of the scale panel plotted to the left of the data tracks and to the right of all other panels ('auto'/'automatic' or positive numeric value) - 'auto' is only accepted if full_width_cm=NULL
#' @param margin_width_cm width (in cm) of the margins on each side of the data tracks (NULL or positive numeric value - will auto-default with warning message)
#' @param track_width_cm width (in cm) of the data tracks (NULL or positive numeric value)
#' @param full_width_cm width (in cm) of the full plotted area (NULL or positive numeric value)
#' @param incl_track_scales if TRUE tracks scales will be included (left of the data tracks and to the right of all other panels)
#' @param verbosity indicated by an integer 0 to 3 referring to the levels 'off', 'no warnings', 'normal', and 'detailed'
#' @param interface 'R' or 'shiny'
#'
#' @note the following expression needs to be true: full_width_cm = panels_max_width_cm + scale_panel_width_cm + track_width_cm + 2 * margin_width_cm
#' consider setting one of the arguments "full_width_cm" or "track_width_cm" to NULL 
#'
#' @return a named vector with values for 'panels.max.width.cm', 'scale.panel.width.cm', 'margin.width.cm', 'track.width.cm' and 'full.width.cm'
#'
#' @examples
#' plot_widths = PlotWidths(panels_max_width_cm='auto', scale_panel_width_cm='auto', margin_width_cm=0.05, track_width_cm=12, full_width_cm=NULL, incl_track_scales=TRUE, verbosity=3, interface='R')
#' plot_widths = PlotWidths(panels_max_width_cm='auto', scale_panel_width_cm='auto', margin_width_cm=0.05, track_width_cm=12, full_width_cm=15, incl_track_scales=TRUE, verbosity=3, interface='R')
#' plot_widths = PlotWidths(panels_max_width_cm=2, scale_panel_width_cm=0.6, margin_width_cm=0.05, track_width_cm=12, full_width_cm=15, incl_track_scales=TRUE, verbosity=3, interface='R')
#' plot_widths = PlotWidths(panels_max_width_cm=2.3, scale_panel_width_cm=0.6, margin_width_cm=0.05, track_width_cm=12, full_width_cm=15, incl_track_scales=TRUE, verbosity=3, interface='R')
#' plot_widths = PlotWidths(panels_max_width_cm=2, scale_panel_width_cm=0.6, margin_width_cm=0.05, track_width_cm=NULL, full_width_cm=15, incl_track_scales=TRUE, verbosity=3, interface='R')
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


#' Panel Font Size List
#'
#' @description Internal function: 
#' Constructs a panel_font_size_list (a font size provided to each panel)
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param samples 
#' @param panel_font_sizes 
#' @param panel_font_size_list 
#' @param incl_reps 
#' @param replicate_names 
#' @param verbosity 
#' @param interface 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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
        if (incl_reps[.seqtype] & !is.null(replicate_names)){ 
          .panel.font.size.list[[.seqtype]] = c(.panel.font.size.list[[.seqtype]], rev(.panel.font.size.list[[.seqtype]])[1])
        } 
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
            if (incl_reps[.seqtype] & !is.null(replicate_names)){ 
              .panel.font.size.list[[.seqtype]] = c(.panel.font.size.list[[.seqtype]], rev(.panel.font.size.list[[.seqtype]])[1])
            }
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
#' @description Internal function: 
#' Checks/constructs a horizontal_panels_list
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param samples 
#' @param horizontal_panels_list 
#' @param incl_reps 
#' @param replicate_names 
#' @param verbosity 
#' @param interface 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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
            if (incl_reps[.seqtype] & !is.null(replicate_names)){ 
              .horizontal.panels.list[[.seqtype]] = c(.horizontal.panels.list[[.seqtype]], TRUE)
            }
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


#' Handle Forced Scale From Parameters
#'
#' @description Internal function: 
#' Checks/constructs a force_scale_list
#' Handle force_scale which is part of "parameters" argument but needs to passed differently to plot function
#'
#' @keywords internal
#' 
#' @author MS/SLA
#'
#' @param pars 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
HandleForcedScaleFromParameters = function(pars){
  force_scales = lapply(pars, function(para) {
    fc = para$force_scale
    if (is.null(fc)) {
      suppressWarnings(as.numeric(c(NA,NA)))
    }else if (is.character(fc)){ #% 230519
      fc = strsplit(fc, split=',', fixed=TRUE)[[1]]
      suppressWarnings(as.numeric(fc))
    }else{
      fc = fc
    }
  })
  names(force_scales) = names(pars)
  
  force_scale_list = list(
    '+' = unlist(lapply(force_scales, function(x) x[1])),
    '-' = unlist(lapply(force_scales, function(x) if (length(x)==2){x[2]}else{NULL}))
  )
  return(force_scale_list)
}

#%


#' Force Scale List
#'
#' @description Internal function: 
#' Checks/constructs a force_scale_list
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param samples 
#' @param force_scale 
#' @param strands 
#' @param verbosity 
#' @param interface 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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


#' Organized Panels List
#'
#' @description Internal function: 
#' Organize the panels with track-names to be plotted on the left side of the tracks
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param tracks_listed 
#'
#' @return placeholder
#' 
#' @import S4Vectors
#'
#' @examples
#' NULL
#' 
OrganizedPanelsList = function(tracks_listed){
  panels.list = list()
  for (.seqtype in names(tracks_listed)){
    panels.list[[.seqtype]] = list()
    .subsamples = tracks_listed[[.seqtype]]
    .subsample.matrix = do.call('rbind', sapply(.subsamples, function(.sep) strsplit(.sep, split='.', fixed=T)))
    if (ncol(.subsample.matrix) > 1){
      .rles = lapply(apply(.subsample.matrix, 2, Rle), function(x) structure(runLength(x), names=runValue(x)))
      .subsample.matrix = .subsample.matrix[,order(lengths(.rles))]
    }
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
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param plotting_segment_order 
#' @param sample_names 
#' @param header 
#' @param include_genomic_scale 
#' @param genomic_scale_on_top 
#' @param incl_annot 
#' @param horizontal_spacers 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param plotting_segment_order 
#' @param tracks_listed 
#' @param both_strands 
#' @param include_genomic_scale 
#' @param genomic_scale_on_top 
#' @param any_stranded_beds 
#' @param any_unstranded_beds 
#' @param strands_intermingled 
#' @param verbosity 
#' @param interface 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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
    if (any_unstranded_beds & !("unstranded-beds" %in% .plotting.segment.order.plus)){ 
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
      if (any_unstranded_beds & !("unstranded-beds" %in% plotting_segment_order)){ 
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
          .plotting.segment.order[[.strand]] = .plotting.segment.order[[.strand]][setdiff(1:length(.plotting.segment.order[[.strand]]), .spacers)[1]:length(.plotting.segment.order[[.strand]])]
        }
      }
    }else if (.strand == '-'){
      .spacers = grep('-spacer', .plotting.segment.order[[.strand]], fixed=TRUE)
      if (length(.spacers) > 0){
        if (rev(.spacers)[1]==length(.plotting.segment.order[[.strand]])){
          .plotting.segment.order[[.strand]] = .plotting.segment.order[[.strand]][1:rev(setdiff(1:length(.plotting.segment.order[[.strand]]), .spacers))[1]]
        }
      }
    }
  }
  return(.plotting.segment.order)
}


#' Build Scrutinize Plot Segment Order
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param plotting_segment_order 
#' @param plotted_region 
#' @param datasets 
#' @param plotted_samples 
#' @param header 
#' @param include_genomic_scale 
#' @param genomic_scale_on_top 
#' @param incl_annot 
#' @param horizontal_spacers 
#' @param tracks_listed 
#' @param both_strands 
#' @param any_stranded_beds 
#' @param any_unstranded_beds 
#' @param strands_intermingled 
#' @param verbosity 
#' @param interface 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
BuildScrutinizePlotSegmentOrder = function(plotting_segment_order, plotted_region, datasets, plotted_samples, header, include_genomic_scale, genomic_scale_on_top, incl_annot, horizontal_spacers, tracks_listed, both_strands, any_stranded_beds, any_unstranded_beds, strands_intermingled, verbosity, interface){
  .plotting.segment.order = NULL
  if (!is.null(plotting_segment_order)){
    if (is.list(plotting_segment_order) & both_strands & !strands_intermingled){ 
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
      if (is.list(plotting_segment_order)){ 
        plotting_segment_order = plotting_segment_order[['+']]
      } 
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


#' Estimate Plot Heights
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param annot_info 
#' @param incl_feature_names 
#' @param annotation_packing 
#' @param incl_feature_brackets 
#' @param plotting_segment_order 
#' @param tracks_listed 
#' @param track_height_cm 
#' @param full_height_cm 
#' @param stranded_beds 
#' @param plot_vertical_parameters 
#' @param verbosity 
#' @param interface 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
EstimatePlotHeights = function(annot_info, incl_feature_names, annotation_packing, incl_feature_brackets, plotting_segment_order, tracks_listed, track_height_cm, full_height_cm, stranded_beds, plot_vertical_parameters, verbosity, interface){
  .messages = list('output'=list(), 'errors'=list(), 'warnings'=list())
  .warning.message = NULL
  if (is.null(track_height_cm) & is.null(full_height_cm)){
    .error.message = paste0(' - ', 'both ', ifelse(interface=='R', '"track_height_cm"', '"Tracks Height"'), ' and ', ifelse(interface=='R', '"full_height_cm"', '"Full Plot Height"'), ' are NULL - one of them has to be defined')
  }else if (is.null(full_height_cm)){
    .error.message = paste0(' - ', ifelse(interface=='R', '"track_height_cm"', '"Tracks Height"'), ' = ', track_height_cm, ' - it should be set to a positive numeric value (recommended 0.2-1 cm)')
    .warning.message = paste0(' - ', ifelse(interface=='R', '"track_height_cm"', '"Tracks Height"'), ' = ', track_height_cm, ' cm - the recommended value is 0.2-1 cm')
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
              .max.annot.lines[[.annot]] = as.integer(max(1L, lengths(annot_info[[.annot]][['packing']])))
              .annot.heights[[.annot]] = as.numeric(ifelse(annotation_packing[.annot]=='expanded', plot_vertical_parameters['annot'], plot_vertical_parameters['annot_squished']) * .max.annot.lines[[.annot]])
            }else if (annotation_packing[.annot]=='collapsed2' | annotation_packing[.annot]=='collapsed'){
              .pk2.for.height = if (!is.null(annot_info[[.annot]][['packing2_display']])) annot_info[[.annot]][['packing2_display']] else annot_info[[.annot]][['packing2']]
              .max.annot.lines[[.annot]] = as.integer(max(1L, lengths(.pk2.for.height)))
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
          .max.bracket.lines = length(annot_info[[.annot]][['collapsed2']])
          .max.bracket.heights = as.numeric(plot_vertical_parameters['annot'] * .max.bracket.lines)
          # For expanded/squished modes, names are drawn inline by default.
          # When brackets are ON, the inline pipeline is skipped and names live
          # in a bracket section below the transcripts: bracket-row count plus
          # c2 above/below fallback rows are added on top of the transcript area.
          if (annotation_packing[.annot]=='expanded' | annotation_packing[.annot]=='squished') {
            # Inline-name extra rows live in the same y-coordinate system as the
            # transcript rows, so use the same per-row height: annot for
            # expanded, annot_squished for squished.
            .row.height.cm = if (annotation_packing[.annot]=='squished')
              as.numeric(plot_vertical_parameters['annot_squished'])
            else
              as.numeric(plot_vertical_parameters['annot'])
            .brackets.on = isTRUE(incl_feature_brackets[.annot])
            if (.brackets.on) {
              .pk2.rows = if (!is.null(annot_info[[.annot]][['packing2_display']])) max(0L, lengths(annot_info[[.annot]][['packing2_display']])) else 0L
              .c2.extra.rows = if (!is.null(annot_info[[.annot]][['c2_inline_name_extra_rows']])) annot_info[[.annot]][['c2_inline_name_extra_rows']] else 0L
              .c2.above.rows = if (!is.null(annot_info[[.annot]][['c2_inline_name_above_rows']])) annot_info[[.annot]][['c2_inline_name_above_rows']] else 0L
              .extra.height = (.pk2.rows + .c2.extra.rows + .c2.above.rows) * .row.height.cm
              .min.annot.heights.incl.text[[.annot]] = .annot.heights[[.annot]] + .extra.height
              .max.annot.heights.incl.text[[.annot]] = .annot.heights[[.annot]] + .extra.height
            } else {
              .extra.rows = if (!is.null(annot_info[[.annot]][['inline_name_extra_rows']])) annot_info[[.annot]][['inline_name_extra_rows']] else 0L
              .above.rows = if (!is.null(annot_info[[.annot]][['inline_name_above_rows']])) annot_info[[.annot]][['inline_name_above_rows']] else 0L
              .extra.height = (.extra.rows + .above.rows) * .row.height.cm
              .min.annot.heights.incl.text[[.annot]] = .annot.heights[[.annot]] + .extra.height
              .max.annot.heights.incl.text[[.annot]] = .annot.heights[[.annot]] + .extra.height
            }
          } else if (annotation_packing[.annot] %in% c('collapsed', 'collapsed2') &&
                     !is.null(annot_info[[.annot]][['c2_inline_name_placements']])) {
            .extra.rows = if (!is.null(annot_info[[.annot]][['c2_inline_name_extra_rows']])) annot_info[[.annot]][['c2_inline_name_extra_rows']] else 0L
            .above.rows = if (!is.null(annot_info[[.annot]][['c2_inline_name_above_rows']])) annot_info[[.annot]][['c2_inline_name_above_rows']] else 0L
            .extra.height = (.extra.rows + .above.rows) * as.numeric(plot_vertical_parameters['annot_text_segment'])
            .min.annot.heights.incl.text[[.annot]] = .annot.heights[[.annot]] + .extra.height
            .max.annot.heights.incl.text[[.annot]] = .annot.heights[[.annot]] + .extra.height
          } else {
            .min.annot.heights.incl.text[[.annot]] = .annot.heights[[.annot]] + ifelse(incl_feature_names[.annot], 1, 0) * (plot_vertical_parameters['annot_text_segment'] * .min.bracket.lines + ifelse(incl_feature_brackets[.annot], 1, 0) * .min.bracket.heights)
            .max.annot.heights.incl.text[[.annot]] = as.numeric(.annot.heights[[.annot]] + ifelse(incl_feature_names[.annot], 1, 0) * (plot_vertical_parameters['annot_text_segment'] * .max.bracket.lines + ifelse(incl_feature_brackets[.annot], 1, 0) * .max.bracket.heights))
          }
        }
        .min.annot.heights.combined = sum(unlist(.min.annot.heights.incl.text)) 
        .max.annot.heights.combined = sum(unlist(.max.annot.heights.incl.text)) 
      }else{
        .min.annot.heights.combined = 0
        .max.annot.heights.combined = 0
        .max.annot.heights.incl.text = 0
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
    if (!is.null(tracks_listed)){
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
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param estimated_plot_heights 
#' @param plot_vertical_parameters 
#' @param full_height_cm 
#' @param track_height_cm 
#' @param title_field_height_cm 
#' @param genomic_scale_height_cm 
#' @param annotation_height_cm 
#' @param spacer_height_cm 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
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
        .n.tracks = sum(!as.logical(grepl('header', names(.min.combined.track.vector)) + grepl('scale', names(.min.combined.track.vector)) + grepl('-spacer', names(.min.combined.track.vector)) + grepl('annot', names(.min.combined.track.vector))))
      }else{ #@ -> added 2023-06-26
        .n.tracks = sum(!as.logical(grepl('header', names(.min.combined.track.vector)) + grepl('scale', names(.min.combined.track.vector)) + grepl('-spacer', names(.min.combined.track.vector)) + grepl('annot', names(.min.combined.track.vector)))) + sum(.min.combined.track.vector[grepl('annot', names(.min.combined.track.vector))])
      } #@ <- added 2023-06-26
      estimated_plot_heights[[.strand]][['min.track.height.cm.est']] = as.numeric((full_height_cm-.max.non.track.height.cm)/.n.tracks)
      estimated_plot_heights[[.strand]][['max.track.height.cm.est']] = as.numeric((full_height_cm-.min.non.track.height.cm)/.n.tracks)
      estimated_plot_heights[[.strand]][['min.full.height.cm.est']] = full_height_cm
      estimated_plot_heights[[.strand]][['max.full.height.cm.est']] = full_height_cm
    }
    estimated_plot_heights[[.strand]][['annot.heights']] = lapply(estimated_plot_heights[[.strand]][['annot.heights']], function(x) x/plot_vertical_parameters[['annot']])
    estimated_plot_heights[[.strand]][['min.combined.track.vector']] = .min.combined.track.vector
    estimated_plot_heights[[.strand]][['max.combined.track.vector']] = .max.combined.track.vector
  }
  return(estimated_plot_heights)
}


#' Recommended Font Sizes
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#' 
#' @param est_track_height_cm 
#' @param est_min_annot_height 
#' @param plot_vertical_parameters 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
RecommendedFontSizes = function(est_track_height_cm, est_min_annot_height, plot_vertical_parameters_cm){
  constants_defaults = ConstantsDefaults()
  std_letter_height = constants_defaults['std_letter_height'] #@ 2022-10-05
  min_font_size = constants_defaults['min_font_size'] #@ 2022-10-05
  .max.font.size.std.tracks = round( est_track_height_cm / std_letter_height, 0)
  .est.min.annot.height.cm = est_track_height_cm * est_min_annot_height
  .max.font.size.std.annot = round( .est.min.annot.height.cm / std_letter_height, 0) + 2
  .max.font.size.std = min(.max.font.size.std.tracks, .max.font.size.std.annot)
  #c('std', 'main', 'sub', 'scale', 'genomic_axis', 'signal_axis', 'annotation_features')
  #.plot.vertical.parameters.cm = est_track_height_cm * plot_vertical_parameters
  .main = round(as.numeric(9*plot_vertical_parameters_cm['header']/0.66), 0)
  .sub = round(as.numeric(6*plot_vertical_parameters_cm['header']/0.66), 0)
  .scale = round(as.numeric(6*plot_vertical_parameters_cm['header']/0.66), 0)
  .genomic.axis = round(as.numeric(5*plot_vertical_parameters_cm['scale']/0.24), 0)
  .signal.axis = round(as.numeric(0.7*.max.font.size.std), 0)
  .annotation.features = round(as.numeric(6*plot_vertical_parameters_cm['annot_text_segment']/0.24), 0)
  .recommended.font.sizes = structure(c(.max.font.size.std, .main, .sub, .scale, .genomic.axis, .signal.axis, .annotation.features), names=c('std', 'main', 'sub', 'scale', 'genomic_axis', 'signal_axis', 'annotation_features'))
  if (.recommended.font.sizes['std'] >= .recommended.font.sizes['main']){
    .recommended.font.sizes['std'] = max(.recommended.font.sizes['main']-1, min_font_size)
    .recommended.font.sizes['signal_axis'] = round(.recommended.font.sizes['std'] * 0.7, 0)
  }
  return(.recommended.font.sizes)
}


#' Update Plot Vertical Parameters
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param plot_vertical_parameters 
#' @param track_height_cm_estimate 
#' @param title_field_height_cm 
#' @param genomic_scale_height_cm 
#' @param annotation_height_cm 
#' @param spacer_height_cm 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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
    # Squished/expanded ratio = annot_squished / annot from PlotVerticalParameters()
    # defaults (currently 0.5/0.8 = 0.625).
    plot_vertical_parameters['annot_squished'] = 0.625*plot_vertical_parameters['annot']
  }
  if (!is.null(spacer_height_cm)){
    plot_vertical_parameters['line-spacer'] = spacer_height_cm/track_height_cm_estimate
    plot_vertical_parameters['empty-spacer'] = plot_vertical_parameters['line-spacer']
    plot_vertical_parameters['thickline-spacer'] = 2*plot_vertical_parameters['line-spacer']
  }
  return(plot_vertical_parameters)
}


#' Calculate Track Height
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param combined_track_vector 
#' @param total_annotation_lines 
#' @param full_height_cm 
#' @param title_field_height_cm 
#' @param genomic_scale_height_cm 
#' @param annotation_height_cm 
#' @param spacer_height_cm 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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
    .n.tracks = sum(!as.logical(grepl('header', names(combined_track_vector)) + grepl('scale', names(combined_track_vector)) + grepl('-spacer', names(combined_track_vector)) + grepl('annot', names(combined_track_vector))))
  }else{ #@ -> added 2023-06-26
    .n.tracks = sum(!as.logical(grepl('header', names(combined_track_vector)) + grepl('scale', names(combined_track_vector)) + grepl('-spacer', names(combined_track_vector)) + grepl('annot', names(combined_track_vector)))) + sum(combined_track_vector[grepl('annot', names(combined_track_vector))])
  } #@ <- added 2023-06-26
  .track.height.cm = (full_height_cm-.non.track.height.cm)/.n.tracks
  return(.track.height.cm)
}


#' Update Track Vector
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param track_vector 
#' @param plot_vertical_parameters 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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


#' Plot Height Parameters
#'
#' @description Internal function: 
#'
#' @keywords internal
#' 
#' @author SLA
#'
#' @param combined_track_vector 
#' @param track_vector 
#' @param annotation_lines 
#' @param total_annotation_lines 
#' @param annot_heights_incl_text 
#' @param max_annot_lines 
#' @param annot_heights 
#' @param track_height_cm 
#' @param full_height_cm 
#' @param title_field_height_cm 
#' @param genomic_scale_height_cm 
#' @param annotation_height_cm 
#' @param spacer_height_cm 
#' @param plot_vertical_parameters 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
PlotHeightParameters = function(combined_track_vector, track_vector, annotation_lines, total_annotation_lines, annot_heights_incl_text, max_annot_lines, annot_heights, track_height_cm, full_height_cm, title_field_height_cm, genomic_scale_height_cm, annotation_height_cm, spacer_height_cm, plot_vertical_parameters){
  constants_defaults = ConstantsDefaults()
  cm_to_in = constants_defaults['cm_to_in'] #@ 2022-10-05
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
  .full.height.in = .full.height.cm * cm_to_in ## full_height_cm of figure in inches for pdf
  return(list('full.height.in'=.full.height.in, 'track.vector'=.track.vector, 'n.tracks.annots'=.n.tracks.annots, 'track.height.cm'=.track.height.cm, 'max.annot.lines'=max_annot_lines, 'annot.heights'=.annot.heights, 'annot.heights.incl.text'=.annot.heights.incl.text))
}


#' Organize Panels Dimensions
#'
#' @description Internal function: 
#' The overall panel dimensions can be fixed upfront or it can be left open for the function to determine the dimension - with some initial parameters defined
#'
#' @keywords internal
#'  
#' @author SLA
#'
#' @param datasets 
#' @param min_word_length 
#' @param replicate_names 
#' @param print_one_line_sample_names 
#' @param incl_first_panel 
#' @param plot_height_parameters 
#' @param feature_names_font_size 
#' @param font_size_range 
#' @param recommended_font_sizes 
#' @param scale_font_size 
#' @param horizontal_panels_list 
#' @param panel_font_size_list 
#' @param panels_list 
#' @param plot_widths_cm 
#' @param panel_separators 
#' @param strand 
#' @param both_strands 
#' @param strands_intermingled 
#' @param stranded_samples 
#' @param fixed_panel_width 
#' @param verbosity 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
#' @note .penalties.0hor.list obsolete? (#%)
#' 
OrganizePanelsDimensions = function(datasets, min_word_length, replicate_names, print_one_line_sample_names, incl_first_panel, plot_height_parameters, feature_names_font_size, font_size_range, recommended_font_sizes, scale_font_size, horizontal_panels_list, panel_font_size_list, panels_list, plot_widths_cm, panel_separators, strand, both_strands, strands_intermingled, stranded_samples, fixed_panel_width=FALSE, verbosity){
  constants_defaults = ConstantsDefaults()
  std_letter_width = constants_defaults['std_letter_width'] #@ 2022-10-05
  std_letter_height = constants_defaults['std_letter_height'] #@ 2022-10-05
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
  #% .penalties.0hor.list = list()
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
    # If the dataset isn't present in this strand's track-vector (e.g. the
    # user removed it from the Plot Segment Order table), it has no panels
    # to size: skip cleanly. Without this guard, do.call('rbind', sapply(...))
    # returns NULL, ncol(NULL) returns NULL, and downstream
    # `if (.n.levels > 1)` raises "argument of length 0".
    if (length(.subsamples)==0){
      if (isTRUE(getOption("seqNdisplayR.debug", FALSE))) {
        message("[seqNdisplayR debug] OrganizePanelsDimensions: dataset '",
                .dataset, "' has no subsamples in track-vector for this strand; ",
                "skipping (likely removed from plotting_segment_order).")
      }
      next
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
    # Reorder the subgroup columns by number-of-unique-values (fewer = leftmost),
    # matching the reorder OrganizedPanelsList applies to `panels_list`. Without
    # this the width-per-column array here would be indexed in Excel column
    # order (subgroup_1, subgroup_2, ...) while the rendered panel is drawn in
    # RLE order (constant subgroups leftmost) -- and the width reserved for
    # each visual column would come from the wrong source subgroup, causing
    # exactly-right widths for constant subgroups and truncation for the
    # varying subgroup that got the constant subgroup's tiny reservation.
    if (.n.levels > 1) {
      .rles.local = lapply(apply(.subsample.matrix, 2, S4Vectors::Rle),
                           function(x) S4Vectors::runLength(x))
      .subsample.matrix = .subsample.matrix[, order(lengths(.rles.local)), drop = FALSE]
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
    #% .penalties.0hor.list[[.dataset]] = .penalties.0hor
  }
  .eligible.font.sizes = apply(do.call('rbind', lapply(.penalties.list, function(m) apply(m, 2, function(c) any(c==0)))), 2, function(c) all(c))
  .common.font.size = ifelse(any(.eligible.font.sizes), .font.size.range[max(which(.eligible.font.sizes))], min(.font.size.range))
  .panel.config = list()
  if (!is.null(horizontal_panels_list)){
    .panel.config = horizontal_panels_list[datasets] 
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
      .eligible.font.sizes = apply(do.call('rbind', lapply(.penalties.list, function(m) apply(m, 2, function(c) any(c==0)))), 2, function(c) all(c))
      .panel.config = list()
      if (any(.eligible.font.sizes)){
        .common.font.size = .font.size.range[max(which(.eligible.font.sizes))]
        for (.dataset in names(.penalties.list)){ #%  .penalties.0hor.list <-> .penalties.list
          .config = rep(TRUE, .n.levels.list[[.dataset]])  ## TRUE refers to horizontal or not
          .n.ver.panels = max(which(.penalties.list[[.dataset]][,paste0('f', .common.font.size)]==0))-1 #% .penalties.0hor.list <-> .penalties.list
          if (.n.ver.panels > 0){
            .config[1:.n.ver.panels] = FALSE
          }
          .panel.config[[.dataset]] = .config
        }
      }else{
        .common.font.size = min(.font.size.range)
        for (.dataset in names(.penalties.list)){ #% .penalties.0hor.list <-> .penalties.list
          .config = rep(TRUE, .n.levels.list[[.dataset]])  ## TRUE refers to horizontal or not
          if (any(.penalties.list[[.dataset]][,paste0('f', .common.font.size)]==0)){ #% .penalties.0hor.list <-> .penalties.list
            .n.ver.panels = max(which(.penalties.list[[.dataset]][,paste0('f', .common.font.size)]==0))-1 #% .penalties.0hor.list <-> .penalties.list
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
            if (all(.penalties < 0)){ 
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
            }
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
      # Skip datasets that were removed from this strand's segment order;
      # they weren't populated into .n.levels.list / .panel.config in the
      # earlier loop, so trying to use them here would raise "argument of
      # length 0" (e.g. from `2:NULL`).
      if (is.null(.n.levels.list[[.dataset]])) next
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
      .subpanels = sapply(1:.n.levels, function(.n.level) length(runValue(Rle(.subsample.matrices[[.dataset]][,.n.level])))) 
      .panel.word.widths = matrix(ncol=.n.levels, nrow=max(.subpanels))
      .panel.word.widths[,1] = .common.font.size * (ifelse(!.config[1], nchar(.dataset), max(nchar(datasets))) + .word.extensions) * std_letter_width
      .panel.word.heights = .common.font.size * matrix(1, ncol=.n.levels, nrow=max(.subpanels)) * std_letter_height
      .panel.heights = matrix(rep(.heigths.cm, each=max(.subpanels)), ncol=.n.levels, nrow=max(.subpanels))
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
    # Skip datasets that were removed from this strand's segment order
    # (not populated into .n.levels.list above); without this guard
    # `rep(..., NULL)` produces numeric(0) and downstream indexing errors.
    if (is.null(.n.levels.list[[.dataset]])) next
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
  .panel.width.list = lapply(.panel.width.list, function(x) {l=length(x); if (l > 1){.n.panel.separators=l-1-ifelse(.incl.first.panel,0,1); if (.n.panel.separators>0){y=rep(NA, l+.n.panel.separators); y[setdiff(1:length(y), seq(ifelse(.incl.first.panel, 2, 3), length(y), by=2))]=x; y[seq(ifelse(.incl.first.panel, 2, 3), length(y), by=2)]=.panel.separator.cm}else{y=x}; return(y)}else{return(x)}}) 
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
    # panels_max_width_cm = 'auto' -> widen to whatever labels need (bounded
    # below by min_word_length). fixed_panel_width is meaningless in this
    # mode; force it off so downstream code doesn't paint imaginary space.
    fixed_panel_width = FALSE
    .min.word.length.cm = min_word_length * .common.font.size * std_letter_width - .scale.panel.width.cm
    .panels.max.width.cm = max(max(sapply(.panel.width.list, sum)), .min.word.length.cm)
  } else if (!fixed_panel_width) {
    # User picked a specific Panel Width AND left "Use full panel width"
    # unchecked. Semantic: fit labels automatically -- shrink the panel
    # when labels are smaller (already handled below via .panel.width),
    # widen when labels are LARGER, provided we can do so without breaking
    # a user-fixed Full Plot Width. When "Use full panel width" is ticked
    # (fixed_panel_width = TRUE) we honour the exact value even if labels
    # get truncated -- that's the user's explicit override.
    .labels.width.needed = max(sapply(.panel.width.list, sum))
    if (.labels.width.needed > .panels.max.width.cm) {
      if (plot_widths_cm['full.width.cm']==-1) {
        # Full Plot Width is 'auto' -> widen the panel freely; the full
        # width will grow to accommodate below.
        .panels.max.width.cm = .labels.width.needed
      } else {
        # Full Plot Width is fixed. See if labels fit into the space
        # currently allocated to the panel PLUS whatever slack remains
        # inside the fixed total.
        .max.allowable.panel = as.numeric(plot_widths_cm['full.width.cm']) -
                               .scale.panel.width.cm -
                               plot_widths_cm['track.width.cm'] -
                               2 * plot_widths_cm['margin.width.cm']
        if (.labels.width.needed <= .max.allowable.panel) {
          .panels.max.width.cm = .labels.width.needed
        } else if ((both_strands & strand=='+') | !both_strands) {
          if (verbosity > 1) {
            cat('WARNING: sample labels need ', round(.labels.width.needed, 2),
                ' cm but only ', round(.max.allowable.panel, 2),
                ' cm is available for the Panel within Full Plot Width; ',
                'labels will be truncated. To avoid truncation, increase ',
                'Full Plot Width or set Panel Width to "auto".',
                '\n', sep='')
          }
        }
      }
    }
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
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA
#'
#' @param panel_info 
#' @param both_strands 
#'
#' @return placeholder
#'
#' @examples
#' NULL
#' 
FinalizePanelsDimensions = function(panel_info, both_strands){
  if (both_strands){
    if (is.null(panel_info[['-']])){
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
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA
#'
#' @param bin_size 
#' @param plot_width 
#' @param tracks_width_cm 
#' @param bins_per_cm 
#' @param verbosity 
#'
#' @return placeholder
#'
#' @examples
#' NULL
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


#' Basic Plot Parameters
#'
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA
#'
#' @param plotted_strand 
#' @param plotted_region 
#' @param feature_names_font_size 
#' @param plot_height_parameters 
#' @param plot_width_parameters 
#' @param full_width_cm 
#' @param full_height_cm 
#' @param track_height_cm 
#' @param plot_vertical_parameters 
#' @param bin_size 
#' @param bins_per_cm 
#' @param plotting_segment_order 
#' @param tracks_listed 
#' @param unstranded_beds 
#'
#' @return placeholder
#' 
#' @import IRanges
#' @import S4Vectors
#'
#' @examples
#' NULL
#' 
BasicPlotParameters = function(plotted_strand, plotted_region, feature_names_font_size, plot_height_parameters, plot_width_parameters, full_width_cm, full_height_cm=NULL, track_height_cm=0.3, plot_vertical_parameters, bin_size, bins_per_cm, plotting_segment_order, tracks_listed, unstranded_beds){
  constants_defaults = ConstantsDefaults()
  line_width_scaling_factor = constants_defaults['line_width_scaling_factor'] #@ 2022-10-05
  points_per_cm = constants_defaults['points_per_cm'] #@ 2022-10-05
  cm_to_in = constants_defaults['cm_to_in'] #@ 2022-10-05
  # binning
  .plot.width = IRanges::width(plotted_region[[plotted_strand]]) + 1 #@ 2024-07-31 added +1
  .tracks.width.cm = plot_width_parameters[['tracks.width.cm']]
  .bases.per.cm = .plot.width/.tracks.width.cm
  .bins.per.cm = .bases.per.cm/bin_size
  .points.per.bin = points_per_cm/.bins.per.cm
  .bin.width = 2*line_width_scaling_factor*.points.per.bin # 2*line_width_scaling_factor ~ 1pt in pdf
  #@ 2024-07-31 -->
  # if (.bases.per.cm < bins_per_cm){
  #   .bin.width = .bin.width * bins_per_cm/.bases.per.cm  # to ensure same density of colors
  # }
  #@ 2024-07-31 <--
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
  if (!is.null(tracks_listed)){ 
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
  .annot.heights = lapply(plot_height_parameters[[plotted_strand]][['annot.heights']], function(x) x[feature_names_font_size]) 
  return(list('track.vector'=.track.vector, 'windows.height'=.windows.height, 'max.annot.lines'=plot_height_parameters[[plotted_strand]][['max.annot.lines']], 'annot.heights'=.annot.heights, 'plot.dim.in'=.plot.dim.in, 'track.height.cm'=.track.height.cm, 'bin.info'=.bin.info))
}


#' Align Basic Plot Parameters
#'
#' @description Internal function: 
#'
#' @keywords internal
#'  
#' @author SLA
#'
#' @param basic_plot_parameters 
#' @param both_strands 
#' @param strands_intermingled 
#' @param fixed_plot_vertical_parameters 
#' @param vertical_parameters 
#' @param full_height_cm
#


#' @return placeholder
#'
#' @examples
#' NULL
#' 
AlignBasicPlotParameters = function(basic_plot_parameters, both_strands, strands_intermingled, fixed_plot_vertical_parameters, vertical_parameters, minimal_units, full_height_cm, annotation_packing){ #@ 2023-06-27 added ,full_height_cm 2023-07-13 added, annotation_packing
  constants_defaults = ConstantsDefaults()
  cm_to_in = constants_defaults['cm_to_in'] #@ 2022-10-05
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

      # ---- Keep + and - annotation blocks together at the user's chosen
      # position in the Plot Segment Order. The + strand's annotation
      # entries (<annot>+) sit wherever 'annotations' appears in the user's
      # segment order; the - strand's annotation entries (<annot>-) come
      # from a separate - segment order that FinalizePlottingSegmentOrder
      # force-positions at the top of the - layout (with a thickline-spacer
      # above as the visual separator between strands). After the
      # `unique(c(+,-))` merge above, the - entries are naively appended
      # at the end of the unified layout -- so dragging 'annotations' in
      # the segment order only moves the + band. We splice the - block
      # (leading separator + <annot>- entries, in - strand order) in
      # immediately after the + annotation block and drop the trailing
      # empty-spacer that's only in the - order (vestigial once relocated).
      .track.vector.names <- tryCatch({
        .reorder.tvn <- .track.vector.names
        .plus.names           <- names(.basic.plot.parameters[['+']][['track.vector']])
        .minus.names          <- names(.basic.plot.parameters[['-']][['track.vector']])
        .plus.annot.entries   <- if (length(.annot.names)) paste0(.annot.names, '+') else character(0)
        .minus.annot.entries  <- if (length(.annot.names)) paste0(.annot.names, '-') else character(0)
        .minus.annot.present  <- intersect(.minus.annot.entries, .minus.names)
        if (length(.minus.annot.present) > 0L) {
          .first.minus.annot.idx <- min(match(.minus.annot.present, .minus.names))
          .leading.sep <- character(0)
          if (is.finite(.first.minus.annot.idx) && .first.minus.annot.idx > 1L) {
            .cand <- .minus.names[.first.minus.annot.idx - 1L]
            if (length(.cand) == 1L && !is.na(.cand) &&
                grepl('-spacer', .cand, fixed = TRUE) &&
                !.cand %in% .plus.names) {
              .leading.sep <- .cand
            }
          }
          .minus.block <- c(.leading.sep, .minus.annot.present)

          .minus.only        <- setdiff(.minus.names, .plus.names)
          .drop.from.unified <- setdiff(.minus.only, .minus.block)
          if (length(.drop.from.unified) > 0L) {
            .reorder.tvn <- .reorder.tvn[!.reorder.tvn %in% .drop.from.unified]
          }

          .plus.annot.positions <- which(.reorder.tvn %in% .plus.annot.entries)
          if (length(.plus.annot.positions) > 0L) {
            .reorder.tvn <- .reorder.tvn[!.reorder.tvn %in% .minus.block]
            .plus.annot.positions <- which(.reorder.tvn %in% .plus.annot.entries)
            if (length(.plus.annot.positions) > 0L) {
              .reorder.tvn <- append(.reorder.tvn,
                                     .minus.block,
                                     after = max(.plus.annot.positions))
            }
          }
        }
        .reorder.tvn
      }, error = function(e) {
        message("[seqNdisplayR] intermingled-annot reorder skipped: ",
                conditionMessage(e))
        .track.vector.names
      })
      # ----------------------------------------------------------------

      .spacer.indices = rev(grep('spacer', .track.vector.names, fixed=TRUE))
      if (length(.spacer.indices) > 0L) {
        .split.spacer.indices = split(.spacer.indices, cumsum(c(1, diff(.spacer.indices) != -1)))
        if (length(.split.spacer.indices) > 0L &&
            length(.split.spacer.indices[[1]]) > 0L &&
            .split.spacer.indices[[1]][1]==length(.track.vector.names)){
          .track.vector.names = .track.vector.names[-.split.spacer.indices[[1]]]
        }
      }
      .track.vector.plus = structure(sapply(.track.vector.names, function(.name) .basic.plot.parameters[['+']][['track.vector']][.name], USE.NAMES=FALSE), names=.track.vector.names)
      .track.vector.plus[is.na(.track.vector.plus)] = 0
      .track.vector.minus = structure(sapply(.track.vector.names, function(.name) .basic.plot.parameters[['-']][['track.vector']][.name], USE.NAMES=FALSE), names=.track.vector.names)
      .track.vector.minus[is.na(.track.vector.minus)] = 0
      .track.vector = .track.vector.plus + .track.vector.minus
      .full.height.cm = sum(.basic.plot.parameters[['+']][['track.height.cm']] * .track.vector) #@ 2023-06-27 added 
      if (!is.null(full_height_cm)){ #@ -> 2023-6-27 added clumpsy
        if (.full.height.cm != full_height_cm){
          #@.track.height.cm = full_height_cm/sum(.track.vector)
          .full.height.cm = full_height_cm
        }
      } #@ <- 2023-6-27 added clumpsy
      #@ if (any(!fixed_plot_vertical_parameters)){ #@ 2023-06-27 !fixed_plot_vertical_parameters[1]
      .unadjusted.track.vector.sum = sum(.track.vector)
      #@ .diff = sum(c(.basic.plot.parameters[['+']][['track.vector']], .basic.plot.parameters[['-']][['track.vector']])) - .unadjusted.track.vector.sum #@ 2023-06-27 removed 
      .unadjusted.track.vector.height.cm = .basic.plot.parameters[['+']][['track.height.cm']] * .track.vector #@ 2023-06-27 added 
      #.minimal.units = vertical_parameters/vertical_parameters['tracks']
      .thick.spacers.only = all(grepl('thickline-spacer', grep('-spacer', .track.vector.names, value=TRUE)))
      .indices = list()
      .weights = list()
      if (any(grepl('^header$', names(.track.vector)))){
        .indices[['header']] = grep('^header$', names(.track.vector))
        .weights[['header']] = 1
      }
      if (any(grepl('^scale$', names(.track.vector)))){
        .indices[['scale']] = grep('^scale$', names(.track.vector))
        .weights[['scale']] = 1
      }
      if (any(grepl('-spacer', names(.track.vector)))){
        .indices[['spacers']] = grep('-spacer', names(.track.vector))
        .weights[['spacers']] = .track.vector[.indices[['spacers']]]/ifelse(.thick.spacers.only, min(.track.vector[.indices[['spacers']]])/2, min(.track.vector[.indices[['spacers']]]))
      }
      if (!is.null(c(.annot.names, .unstranded.beds.names))){
        .indices[['annots']] = sort(unlist(lapply(c(.annot.names, .unstranded.beds.names), function(a) grep(paste0('^', a), names(.track.vector)))))
        .weights[['annots']] = .track.vector[.indices[['annots']]]/(min(.track.vector[.indices[['annots']]])/minimal_units['annots'])
      }
      .indices[['tracks']] = setdiff(1:length(.track.vector), unlist(.indices[1:4]))
      .weights[['tracks']] = .track.vector[.indices[['tracks']]]/(min(.track.vector[.indices[['tracks']]])/minimal_units['tracks'])
      if (!is.na(vertical_parameters['tracks'])){
        .unadjusted.track.vector.height.cm[.indices[['tracks']]] = .weights[['tracks']] * vertical_parameters['tracks']
      }
      if (!is.na(vertical_parameters['header']) & 'header' %in% names(.indices)){
        .unadjusted.track.vector.height.cm[.indices[['header']]] = .weights[['header']] * vertical_parameters['header']
      }
      if (!is.na(vertical_parameters['scale']) & 'scale' %in% names(.indices)){
        .unadjusted.track.vector.height.cm[.indices[['scale']]] = .weights[['scale']] * vertical_parameters['scale']
      }
      if (!is.na(vertical_parameters['spacers']) & 'spacers' %in% names(.indices)){
        .unadjusted.track.vector.height.cm[.indices[['spacers']]] = .weights[['spacers']] * vertical_parameters['spacers']
      }
      if (!is.na(vertical_parameters['annots']) & 'annots' %in% names(.indices)){
        .unadjusted.track.vector.height.cm[.indices[['annots']]] = .weights[['annots']] * vertical_parameters['annots']
      }
      .full.height.cm = sum(.unadjusted.track.vector.height.cm) #@ 2023-06-28
      if (!is.null(full_height_cm)){
        .full.height.cm = full_height_cm
      }
      if (any(!fixed_plot_vertical_parameters)){ #@ 2023-06-27
        .diff.indices = unlist(.indices[names(fixed_plot_vertical_parameters)[!fixed_plot_vertical_parameters]])
        .diff.weights = unlist(.weights[names(fixed_plot_vertical_parameters)[!fixed_plot_vertical_parameters]])
        .adjustable.tracks.cm = .unadjusted.track.vector.height.cm[.diff.indices]
        .fixed.heights.cm = .unadjusted.track.vector.height.cm[-.diff.indices]
        if (sum(.adjustable.tracks.cm) + sum(.fixed.heights.cm) != .full.height.cm){
          if (.full.height.cm > sum(.fixed.heights.cm)){
            .leftover.height.cm = .full.height.cm - sum(.fixed.heights.cm)
            .adjusted.tracks.cm = .leftover.height.cm * .adjustable.tracks.cm/sum(.adjustable.tracks.cm)
            .unadjusted.track.vector.height.cm[.diff.indices] = .adjusted.tracks.cm
          }else if (.full.height.cm < sum(.fixed.heights.cm)){
            .unadjusted.track.vector.height.cm = .full.height.cm * .unadjusted.track.vector.height.cm/sum(.unadjusted.track.vector.height.cm)
          }
        }
        #@ .track.vector[.diff.indices] = .track.vector[.diff.indices] + .diff*.diff.weights/sum(.diff.weights)
        #@ .basic.plot.parameters[['+-']][['weight']] = unique(.track.vector[.indices[['tracks']]]/as.integer(.track.vector[.indices[['tracks']]])) #@ 2023-6-27 added 
      }
      .track.height.cm = unique(.unadjusted.track.vector.height.cm[.indices[['tracks']]]/.weights[['tracks']])
      .track.vector = .unadjusted.track.vector.height.cm / .track.height.cm 
      .windows.height = c('top'=1, 1-cumsum(.track.vector)/sum(.track.vector)); .windows.height[length(.windows.height)] = 0
      .basic.plot.parameters[['+-']] = list('track.vector'=.track.vector, 'windows.height'=.windows.height)
      .annot.names = names(.basic.plot.parameters[['+']][['max.annot.lines']])
      .basic.plot.parameters[['+-']][['max.annot.lines']] = structure(lapply(.annot.names, function(.annot.name) .basic.plot.parameters[['+']][['max.annot.lines']][[.annot.name]] + ifelse(.annot.name %in% .unstranded.beds.names, 0, .basic.plot.parameters[['-']][['max.annot.lines']][[.annot.name]])), names=.annot.names)
      .rel.annot.height = as.numeric(unique(.track.vector[.indices[['annots']]] / .weights[['annots']])[1])
      #@ -> 2023-07-11  #@ add expanded/squished here
      .basic.plot.parameters[['+']][['annot.heights']] = structure(lapply(.annot.names, function(.annot.name) .basic.plot.parameters[['+']][['max.annot.lines']][[.annot.name]] * .rel.annot.height * as.numeric(ifelse(annotation_packing[.annot.name] == 'squished', 0.625, 1)) ), names=.annot.names)
      .basic.plot.parameters[['-']][['annot.heights']] = structure(lapply(.annot.names, function(.annot.name) .basic.plot.parameters[['-']][['max.annot.lines']][[.annot.name]] * .rel.annot.height * as.numeric(ifelse(annotation_packing[.annot.name] == 'squished', 0.625, 1)) ), names=.annot.names)
      #@ <-
      .basic.plot.parameters[['+-']][['annot.heights']] = structure(lapply(.annot.names, function(.annot.name) .basic.plot.parameters[['+']][['annot.heights']][[.annot.name]] + ifelse(.annot.name %in% .unstranded.beds.names, 0, .basic.plot.parameters[['-']][['annot.heights']][[.annot.name]])), names=.annot.names)
      #@ .basic.plot.parameters[['+-']][['weight']] = 1 #@ 2023-06-27 added 
      #@ .track.height.cm = .basic.plot.parameters[['+']][['track.height.cm']] #@ 2023-06-27 added 
      .basic.plot.parameters[['+-']][['plot.dim.in']] = c(.basic.plot.parameters[['+']][['plot.dim.in']][1], cm_to_in * .full.height.cm) #@ 2023-6-27 sum(.basic.plot.parameters[['+']][['track.height.cm']] * .track.vector)
      .basic.plot.parameters[['+-']][['track.height.cm']] = .track.height.cm #@ 2023-6-27 .basic.plot.parameters[['+']][['track.height.cm']]  
      .basic.plot.parameters[['+-']][['bin.info']] = .basic.plot.parameters[['+']][['bin.info']]
      .basic.plot.parameters[['+-']][['plot.vertical.parameters']] = c( 'header'=as.numeric(.track.vector[.indices[['header']]]),             
                                                                        'seq'=1,                  
                                                                        'scale'=as.numeric(.track.vector[.indices[['scale']]]),              
                                                                        'line-spacer'=as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),        
                                                                        'empty-spacer'=as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),       
                                                                        'thickline-spacer'=2*as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),   
                                                                        'annot'=.rel.annot.height,              
                                                                        'annot_squished'=0.625*.rel.annot.height,     
                                                                        'annot_text_segment'=.rel.annot.height)
    }else{
      #@.windows.height.adjusted = .basic.plot.parameters[['+']][['windows.height']]*.basic.plot.parameters[['+']][['plot.dim.in']][2] + .basic.plot.parameters[['-']][['plot.dim.in']][2]
      # .basic.plot.parameters[['+']][['windows.height']] = .windows.height.adjusted/max(.windows.height.adjusted)
      # .basic.plot.parameters[['-']][['windows.height']] = (.basic.plot.parameters[['-']][['windows.height']]*.basic.plot.parameters[['-']][['plot.dim.in']][2])/max(.windows.height.adjusted)
      #@ -> 2023-07-11 added
      .track.vector = c(.basic.plot.parameters[['+']][['track.vector']], .basic.plot.parameters[['-']][['track.vector']])
      .track.vector.names = names(.track.vector)
      .annot.names = names(.basic.plot.parameters[["-"]][["max.annot.lines"]])
      .unstranded.beds.names = setdiff(names(.basic.plot.parameters[["+"]][["max.annot.lines"]]), .annot.names)
      .spacer.indices = rev(grep('spacer', .track.vector.names, fixed=TRUE))
      .split.spacer.indices = split(.spacer.indices, cumsum(c(1, diff(.spacer.indices) != -1)))
      if (.split.spacer.indices[[1]][1]==length(.track.vector.names)){
        .track.vector.names = .track.vector.names[-.split.spacer.indices[[1]]]
      }
      .full.height.cm = sum(mean(c(.basic.plot.parameters[['+']][['track.height.cm']], .basic.plot.parameters[['-']][['track.height.cm']])) * .track.vector) #@ 2023-06-27 added 
      if (!is.null(full_height_cm)){ #@ -> 2023-6-27 added clumpsy
        if (.full.height.cm != full_height_cm){
          #@.track.height.cm = full_height_cm/sum(.track.vector)
          .full.height.cm = full_height_cm
        }
      } #@ <- 2023-6-27 added clumpsy
      #@ if (any(!fixed_plot_vertical_parameters)){ #@ 2023-06-27 !fixed_plot_vertical_parameters[1]
      .unadjusted.track.vector.sum = sum(.track.vector)
      #@ .diff = sum(c(.basic.plot.parameters[['+']][['track.vector']], .basic.plot.parameters[['-']][['track.vector']])) - .unadjusted.track.vector.sum #@ 2023-06-27 removed 
      .unadjusted.track.vector.height.cm = .basic.plot.parameters[['+']][['track.height.cm']] * .track.vector #@ 2023-06-27 added 
      #.minimal.units = vertical_parameters/vertical_parameters['tracks']
      .thick.spacers.only = all(grepl('thickline-spacer', grep('-spacer', .track.vector.names, value=TRUE)))
      .indices = list()
      .weights = list()
      if (any(grepl('^header$', names(.track.vector)))){
        .indices[['header']] = grep('^header$', names(.track.vector))
        .weights[['header']] = 1
      }
      if (any(grepl('^scale$', names(.track.vector)))){
        .indices[['scale']] = grep('^scale$', names(.track.vector))
        .weights[['scale']] = 1
      }
      if (any(grepl('-spacer', names(.track.vector)))){
        .indices[['spacers']] = grep('-spacer', names(.track.vector))
        .weights[['spacers']] = .track.vector[.indices[['spacers']]]/ifelse(.thick.spacers.only, min(.track.vector[.indices[['spacers']]])/2, min(.track.vector[.indices[['spacers']]]))
      }
      if (!is.null(c(.annot.names, .unstranded.beds.names))){
        .indices[['annots']] = sort(unlist(lapply(c(.annot.names, .unstranded.beds.names), function(a) grep(paste0('^', a), names(.track.vector)))))
        .weights[['annots']] = .track.vector[.indices[['annots']]]/(min(.track.vector[.indices[['annots']]])/minimal_units['annots'])
      }
      .indices[['tracks']] = setdiff(1:length(.track.vector), unlist(.indices[1:4]))
      .weights[['tracks']] = .track.vector[.indices[['tracks']]]/(min(.track.vector[.indices[['tracks']]])/minimal_units['tracks'])
      if (!is.na(vertical_parameters['tracks'])){
        .unadjusted.track.vector.height.cm[.indices[['tracks']]] = .weights[['tracks']] * vertical_parameters['tracks']
      }
      if (!is.na(vertical_parameters['header']) & 'header' %in% names(.indices)){
        .unadjusted.track.vector.height.cm[.indices[['header']]] = .weights[['header']] * vertical_parameters['header']
      }
      if (!is.na(vertical_parameters['scale']) & 'scale' %in% names(.indices)){
        .unadjusted.track.vector.height.cm[.indices[['scale']]] = .weights[['scale']] * vertical_parameters['scale']
      }
      if (!is.na(vertical_parameters['spacers']) & 'spacers' %in% names(.indices)){
        .unadjusted.track.vector.height.cm[.indices[['spacers']]] = .weights[['spacers']] * vertical_parameters['spacers']
      }
      if (!is.na(vertical_parameters['annots']) & 'annots' %in% names(.indices)){
        .unadjusted.track.vector.height.cm[.indices[['annots']]] = .weights[['annots']] * vertical_parameters['annots']
      }
      .full.height.cm = sum(.unadjusted.track.vector.height.cm) #@ 2023-06-28
      if (!is.null(full_height_cm)){
        .full.height.cm = full_height_cm
      }
      if (any(!fixed_plot_vertical_parameters)){ #@ 2023-06-27
        .diff.indices = unlist(.indices[names(fixed_plot_vertical_parameters)[!fixed_plot_vertical_parameters]])
        .diff.weights = unlist(.weights[names(fixed_plot_vertical_parameters)[!fixed_plot_vertical_parameters]])
        .adjustable.tracks.cm = .unadjusted.track.vector.height.cm[.diff.indices]
        .fixed.heights.cm = .unadjusted.track.vector.height.cm[-.diff.indices]
        if (sum(.adjustable.tracks.cm) + sum(.fixed.heights.cm) != .full.height.cm){
          if (.full.height.cm > sum(.fixed.heights.cm)){
            .leftover.height.cm = .full.height.cm - sum(.fixed.heights.cm)
            .adjusted.tracks.cm = .leftover.height.cm * .adjustable.tracks.cm/sum(.adjustable.tracks.cm)
            .unadjusted.track.vector.height.cm[.diff.indices] = .adjusted.tracks.cm
          }else if (.full.height.cm < sum(.fixed.heights.cm)){
            .unadjusted.track.vector.height.cm = .full.height.cm * .unadjusted.track.vector.height.cm/sum(.unadjusted.track.vector.height.cm)
          }
        }
        #@ .track.vector[.diff.indices] = .track.vector[.diff.indices] + .diff*.diff.weights/sum(.diff.weights)
        #@ .basic.plot.parameters[['+-']][['weight']] = unique(.track.vector[.indices[['tracks']]]/as.integer(.track.vector[.indices[['tracks']]])) #@ 2023-6-27 added 
      }
      .track.height.cm = unique(.unadjusted.track.vector.height.cm[.indices[['tracks']]]/.weights[['tracks']])
      .track.vector = .unadjusted.track.vector.height.cm / .track.height.cm 
      .windows.height = c('top'=1, 1-cumsum(.track.vector)/sum(.track.vector)); .windows.height[length(.windows.height)] = 0
      .basic.plot.parameters[['+']][['windows.height']] = .windows.height[1:length(.basic.plot.parameters[['+']][['windows.height']])]
      .basic.plot.parameters[['-']][['windows.height']] = .windows.height[(length(.basic.plot.parameters[['+']][['windows.height']])-1)+1:length(.basic.plot.parameters[['-']][['windows.height']])]
      names(.basic.plot.parameters[['-']][['windows.height']])[1] = 'top'
      .basic.plot.parameters[['+']][['track.vector']] = .track.vector[1:length(.basic.plot.parameters[['+']][['track.vector']])]
      .basic.plot.parameters[['-']][['track.vector']] = .track.vector[length(.basic.plot.parameters[['+']][['track.vector']]) + 1:length(.basic.plot.parameters[['-']][['track.vector']])]
      .annot.names = names(.basic.plot.parameters[['+']][['max.annot.lines']])
      .rel.annot.height = as.numeric(unique(.track.vector[.indices[['annots']]] / .weights[['annots']])[1])
      #@ -> 2023-07-11
      .basic.plot.parameters[['+']][['annot.heights']] = structure(lapply(.annot.names, function(.annot.name) .basic.plot.parameters[['+']][['max.annot.lines']][[.annot.name]] * .rel.annot.height * as.numeric(ifelse(annotation_packing[.annot.name] == 'squished', 0.625, 1)) ), names=.annot.names)
      .basic.plot.parameters[['-']][['annot.heights']] = structure(lapply(.annot.names, function(.annot.name) .basic.plot.parameters[['-']][['max.annot.lines']][[.annot.name]] * .rel.annot.height * as.numeric(ifelse(annotation_packing[.annot.name] == 'squished', 0.625, 1)) ), names=.annot.names)
      #@ <-
      .basic.plot.parameters[['+']][['plot.dim.in']] = c(.basic.plot.parameters[['+']][['plot.dim.in']][1], cm_to_in * .track.height.cm * sum(.basic.plot.parameters[['+']][['track.vector']])) 
      .basic.plot.parameters[['-']][['plot.dim.in']] = c(.basic.plot.parameters[['+']][['plot.dim.in']][1], cm_to_in * .track.height.cm * sum(.basic.plot.parameters[['-']][['track.vector']]))
      .basic.plot.parameters[['+']][['track.height.cm']] = .track.height.cm 
      .basic.plot.parameters[['-']][['track.height.cm']] = .track.height.cm 
      .basic.plot.parameters[['+']][['plot.vertical.parameters']] = c( 'header'=as.numeric(.track.vector[.indices[['header']]]),             
                                                                        'seq'=1,                  
                                                                        'scale'=as.numeric(.track.vector[.indices[['scale']]]),              
                                                                        'line-spacer'=as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),        
                                                                        'empty-spacer'=as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),       
                                                                        'thickline-spacer'=2*as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),   
                                                                        'annot'=.rel.annot.height,              
                                                                        'annot_squished'=0.625*.rel.annot.height,     
                                                                        'annot_text_segment'=.rel.annot.height)
      .basic.plot.parameters[['-']][['plot.vertical.parameters']] = c( 'header'=as.numeric(.track.vector[.indices[['header']]]),             
                                                                        'seq'=1,                  
                                                                        'scale'=as.numeric(.track.vector[.indices[['scale']]]),              
                                                                        'line-spacer'=as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),        
                                                                        'empty-spacer'=as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),       
                                                                        'thickline-spacer'=2*as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),   
                                                                        'annot'=.rel.annot.height,              
                                                                        'annot_squished'=0.625*.rel.annot.height,     
                                                                        'annot_text_segment'=.rel.annot.height)
      #@ <- 2023-07-11
    }
    .basic.plot.parameters[['+']][['plot.dim.in']][2] = .height.in
    .basic.plot.parameters[['-']][['plot.dim.in']][2] = .height.in
  }else{
    .only.strand = names(.basic.plot.parameters)
    .temp.basic.plot.parameters = structure(list(names(.basic.plot.parameters[[.only.strand]][['track.vector']])), names=.only.strand)
    .spacer.names = NumberingSpacers(.temp.basic.plot.parameters)
    names(.basic.plot.parameters[[.only.strand]][['track.vector']]) = .spacer.names[[.only.strand]]
    names(.basic.plot.parameters[[.only.strand]][['windows.height']])[1+1:length(.spacer.names[[.only.strand]])] = .spacer.names[[.only.strand]]
    #@ -> 2023-07-11
    .track.vector = .basic.plot.parameters[[.only.strand]][['track.vector']]
    .track.vector.names = names(.track.vector)
    .annot.names = names(.basic.plot.parameters[[.only.strand]][["max.annot.lines"]])
    #.unstranded.beds.names = setdiff(names(.basic.plot.parameters[["+"]][["max.annot.lines"]]), .annot.names)
    .spacer.indices = rev(grep('spacer', .track.vector.names, fixed=TRUE))
    .split.spacer.indices = split(.spacer.indices, cumsum(c(1, diff(.spacer.indices) != -1)))
    if (.split.spacer.indices[[1]][1]==length(.track.vector.names)){
      .track.vector.names = .track.vector.names[-.split.spacer.indices[[1]]]
    }
    .full.height.cm = sum(.basic.plot.parameters[[.only.strand]][['track.height.cm']] * .track.vector) #@ 2023-06-27 added 
    if (!is.null(full_height_cm)){ #@ -> 2023-6-27 added clumpsy
      if (.full.height.cm != full_height_cm){
        .full.height.cm = full_height_cm
      }
    } #@ <- 2023-6-27 added clumpsy
    #@ if (any(!fixed_plot_vertical_parameters)){ #@ 2023-06-27 !fixed_plot_vertical_parameters[1]
    .unadjusted.track.vector.sum = sum(.track.vector)
    #@ .diff = sum(c(.basic.plot.parameters[['+']][['track.vector']], .basic.plot.parameters[['-']][['track.vector']])) - .unadjusted.track.vector.sum #@ 2023-06-27 removed 
    .unadjusted.track.vector.height.cm = .basic.plot.parameters[[.only.strand]][['track.height.cm']] * .track.vector #@ 2023-06-27 added 
    #.minimal.units = vertical_parameters/vertical_parameters['tracks']
    .thick.spacers.only = all(grepl('thickline-spacer', grep('-spacer', .track.vector.names, value=TRUE)))
    .indices = list()
    .weights = list()
    if (any(grepl('^header$', names(.track.vector)))){
      .indices[['header']] = grep('^header$', names(.track.vector))
      .weights[['header']] = 1
    }
    if (any(grepl('^scale$', names(.track.vector)))){
      .indices[['scale']] = grep('^scale$', names(.track.vector))
      .weights[['scale']] = 1
    }
    if (any(grepl('-spacer', names(.track.vector)))){
      .indices[['spacers']] = grep('-spacer', names(.track.vector))
      .weights[['spacers']] = .track.vector[.indices[['spacers']]]/ifelse(.thick.spacers.only, min(.track.vector[.indices[['spacers']]])/2, min(.track.vector[.indices[['spacers']]]))
    }
    if (!is.null(c(.annot.names))){
      .indices[['annots']] = sort(unlist(lapply(c(.annot.names), function(a) grep(paste0('^', a), names(.track.vector)))))
      .weights[['annots']] = .track.vector[.indices[['annots']]]/(min(.track.vector[.indices[['annots']]])/minimal_units['annots'])
    }
    .indices[['tracks']] = setdiff(1:length(.track.vector), unlist(.indices[1:4]))
    .weights[['tracks']] = .track.vector[.indices[['tracks']]]/(min(.track.vector[.indices[['tracks']]])/minimal_units['tracks'])
    if (!is.na(vertical_parameters['tracks'])){
      .unadjusted.track.vector.height.cm[.indices[['tracks']]] = .weights[['tracks']] * vertical_parameters['tracks']
    }
    if (!is.na(vertical_parameters['header']) & 'header' %in% names(.indices)){
      .unadjusted.track.vector.height.cm[.indices[['header']]] = .weights[['header']] * vertical_parameters['header']
    }
    if (!is.na(vertical_parameters['scale']) & 'scale' %in% names(.indices)){
      .unadjusted.track.vector.height.cm[.indices[['scale']]] = .weights[['scale']] * vertical_parameters['scale']
    }
    if (!is.na(vertical_parameters['spacers']) & 'spacers' %in% names(.indices)){
      .unadjusted.track.vector.height.cm[.indices[['spacers']]] = .weights[['spacers']] * vertical_parameters['spacers']
    }
    if (!is.na(vertical_parameters['annots']) & 'annots' %in% names(.indices)){
      .unadjusted.track.vector.height.cm[.indices[['annots']]] = .weights[['annots']] * vertical_parameters['annots']
    }
    .full.height.cm = sum(.unadjusted.track.vector.height.cm) #@ 2023-06-28
    if (!is.null(full_height_cm)){
      .full.height.cm = full_height_cm
    }
    if (any(!fixed_plot_vertical_parameters)){ #@ 2023-06-27
      .diff.indices = unlist(.indices[names(fixed_plot_vertical_parameters)[!fixed_plot_vertical_parameters]])
      .diff.weights = unlist(.weights[names(fixed_plot_vertical_parameters)[!fixed_plot_vertical_parameters]])
      .adjustable.tracks.cm = .unadjusted.track.vector.height.cm[.diff.indices]
      .fixed.heights.cm = .unadjusted.track.vector.height.cm[-.diff.indices]
      if (sum(.adjustable.tracks.cm) + sum(.fixed.heights.cm) != .full.height.cm){
        if (.full.height.cm > sum(.fixed.heights.cm)){
          .leftover.height.cm = .full.height.cm - sum(.fixed.heights.cm)
          .adjusted.tracks.cm = .leftover.height.cm * .adjustable.tracks.cm/sum(.adjustable.tracks.cm)
          .unadjusted.track.vector.height.cm[.diff.indices] = .adjusted.tracks.cm
        }else if (.full.height.cm < sum(.fixed.heights.cm)){
          .unadjusted.track.vector.height.cm = .full.height.cm * .unadjusted.track.vector.height.cm/sum(.unadjusted.track.vector.height.cm)
        }
      }
      #@ .track.vector[.diff.indices] = .track.vector[.diff.indices] + .diff*.diff.weights/sum(.diff.weights)
      #@ .basic.plot.parameters[['+-']][['weight']] = unique(.track.vector[.indices[['tracks']]]/as.integer(.track.vector[.indices[['tracks']]])) #@ 2023-6-27 added 
    }
    .track.height.cm = unique(.unadjusted.track.vector.height.cm[.indices[['tracks']]]/.weights[['tracks']])
    .track.vector = .unadjusted.track.vector.height.cm / .track.height.cm 
    .windows.height = c('top'=1, 1-cumsum(.track.vector)/sum(.track.vector)); .windows.height[length(.windows.height)] = 0
    .basic.plot.parameters[[.only.strand]][['windows.height']] = .windows.height
    .basic.plot.parameters[[.only.strand]][['track.vector']] = .track.vector
    .annot.names = names(.basic.plot.parameters[[.only.strand]][['max.annot.lines']])
    .rel.annot.height = as.numeric(unique(.track.vector[.indices[['annots']]] / .weights[['annots']])[1])
    #@ -> 2023-07-11
    .basic.plot.parameters[[.only.strand]][['annot.heights']] = structure(lapply(.annot.names, function(.annot.name) .basic.plot.parameters[[.only.strand]][['max.annot.lines']][[.annot.name]] * .rel.annot.height * as.numeric(ifelse(annotation_packing[.annot.name] == 'squished', 0.625, 1)) ), names=.annot.names)
    #@ <-
    .basic.plot.parameters[[.only.strand]][['plot.dim.in']] = c(.basic.plot.parameters[[.only.strand]][['plot.dim.in']][1], cm_to_in * .track.height.cm * sum(.basic.plot.parameters[[.only.strand]][['track.vector']])) 
    .basic.plot.parameters[[.only.strand]][['track.height.cm']] = .track.height.cm 
    .basic.plot.parameters[[.only.strand]][['plot.vertical.parameters']] = c( 'header'=as.numeric(.track.vector[.indices[['header']]]),             
                                                                     'seq'=1,                  
                                                                     'scale'=as.numeric(.track.vector[.indices[['scale']]]),              
                                                                     'line-spacer'=as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),        
                                                                     'empty-spacer'=as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),       
                                                                     'thickline-spacer'=2*as.numeric(unique(.track.vector[.indices[['spacers']]] / .weights[['spacers']])[1]),   
                                                                     'annot'=.rel.annot.height,              
                                                                     'annot_squished'=0.625*.rel.annot.height,     
                                                                     'annot_text_segment'=.rel.annot.height)
    
    #@ <- 2023-07-11
  }
  return(.basic.plot.parameters)
}


