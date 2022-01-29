library(shiny)
library(shinyBS)
library(shinyjs)
library(spsComps)
library(seqNdisplayR)
library(shinyTree)
library(dplyr)
library(shinybusy)


share <- list(title = "{seqNdisplayR} package",
              description = "A Tool for Customizable and Reproducible Plotting of Sequencing Coverage Data")

options(shinyTree.setState = TRUE)
options(shinyTree.refresh  = TRUE)

options_table <- readxl::read_excel(system.file('extdata', 'variable_defaults_and_help.xlsx', package='seqNdisplayR'), sheet='Shiny_Args')

create_input_element <- function(option) {
  option_par <- options_table[options_table$shiny_varname == option,]
  if ( option_par$option_group == 'dataset_option' | option_par$option_group == 'annotation_option') {
      if ( option_par$option_class == 'bool' ) {
        spsComps::bsTooltip(
          checkboxGroupInput(option_par$shiny_varname,
                             option_par$shiny_label,
                             choices = c('all samples'),
                             selected = option_par$option_default == 'TRUE'),
          title=option_par$shiny_tooltip,
          placement ='left')
      } else if ( option_par$option_class == 'text_choices' ) {
        opt_choices  <- strsplit(option_par$option_options,',')[[1]]
        fluidRow(
            tags$head(
              tags$style(type="text/css", "#title {padding-left: 15px} #radios .shiny-input-radiogroup{padding-left: 25px}")
          ),
          tags$div(id = "title",spsComps::bsTooltip(h5(option_par$shiny_label, style= 'font-weight: bold'),
                                                    title=option_par$shiny_tooltip,
                                                    placement ='left')),
          tags$div(id = "radios",radioButtons(option_par$shiny_varname,
                       label = 'all',
                       choices = opt_choices,
                       selected = option_par$option_default,
                       inline = TRUE)),
          tags$br())
      } else if ( option_par$option_class == 'text' ) {
        fluidRow(tags$head(
          tags$style(type="text/css", "#title {padding-left: 15px} #inline label{ display: table-cell; text-align: left; vertical-align: left; padding-left: 25px; font-weight: normal}
                #inline .form-group { display: table-row; line-height: 5px}")
        ),
        tags$div(id = "title",spsComps::bsTooltip(h5(option_par$shiny_label, style= 'font-weight: bold'),
                                                  title=option_par$shiny_tooltip,
                                                  placement ='left')),
        tags$div(id = "inline",
                 tags$div(id=option_par$shiny_varname,
                          textInput(paste0(option_par$shiny_varname, 'xvalue'),
                             label='all samples',
                             placeholder = option_par$option_default,
                             value = option_par$option_default))
        ),
        tags$br())
      } else if ( option_par$option_class == 'numeric' ) {
        fluidRow(tags$head(
          tags$style(paste0("#title {padding-left: 15px}",
                            " #inline label{ display: table-cell; text-align: left; vertical-align: left; padding-left: 20px; padding-right: 5px; font-weight: normal}",
                            " #",option_par$shiny_varname, "xvalue.form-control.shiny-bound-input { display: table-cell; text-align: left; vertical-align: left; margin-left: 30px; width: 100px; padding-left: 5px; font-weight: normal}"))
        ),
        tags$div(id = "title",spsComps::bsTooltip(h5(option_par$shiny_label, style= 'font-weight: bold'),
                                                  title=option_par$shiny_tooltip,
                                                  placement ='left')),
        tags$div(id = 'inline',
                 tags$div(id=option_par$shiny_varname,
                          numericInput(paste0(option_par$shiny_varname, 'xvalue'),
                            label='all samples',
                            value = option_par$option_default))
        ),
        tags$br())
      }
  } else {
    if ( option_par$option_class == 'bool' ) {
      spsComps::bsTooltip(
        checkboxInput(option_par$shiny_varname, option_par$shiny_label, value = option_par$option_default == 'TRUE'),
        title=option_par$shiny_tooltip,
        placement ='left')
    } else if ( option_par$option_class == 'text' ) {
      spsComps::bsTooltip(
        textInput(option_par$shiny_varname,
                  label =  option_par$shiny_label,
                  placeholder = option_par$option_default,
                  value = option_par$option_default,
                  width = '500px'),
        title=option_par$shiny_tooltip,
        placement ='left')
    } else if ( option_par$option_class == 'text_choices' ) {
      opt_choices  <- strsplit(option_par$option_options,',')[[1]]
      spsComps::bsTooltip(
          radioButtons(option_par$shiny_varname,
                                            label = option_par$shiny_label,
                                            choices = opt_choices,
                                            selected = option_par$option_default,
                                            inline = TRUE),
          title=option_par$shiny_tooltip,
          placement ='left')
    } else if ( option_par$option_class == 'numeric' ) {
      spsComps::bsTooltip(
        numericInput(option_par$shiny_varname,
                     label =  option_par$shiny_label,
                     value = as.numeric(option_par$option_default),
                     width = '500px'),
        title=option_par$shiny_tooltip,
        placement ='left')
    }
  }
}


## TO DO: export identical function from seqNdisplayR.R?
ListDepth = function(query_list){
  ifelse(is.list(query_list), 1L + max(sapply(query_list, ListDepth)), 0L)
}


# UI ####
ui <- fluidPage(
  useShinyjs(),
  #shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
  shinybusy::use_busy_spinner(spin = "fading-circle"),

  # HEADER ####
  titlePanel(h1("seq'N'display'R", align = "center")),
  h3(
    "A Tool for Customizable and Reproducible Plotting of Sequencing Coverage Data", align='center'
  ),
  tags$br(),


  # TOP BUTTONS ####
  tags$head(
    tags$style(
      HTML(
        "#buttons_div .tooltip-inner{width: 200px; background-color:gray}"
      )
    )
  ),


  sidebarLayout(
    sidebarPanel(width=3,
                 div(id='buttons_div',
                     actionButton("plot", "Draw Plot"),
                     #note the tooltop, clean-up of space only visible upon mouse-over
                     shinyBS::bsTooltip(id = "plot", title = "The plot will appear in an independent window. </br> </br> The process can take some time depending on the number of tracks and annotations plotted as well as the arguments used.",
                                        placement = "right", trigger = "hover",),
                     suppressWarnings(downloadButton("save_pdf", "Save as Pdf")),
                     shinyBS::bsTooltip(id = "save_pdf", title = "The plot will be saved as pdf. </br> </br> The process can take some time depending on the number of tracks and annotations plotted as well as the arguments used.",
                                        placement = "right", trigger = "hover"),
                     suppressWarnings(downloadButton("save_settings", "Save Settings")),
                     shinyBS::bsTooltip(id = "save_settings", title = "Save current settings in an Excel template for reuse.",
                                        placement = "right", trigger = "hover"),
                     ## TODO: remove debugging buttons
                     actionButton("show_settings", "Show Settings"),
                     shinyBS::bsTooltip(id = "show_settings", title = "Show settings of active session in console.",
                                        placement = "right", trigger = "hover"),
                     actionButton("show_parameters", "Show Parameters"),
                     shinyBS::bsTooltip(id = "show_parameters", title = "Show dataset-specific options (parameters) of active session in console.",
                                        placement = "right", trigger = "hover"),
                     actionButton("show_samples", "Show Samples"),
                     shinyBS::bsTooltip(id = "show_samples", title = "Show currently selected samples and their order",
                                        placement = "right", trigger = "hover"),
                     actionButton("which_samples", "Show whichSamples"),
                     shinyBS::bsTooltip(id = "which_samples", title = "Show currently whichSamples argument, selected samples for display. Samples listed in Show samples but not in here are still used for batch correction",
                                        placement = "right", trigger = "hover"),
                     actionButton("clean_console", "Clean Console"),
                     shinyBS::bsTooltip(id = "clean_console", title = "Clean the text output displayed to the right",
                                        placement = "right", trigger = "hover"),

                     actionButton("reset", "Reset Session"),
                     actionButton("reload_app", "Reload Page"),
                     shinyBS::bsTooltip(id = "reset", title = "Reset all options to values from the currently loaded template",
                                        placement = "right", trigger = "hover")

                 )
    ),
    mainPanel(
      tags$p("1. choose a template and locus in the Input section"),
      tags$p("2. modify any options in Input or Optional Arguments below"),
      tags$p("3. select to draw the plot, save as pdf or save current settings to excel on the left"),
      tags$p(em("A convenient website to easily design pleasing color palettes, with colorblind friendly options can be found "), a("here", href="https://coolors.co/"), em('or'), a("here", href="https://medialab.github.io/iwanthue/")),
      tags$p(strong("Optional arguments marked with [*] should be used with caution - only recommended for experienced users! Consult the vignette for more details.")),
      verbatimTextOutput("console"))
  ),
  tags$br(),

  # BODY SECTION ####
  fluidRow(
    column(
    12,
    offset = 0,
    navlistPanel(
      "Input",
      tabPanel(
        "Select Template and Locus",
        p(
          "Filling out the boxes in the 'Input' window is the minimum requirement for plotting. Multiple accessory arguments can be adjusted below, but plotting can be performed merely from importing the template excel file, specifying a locus by name or coordinates and pressing the 'Draw Plot' button (locus name overwrites coordinates)."
        ),
        fileInput(
          "input_file",
          "Load excel file (or IGV session xml file)",
          multiple = FALSE,
          accept = c(".xls", ".xlsx", ".xlsm", '.xml')
        ),
        verbatimTextOutput("File_import_msg"),
        tags$br(),
        splitLayout(
          cellWidths = c('180px', '50px', '180px'),
          textInput("gene", label = 'Enter locus name', placeholder = 'e.g. GAPDH'),
          #@
          p("  or  "),
          textInput("coordinates", label =
                      'Enter locus coordinates', placeholder = 'e.g. chr1:+:1000:2000')
        ),
        tags$p(em(
          "The name has to be present in one of the supplied annotations and match case. When entering name and coordinates simultaneously, only the name will be considered."
        )),
        tags$br(),
        create_input_element('verbosity')
      ),

      tabPanel(
        "Template Load Options",
        h4('Excel template and IGVsession import options'),
        spsComps::bsTooltip(
          checkboxInput("load_annotations", "Load annotations during import", value = TRUE),
          title='Load annotations already during import. This can take some time during session loading, but will save time when plotting.',
          placement ='right'),
        tags$br(),
        h4('IGVsession-specific import options'),
        spsComps::bsTooltip(
          radioButtons("igv_groupby", "Group igv-loaded samples to datasets using:",
                           choices = list('Do not group'='dont_group',
                                          'IGV autoscale groups'='autoscalegroups',
                                          'Common prefix in names'='common_prefix'
                                          ),
                           selected = 'dont_group'),
          title='Not every igv session is fully compatible with seqNdisplayR, but you should be able to load the session and then export to then modify the downloaded Excel sheet to create meaningful sample divisions, reload the Excel template and plot. The default "Do not group" option should be most robust, in particular together with setting plus,minus strand identifiers below as NULL.',
          placement ='right'),
        spsComps::bsTooltip(
          textInput("igv_strand_regex", "IGV-import: plus,minus strand identifiers",value = 'plus,minus'),
          title="Substring in bigwig file names used for strand assignment. Bigwig file names for plus/minus strand MUST be direct replacement of plus vs minus regex. If setting this option as NULL, will not try to assign strand. Together with Do not group option above this should work for most IGV session files.",
          placement ='right')
      ),


      # Sample Selection ####
      tabPanel(
        "Tracks Overview and Selection",
        p('You can reorder within each dataset, and select which samples to display. Reordering and selection are used for plotting by using the dataset-specific whichSamples parameter and this will also be saved in the Excel session like that. If you want to fully remove a sample, this has to be done in the Excel template. Reordering of datasets does not work for now. This has to be done in the Excel sheet. Note also that whichSamples is currently not respected when drawing the tree upon loading of an Excel template.'),
        h4('Tracks to plot:'),
        shinyTree::shinyTree("tree", checkbox=TRUE, dragAndDrop=TRUE, multiple = TRUE, animation = FALSE, themeIcons = FALSE)
      ),


      # Optional Args ####
      "Optional Arguments",
      tabPanel(
        "Plot Layout",
        tags$br(),
        column(
          10,
          offset = 0,
          create_input_element('dummy'),
          create_input_element('pdf_scale'),
          create_input_element('header_display'),
          create_input_element('header_name'),
          create_input_element('include_genomic_scale'),
          create_input_element('genomic_scale_on_top'),
          create_input_element('for_op'),
          create_input_element('bothstrands'),
          create_input_element('intermingled'),
          create_input_element('neg_as_neg'),
          create_input_element('true_strand'),
          create_input_element('plotting_segments'),
          create_input_element('plotting_segments_bottom'),
        )
      ),


      tabPanel(
        "Plot Display Parameters",
        tags$br(),
        column(
          10,
          offset = 0,
          create_input_element('feat_extend'),
          create_input_element('tracks_height'),
          create_input_element('title_field_height_cm'),
          create_input_element('genomic_scale_height_cm'),
          create_input_element('annotation_height_cm'),
          create_input_element('spacer_height'),
          create_input_element('tracks_width_in_cm'),
          create_input_element('margin_width_I'),
          create_input_element('tracks_name_width'),
          create_input_element('include_tracks_scale'),
          create_input_element('scale_per_space'),
          create_input_element('plot_height'),
          create_input_element('plot_width')
        )
      ),

      tabPanel(
        "Dataset-Specific Options",
        tags$br(),
        column(
          10,
          offset = 0,
          create_input_element('calcMean'),
          create_input_element('negative_valued_bw'),
          create_input_element('binning_type'),
          create_input_element('enhance_signal'),
          create_input_element('log2transform'),
          create_input_element('pseudoCount'),
          create_input_element('negValsSet0'),
          create_input_element('batch_correct')
        )
      ),

      tabPanel(
        "Panels Display",
        tags$br(),
        column(
          10,
          offset = 0,
          h4('Panel Text Display:'),
          create_input_element('panels_width_fixed'),
          create_input_element('panel_horizontal'),
          tags$head(
            tags$style(type="text/css", "#panel_horizontal_div {padding-left: 15px}")
          ),
          div(id = "panel_horizontal_div",
              h5("Display panel text horizontally:", style= 'font-weight: bold'),
              div(id = "panel_horizontal_checkboxes"),
              tags$br()),
          create_input_element('print_one_line_sample_names'),
          create_input_element('replicate_names'),
          create_input_element('pan_col'),
          create_input_element('panel_font_easy'),
          create_input_element('panel_font'),
          tags$br(),
          h4('Spacing, Background and Separators'),
          create_input_element('incl_first_panel'),
          create_input_element('horizon_space'),
          create_input_element('panel_separator'),
          create_input_element('sep_col'),
          create_input_element('sep_thick'),
          create_input_element('alternating_background_usage'),
          create_input_element('background_colors'),
          create_input_element('background_opacity'),
        )
      ),



      tabPanel(
        "Tracks Binning",
        tags$br(),
        column(
          10,
          offset = 0,
          p("Data binning concerns how coverage data is summarized into bins before plotting. By default optimal binning conditions are determined based on locus size but the individual binning settings can be changed below."),
          tags$br(),
          create_input_element('binning_size'),
          create_input_element('binning_start'),
          create_input_element('bins_cm')
        )
      ),


      tabPanel(
        "Header and Genomic Region",
        tags$br(),
        column(
          10,
          offset = 0,
          h4('Header'),
          create_input_element('header_font'),
          create_input_element('header_color'),
          h4('Genomic region'),
          create_input_element('gen_scal_font_col'),
          create_input_element('gen_scal_font')
        )
      ),

      tabPanel(
        "Data Scales Display",
        tags$br(),
        column(
          10,
          offset = 0,
          create_input_element('manual_scales'),
          create_input_element('scale_scientific_format'),
          create_input_element('scale_col'),
          create_input_element('scale_character_size')
        )
      ),

      tabPanel(
        "Annotation Display",
        tags$br(),
        column(
          10,
          offset = 0,
          create_input_element('annotation_compac'),
          create_input_element('annotations_names'),
          create_input_element('mass_centre'),
          create_input_element('feature_above'),
          create_input_element('feature_alternate'),
          create_input_element('locus_arrow'),
          create_input_element('which_annot_colors'),
          create_input_element('annotation_character_size'),
          create_input_element('annotation_character_color'),
          tags$br(),
          h4('Annotation Titles'),
          create_input_element('annot_col_name'),
          create_input_element('annot_font'),
          tags$br(),
          h4('Loci Shading'),
          create_input_element('incl_locus_colors'),
          create_input_element('feature_color'),
          create_input_element('feature_shading_alpha')
        )
      )
      )
    )
  )
)


# SERVER ####
server <- function(input, output, session) {

  current_session <- reactiveVal(NULL)
  current_session_fname <- reactiveVal('')
  textLog <- reactiveVal("")


  # reactive track selection tree display ####
  ## from list x creates nested node list for tree where all nodes are selected
  set_all_selected <- function(x) {
    if ( is.list(x) ) {
      structure(lapply(x, set_all_selected), stselected=T, stopened=TRUE)
    } else {
      names(x) <- x
      lapply(x, function(xi) structure(xi, stselected=T, stopened=TRUE))
    }
  }

  ## from list x creates nested node list for tree where all nodes are deselected
  set_all_deselected <- function(x) {
    if ( is.list(x) ) {
      structure(lapply(x, set_all_deselected), stselected=F, stopened=TRUE)
    } else {
      names(x) <- x
      structure(as.list(x), stselected=T, stopened=TRUE)
    }
  }

  ## from list x creates nested node list for tree where all nodes are selected if present in x
  set_tree_nodes <- function(samples, whichSamples) {
    if( is.list(samples) ) {
      print(paste0('list:', names(samples), ' -->', names(whichSamples)))
      nodes <- lapply(names(samples), function(dataset) {
        print(paste0(' dataset: ', dataset,
                     ' in whichSamples: ', (dataset %in% names(whichSamples))))#,
        if ( is.null(whichSamples[[dataset]]) ){
          print('  set sel')
          structure(set_all_selected(samples[[dataset]]),
                    stselected=T, stopened=T)
        } else if ( is.na(whichSamples[[dataset]]) ) {
          print('  set desel')
          structure(set_all_deselected(samples[[dataset]]),
                    stselected=F, stopened=T)
        } else if ( identical(unlist(samples[[dataset]]), unlist(whichSamples[[dataset]])) ){
          print('  set sel')
          structure(set_all_selected(samples[[dataset]]),
                    stselected=T, stopened=T)
        } else {
          print('  set custom')
          structure(set_tree_nodes(samples[[dataset]], whichSamples[[dataset]]),
                    stopened=T)
        }
      })
      names(nodes) <- names(samples)
      structure(nodes, stopened=T)
    } else {
      names(samples) <- samples
      lapply(samples, function(sample) structure(sample, stselected=(sample %in% whichSamples), stopened=TRUE))
    }
  }


  ## nested list of selected nodes from shinyTree t
  get_selected_tree <- function(t) {
    sel_t <- t
    if ( is.list(sel_t[[1]]) ) {
      l <- lapply(sel_t, get_selected_tree)
      sel <- !sapply(l, function(li) {is.null(li) | length(li) == 0})
      l <- l[sel]
      names(l) <- names(sel_t)[sel]
      l
    } else {
      sel_x <- sapply(t, function(ti) !is.null(attr(ti, "stselected")))
      if ( length(sel_x) == 0 ) {
        NULL
      } else {
        names(sel_t[sel_x])
      }
    }
  }

  observe({
    toggle(id = "panel_horizontal_div", condition = input$panel_horizontal)
  })


  # set all values in ui elements to the ones from a seqNdisplayR session
  update_ui_to_session <- function(seqNdisplayR_session) {
    global_options <- options_table[options_table$option_group == 'global',]
    for ( i in 1:nrow(global_options) ) {
      opt_line <- global_options[i,]
      opt <- global_options$option_name[i]
      if ( opt %in% names(seqNdisplayR_session) ) {
        if ( opt_line$option_class == 'bool' ) {
          updateCheckboxInput(session, opt_line$shiny_varname, value = seqNdisplayR_session[[opt]])
        } else if ( opt_line$option_class == 'text' ) {
          updateTextInput(session, opt_line$shiny_varname, value = seqNdisplayR::deparse_option(seqNdisplayR_session[[opt]]))
        } else if ( opt_line$option_class == 'numeric' ) {
          updateNumericInput(session, opt_line$shiny_varname, value = seqNdisplayR_session[[opt]])
        }
      }
    }

    output$tree <- shinyTree::renderTree( {
      #set_all_selected(seqNdisplayR_session$samples) #TODO: Fix to select only whichSamples
      whichSamples <- lapply(seqNdisplayR_session$parameters, function(p) p$whichSamples)
      names(whichSamples) <- names(seqNdisplayR_session$parameters)
      set_tree_nodes(seqNdisplayR_session$samples,
                     whichSamples)
      #set_all_deselected(seqNdisplayR_session$bigwigs[['+']])
    } )

    dataset_names <- names(seqNdisplayR_session$samples)
    dataset_options <- options_table[options_table$option_group == 'dataset_option',]
    for ( i in 1:nrow(dataset_options) ) {
      opt_line <- dataset_options[i,]
      opt <- dataset_options$option_name[i]
      para <- lapply(dataset_names, function(d) seqNdisplayR_session$parameters[[d]][[opt]])
      names(para) <- dataset_names
      if ( opt_line$option_class == 'bool' ) {
        updateCheckboxGroupInput(session,
                                 opt_line$shiny_varname,
                                 choices = character(0),
                                 selected = character(0))

        updateCheckboxGroupInput(session,
                                 opt_line$shiny_varname,
                                 choices = names(para),
                                 selected = names(para)[unlist(para)])
      } else {

        ## idea is to remove for each sample name and line with sample name and parameter
        ## entries from previous loaded template are removed (except one, ie anchor)
        ## this anchor (or the dummy created before template loading) serve as anchor to add sample-lines below
        ## after this the anchor is removed

        #shinyCatch({message(paste0('@: ', opt_line$shiny_varname, '\n'))},shiny=F)

        shiny_elems <- names(input)[grepl(paste0(opt_line$shiny_varname, '_'),names(input)) | grepl(paste0(opt_line$shiny_varname, 'xvalue_'),names(input))]

        if ( length(shiny_elems) != 0 ) {

          for ( elem in shiny_elems[1:length(shiny_elems)] ) {
            #shinyCatch({message(paste0('deleting UI element: ', elem, '\n'))},shiny=F)
            removeUI(selector = paste0('#', elem))
            if(grepl('xvalue', elem)){
              removeUI(selector = paste0('#', sub('xvalue', '', elem)))
            }
          }
        }

        anchor_elem <- opt_line$shiny_varname

        ##insert one text input per sample name
        for ( name in rev(names(para)) ) {
          #shinyCatch({message(paste0('   adding: ', name, '\n'))},shiny=F)
          if ( opt_line$option_class == 'text' ) {
            insertUI(
              selector = paste0('#', anchor_elem),
              where = "afterEnd",
              ui = tags$div(id=paste0(opt_line$shiny_varname, '_', name),
                            textInput(paste0(opt_line$shiny_varname, 'xvalue_', name),
                             label = name,
                             value = deparse_option(para[[name]])
              ))
            )
          } else if ( opt_line$option_class == 'numeric' ) {
            insertUI(
              selector = paste0('#', anchor_elem),
              where = "afterEnd",
              ui = tags$div(id=paste0(opt_line$shiny_varname, '_', name),
                            numericInput(paste0(opt_line$shiny_varname, 'xvalue_', name),
                             label = name,
                             value = deparse_option(para[[name]]))
              )
            )
          } else if ( opt_line$option_class == 'text_choices' ) {
            opt_choices = strsplit(opt_line$option_options,',')[[1]]
            insertUI(
                selector = paste0('#', anchor_elem),
                where = "afterEnd",
                ui = tags$div(radioButtons(inputId = paste0(opt_line$shiny_varname,'_',name),
                                  label = name,
                                  choices = opt_choices,
                                  selected = deparse_option(para[[name]]),
                                  inline=TRUE)))
          }
        }

        #hide the anchor
        shinyjs::hide(id = anchor_elem)
        #shinyCatch({message(paste0('hiding anchor UI element: ', anchor_elem, '\n'))},shiny=F)
      }
    }


    #### panels horizontal special case of expandable option upon enable

    #remove previous exisiting checkboxGroups
    shiny_elems <- names(input)[grepl('panel_horizontal_',names(input))]
    for ( elem in shiny_elems ) {
      removeUI(selector = paste0('#', elem))
    }

    for ( name in rev(dataset_names) ) {
      dataset_group_depth = ListDepth(seqNdisplayR_session$samples[[name]]) + 1
      levels=c('dataset')
      if( dataset_group_depth > 0 ) {
        levels=c(levels, paste0('subgroup_', 1:dataset_group_depth))
      }

      insertUI(
        selector = '#panel_horizontal_checkboxes',
        where = "afterEnd",
        ui = checkboxGroupInput(
          paste0('panel_horizontal_', name),
          label = name,
          choices = levels,
          selected = levels
        )
      )
    }


    #### annotation options specific to each annotation (all booleans for now)
    anno_options <- options_table[options_table$option_group == 'annotation_option',]
    for ( i in 1:nrow(anno_options) ) {
      opt_line <- anno_options[i,]
      opt <- anno_options$option_name[i]
      opts <- seqNdisplayR_session[[opt]]

      if ( length(names(opts)) == 0 ) {
        opts <- opt_line$option_default
        names(opts) <- 'NA'
      }


      if ( opt_line$option_class == 'bool' ) {
        updateCheckboxGroupInput(session,
                                 opt_line$shiny_varname,
                                 choices = names(opts),
                                 selected = names(opts)[unlist(opts)])
      } else {

        ## idea is to remove for each sample name and line with sample name and parameter
        ## entries from previous loaded template are removed (except one, ie anchor)
        ## this anchor (or the dummy created before template loading) serve as anchor to add sample-lines below
        ## after this the anchor is removed
        shiny_elems <- names(input)[grepl(paste0(opt_line$shiny_varname, '_'),names(input)) | grepl(paste0(opt_line$shiny_varname, 'xvalue_'),names(input))]

        if ( length(shiny_elems) != 0 ) {

          for ( elem in shiny_elems[1:length(shiny_elems)] ) {
            #shinyCatch({message(paste0('deleting UI element: ', elem, '\n'))},shiny=F)
            removeUI(selector = paste0('#', elem))
            if( grepl('xvalue', elem) ){
              removeUI(selector = paste0('#', sub('xvalue', '', elem)))
            }
          }
        }

        anchor_elem <- opt_line$shiny_varname

        ##insert one text input per annotation name
        for ( name in names(opts) ) {
          if ( opt_line$option_class == 'text' ) {
            insertUI(
                selector = paste0('#', anchor_elem),
                where = "afterEnd",
                ui = tags$div(id=paste0(opt_line$shiny_varname, '_', name),
                              textInput(paste0(opt_line$shiny_varname, 'xvalue_', name),
                               label = name,
                               value = deparse_option(opts[[name]]))
                )
              )
          } else if ( opt_line$option_class == 'numeric' ) {
            insertUI(
              selector = paste0('#', anchor_elem),
              where = "afterEnd",
              ui = tags$div(id=paste0(opt_line$shiny_varname, '_', name),
                            numericInput(paste0(opt_line$shiny_varname, 'xvalue_', name),
                                         label = name,
                                         value = deparse_option(opts[[name]]))
              )
            )
          } else if ( opt_line$option_class == 'text_choices' ) {
            opt_choices = strsplit(opt_line$option_options,',')[[1]]
            insertUI(
              selector = paste0('#', anchor_elem),
              where = "afterEnd",
              ui = tags$div(radioButtons(inputId = paste0(opt_line$shiny_varname,'_',name),
                                         label = name,
                                         choices = opt_choices,
                                         selected = deparse_option(opts[[name]]),
                                         inline=TRUE)))
          }
        }
        #remove the anchor
        #shinyCatch({message(paste0('deleting UI element (anchor): ', anchor_elem, '\n'))},shiny=F)
        shinyjs::hide(id = anchor_elem)
      }
    }

  }



  get_selected_samples <- function(){
    tree <- input[['tree']]
    #ups the tree cannot be selected without first being shown
    # a decision made in shinyTree package we use for the tree
    if ( is.null(tree) ) {
      current_session()$samples
    } else {
      get_selected_tree(tree)
    }
  }#)



  # reactive excel or igv template load ####
  load_template <- reactive({
    filename <- input$input_file$name
    if ( is.null(filename) ) {
      return(NULL)
    }
    if ( !(grepl('.xml$', filename) | grepl('.xls$', filename) | grepl('.xlsx$', filename))  ) {
      output$console <- renderText({'Please provide valid template file in .xls, .xlsx or igv session .xml file'})
      return(NULL)
    }
    if ( current_session_fname() != filename ) {
      fname <- input$input_file$datapath[1]


      show_modal_spinner(spin='circle', text='Loading and parsing template')
      current_session_fname('')
      current_session(NULL)
      loaded_session <- NULL

      shinyCatch({
        if ( grepl('.xml$', fname)  ) {
          if ( is.null(input$igv_strand_regex) ) {
            igv_strand_regex <- NULL
          } else {
            igv_strand_regex <- list('+'=sub(",.*","", input$igv_strand_regex),
                                     '-'=sub(".*,","", input$igv_strand_regex))
          }

          loaded_session <- seqNdisplayR::load_igvsession(fname,
                                                       group_by = input$igv_groupby,
                                                       strand_regex = igv_strand_regex)
        } else {
          x <- capture.output(loaded_session <- seqNdisplayR::load_excel(fname, load_annotations = input$load_annotations))
          output$console <- renderText({paste(x, collapse  = "\n")})
        }

        #fill all option field with values from session
        update_ui_to_session(loaded_session)

        #set reactive val current_session to the loaded session!
        current_session_fname(filename)
        current_session(loaded_session)
      },position = 'top-center',blocking_level = 'none',shiny=T)

      remove_modal_spinner()

      #remember to return the session
      loaded_session
    } else {
      current_session()
    }
  }
  )


  #status message for import
  # Note: this also ensures that the session is loaded automatically!
  output$File_import_msg <- renderText(
    if( !is.null(load_template()) ) {
      if ( current_session_fname() == '' | is.null(current_session()) ){
        'Please load a valid session file.'
      } else {
        paste0('from ', current_session_fname(), '\n',
               'loaded session with datasets: ',
               paste(names(current_session()$samples), collapse = ', '),
               '\n')
      }
    }
  )



  # feature and locus retrieval
  get_feature <- reactive(input$gene)

  get_locus <- reactive({
    coord <- input$coordinates

    if ( coord != '' ) {
      strsplit(coord, ':')[[1]]
    } else {
      coord
    }

  })



  # get all setting, options and parameters if changed in shiny ####
  ## called ie before plot or save
  get_shiny_global_options <- reactive({
    opts <- options_table$option_name[options_table$option_group == 'global']

    l <- lapply(opts, function(opt) {
      opt_line <- options_table[options_table$option_name == opt,]
      shiny_varname <- opt_line$shiny_varname
      if ( shiny_varname %in% names(input) ) {
        value <- input[[shiny_varname]]
        if ( opt_line$option_class == 'text' ) {
          value <- seqNdisplayR::parse_option(value)
        }
        value
      } else {
        NULL
      }
    })
    names(l) <- opts

    ## special case for plotting_segment_order if bottom specified
    if (!is.null(l$plotting_segment_order_bottom)){
     l$plotting_segment_order = list('+'=l$plotting_segment_order, '-'=l$plotting_segment_order_bottom)
    }

    ## special case for horizontal_panels_list if panel_horizontal is enabled
    if ( l$horizontal_panels_list ){
      l$horizontal_panels_list <- list()
      shiny_chkbxgrps <- names(input)[grepl('panel_horizontal_', names(input))]

      # one for each dataset
      for ( elem in shiny_chkbxgrps ) {
        #checkbox for each level
        dataset_name = sub('panel_horizontal_', '', elem)

        ## get available levels in this dataset
        dataset_group_depth = ListDepth(current_session()$samples[[dataset_name]]) + 1
        available_levels=c('dataset')
        if( dataset_group_depth > 0 ) {
          available_levels=c(available_levels, paste0('subgroup_', 1:dataset_group_depth))
        }

        ## get check levels in this dataset
        checked_levels = input[[elem]]

        ## return boolena if checked
        l$horizontal_panels_list[[dataset_name]] = (available_levels %in% checked_levels)

      }
    } else {
      l[names(l) == 'horizontal_panels_list'] = list(NULL)
    }

    l

  })

  # get dataset options from shiny ####
  get_shiny_dataset_options <- reactive({
    opts <- options_table$option_name[options_table$option_group == 'dataset_option']
    datasets <- names(current_session()$samples)
    l <- lapply(opts, function(opt) {
      opt_line <- options_table[options_table$option_name == opt,]
      shiny_varname <- opt_line$shiny_varname
        if ( opt_line$option_class == 'bool' ) {
          selected <- input[[shiny_varname]]
          res <- (datasets %in% selected)
          names(res) <- datasets
          res
        } else if(opt_line$option_class == 'text_choices') {
          #text_elems <- names(input)[grepl(paste0(opt_line$shiny_varname,'_'), names(input))]
          res <- sapply(datasets,
                 function(dataset) {
                   parse_option(input[[paste0(opt_line$shiny_varname,'_',dataset)]])
                 })
          names(res) <- datasets
          res
        } else  {
          #text_elems <- names(input)[grepl(paste0(opt_line$shiny_varname,'xvalue_'), names(input))]
          res <- sapply(datasets,
                        function(dataset) {
                          parse_option(input[[paste0(opt_line$shiny_varname,'xvalue_',dataset)]])
                        })
          names(res) <- datasets
          res
        }
      })

    names(l) <- opts
    l
  })


  # get annotation options from shiny ####
  get_shiny_annotation_options <- reactive({
    opts <- options_table$option_name[options_table$option_group == 'annotation_option']
    anno_names <- names(current_session()$annots)
    if( length(current_session()$annots) == 0 ){
      return(NULL)
    }
    l <- lapply(opts, function(opt) {
      opt_line <- options_table[options_table$option_name == opt,]
      shiny_varname <- opt_line$shiny_varname
      if ( opt_line$option_class == 'bool' ) {
        checked <- input[[shiny_varname]]
        res <- (anno_names %in% checked)
        names(res) <- anno_names
        res
      } else if(opt_line$option_class == 'text_choices') {
        #text_elems <- names(input)[grepl(paste0(opt_line$shiny_varname,'_'), names(input))]
        res <- sapply(anno_names,
                      function(anno) {
                        parse_option(input[[paste0(opt_line$shiny_varname,'_',anno)]])
                      })
        names(res) <- anno_names
        res
      } else {
        #text_elems <- names(input)[grepl(paste0(opt_line$shiny_varname,'xvalue_'), names(input))]
          res <- sapply(anno_names,
                 function(anno) {
                   parse_option(input[[paste0(opt_line$shiny_varname,'xvalue_',anno)]])
                 })
          names(res) <- anno_names
          res
      }
    })

    names(l) <- opts
    l
  })



  seqNdisplayR_session <- reactive({
    template_session <- load_template()
    if (length(template_session$annots) == 0) {
      template_session['annots'] <- list(NULL)
    }

    shiny_session_global_options <- get_shiny_global_options()

    op_names <- names(shiny_session_global_options)
    template_session[op_names] <- shiny_session_global_options[op_names]

    shiny_session_dataset_options <- get_shiny_dataset_options()
    #template_session$force_scale <- shiny_session_dataset_options$force_scale
    #shiny_session_dataset_options$force_scale <- NULL
    #template_session$force_scale <- NULL
    for ( sample_name in names(template_session$parameters) ) {
      for ( op in names(shiny_session_dataset_options) ) {
        opt <- shiny_session_dataset_options[[op]]
        if ( sample_name %in% names(opt) ) {
          template_session$parameters[[sample_name]][[op]] <- opt[[sample_name]]
        } else {
          template_session$parameters[[sample_name]][[op]] <- FALSE
        }
      }
    }

    shiny_session_annotation_options <- get_shiny_annotation_options()
    if( !is.null(shiny_session_annotation_options) ) {
      op_names <- names(shiny_session_annotation_options)
      template_session[op_names] <- shiny_session_annotation_options[op_names]
    }

    ## which samples from tree
    which_samples <- get_selected_samples()
    for ( sample_name in names(template_session$parameters) ) {
      if ( !(sample_name %in% names(which_samples)) ) {
        #exclude all
        template_session$parameters[[sample_name]][['whichSamples']] <- NA
      } else if (identical(which_samples[[sample_name]], template_session$samples[[sample_name]])){
        #include all
        template_session$parameters[[sample_name]]['whichSamples'] <- list(NULL)
      } else { #@
        #include specific cases
        template_session$parameters[[sample_name]][['whichSamples']] <- which_samples[[sample_name]]
      }
    }

    output$console <- renderText({textLog()})

    template_session
  })




  #create plot when hitting plot button
  observeEvent(input$plot,
               {
                 if (is.null(input$input_file)) {
                   "Please load Excel or IGV template and provide locus name or coordinates."
                 } else {
                   feature <- get_feature()
                   locus <- get_locus()
                   if (feature == '' & locus[1] == '') {
                     output$console <- renderText({"Please provide locus name or coordinates for region to be plotted."})
                   } else {
                     session_to_plot <- seqNdisplayR_session()

                     show_modal_spinner(spin='circle', text='Creating plot, this can take some time. The plot will appear in a different window')
                     x <- 'Plotting failed, please check your settings'
                     spsComps::shinyCatch({
                       if (feature != '') {
                         x <- capture.output(plot(session_to_plot, feature=feature, interface='shiny'))
                       } else if (locus[1] != '') {
                         x <- capture.output(plot(session_to_plot, locus=locus, interface='shiny'))
                       }
                     },position = 'top-center',blocking_level='none', prefix='Plotting error', shiny=TRUE)
                     output$console <- renderText({paste(x, collapse  = "\n")})
                     remove_modal_spinner()
                     }
                   }
                 })


  ## save to pdf --> when hitting save button
  output$save_pdf <- downloadHandler(
    filename = function() {
      feature <- get_feature()
      locus <- get_locus()
      if ( feature == ''  & locus[1] == '' ) {
         output$console <- renderText({"Please provide locus name or coordinates for region to be plotted."})
      }
      paste0("seqNdisplayR_", Sys.Date(), '_', feature, locus, ".pdf")
    },
    content = function(file) {
      if (is.null(input$input_file)) {
        "Please load Excel or IGV template and provide locus name or coordinates."
      } else {
        feature <- get_feature()
        locus <- get_locus()
        if (feature == '' & locus[1] == '') {
          output$console <- renderText({"Please provide locus name or coordinates for region to be plotted."})
        } else {
          session_to_plot <- seqNdisplayR_session()

          show_modal_spinner(spin='circle', text='Creating plot, this can take some time. The plot will appear in a different window')
          x <- 'plotting failed, please check your settings'
          spsComps::shinyCatch({
            if (feature != '') {
              x <- capture.output(plot(session_to_plot, feature=feature, interface='shiny',
                                       pdf = TRUE,
                                       pdf_name = sub('.pdf', '', basename(file)),
                                       pdf_dir = dirname(file)))
            } else if (locus[1] != '') {
              x <- capture.output(plot(session_to_plot, locus=locus, interface='shiny',
                                       pdf = TRUE,
                                       pdf_name = sub('.pdf', '', basename(file)),
                                       pdf_dir = dirname(file)))
            }
          },position = 'top-center',blocking_level='none', prefix='Plotting error', shiny=TRUE)
          output$console <- renderText({paste0('pdf creation log:\n', paste(x, collapse  = "\n"))})
          remove_modal_spinner()
        }
      }
    }
  )



  ## save settings to excel file --> when hitting save button
  output$save_settings <- downloadHandler(
    filename = function() {
      paste0("seqNdisplayRsession", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if ( is.null(input$input_file) ) {
        output$console <- renderText("Please first load Excel or IGV template")
      } else {
        loaded_session <- seqNdisplayR_session()
        seqNdisplayR::session_to_xlsx(loaded_session, path = file)
      }
    }
  )


  # TO DO: remove debugging buttons
  observeEvent(input$show_settings, {
    shiny_session_global_options <- get_shiny_global_options()
    shiny_session_annotation_options <- get_shiny_annotation_options()
    if ( !is.null(shiny_session_annotation_options) ) {
      opt_names <- c(names(shiny_session_global_options), names(shiny_session_annotation_options))
    } else {
      opt_names <- names(shiny_session_global_options)
    }

    cur_session <- seqNdisplayR_session()

    output$console <- renderText(
      paste(
        lapply(intersect(opt_names,names(cur_session)),
               function(i) paste0(i, '  -->  ', seqNdisplayR::deparse_option(cur_session[[i]]),  '\t', class(cur_session[[i]]))  # , '\n'
        ),
        collapse ='\n')
    )
  })

  observeEvent(input$show_parameters, {
    template_session <- load_template()
    shiny_session_dataset_options <- get_shiny_dataset_options()

    textLog('dataset options:\n')
    for ( para in names(shiny_session_dataset_options) ) {
      textLog(paste0(textLog(), para, '\n'))
      for ( sample_name in names(shiny_session_dataset_options[[para]]) ) {
        textLog(paste0(textLog(), '  ', sample_name, ': ', seqNdisplayR::deparse_option(shiny_session_dataset_options[[para]][[sample_name]]), '\n'))
      }
    }
    output$console <- renderText( textLog() )
  })

  observeEvent(input$show_samples, {
    output$console  <- renderPrint({
      current_session()$samples
    })
  })


  observeEvent(input$which_samples, {
    output$console  <- renderPrint({
      get_selected_samples()
    })
  })

  observeEvent(input$clean_console, {
    output$console = renderText('')
  })


  observeEvent(input$reset, {
    current_session_fname('')
    load_template()
    output$console  <- renderPrint('All values were reset to template values')
  })

  observeEvent(input$reload_app, {
    refresh()
  })


}


# Run the application
shinyApp(ui = ui, server = server, options = list(width = 1200, height = 2000)) #@ keep an eye on options
