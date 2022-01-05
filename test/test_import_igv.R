

#igvsession_fname <- '../Shiny App V7 test/random_stuf_for_shiny_test_server.xml'
#igvsession_fname <- '../Shiny App V7 test/random_stuf_for_shiny_test.xml'
#igvsession_fname <- '../Shiny App V7 test/random_stuf_for_shiny_test_server.xml'
#igvsession_fname <- '~/Downloads/randomstuftotestIGVinshiny.xml'

#igvsession_fname <- 'inst/extdata/example_igv_session.xml'
igvsession_fname <-
  system.file('extdata','example_igv_session.xml',package='Seq2PlotR')



## group into seqtypes using autoscale group info from IGV
session <- load_igvsession( igvsession_fname, group_by = 'autoscalegroups' )

session

print(session, verbose=T)

plot(session, feature='Gapdh')

Seq2PlotR::session_to_xlsx(session, path='test/test_output/igv_loaded_autoscalegroups.xlsx')


## group into seqtypes using common prefix in names of tracks in IGV session
session <- load_igvsession( igvsession_fname, group_by = 'common_prefix' )

print(session, verbose=T)

plot(session, feature='Gapdh')

Seq2PlotR::session_to_xlsx(session, path='~/Downloads/igv_loaded_common_prefix.xlsx')

## no grouping (the default)
session <- load_igvsession( igvsession_fname, group_by = '' )

session

plot(session, feature='Gapdh')

Seq2PlotR::session_to_xlsx(session, path='~/Downloads/igv_loaded_nogroups.xlsx')


## no grouping and no strand assigment
session <- load_igvsession( igvsession_fname, group_by = '', strand_regex = NULL )

session

plot(session, feature='Gapdh')

Seq2PlotR::session_to_xlsx(session, path='~/Downloads/igv_loaded_nogroups.xlsx')



##modify this session, easiest in table format
modified_session_tbl <- as.data.frame(session) %>%
  dplyr::mutate(
    full_name = dataset,
    #dummy container for full name
    dataset = dplyr::case_when(
      grepl('CAGE', full_name) ~ "CAGE",
      grepl('RNAseq', full_name) ~ "RNAseq",
      grepl('3.seq', full_name) ~ "3'seq",
      grepl('ChIPSeq', full_name) ~ "ChIPSeq"
    ),
    subgroup_1 = dplyr::case_when(
      grepl('ChIPSeq', full_name) ~ sub('ChIPSeq ', '',  full_name),
      grepl('mm', full_name) ~ 'multimappers',
      !grepl('mm', full_name) ~ 'uniquemappers'
    ),
    subgroup_2 = dplyr::case_when(
      grepl('CAGE', full_name) ~ sub('.*CAGE ', '',  full_name),
      grepl('RNAseq', full_name) ~ sub('.*RNAseq ', '',  full_name),
      grepl('3.seq', full_name) ~ sub('.*_', '',  full_name)
    ),
    subgroup_3 = dplyr::case_when(grepl('3.seq', full_name) ~ sub('_.*', '', sub(
      '.*3.seq ', '',  full_name
    )))
  ) %>%
  dplyr::select(-full_name)


modified_session <- Seq2PlotRSession(modified_session_tbl, annotations = session$annots)
plot(modified_session, feature='Gapdh')
plot(modified_session, feature='Gapdh', dummy_plot=T)
