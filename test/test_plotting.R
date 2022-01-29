# TEST data information

## sequencing experiments
#seqtypes = c("TT-seq", "RNA-seq", "3'-seq", "ChIP-seq")

#seqtypes = c("TT-seq", "RNA-seq", "3'-seq")

## samples under each sequencing experiment
# single- or multi-level experiments (number of levels will determine number of panes with sample name designations in the plot - will be overruled by the number of levels supplied by whichSamples)
samples = list("TT-seq" = c('CTRL', 'ARS2', 'CPSF73', 'INTS11', 'RRP40'),
               "RNA-seq" = c('CTRL', 'ARS2', 'CPSF73', 'INTS11', 'RRP40'),
               "3'-seq" = list('noPAP' = list('WT'=c('0h', '2h', '6h'),
                                              'MTR4'=c('0h', '2h', '6h'),
                                              'RRP40'=c('0h', '2h', '6h'),
                                              'ZCCHC8'=c('0h', '2h', '6h'),
                                              'ZFC3H1'=c('0h', '2h', '6h')
               ),
               'EPAP' = list('WT'=c('0h', '2h', '6h'),
                             'MTR4'=c('0h', '2h', '6h'),
                             'RRP40'=c('0h', '2h', '6h'),
                             'ZCCHC8'=c('0h', '2h', '6h'),
                             'ZFC3H1'=c('0h', '2h', '6h')
               )
               ),
               'ChIP-seq'=c('INTS11')
)


# samples = list("TT-seq" = c('CTRL', 'ARS2', 'CPSF73', 'INTS11', 'RRP40'),
#                "RNA-seq" = c('CTRL', 'ARS2', 'CPSF73', 'INTS11', 'RRP40'),
#                "3'-seq" = list('noPAP' = list('WT'=c('0h', '2h', '6h'),
#                                               'MTR4'=c('0h', '2h', '6h'),
#                                               'RRP40'=c('0h', '2h', '6h'),
#                                               'ZCCHC8'=c('0h', '2h', '6h'),
#                                               'ZFC3H1'=c('0h', '2h', '6h')
#                                               ),
#                                'EPAP' = list('WT'=c('0h', '2h', '6h'),
#                                               'MTR4'=c('0h', '2h', '6h'),
#                                               'RRP40'=c('0h', '2h', '6h'),
#                                               'ZCCHC8'=c('0h', '2h', '6h'),
#                                               'ZFC3H1'=c('0h', '2h', '6h')
#                                              )
#                                )
#                )

# colors - same structure as samples one level deeper

colors = list("TT-seq" = c('CTRL'='#5E5760', 'ARS2'='#84A982', 'CPSF73'='#C7164C', 'INTS11'='#1989B6', 'RRP40'='#E2856A'),
              "RNA-seq" = c('CTRL'='#5E5760', 'ARS2'='#84A982', 'CPSF73'='#C7164C', 'INTS11'='#1989B6', 'RRP40'='#E2856A'),
              "3'-seq" = list('noPAP' = list('WT'=c('0h'='#5D5D5D', '2h'='#5D5D5D', '6h'='#5D5D5D'),
                                             'MTR4'=c('0h'='#AB3068', '2h'='#AB3068', '6h'='#AB3068'),
                                             'RRP40'=c('0h'='#6061A9', '2h'='#6061A9', '6h'='#6061A9'),
                                             'ZCCHC8'=c('0h'='#469B8D', '2h'='#469B8D', '6h'='#469B8D'),
                                             'ZFC3H1'=c('0h'='#E19247', '2h'='#E19247', '6h'='#E19247')
              ),
              'EPAP' = list('WT'=c('0h'='#5D5D5D', '2h'='#5D5D5D', '6h'='#5D5D5D'),
                            'MTR4'=c('0h'='#AB3068', '2h'='#AB3068', '6h'='#AB3068'),
                            'RRP40'=c('0h'='#6061A9', '2h'='#6061A9', '6h'='#6061A9'),
                            'ZCCHC8'=c('0h'='#469B8D', '2h'='#469B8D', '6h'='#469B8D'),
                            'ZFC3H1'=c('0h'='#E19247', '2h'='#E19247', '6h'='#E19247')
              )
              ),
              'ChIP-seq'=c('INTS11'='#000000')
)

# colors = list("TT-seq" = c('CTRL'='#5E5760', 'ARS2'='#84A982', 'CPSF73'='#C7164C', 'INTS11'='#1989B6', 'RRP40'='#E2856A'),
#                "RNA-seq" = c('CTRL'='#5E5760', 'ARS2'='#84A982', 'CPSF73'='#C7164C', 'INTS11'='#1989B6', 'RRP40'='#E2856A'),
#                "3'-seq" = list('noPAP' = list('WT'=c('0h'='#5D5D5D', '2h'='#5D5D5D', '6h'='#5D5D5D'),
#                                               'MTR4'=c('0h'='#AB3068', '2h'='#AB3068', '6h'='#AB3068'),
#                                               'RRP40'=c('0h'='#6061A9', '2h'='#6061A9', '6h'='#6061A9'),
#                                               'ZCCHC8'=c('0h'='#469B8D', '2h'='#469B8D', '6h'='#469B8D'),
#                                               'ZFC3H1'=c('0h'='#E19247', '2h'='#E19247', '6h'='#E19247')
#                                               ),
#                                'EPAP' = list('WT'=c('0h'='#5D5D5D', '2h'='#5D5D5D', '6h'='#5D5D5D'),
#                                              'MTR4'=c('0h'='#AB3068', '2h'='#AB3068', '6h'='#AB3068'),
#                                              'RRP40'=c('0h'='#6061A9', '2h'='#6061A9', '6h'='#6061A9'),
#                                              'ZCCHC8'=c('0h'='#469B8D', '2h'='#469B8D', '6h'='#469B8D'),
#                                              'ZFC3H1'=c('0h'='#E19247', '2h'='#E19247', '6h'='#E19247')
#                                              )
#                                )
#                )


# directories for bigwigs - local or http
bigwig_dirs = c("TT-seq" = 'http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Human/THJ/2017_Molska_TT-seq_RNAseq/TT-seq/hg38/',
                "RNA-seq" = 'http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Human/THJ/2017_Molska_TT-seq_RNAseq/RNA-seq/hg38/',
                "3'-seq" =  'http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Human/THJ/2019_Gockert_3pseq/hg38/NORM_BIGWIG/',
                "ChIP-seq" = 'http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Human/GEO/GSE125534_Beckedorff_(Shiekhattar)_2020_nuclear-ncRNA-seq_RNA-seq_CAGE-seq_ChIP-seq_ATAC-seq/ChIP-seq/hg38/')


# bigwig_dirs = c("TT-seq" = 'http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Human/THJ/2017_Molska_TT-seq_RNAseq/TT-seq/hg38/',
#                 "RNA-seq" = 'http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Human/THJ/2017_Molska_TT-seq_RNAseq/RNA-seq/hg38/',
#                 "3'-seq" =  'http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Human/THJ/2019_Gockert_3pseq/hg38/NORM_BIGWIG/')


# bigwig file names and their relation to supplied sample names
# plus
bigwigs_plus = list("TT-seq" = list('CTRL'=c('L_EGFP_rep1_tt_corr_ff_noJncReads_plus.bw', 'L_EGFP_rep2_tt_corr_ff_noJncReads_plus.bw'),
                                    'ARS2'=c('L_ARS2_rep1_tt_corr_ff_noJncReads_plus.bw', 'L_ARS2_rep2_tt_corr_ff_noJncReads_plus.bw'),
                                    'CPSF73'=c('L_CPSF73_rep1_tt_corr_ff_noJncReads_plus.bw', 'L_CPSF73_rep2_tt_corr_ff_noJncReads_plus.bw'),
                                    'INTS11'=c('L_INTS11_rep1_tt_corr_ff_noJncReads_plus.bw', 'L_INTS11_rep2_tt_corr_ff_noJncReads_plus.bw'),
                                    'RRP40'=c('L_RRP40_rep1_tt_corr_ff_noJncReads_plus.bw', 'L_RRP40_rep2_tt_corr_ff_noJncReads_plus.bw')
),
"RNA-seq" = list('CTRL'=c('T_EGFP_rep1_tt_corr_plus.bw', 'T_EGFP_rep2_tt_corr_plus.bw'),
                 'ARS2'=c('T_ARS2_rep1_tt_corr_plus.bw', 'T_ARS2_rep2_tt_corr_plus.bw'),
                 'CPSF73'=c('T_CPSF73_rep1_tt_corr_plus.bw', 'T_CPSF73_rep2_tt_corr_plus.bw'),
                 'INTS11'=c('T_INTS11_rep1_tt_corr_plus.bw', 'T_INTS11_rep2_tt_corr_plus.bw'),
                 'RRP40'=c('T_RRP40_rep1_tt_corr_plus.bw', 'T_RRP40_rep2_tt_corr_plus.bw')
),
"3'-seq" = list('noPAP' = list('WT'=list('0h'=c('WT_0h_noPAP_1_plus.bw'), '2h'=c('WT_2h_noPAP_1_plus.bw'), '6h'=c('WT_6h_noPAP_1_plus.bw')),
                               'MTR4'=list('0h'=c('MTR4_0h_noPAP_1_plus.bw', 'MTR4_0h_noPAP_2_plus.bw'), '2h'=c('MTR4_2h_noPAP_1_plus.bw', 'MTR4_2h_noPAP_2_plus.bw'), '6h'=c('MTR4_6h_noPAP_1_plus.bw', 'MTR4_6h_noPAP_2_plus.bw')),
                               'RRP40'=list('0h'=c('RRP40_0h_noPAP_1_plus.bw', 'RRP40_0h_noPAP_2_plus.bw'), '2h'=c('RRP40_2h_noPAP_1_plus.bw', 'RRP40_2h_noPAP_2_plus.bw'), '6h'=c('RRP40_6h_noPAP_1_plus.bw', 'RRP40_6h_noPAP_2_plus.bw')),
                               'ZCCHC8'=list('0h'=c('ZCCHC8_0h_noPAP_1_plus.bw', 'ZCCHC8_0h_noPAP_2_plus.bw'), '2h'=c('ZCCHC8_2h_noPAP_1_plus.bw', 'ZCCHC8_2h_noPAP_2_plus.bw'), '6h'=c('ZCCHC8_6h_noPAP_1_plus.bw', 'ZCCHC8_6h_noPAP_2_plus.bw')),
                               'ZFC3H1'=list('0h'=c('ZFC3H1_0h_noPAP_1_plus.bw', 'ZFC3H1_0h_noPAP_2_plus.bw'), '2h'=c('ZFC3H1_2h_noPAP_1_plus.bw', 'ZFC3H1_2h_noPAP_2_plus.bw'), '6h'=c('ZFC3H1_6h_noPAP_1_plus.bw', 'ZFC3H1_6h_noPAP_2_plus.bw'))
),
'EPAP' = list('WT'=list('0h'=c('WT_0h_EPAP_1_plus.bw'), '2h'=c('WT_2h_EPAP_1_plus.bw'), '6h'=c('WT_6h_EPAP_1_plus.bw')),
              'MTR4'=list('0h'=c('MTR4_0h_EPAP_1_plus.bw', 'MTR4_0h_EPAP_2_plus.bw'), '2h'=c('MTR4_2h_EPAP_1_plus.bw', 'MTR4_2h_EPAP_2_plus.bw'), '6h'=c('MTR4_6h_EPAP_1_plus.bw', 'MTR4_6h_EPAP_2_plus.bw')),
              'RRP40'=list('0h'=c('RRP40_0h_EPAP_1_plus.bw', 'RRP40_0h_EPAP_2_plus.bw'), '2h'=c('RRP40_2h_EPAP_1_plus.bw', 'RRP40_2h_EPAP_2_plus.bw'), '6h'=c('RRP40_6h_EPAP_1_plus.bw', 'RRP40_6h_EPAP_2_plus.bw')),
              'ZCCHC8'=list('0h'=c('ZCCHC8_0h_EPAP_1_plus.bw', 'ZCCHC8_0h_EPAP_2_plus.bw'), '2h'=c('ZCCHC8_2h_EPAP_1_plus.bw', 'ZCCHC8_2h_EPAP_2_plus.bw'), '6h'=c('ZCCHC8_6h_EPAP_1_plus.bw', 'ZCCHC8_6h_EPAP_2_plus.bw')),
              'ZFC3H1'=list('0h'=c('ZFC3H1_0h_EPAP_1_plus.bw', 'ZFC3H1_0h_EPAP_2_plus.bw'), '2h'=c('ZFC3H1_2h_EPAP_1_plus.bw', 'ZFC3H1_2h_EPAP_2_plus.bw'), '6h'=c('ZFC3H1_6h_EPAP_1_plus.bw', 'ZFC3H1_6h_EPAP_2_plus.bw'))
)

),
'ChIP-seq' = list('INTS11'=c('GSM3576716_ChIP_INTS11_abcam_shINTS11_ctrl_Hg38.bw', 'GSM3576717_ChIP_INTS11_sig_shINTS11_ctrl_r1_Hg38.bw'))
)


# bigwigs_plus = list("TT-seq" = list('CTRL'=c('L_EGFP_rep1_tt_corr_ff_noJncReads_plus.bw', 'L_EGFP_rep2_tt_corr_ff_noJncReads_plus.bw'),
#                                     'ARS2'=c('L_ARS2_rep1_tt_corr_ff_noJncReads_plus.bw', 'L_ARS2_rep2_tt_corr_ff_noJncReads_plus.bw'),
#                                     'CPSF73'=c('L_CPSF73_rep1_tt_corr_ff_noJncReads_plus.bw', 'L_CPSF73_rep2_tt_corr_ff_noJncReads_plus.bw'),
#                                     'INTS11'=c('L_INTS11_rep1_tt_corr_ff_noJncReads_plus.bw', 'L_INTS11_rep2_tt_corr_ff_noJncReads_plus.bw'),
#                                     'RRP40'=c('L_RRP40_rep1_tt_corr_ff_noJncReads_plus.bw', 'L_RRP40_rep2_tt_corr_ff_noJncReads_plus.bw')
#                                     ),
#                     "RNA-seq" = list('CTRL'=c('T_EGFP_rep1_tt_corr_plus.bw', 'T_EGFP_rep2_tt_corr_plus.bw'),
#                                      'ARS2'=c('T_ARS2_rep1_tt_corr_plus.bw', 'T_ARS2_rep2_tt_corr_plus.bw'),
#                                      'CPSF73'=c('T_CPSF73_rep1_tt_corr_plus.bw', 'T_CPSF73_rep2_tt_corr_plus.bw'),
#                                      'INTS11'=c('T_INTS11_rep1_tt_corr_plus.bw', 'T_INTS11_rep2_tt_corr_plus.bw'),
#                                      'RRP40'=c('T_RRP40_rep1_tt_corr_plus.bw', 'T_RRP40_rep2_tt_corr_plus.bw')
#                                      ),
#                     "3'-seq" = list('noPAP' = list('WT'=list('0h'=c('WT_0h_noPAP_1_plus.bw'), '2h'=c('WT_2h_noPAP_1_plus.bw'), '6h'=c('WT_6h_noPAP_1_plus.bw')),
#                                                    'MTR4'=list('0h'=c('MTR4_0h_noPAP_1_plus.bw', 'MTR4_0h_noPAP_2_plus.bw'), '2h'=c('MTR4_2h_noPAP_1_plus.bw', 'MTR4_2h_noPAP_2_plus.bw'), '6h'=c('MTR4_6h_noPAP_1_plus.bw', 'MTR4_6h_noPAP_2_plus.bw')),
#                                                    'RRP40'=list('0h'=c('RRP40_0h_noPAP_1_plus.bw', 'RRP40_0h_noPAP_2_plus.bw'), '2h'=c('RRP40_2h_noPAP_1_plus.bw', 'RRP40_2h_noPAP_2_plus.bw'), '6h'=c('RRP40_6h_noPAP_1_plus.bw', 'RRP40_6h_noPAP_2_plus.bw')),
#                                                    'ZCCHC8'=list('0h'=c('ZCCHC8_0h_noPAP_1_plus.bw', 'ZCCHC8_0h_noPAP_2_plus.bw'), '2h'=c('ZCCHC8_2h_noPAP_1_plus.bw', 'ZCCHC8_2h_noPAP_2_plus.bw'), '6h'=c('ZCCHC8_6h_noPAP_1_plus.bw', 'ZCCHC8_6h_noPAP_2_plus.bw')),
#                                                    'ZFC3H1'=list('0h'=c('ZFC3H1_0h_noPAP_1_plus.bw', 'ZFC3H1_0h_noPAP_2_plus.bw'), '2h'=c('ZFC3H1_2h_noPAP_1_plus.bw', 'ZFC3H1_2h_noPAP_2_plus.bw'), '6h'=c('ZFC3H1_6h_noPAP_1_plus.bw', 'ZFC3H1_6h_noPAP_2_plus.bw'))
#                                                    ),
#                                     'EPAP' = list('WT'=list('0h'=c('WT_0h_EPAP_1_plus.bw'), '2h'=c('WT_2h_EPAP_1_plus.bw'), '6h'=c('WT_6h_EPAP_1_plus.bw')),
#                                                   'MTR4'=list('0h'=c('MTR4_0h_EPAP_1_plus.bw', 'MTR4_0h_EPAP_2_plus.bw'), '2h'=c('MTR4_2h_EPAP_1_plus.bw', 'MTR4_2h_EPAP_2_plus.bw'), '6h'=c('MTR4_6h_EPAP_1_plus.bw', 'MTR4_6h_EPAP_2_plus.bw')),
#                                                   'RRP40'=list('0h'=c('RRP40_0h_EPAP_1_plus.bw', 'RRP40_0h_EPAP_2_plus.bw'), '2h'=c('RRP40_2h_EPAP_1_plus.bw', 'RRP40_2h_EPAP_2_plus.bw'), '6h'=c('RRP40_6h_EPAP_1_plus.bw', 'RRP40_6h_EPAP_2_plus.bw')),
#                                                   'ZCCHC8'=list('0h'=c('ZCCHC8_0h_EPAP_1_plus.bw', 'ZCCHC8_0h_EPAP_2_plus.bw'), '2h'=c('ZCCHC8_2h_EPAP_1_plus.bw', 'ZCCHC8_2h_EPAP_2_plus.bw'), '6h'=c('ZCCHC8_6h_EPAP_1_plus.bw', 'ZCCHC8_6h_EPAP_2_plus.bw')),
#                                                   'ZFC3H1'=list('0h'=c('ZFC3H1_0h_EPAP_1_plus.bw', 'ZFC3H1_0h_EPAP_2_plus.bw'), '2h'=c('ZFC3H1_2h_EPAP_1_plus.bw', 'ZFC3H1_2h_EPAP_2_plus.bw'), '6h'=c('ZFC3H1_6h_EPAP_1_plus.bw', 'ZFC3H1_6h_EPAP_2_plus.bw'))
#                                                   )
#
#                                     )
#                     )

# minus
bigwigs_minus = list("TT-seq" = list('CTRL'=c('L_EGFP_rep1_tt_corr_ff_noJncReads_minus.bw', 'L_EGFP_rep2_tt_corr_ff_noJncReads_minus.bw'),
                                     'ARS2'=c('L_ARS2_rep1_tt_corr_ff_noJncReads_minus.bw', 'L_ARS2_rep2_tt_corr_ff_noJncReads_minus.bw'),
                                     'CPSF73'=c('L_CPSF73_rep1_tt_corr_ff_noJncReads_minus.bw', 'L_CPSF73_rep2_tt_corr_ff_noJncReads_minus.bw'),
                                     'INTS11'=c('L_INTS11_rep1_tt_corr_ff_noJncReads_minus.bw', 'L_INTS11_rep2_tt_corr_ff_noJncReads_minus.bw'),
                                     'RRP40'=c('L_RRP40_rep1_tt_corr_ff_noJncReads_minus.bw', 'L_RRP40_rep2_tt_corr_ff_noJncReads_minus.bw')
),
"RNA-seq" = list('CTRL'=c('T_EGFP_rep1_tt_corr_minus.bw', 'T_EGFP_rep2_tt_corr_minus.bw'),
                 'ARS2'=c('T_ARS2_rep1_tt_corr_minus.bw', 'T_ARS2_rep2_tt_corr_minus.bw'),
                 'CPSF73'=c('T_CPSF73_rep1_tt_corr_minus.bw', 'T_CPSF73_rep2_tt_corr_minus.bw'),
                 'INTS11'=c('T_INTS11_rep1_tt_corr_minus.bw', 'T_INTS11_rep2_tt_corr_minus.bw'),
                 'RRP40'=c('T_RRP40_rep1_tt_corr_minus.bw', 'T_RRP40_rep2_tt_corr_minus.bw')
),
"3'-seq" = list('noPAP' = list('WT'=list('0h'=c('WT_0h_noPAP_1_minus.bw'), '2h'=c('WT_2h_noPAP_1_minus.bw'), '6h'=c('WT_6h_noPAP_1_minus.bw')),
                               'MTR4'=list('0h'=c('MTR4_0h_noPAP_1_minus.bw', 'MTR4_0h_noPAP_2_minus.bw'), '2h'=c('MTR4_2h_noPAP_1_minus.bw', 'MTR4_2h_noPAP_2_minus.bw'), '6h'=c('MTR4_6h_noPAP_1_minus.bw', 'MTR4_6h_noPAP_2_minus.bw')),
                               'RRP40'=list('0h'=c('RRP40_0h_noPAP_1_minus.bw', 'RRP40_0h_noPAP_2_minus.bw'), '2h'=c('RRP40_2h_noPAP_1_minus.bw', 'RRP40_2h_noPAP_2_minus.bw'), '6h'=c('RRP40_6h_noPAP_1_minus.bw', 'RRP40_6h_noPAP_2_minus.bw')),
                               'ZCCHC8'=list('0h'=c('ZCCHC8_0h_noPAP_1_minus.bw', 'ZCCHC8_0h_noPAP_2_minus.bw'), '2h'=c('ZCCHC8_2h_noPAP_1_minus.bw', 'ZCCHC8_2h_noPAP_2_minus.bw'), '6h'=c('ZCCHC8_6h_noPAP_1_minus.bw', 'ZCCHC8_6h_noPAP_2_minus.bw')),
                               'ZFC3H1'=list('0h'=c('ZFC3H1_0h_noPAP_1_minus.bw', 'ZFC3H1_0h_noPAP_2_minus.bw'), '2h'=c('ZFC3H1_2h_noPAP_1_minus.bw', 'ZFC3H1_2h_noPAP_2_minus.bw'), '6h'=c('ZFC3H1_6h_noPAP_1_minus.bw', 'ZFC3H1_6h_noPAP_2_minus.bw'))
),
'EPAP' = list('WT'=list('0h'=c('WT_0h_EPAP_1_minus.bw'), '2h'=c('WT_2h_EPAP_1_minus.bw'), '6h'=c('WT_6h_EPAP_1_minus.bw')),
              'MTR4'=list('0h'=c('MTR4_0h_EPAP_1_minus.bw', 'MTR4_0h_EPAP_2_minus.bw'), '2h'=c('MTR4_2h_EPAP_1_minus.bw', 'MTR4_2h_EPAP_2_minus.bw'), '6h'=c('MTR4_6h_EPAP_1_minus.bw', 'MTR4_6h_EPAP_2_minus.bw')),
              'RRP40'=list('0h'=c('RRP40_0h_EPAP_1_minus.bw', 'RRP40_0h_EPAP_2_minus.bw'), '2h'=c('RRP40_2h_EPAP_1_minus.bw', 'RRP40_2h_EPAP_2_minus.bw'), '6h'=c('RRP40_6h_EPAP_1_minus.bw', 'RRP40_6h_EPAP_2_minus.bw')),
              'ZCCHC8'=list('0h'=c('ZCCHC8_0h_EPAP_1_minus.bw', 'ZCCHC8_0h_EPAP_2_minus.bw'), '2h'=c('ZCCHC8_2h_EPAP_1_minus.bw', 'ZCCHC8_2h_EPAP_2_minus.bw'), '6h'=c('ZCCHC8_6h_EPAP_1_minus.bw', 'ZCCHC8_6h_EPAP_2_minus.bw')),
              'ZFC3H1'=list('0h'=c('ZFC3H1_0h_EPAP_1_minus.bw', 'ZFC3H1_0h_EPAP_2_minus.bw'), '2h'=c('ZFC3H1_2h_EPAP_1_minus.bw', 'ZFC3H1_2h_EPAP_2_minus.bw'), '6h'=c('ZFC3H1_6h_EPAP_1_minus.bw', 'ZFC3H1_6h_EPAP_2_minus.bw'))
)
)
)

# combined
bigwigs = list('+'=bigwigs_plus,
               '-'=bigwigs_minus)

rm(bigwigs_plus, bigwigs_minus)

# parameters for each 'dataset'
parameters = list("TT-seq" = list('whichSamples'=NULL, 'bin_stats' = 'mean', 'enhance_signals'=FALSE, 'log2transform'=F, 'pseudoCount'=1, 'batchCorrect'=T, 'batch'=c(1,2,1,2,1,2,1,2,2,2), 'whichReps'=NULL, 'calcMean'=T, 'negValsSet0'=T, 'preMean'=F),
                  "RNA-seq" = list('whichSamples'=NULL, 'bin_stats' = 'mean', 'enhance_signals'=FALSE, 'log2transform'=F, 'pseudoCount'=1, 'batchCorrect'=T, 'batch'=c(1,2,1,2,1,2,1,2,2,2), 'whichReps'=NULL, 'calcMean'=T, 'negValsSet0'=T, 'preMean'=F),
                  "3'-seq" = list('whichSamples'=NULL, 'bin_stats' = 'mean', 'enhance_signals'=FALSE, 'log2transform'=F, 'pseudoCount'=1, 'batchCorrect'=F, 'batch'=NULL, 'whichReps'=NULL, 'calcMean'=T, 'negValsSet0'=T, 'preMean'=F),
                  "ChIP-seq" = list('whichSamples'=NULL, 'bin_stats' = 'mean', 'enhance_signals'=FALSE, 'log2transform'=F, 'pseudoCount'=1, 'batchCorrect'=F, 'batch'=NULL, 'whichReps'=NULL, 'calcMean'=T, 'negValsSet0'=T, 'preMean'=F)
)

## individual replicates
if (FALSE){
parameters = list("TT-seq" = list('whichSamples'=NULL, 'bin_stats' = 'mean', 'enhance_signals'=FALSE, 'log2transform'=F, 'pseudoCount'=1, 'batchCorrect'=F, 'batch'=c(1,2,1,2,1,2,1,2,2,2), 'whichReps'=NULL, 'calcMean'=F, 'negValsSet0'=T, 'preMean'=F),
                  "RNA-seq" = list('whichSamples'=NULL, 'bin_stats' = 'mean', 'enhance_signals'=FALSE, 'log2transform'=F, 'pseudoCount'=1, 'batchCorrect'=F, 'batch'=c(1,2,1,2,1,2,1,2,2,2), 'whichReps'=NULL, 'calcMean'=F, 'negValsSet0'=T, 'preMean'=F),
                  "3'-seq" = list('whichSamples'=NULL, 'bin_stats' = 'mean', 'enhance_signals'=FALSE, 'log2transform'=F, 'pseudoCount'=1, 'batchCorrect'=F, 'batch'=NULL, 'whichReps'=NULL, 'calcMean'=F, 'negValsSet0'=T, 'preMean'=F),
                  "ChIP-seq" = list('whichSamples'=NULL, 'bin_stats' = 'mean', 'enhance_signals'=FALSE, 'log2transform'=F, 'pseudoCount'=1, 'batchCorrect'=F, 'batch'=NULL, 'whichReps'=NULL, 'calcMean'=F, 'negValsSet0'=T, 'preMean'=F)
)
}

# parameters = list("TT-seq" = list('whichSamples'=NULL, 'log2transform'=F, 'pseudoCount'=1, 'batchCorrect'=T, 'batch'=c(1,2,1,2,1,2,1,2,2,2), 'whichReps'=NULL, 'calcMean'=T, 'negValsSet0'=T, 'preMean'=F),
#                 "RNA-seq" = list('whichSamples'=NULL, 'log2transform'=F, 'pseudoCount'=1, 'batchCorrect'=T, 'batch'=c(1,2,1,2,1,2,1,2,2,2), 'whichReps'=NULL, 'calcMean'=T, 'negValsSet0'=T, 'preMean'=F),
#                 "3'-seq" = list('whichSamples'=NULL, 'log2transform'=F, 'pseudoCount'=1, 'batchCorrect'=F, 'batch'=NULL, 'whichReps'=NULL, 'calcMean'=T, 'negValsSet0'=T, 'preMean'=F)
#                 )

parameters[["TT-seq"]][['whichSamples']] = c('CTRL', 'INTS11')
parameters[["RNA-seq"]][['whichSamples']] = c('CTRL', 'INTS11')
parameters[["3'-seq"]][['whichSamples']] = list('noPAP'=c('WT', 'RRP40'), 'EPAP'=c('WT', 'RRP40'))

## switch the order of levels within a dataset (only possible when more than one level - duh!)
if (FALSE){
  parameters[["3'-seq"]][['whichSamples']] = list('noPAP'=list('0h'=c('WT', 'RRP40'), '2h'=c('WT', 'RRP40'), '6h'=c('WT', 'RRP40')), 'EPAP'=list('0h'=c('WT', 'RRP40'), '2h'=c('WT', 'RRP40'), '6h'=c('WT', 'RRP40')))
}


gc21 = 'http://mbg-ftp-ro:MBG-F-RO-17461@genome-ftp.mbg.au.dk/files/THJ/NGS/Human/Annotations/Gencode/gencode.v21.annotation.nohosted.bed'
annots=list('gencode v21'=gc21)

system.file('inst/extdata/hg38_TAF1D.bed', package='Seq2PlotR')
bed=rtracklayer::import('inst/extdata/hg38_TAF1D.bed')
annots=list('gencode v21'=bed)

#### Example 1:
# TAF1D
# seqtypes: TT-seq, RNA-seq, 3'-seq, ChIP-seq
# auto-panels (track_width_cm=15, full_width_cm=NULL)
Seq2Plot(samples[1:2], colors, bigwig_dirs, bigwigs, parameters, plotting_segment_order=NULL, preloaded_tracks=NULL, output_tracks=FALSE, output_parameters=FALSE, input_parameters=NULL,
         both_strands=TRUE, strands_intermingled=TRUE, neg_vals_neg_strand=TRUE, actual_strand_direction=TRUE, alternating_background=TRUE, bgr_colors=c('#C1B49A', '#F1F1F2'), bgr_alpha=0.2, strands_alpha=c(100,50),
         feature='SGK1', locus=NULL, extra_space=c(1.5,1.5),
         annots=annots, annotation_packing='expanded', annot_cols=NULL, annot_panel_color='steelblue', annot_panel_font_size=NULL,
         bin_start=NULL, bin_size='auto', bins_per_cm=250, track_width_cm=15, full_width_cm=NULL, full_height_cm=NULL, track_height_cm=0.3,
         panels_max_width_cm='auto', margin_width_cm=0.05, fixed_panel_width=FALSE, horizontal_panels_list=NULL, panel_font_sizes=NULL, panel_font_size_list=NULL, panel_text_colors=c('darkgreen', 'black'),
         horizontal_spacers=TRUE, panel_separators=c(FALSE, TRUE), separators_lwds=c(0.5, 1, 0.5), separators_colors='black', incl_first_panel=TRUE, print_one_line_sample_names=FALSE, replicate_names=NULL,
         incl_track_scales=TRUE, scientific_scale=c('allow', 'all', 'none')[1], force_scale=NULL, scale_font_size=NULL, scale_panel_width_cm='auto', scale_font_color='darkred',
         header=NULL, suppress_header=FALSE, header_font_sizes=NULL, header_font_colors=c('black', 'darkgray', 'black'),
         include_genomic_scale=TRUE, genomic_scale_on_top=TRUE, genomic_scale_font_size=NULL, genomic_scale_font_color='black',
         incl_feature_names=TRUE, feature_names_above=FALSE, feature_names_alternating=TRUE, feature_names_font_size=NULL, incl_feature_brackets=TRUE, incl_feature_shadings=TRUE, feature_shading_colors=c('steelblue', 'hotpink'), feature_shading_alpha=0.05, center_of_mass=FALSE, feature_names_font_color='black',
         dummy_plot=FALSE, pdf=FALSE, pdf_name=NULL, pdf_dir='./testplotting', scaling_factor=1.2)

#### Example 2:
# TAF1D
# seqtypes: TT-seq, RNA-seq, 3'-seq
# auto-panels (track_width_cm=15, full_width_cm=NULL)
Seq2Plot(samples[1:3], colors, bigwig_dirs, bigwigs, parameters, plotting_segment_order=NULL, preloaded_tracks=NULL, output_tracks=FALSE, output_parameters=FALSE, input_parameters=NULL,
         both_strands=TRUE, strands_intermingled=TRUE, neg_vals_neg_strand=TRUE, actual_strand_direction=TRUE, alternating_background=TRUE, bgr_colors=c('#C1B49A', '#F1F1F2'), bgr_alpha=0.2, strands_alpha=c(100,50),
         feature='TAF1D', locus=NULL, extra_space=c(1.5,1.5),
         annots=annots, annotation_packing='collapsed2', annot_cols=NULL, annot_panel_color='steelblue', annot_panel_font_size=NULL,
         bin_start=NULL, bin_size='auto', bins_per_cm=250, track_width_cm=15, full_width_cm=NULL, full_height_cm=NULL, track_height_cm=0.3,
         panels_max_width_cm='auto', margin_width_cm=0.05, fixed_panel_width=FALSE, horizontal_panels_list=NULL, panel_font_sizes=NULL, panel_font_size_list=NULL, panel_text_colors=c('darkgreen', 'black'),
         horizontal_spacers=TRUE, panel_separators=c(FALSE, TRUE), separators_lwds=c(0.5, 1, 0.5), separators_colors='black', incl_first_panel=TRUE, print_one_line_sample_names=FALSE, replicate_names='rep',
         incl_track_scales=TRUE, scientific_scale=c('allow', 'all', 'none')[1], force_scale=NULL, scale_font_size=NULL, scale_panel_width_cm='auto', scale_font_color='darkred',
         header=NULL, suppress_header=FALSE, header_font_sizes=NULL, header_font_colors=c('black', 'darkgray', 'black'),
         include_genomic_scale=TRUE, genomic_scale_on_top=TRUE, genomic_scale_font_size=NULL, genomic_scale_font_color='black',
         incl_feature_names=TRUE, feature_names_above=FALSE, feature_names_alternating=TRUE, feature_names_font_size=NULL, incl_feature_brackets=TRUE, incl_feature_shadings=TRUE, feature_shading_colors=c('steelblue', 'hotpink'), feature_shading_alpha=0.05, center_of_mass=FALSE, feature_names_font_color='black',
         dummy_plot=FALSE, pdf=FALSE, pdf_name=NULL, pdf_dir='./testplotting', scaling_factor=1.2)

#### Example 3:
# TAF1D
# seqtypes: TT-seq, RNA-seq, 3'-seq
# auto-panels (track_width_cm=15, full_width_cm=NULL)
# bin_stats and enhance_signal specified for each seqtype
parameters[["3'-seq"]][['bin_stats']] = 'max'
parameters[["3'-seq"]][['enhance_signals']] = TRUE

Seq2Plot(samples[1:3], colors, bigwig_dirs, bigwigs, parameters, plotting_segment_order=NULL, preloaded_tracks=NULL, output_tracks=FALSE, output_parameters=FALSE, input_parameters=NULL,
         both_strands=TRUE, strands_intermingled=TRUE, neg_vals_neg_strand=TRUE, actual_strand_direction=TRUE, alternating_background=TRUE, bgr_colors=c('#C1B49A', '#F1F1F2'), bgr_alpha=0.2, strands_alpha=c(100,50),
         feature='TAF1D', locus=NULL, extra_space=c(1.5,1.5),
         annots=annots, annotation_packing='collapsed2', annot_cols=NULL, annot_panel_color='steelblue', annot_panel_font_size=NULL,
         bin_start=NULL, bin_size='auto', bins_per_cm=250, track_width_cm=15, full_width_cm=NULL, full_height_cm=NULL, track_height_cm=0.3,
         panels_max_width_cm='auto', margin_width_cm=0.05, fixed_panel_width=FALSE, horizontal_panels_list=NULL, panel_font_sizes=NULL, panel_font_size_list=NULL, panel_text_colors=c('darkgreen', 'black'),
         horizontal_spacers=TRUE, panel_separators=c(FALSE, TRUE), separators_lwds=c(0.5, 1, 0.5), separators_colors='black', incl_first_panel=TRUE, print_one_line_sample_names=FALSE, replicate_names='rep',
         incl_track_scales=TRUE, scientific_scale=c('allow', 'all', 'none')[1], force_scale=NULL, scale_font_size=NULL, scale_panel_width_cm='auto', scale_font_color='darkred',
         header=NULL, suppress_header=FALSE, header_font_sizes=NULL, header_font_colors=c('black', 'darkgray', 'black'),
         include_genomic_scale=TRUE, genomic_scale_on_top=TRUE, genomic_scale_font_size=NULL, genomic_scale_font_color='black',
         incl_feature_names=TRUE, feature_names_above=FALSE, feature_names_alternating=TRUE, feature_names_font_size=NULL, incl_feature_brackets=TRUE, incl_feature_shadings=TRUE, feature_shading_colors=c('steelblue', 'hotpink'), feature_shading_alpha=0.05, center_of_mass=FALSE, feature_names_font_color='black',
         dummy_plot=FALSE, pdf=FALSE, pdf_name=NULL, pdf_dir='./testplotting', scaling_factor=1.2)


#### Example 4:
# TAF1D
# seqtypes: TT-seq, RNA-seq, 3'-seq
# auto-panels (track_width_cm=15, full_width_cm=NULL)
# bin_stats and enhance_signal specified for each seqtype
# panel font sizes user-defined (panel_font_sizes=6)
Seq2Plot(samples[1:3], colors, bigwig_dirs, bigwigs, parameters, plotting_segment_order=NULL, preloaded_tracks=NULL, output_tracks=FALSE, output_parameters=FALSE, input_parameters=NULL,
         both_strands=TRUE, strands_intermingled=TRUE, neg_vals_neg_strand=TRUE, actual_strand_direction=TRUE, alternating_background=TRUE, bgr_colors=c('#C1B49A', '#F1F1F2'), bgr_alpha=0.2, strands_alpha=c(100,50),
         feature='TAF1D', locus=NULL, extra_space=c(1.5,1.5),
         annots=list('gencode v21'=gc21), annotation_packing='collapsed2', annot_cols=NULL, annot_panel_color='steelblue', annot_panel_font_size=NULL,
         bin_start=NULL, bin_size='auto', bins_per_cm=250, track_width_cm=15, full_width_cm=NULL, full_height_cm=NULL, track_height_cm=0.3,
         panels_max_width_cm='auto', margin_width_cm=0.05, fixed_panel_width=FALSE, horizontal_panels_list=NULL, panel_font_sizes=6, panel_font_size_list=NULL, panel_text_colors=c('darkgreen', 'black'),
         horizontal_spacers=TRUE, panel_separators=c(FALSE, TRUE), separators_lwds=c(0.5, 1, 0.5), separators_colors='black', incl_first_panel=TRUE, print_one_line_sample_names=FALSE, replicate_names='rep',
         incl_track_scales=TRUE, scientific_scale=c('allow', 'all', 'none')[1], force_scale=NULL, scale_font_size=NULL, scale_panel_width_cm='auto', scale_font_color='darkred',
         header=NULL, suppress_header=FALSE, header_font_sizes=NULL, header_font_colors=c('black', 'darkgray', 'black'),
         include_genomic_scale=TRUE, genomic_scale_on_top=TRUE, genomic_scale_font_size=NULL, genomic_scale_font_color='black',
         incl_feature_names=TRUE, feature_names_above=FALSE, feature_names_alternating=TRUE, feature_names_font_size=NULL, incl_feature_brackets=TRUE, incl_feature_shadings=TRUE, feature_shading_colors=c('steelblue', 'hotpink'), feature_shading_alpha=0.05, center_of_mass=FALSE, feature_names_font_color='black',
         dummy_plot=FALSE, pdf=FALSE, pdf_name=NULL, pdf_dir='./testplotting', scaling_factor=1.2)


#### Example 5:
# TAF1D
# seqtypes: TT-seq, RNA-seq, 3'-seq
# auto-panels (track_width_cm=15, full_width_cm=NULL)
# bin_stats and enhance_signal specified for each seqtype
# panel font sizes user-defined (panel_font_sizes=6)
# all samplename-panels printed horizontally ( list("TT-seq"=c(T,T), "RNA-seq"=c(T,T), "3'-seq"=c(T,T,T,T)) )
Seq2Plot(samples[1:3], colors, bigwig_dirs, bigwigs, parameters, plotting_segment_order=NULL, preloaded_tracks=NULL, output_tracks=FALSE, output_parameters=FALSE, input_parameters=NULL,
         both_strands=TRUE, strands_intermingled=TRUE, neg_vals_neg_strand=TRUE, actual_strand_direction=TRUE, alternating_background=TRUE, bgr_colors=c('#C1B49A', '#F1F1F2'), bgr_alpha=0.2, strands_alpha=c(100,50),
         feature='TAF1D', locus=NULL, extra_space=c(1.5,1.5),
         annots=list('gencode v21'=gc21), annotation_packing='collapsed2', annot_cols=NULL, annot_panel_color='steelblue', annot_panel_font_size=NULL,
         bin_start=NULL, bin_size='auto', bins_per_cm=250, track_width_cm=15, full_width_cm=NULL, full_height_cm=NULL, track_height_cm=0.3,
         panels_max_width_cm='auto', margin_width_cm=0.05, fixed_panel_width=FALSE, horizontal_panels_list=list("TT-seq"=c(T,T), "RNA-seq"=c(T,T), "3'-seq"=c(T,T,T,T)), panel_font_sizes=6, panel_font_size_list=NULL, panel_text_colors=c('darkgreen', 'black'),
         horizontal_spacers=TRUE, panel_separators=c(FALSE, TRUE), separators_lwds=c(0.5, 1, 0.5), separators_colors='black', incl_first_panel=TRUE, print_one_line_sample_names=FALSE, replicate_names='rep',
         incl_track_scales=TRUE, scientific_scale=c('allow', 'all', 'none')[1], force_scale=NULL, scale_font_size=NULL, scale_panel_width_cm='auto', scale_font_color='darkred',
         header=NULL, suppress_header=FALSE, header_font_sizes=NULL, header_font_colors=c('black', 'darkgray', 'black'),
         include_genomic_scale=TRUE, genomic_scale_on_top=TRUE, genomic_scale_font_size=NULL, genomic_scale_font_color='black',
         incl_feature_names=TRUE, feature_names_above=FALSE, feature_names_alternating=TRUE, feature_names_font_size=NULL, incl_feature_brackets=TRUE, incl_feature_shadings=TRUE, feature_shading_colors=c('steelblue', 'hotpink'), feature_shading_alpha=0.05, center_of_mass=FALSE, feature_names_font_color='black',
         dummy_plot=FALSE, pdf=FALSE, pdf_name=NULL, pdf_dir='./testplotting', scaling_factor=1.2)


#### Example 6:
# TAF1D
# seqtypes: TT-seq, RNA-seq
# individual replicates plotted
# auto-panels (track_width_cm=15, full_width_cm=NULL)
# panel font sizes user-defined (panel_font_sizes=6)
parameters[["TT-seq"]][['calcMean']] = FALSE
parameters[["RNA-seq"]][['calcMean']] = FALSE
Seq2Plot(samples[1:2], colors, bigwig_dirs, bigwigs, parameters, plotting_segment_order=NULL, preloaded_tracks=NULL, output_tracks=FALSE, output_parameters=FALSE, input_parameters=NULL,
         both_strands=TRUE, strands_intermingled=TRUE, neg_vals_neg_strand=TRUE, actual_strand_direction=TRUE, alternating_background=TRUE, bgr_colors=c('#C1B49A', '#F1F1F2'), bgr_alpha=0.2, strands_alpha=c(100,50),
         feature='TAF1D', locus=NULL, extra_space=c(1.5,1.5),
         annots=list('gencode v21'=gc21), annotation_packing='collapsed2', annot_cols=NULL, annot_panel_color='steelblue', annot_panel_font_size=NULL,
         bin_start=NULL, bin_size='auto', bins_per_cm=250, track_width_cm=15, full_width_cm=NULL, full_height_cm=NULL, track_height_cm=0.3,
         panels_max_width_cm='auto', margin_width_cm=0.05, fixed_panel_width=FALSE, horizontal_panels_list=NULL, panel_font_sizes=6, panel_font_size_list=NULL, panel_text_colors=c('darkgreen', 'black'),
         horizontal_spacers=TRUE, panel_separators=c(FALSE, TRUE), separators_lwds=c(0.5, 1, 0.5), separators_colors='black', incl_first_panel=TRUE, print_one_line_sample_names=FALSE, replicate_names='rep',
         incl_track_scales=TRUE, scientific_scale=c('allow', 'all', 'none')[1], force_scale=NULL, scale_font_size=NULL, scale_panel_width_cm='auto', scale_font_color='darkred',
         header=NULL, suppress_header=FALSE, header_font_sizes=NULL, header_font_colors=c('black', 'darkgray', 'black'),
         include_genomic_scale=TRUE, genomic_scale_on_top=TRUE, genomic_scale_font_size=NULL, genomic_scale_font_color='black',
         incl_feature_names=TRUE, feature_names_above=FALSE, feature_names_alternating=TRUE, feature_names_font_size=NULL, incl_feature_brackets=TRUE, incl_feature_shadings=TRUE, feature_shading_colors=c('steelblue', 'hotpink'), feature_shading_alpha=0.05, center_of_mass=FALSE, feature_names_font_color='black',
         dummy_plot=FALSE, pdf=FALSE, pdf_name=NULL, pdf_dir='./testplotting', scaling_factor=1.2)
