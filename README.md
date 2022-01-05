# Package seqNdisplayR

## Installation:

Install from source by dowloading the source package, open as Project in RStudio and "Install and Restart"; or, without download, using package *devtools* by running within R devtools::install_github('THJlab/seqNdisplayR').


Note: Installation from source using RStudio (Build > Install and Restart) will not install the dependencies. That is, you will have to ensure that all dependencies are installed on your computer before use.

### Requirements:
Depends:
    R (>= 4.0),
    dplyr (>= 0.7),
    rtracklayer,
    GenomicRanges,
    IRanges,
    rlist,
    limma,
    readxl,
    writexl
Suggests:
    xml2,  #for igv_session import
    shiny, #for shiny app
    shinyBS, #for shiny app
    shinyjs, #for shiny app
    spsComps, #for shiny app
    shinyTree, #for shiny app
    shinybusy #for shiny app

