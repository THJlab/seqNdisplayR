# Package seqNdisplayR

## Installation:

see https://rdrr.io/github/THJlab/seqNdisplayR/

## Requirements:
R (>= 4.0), 
rtracklayer, 
RCurl, 
dplyr (>= 0.7), 
GenomicRanges, 
BiocGenerics,
GenomeInfoDb,
S4Vectors,
IRanges,
rlist,
limma,
readxl,
writexl,
shiny,
fastmap,
shinyBS,
shinyjs,
spsComps,
shinyTree,
shinybusy,
colourpicker,
jsonlite,
xml2

## Troubleshooting:

_Warning:_

Some MacOS Sonoma users might experience display issues with their preview plots (see video below). However, this issue appears to be independent of the seqNdisplayR package itself and should be resolved over time through OS and sub-package updates. Importantly, this display problem does not affect the generation of PDF displays. To benefit from the preview display, users can simply enlarge the preview window slightly, which will make the plot appear (see video below). We recommend not enlarging the plot too much, as this may alter the positioning of various elements in the display. Alternatively, users with multiple monitors can simply drag the preview window to another monitor, which will also update the display.

![fixing_plot_preview](https://github.com/THJlab/seqNdisplayR/blob/master/fixing_plot_preview.gif)

Below is a list of potential errors or unexpected behaviors from seqNdisplayR and how to fix them.

1/ A dummy plot is generated without ticking the corresponding option or some tracks are not displayed

If only a subset of tracks is missing, please verify that the path provided in the Excel template is accurate. If the files are not stored locally, ensure that your connection is sufficient to prevent timeouts.

If this is not the issue, or if the entire plot appears as a dummy, it may be due to plotting based on coordinates without including 'chr' in the coordinates. For example, writing '1:+:87297784:87379607' instead of 'chr1:+:87297784:87379607'. Be mindful of including 'chr' in your coordinates to avoid this occurrence.

2/ The plot fails to generate, accompanied by a warning indicating insufficient memory

There exists a constraint on the number of simultaneously open plots that R can handle. Although this limit can fluctuate based on different setups, plot sizes, and complexities, it was observed to be reached with 63 open plots during testing.
To address this issue, close some of the currently open plots and then click the 'Draw Plot' button once more.

3/ The plot fails when utilizing the 'Plot Segments Order' field

Ensure compliance with all specified requirements, both from the hovering tooltip and the console error logs.
If all requirements are met, double-check for any white spaces within your segments list (excluding potential ones in dataset names) or at the list's end. Presence of such white spaces may be misinterpreted as additional segments, resulting in plot failures.

4/ The Excel sheet import encounters repeated failures and continuously resets, accompanied by a warning stating, "argument 'replacement' has length > 1, and only the first element will be used."

The problem is likely arising from an error in populating the 'Color' column in the 'Samples' sheet. It's crucial that the colors align with the sample structure, and only one color per subset should be specified. To resolve this issue, you might want to consider leaving the 'Color' column blank and filling it from within the Shiny app.

<img width="452" alt="image" src="https://github.com/THJlab/seqNdisplayR/assets/95024722/0cde9772-8cdd-4f8a-94aa-e7bdba4f3b6f">


## Report bugs
Before submitting a bug report, please ensure that the issue is reproducible and that you have followed all the guidelines outlined in the accompanying article and instruction videos (https://www.youtube.com/channel/UCxgoRhACK-1gavmUWIXd-eQ).

When reporting a bug, be sure to provide a detailed description of the problem, including relevant files and screenshots if necessary.

Submit your bug report to seqndisplayr@gmail.com, and it will be periodically reviewed.



