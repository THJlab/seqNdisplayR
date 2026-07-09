# Package seqNdisplayR

Customisable, reproducible genome-coverage plots for comparative
sequencing visualisation, with an interactive Shiny app on top.

The canonical source for `seqNdisplayR` is
[github.com/slaish/seqNdisplayR](https://github.com/slaish/seqNdisplayR);
a maintainer-managed mirror lives at
[github.com/THJlab/seqNdisplayR](https://github.com/THJlab/seqNdisplayR).
Either URL works for installation — they always carry the same code and
the same tags.

## Installation

### 1. System dependency: GLPK

The c2 annotation-name placement uses the GNU Linear Programming Kit
(via `ROI.plugin.glpk` / `ompr.roi`).  Install GLPK before the R
packages:

| OS                    | Command                                              |
|-----------------------|------------------------------------------------------|
| Debian / Ubuntu       | `sudo apt-get install libglpk-dev`                   |
| macOS (Homebrew)      | `brew install glpk`                                  |
| Windows               | bundled with the Rtools toolchain (R 4.0+)           |

### 2. Bioconductor dependencies

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(c("rtracklayer", "GenomicRanges", "BiocGenerics",
                       "GenomeInfoDb", "S4Vectors", "IRanges", "limma"),
                     update = FALSE, ask = FALSE)
```

### 3. seqNdisplayR

```r
if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")
remotes::install_github("slaish/seqNdisplayR")
```

This pulls in the GitHub-only [`bwimport`](https://github.com/slaish/bwimport)
automatically via the package's `Remotes:` field.

To install a specific release: `remotes::install_github("slaish/seqNdisplayR@v2.0.0")`.

To install from a downloaded release tarball:

```r
remotes::install_local("seqNdisplayR_2.0.0.tar.gz",
                       dependencies = TRUE, upgrade = "never")
```

The release tarball can also be fetched directly from
the GitHub Release attachment via
[`remotes::install_url(...)`](https://remotes.r-lib.org/reference/install_url.html).

## Quick start

```r
library(seqNdisplayR)
# Launch the Shiny app (the primary usage path)
seqNdisplayR::run_seqNdisplayR_app()
```

The app ships with a couple of example sample sheets under
`system.file("extdata", package = "seqNdisplayR")` — try
`sNdR_sample_example_simple.xlsx` first.

The R API is also fully available for scripting (`LoadExcel()`,
`IGV2Session()`, `plot()` on a `seqNdisplayRSession` object, etc.) —
see the vignettes under `vignettes/`.

## Requirements

R (≥ 4.0).  Imports are installed automatically by `remotes::install_*`:
`bwimport`, `GenomicRanges`, `BiocGenerics`, `GenomeInfoDb`,
`S4Vectors`, `IRanges`, `limma`, `readxl`, `writexl`, `rtracklayer`,
`xml2`, `ompr`, `ompr.roi`, `ROI.plugin.glpk`, `slam`, `shiny`,
`shinyjs`, `shinyTree`, `shinyBS`, `shinybusy`, `spsComps`,
`colourpicker`, `DT`.

Optional (Suggests; installed automatically with `dependencies = TRUE`):
`future`, `future.apply` (parallel bigwig fetch — the package falls
back to sequential if these aren't installed), `fastmap`, `markdown`,
`knitr`, `rmarkdown`.

## Deployment

To host the Shiny app for collaborators, see
[`docs/deployment.md`](docs/deployment.md) — covers the bundled
[`Dockerfile`](Dockerfile), running behind nginx, and per-user
containerisation with ShinyProxy.

## Troubleshooting

_Warning:_

Some macOS Sonoma users might experience display issues with their
preview plots (see video below).  However, this issue appears to be
independent of the seqNdisplayR package itself and should be resolved
over time through OS and sub-package updates.  Importantly, this
display problem does not affect the generation of PDF displays.  To
benefit from the preview display, users can simply enlarge the
preview window slightly, which will make the plot appear (see video
below).  We recommend not enlarging the plot too much, as this may
alter the positioning of various elements in the display.
Alternatively, users with multiple monitors can simply drag the
preview window to another monitor, which will also update the
display.

![fixing_plot_preview](https://github.com/THJlab/seqNdisplayR/blob/master/fixing_plot_preview.gif)

Below is a list of potential errors or unexpected behaviours from
seqNdisplayR and how to fix them.

**1.  Shiny app fails to launch with `"there is no package called 'XYZ'"`.**

Re-run the install with `dependencies = TRUE`:
```r
remotes::install_github("slaish/seqNdisplayR", dependencies = TRUE,
                        upgrade = "never")
```
All Shiny dependencies (including `DT`, used by the new "Plot Segments
& Strands" tab) are in `Imports` and should install automatically.  If
you used `install.packages(tarball, repos = NULL)` directly, that
syntax does **not** install dependencies — use `remotes::install_local()`
instead.

**2.  Plot fails with `"GLPK not found"` / `ROI.plugin.glpk` install errors.**

Install GLPK at the OS level (see [Installation §1](#1-system-dependency-glpk))
and reinstall `ROI.plugin.glpk` (`install.packages("ROI.plugin.glpk")`).

**3.  A test plot is generated without ticking the corresponding option, or some tracks are not displayed.**

If only a subset of tracks is missing, verify the paths in the Excel
template.  If the files are remote, ensure your connection is
sufficient to prevent timeouts.

If the entire plot appears as a test plot, it may be due to plotting
by coordinates without `chr` in the chromosome name.  Use
`chr1:+:87297784:87379607` rather than `1:+:87297784:87379607`.

**4.  The plot fails with a warning indicating insufficient memory or too many devices.**

R has a limit on the number of simultaneously open graphics devices
(around 63 in practice).  Close some of the currently open plots and
click "Draw Plot" again.

**5.  Excel import repeatedly fails / resets with `"argument 'replacement' has length > 1, and only the first element will be used"`.**

Likely an error in populating the 'Color' column in the Samples
sheet.  Colours must align with the sample structure and only one
colour per subset should be specified.  Consider leaving the Color
column blank in Excel and filling it from within the Shiny app.

<img width="452" alt="image" src="https://github.com/THJlab/seqNdisplayR/assets/95024722/0cde9772-8cdd-4f8a-94aa-e7bdba4f3b6f">

**6.  Unreachable bigwig / annotation URLs in `CheckSampleFile()`.**

If the resource is on a password-protected lab server, embed the
credentials in the URL:
`https://user:password@host/path/to/file.bw`.  The Check File
diagnostic now mentions this when an HTTPS path is unreachable.

## Report bugs

Before submitting a bug report, please ensure that the issue is
reproducible and that you have followed all the guidelines outlined
in the accompanying article and instruction videos
(<https://www.youtube.com/channel/UCxgoRhACK-1gavmUWIXd-eQ>).

When reporting a bug, be sure to provide a detailed description of
the problem, including relevant files and screenshots if necessary.

Submit your bug report to <seqndisplayr@gmail.com> (periodically
reviewed) or open an issue at
<https://github.com/slaish/seqNdisplayR/issues>.

## Changelog

See [`NEWS.md`](NEWS.md) for the v2.0.0 release notes and
[`docs/notes/`](docs/notes/) for the longer-form internal design
notes that informed the release.
