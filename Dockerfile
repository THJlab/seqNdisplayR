# seqNdisplayR v2.0.0 -- single-tenant Shiny server image.
#
# Build:
#   docker build -t seqndisplayr:2.0.0 .
#
# Run (lab-internal, no auth, port 3838):
#   docker run --rm -p 3838:3838 seqndisplayr:2.0.0
#
# Run with a shared bigwig/annotation read-only volume:
#   docker run --rm -p 3838:3838 -v /lab/bigwigs:/data/bigwigs:ro seqndisplayr:2.0.0
#
# Then open http://localhost:3838 in a browser.
#
# This image is the artifact for both Path A (rocker + nginx/Shiny Server)
# and Path C (ShinyProxy) in docs/deployment.md. Path B (shinyapps.io) is
# blocked by the GLPK system dependency -- see deployment.md.

FROM rocker/r-ver:4.5.0

ENV DEBIAN_FRONTEND=noninteractive \
    R_PROFILE_USER=/usr/local/lib/R/etc/Rprofile.site

# ---- System libraries -----------------------------------------------------
# libglpk-dev : required by ROI.plugin.glpk / ompr.roi (the c2 ILP solver)
# libxml2-dev : xml2 (IGV session import)
# libcurl4 / libssl : remote bigwig/bed fetches over https
# libpng / libjpeg / libtiff / libcairo / libfontconfig / libfreetype : PDF/PNG output
# libgit2-dev / git : remotes::install_github("slaish/bwimport")
RUN apt-get update && apt-get install --no-install-recommends -y \
    libglpk-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff-dev \
    libcairo2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libgit2-dev \
    git \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# ---- R package installation -----------------------------------------------
# Resolve BiocManager + Bioconductor first (the heavy step), then CRAN, then
# the GitHub-hosted bwimport, then the seqNdisplayR source itself. Splitting
# into layers means Docker can re-use the cache when only the package source
# changes.

# Layer 1: BiocManager + Bioconductor deps (pinned to Bioc 3.21 / R 4.5.x).
RUN R -q -e 'install.packages("BiocManager", repos = "https://cloud.r-project.org")' \
 && R -q -e 'BiocManager::install(version = "3.21", ask = FALSE, update = FALSE)' \
 && R -q -e 'BiocManager::install(c( \
      "GenomicRanges", "BiocGenerics", "GenomeInfoDb", \
      "S4Vectors", "IRanges", "limma", "rtracklayer" \
    ), ask = FALSE, update = FALSE)'

# Layer 2: CRAN packages (Imports + Shiny Suggests we actually need at runtime).
RUN R -q -e 'install.packages(c( \
      "readxl", "writexl", "xml2", \
      "ompr", "ompr.roi", "ROI", "ROI.plugin.glpk", "slam", \
      "shiny", "shinyjs", "shinyTree", "shinyBS", \
      "shinybusy", "spsComps", "colourpicker", \
      "fastmap", "markdown", "future", "future.apply", \
      "remotes" \
    ), repos = "https://cloud.r-project.org")'

# Layer 3: GitHub-hosted bwimport.
RUN R -q -e 'remotes::install_github("slaish/bwimport", upgrade = "never")'

# Layer 4: seqNdisplayR source -- last so iterating on the package doesn't
# invalidate the heavier layers above.
WORKDIR /src/seqNdisplayR
COPY DESCRIPTION NAMESPACE ./
COPY R R
COPY inst inst
COPY man man
RUN R -q -e 'remotes::install_local("/src/seqNdisplayR", upgrade = "never", dependencies = FALSE)'

# ---- Runtime --------------------------------------------------------------
EXPOSE 3838

# Bind to all interfaces so the container is reachable from the host.
# launch.browser = FALSE because there's no browser inside the container.
CMD ["R", "-q", "-e", \
     "seqNdisplayR::run_seqNdisplayR_app(host = '0.0.0.0', port = 3838L, launch.browser = FALSE)"]
