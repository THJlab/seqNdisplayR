# Package startup

.onLoad = function(libname, pkgname) {
  Sys.setlocale(locale="C")

  # Silence libBigWig's [bwHdrRead]/[bwOpen] stderr chatter on transient
  # first-attempt failures. seqNdisplayR retries at the R level (alt chrom
  # name + zero-fallback), so those C-level prints from bwimport (>= 0.2.1)
  # are cosmetic noise. Only set the env var if the user hasn't already set it
  # (leaving it empty gets the seqNdisplayR default; setting it non-empty is
  # an opt-in to quiet; setting it to nothing meaningful is unusual and stays
  # respected).
  if (!nzchar(Sys.getenv("BWIMPORT_QUIET"))) {
    Sys.setenv(BWIMPORT_QUIET = "1")
  }
}

.onAttach = function(libname, pkgname) {
  packageStartupMessage("Welcome to seqNdisplayR (v", utils::packageVersion(pkgname), ")\nThis software is free and comes with ABSOLUTELY NO WARRANTY!")
}