% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seqNdisplayR.R
\name{LoadExcel}
\alias{LoadExcel}
\title{Load Excel}
\usage{
LoadExcel(xl_fname, load_annotations = FALSE)
}
\arguments{
\item{xl_fname}{excel template file including path}

\item{load_annotations}{load annotations (paths in ANNOTATIONS sheet in excel template) as GRanges? default=FALSE.}
}
\value{
seqNdisplayRSession object, essentially a named list with slots samples,
  colors, bigwig_dirs, bigwigs, parameters and annotations and optional arguments
  to be fed into seqNdisplay function
}
\description{
Load Excel Template in a format that fits to seqNdisplayR (see documentation or examples for details)
}
\details{
Load Excel template and parses information to seqNdisplayR session
  object. See examples of the format of Excel Templates in
  \code{ExamplesSampleSheetsFolder()} for more information.
}
\examples{
#' # EXAMPLE 1:
xl_fname = system.file('extdata', 'seqNdisplayR_sample_sheet_elaborate2.xlsx', package='seqNdisplayR')
session = LoadExcel(xl_fname, load_annotations = TRUE)
plot(session, feature='LMO4')

# EXAMPLE 2:
example_folder = ExamplesSampleSheetsFolder()
xl_fname = paste0(example_folder, 'seqNdisplayR_sample_sheet_elaborate2.xlsx')
session = LoadExcel(xl_fname, load_annotations = TRUE)
plot(session, feature='LMO4')

}
\author{
MS/SLA
}
