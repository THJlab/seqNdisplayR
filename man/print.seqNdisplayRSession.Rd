% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seqNdisplayR.R
\name{print.seqNdisplayRSession}
\alias{print.seqNdisplayRSession}
\title{print seq'N'display'R Session}
\usage{
\method{print}{seqNdisplayRSession}(session, verbose = FALSE, ...)
}
\arguments{
\item{session}{seqNdisplayRSession object}

\item{verbose}{print detailed information? default=FALSE}

\item{...}{arguments}
}
\value{
Print to R session.
}
\description{
Prints an overview over samples, colors and associated bigwigs in a seqNdisplayR Session.
Just a pretty overview over a session.
}
\details{
Convenience function for checking parsing of samples, colors and bigwigs.
}
\note{
... arguments are samples, colors, bigwigs objects as used by seqNdisplayR and indent_size (string used for indent spacing of levels in the output)
}
\examples{
xl_fname = system.file('extdata', 'seqNdisplayR_sample_sheet_elaborate2.xlsx', package='seqNdisplayR')
session = LoadExcel(xl_fname, load_annotations =F)
print(session)
print(session, verbose=T)

}
\author{
MS
}
