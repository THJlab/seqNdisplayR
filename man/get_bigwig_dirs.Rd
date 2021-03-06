% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_excel.R
\name{get_bigwig_dirs}
\alias{get_bigwig_dirs}
\title{Get Bigwig Dirs}
\usage{
get_bigwig_dirs(filled_df)
}
\arguments{
\item{filled_df}{a fill data frame (see details)}
}
\value{
Named vector
}
\description{
Get Bigwig Dirs
}
\details{
Get bigwig dirs file names from a data frame containing at a minimum columns
  bigwig_file and dataset. Note: in seqNdisplayR only one bigwig_dir is allowed per seqtype.
}
\examples{
df <- data.frame(bigwig_directory=c(rep('http://seqA/', 4), rep('http://seqB/', 2)),dataset=c(rep('a',4), rep('b',2)), subgroup_1=c('x','x','y','y', 'x','y'), subgroup_2=1:6, stringsAsFactors=FALSE)
get_bigwig_dirs(df)

}
