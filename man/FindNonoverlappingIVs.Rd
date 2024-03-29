% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seqNdisplayR.R
\name{FindNonoverlappingIVs}
\alias{FindNonoverlappingIVs}
\title{Find Nonoverlapping Intervals}
\usage{
FindNonoverlappingIVs(n_IV, n_IVs, overlaps)
}
\arguments{
\item{n_IV}{}

\item{n_IVs}{}

\item{overlaps}{}
}
\description{
Internal function: 
Find nonoverlapping intervals within a GRanges object for each interval (based on the SelfHits object obtained by findOverlaps function applied to the GRanges object beforehand)
- used together with "ConnectIVs" to organize the individual feature annotations, such that they take up as little space as possible
}
\author{
SLA
}
\keyword{internal}
