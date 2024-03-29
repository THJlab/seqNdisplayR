% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seqNdisplayR.R
\name{Hsl2Hex}
\alias{Hsl2Hex}
\title{Hsl2Hex}
\usage{
Hsl2Hex(c_hsl)
}
\arguments{
\item{c_hsl}{}
}
\description{
Internal function:
}
\examples{
c_hex = '#FFA550'
c_hsl = Hex2Hsl(c_hex)
c_hsl_180 = AdjustColorPhi(c_hsl)
c_hex_180 = Hsl2Hex(c_hsl_180)

}
\author{
SLA (based on https://www.rapidtables.com/convert/color/hsl-to-rgb.html)
}
\keyword{internal}
