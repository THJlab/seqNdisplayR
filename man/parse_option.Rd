% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_excel.R
\name{parse_option}
\alias{parse_option}
\title{Parse Option String}
\usage{
parse_option(option_str)
}
\arguments{
\item{option_str}{string representation of the option}
}
\description{
Parse string into relevant R object class
}
\details{
String can represent a named list, unnamed list, named vector, unnamed vector or single value. If string contains ";" assumes a list; if string contains "," assumes a vector; individual strings are interprated as "NULL" -> NULL; if "TRUE" or "FALSE" --> TRUE/FALSE; if single number --> as.numeric; if single non-number --> as.character;
}
\examples{
parse_option("1.2,3")
parse_option("RNA-seq:1.2,3;TT-seq:2,4")

}
