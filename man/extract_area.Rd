% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/length_area_functions.R
\name{extract_area}
\alias{extract_area}
\title{extract_area Function}
\usage{
extract_area(data, metric = "pixels", pixels_per = NULL)
}
\arguments{
\item{data}{}

\item{pixels_per}{}
}
\description{
Extract total area inside plotted points in pixels. Uses get_outline to get the outline, then works out how many pixels are inside it, include the outline
}
\examples{
#extract_length()
}
