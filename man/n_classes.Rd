% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_classes.R
\name{n_classes}
\alias{n_classes}
\title{Compute optimal number of bins for frequency tables or histogram (quantitative data)}
\usage{
n_classes(x)
}
\arguments{
\item{x}{a numeric vector}
}
\value{
Optimal number of bins
}
\description{
Compute optimal number of bins for frequency tables or histogram (quantitative data)
}
\examples{
nbins <- n_classes(rnorm(200,50,8))
}
