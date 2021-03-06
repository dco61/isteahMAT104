% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TblFreq2.R
\name{TblFreq2}
\alias{TblFreq2}
\title{Generate a frequency table with various graphs}
\usage{
TblFreq2(x, bins = 20)
}
\arguments{
\item{x}{a numeric vector}

\item{bins}{number of intervals (bins)}
}
\value{
data frame:
     Lower & Upper limits of intervals,
     Absolute frequencies,
     relative frequencies,
     cumulative frequencies,
     Cumulative/Relative frequencies
     graphs (histogram, boxplot, cumulative, ogive, frequency polygon...)
}
\description{
Generate a frequency table with various graphs
}
\examples{
TblFreq2(rnorm(200,50,8),n_classes(x))
}
