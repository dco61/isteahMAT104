% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DispNominal.R
\name{DispNominal}
\alias{DispNominal}
\title{Dispersion measures for nominal data}
\usage{
DispNominal(x)
}
\arguments{
\item{x}{a numeric or character vector}
}
\value{
vector containing VarRat,DMM,IVQ,IER,and ETM
}
\description{
Dispersion measures for nominal data
}
\examples{
m1 <- DispNominal(sample(1:7,200,replace=TRUE))
m2 <- DispNominal(sample(LETTERS[1:7],200, replace=TRUE))
}
