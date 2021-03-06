% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RandBin.R
\name{RandBin}
\alias{RandBin}
\title{Sample from a binomial distribution, with random n and random p}
\usage{
RandBin(N = 50, n = 10, p = 0.5)
}
\arguments{
\item{N}{scalar value: Number of observations to draw (default = 50)}

\item{n}{scalar value: number of trials (default = 10)}

\item{p}{scalar value: approximate probability of success (default = 0.5)}
}
\value{
vector of observations, integer values
}
\description{
Sample from a binomial distribution, with random n and random p
}
\examples{
 x1 <- RandBin() # use defaults
 x2 <- RandBin(25,5,0.7) # draw 25 observations of results of 5 trials, with p=0.7
}
