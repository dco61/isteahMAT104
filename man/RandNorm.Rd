% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RandNorm.R
\name{RandNorm}
\alias{RandNorm}
\title{Sample from a normal distribution, with random mean and random std. deviation}
\usage{
RandNorm(n = 50, moy = 100, et = 15)
}
\arguments{
\item{n}{scalar value: Number of observations to draw (default = 50)}

\item{moy}{scalar value: approximate mean of the distribution (default = 100)}

\item{et}{scalar value: approximate standard deviation of the distribution (default = 15)}
}
\value{
vector of observations, quantitative values
}
\description{
Sample from a normal distribution, with random mean and random std. deviation
}
\examples{
x1 <- RandNorm() # use defaults
x2 <- RandNorm(25,50,8) # draw 25 observations from a distribution with mean
     randomly chosen between 50 +/- 1.96 Std. Err. of the mean, and a standard deviation
     randomly chosen from 8 +/- et/3
}
