% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SampDistrSim.R
\name{RandPop}
\alias{RandPop}
\title{RandPop function: Generate N observations from a given distribution}
\usage{
RandPop(distr = 2, N = 10000, p1 = 0, p2 = 1)
}
\description{
This function generates a vector of N observations from a specific
distribution. A maximum of 4 parameters can be supplied, as follows:
- distr: population distribution
      1 = Uniform with p1 = minimum, and p2 = maximum
      2 = Normal with p1 = mean, and p2 = standard deviation 
             Default selection, with mean = 100 and s.d. = 15
      3 = Exponential, with p1 = mu
      4 = F, with p1 = df1, and p2 = df2
      5 = Chi-Square with p1 = df1
      6 = Binomial with p1 = n, and p2 = p1
      7 = Multinomial with p1 = number of categories, and 
             p2 = vector of probabilities associated with each category
      8 = Poisson with p1 = lambda
- N: Size of the population (default = 10000)
- p1: first parameter of the distribution (see above)
- p2: Second parameter of the distribution, if any (see above)
}
