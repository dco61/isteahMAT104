#' Confidence interval of the mean - Simulation
#' Samples drawn from a Normal distribution
#' @param nexp = Number of samples to draw
#' @param nrep = Number of resamplings
#' @param n = sample size
#' @param mu = Population mean
#' @param stdev = Population standard deviation
#' @param nconf = Confidence level
#' @export
ConfInt <- function(nexp=100, nrep=5000, n=25, mu=50, stdev=8, nconf=0.95){
  nhit <- 0
  CIdat <- matrix(NA,nrow=nexp,ncol=4)
  for (i in 1:nexp){
    x <- rnorm(n,mu,stdev)
    CIdat[i,] <- exper(x,nrep,n,mu,stdev,nconf)
    nhit <- nhit + ((CIdat[i,3]<=mu) & (CIdat[i,4]>=mu))
  }
  cat("Proportion of Success = ",nhit/nexp)
  Sample <- 1:nexp
  Mean <- CIdat[,2]
  lower <- CIdat[,3]
  upper <- CIdat[,4]
  plot_ci(lower,upper,mu)
}


