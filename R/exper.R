#'
#'Run random experiment
#'@param x Population data
#'@param nrep scalar value: number of samples to draw (default = 5000)
#'@param n scalar value: sample size (default = 25)
#'@param nconf scalar value: Confidence level
#'@return vector containing Population mean, standard error, Percentiles (alpha/2, 1-alpha/2)
#'@examples
#'x1 <- rnorm(50000,100,15)
#'res1 <- exper(x1,nrep=10000,n=15)
#'x2 <- rbinom(50000,10,0.7)
#'res2 <- exper(x2,nrep=10000,n=25,nconf=0.99)
#'@export
exper = function(x,nrep=5000,n=25,nconf=0.95){
  xb = replicate(nrep,mean(sample(x,n,replace=TRUE)))
  popmean = mean(xb)
  stderr = sd(xb)
  lims = quantile(xb,c((1-nconf)/2,1-(1-nconf)/2))
  out = c(popmean,stderr,lims[1],lims[2])
  return(out)
}
