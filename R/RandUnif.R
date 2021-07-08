#'
#'Sample from a uniform distribution, with random lower and upper bounds
#'@param n scalar value: Number of observations to draw (default = 50)
#'@param a vector containing limits of lower bound of distribution
#'@param b vector containing limits of upper bound of distribution
#'@return vector of observations, quantitative values
#'@examples
#'x1 <- RandUnif() # use defaults
#'x2 <- RandUnif(25,c(1,5),c(10,15)) # draw 25 observations within
#'      lower limit set randomly from 1-5, and the upper limit set
#'      randomly from 10-15
#'@export
RandUnif = function(n=50,a=c(10,20),b=c(25,50)){
  if(b[1]<a[2]){
    stop("b[1]<a[2]! Veuillez corriger!")
  }
  liminf = runif(1,a[1],a[2]) # limite inférieure (Pop)
  limsup = runif(1,b[1],b[2]) # limite supérieure (Pop)
  x = runif(n,liminf,limsup)
  return(x)
}
