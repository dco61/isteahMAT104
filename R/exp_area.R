#' From: https://r-coder.com
#' Function exp_area(): Draw an Exponential distribution with specified rate (lambda)
#' 
#' Arguments:
#' rate: lambda parameter
#' lb: lower bound of the area
#' ub: upper bound of the area
#' acolor: color of the area
#' ...: additional arguments to be passed to the lines function
#' 
#' @examples
#' exp_area(rate = 0.3, lb=2, ub=5, acolor="red")
#' @export
#' 
exp_area <- function(rate = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(0, 12/rate, 0.01) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  x2 <- seq(lb, ub, length = 100)    
  plot(x, dexp(x, rate = rate), type = "n", ylab = "Densité", xlim=c(0,qexp(0.999,rate)))
  
  y <- dexp(x2, rate = rate)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dexp(x, rate = rate), type = "l", ...)
  lines(x,rep(0,length(x)))
}