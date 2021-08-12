#' From: https://r-coder.com/normal-distribution-r
#'               /#Plot_normal_cumulative_distribution_function_in_R
#' Function normal_area(): draw a normal distribution with shaded area
#' 
#' Arguments:
#' mean: mean of the Normal variable
#' sd: standard deviation of the Normal variable
#' lb: lower bound of the area
#' ub: upper bound of the area
#' acolor: color of the area
#' ...: additional arguments to be passed to lines function
#' 
#' @example
#' normal_area(mean=100, sd=15, lb=80, ub=125, acolor="red")
#' @export
#' 

normal_area <- function(mean = 0, sd = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(mean - 3 * sd, mean + 3 * sd, length = 100) 
  
  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }
  
  x2 <- seq(lb, ub, length = 100)    
  plot(x, dnorm(x, mean, sd), type = "n", ylab = "")
  
  y <- dnorm(x2, mean, sd)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dnorm(x, mean, sd), type = "l", ...)
  lines(x,rep(0,length(x)))
}