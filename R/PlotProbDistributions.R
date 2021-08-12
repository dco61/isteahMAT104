#' Adapted from: https://r-coder.com/normal-distribution-r
#' Probability distribution plots
#'
#'Graph Poisson Distribution
#' @param lambda mean
#' @param lb lower bound of the sum
#' @param ub upper bound of the sum
#' @param col color
#' @param lwd line width
#' @export
pois_sum <- function(lambda, lb, ub, col = 4, lwd = 1, ...) {
  x <- 0:(lambda + lambda * 2)

  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }

  plot(dpois(x, lambda = lambda), type = "h", lwd = lwd, ...)

  if(lb == min(x) & ub == max(x)) {
    color <- col
  } else {
    color <- rep(1, length(x))
    color[(lb + 1):ub ] <- col
  }

  lines(dpois(x, lambda = lambda), type = "h",
        col =  color, lwd = lwd, ...)
}

#' Adapted from: https://r-coder.com/normal-distribution-r
#' Graph continuous uniform distribution
#'@param x grid of X-axis values (optional)
#'@param  min lower limit of the distribution (a)
#'@param  max upper limit of the distribution (b)
#'@param  lwd line width of the segments of the graph
#'@param  col color of the segments and points of the graph
#' ...: additional arguments to be passed to the plot function
#' @export
plotunif <- function(x, min = 0, max = 1, lwd = 1, col = 1, ...) {

  # Grid of X-axis values
  if (missing(x)) {
    x <- seq(min - 0.5, max + 0.5, 0.01)
  }

  if(max < min) {
    stop("'min' must be lower than 'max'")
  }

  plot(x, dunif(x, min = min, max = max),
       xlim = c(min - 0.25, max + 0.25), type = "l",
       lty = 0, ylab = "f(x)", ...)
  segments(min, 1/(max - min), max, 1/(max - min), col = col, lwd = lwd)
  segments(min - 2, 0, min, 0, lwd = lwd, col = col)
  segments(max, 0, max + 2, 0, lwd = lwd, col = col)
  points(min, 1/(max - min), pch = 19, col = col)
  points(max, 1/(max - min), pch = 19, col = col)
  segments(min, 0, min, 1/(max - min), lty = 2, col = col, lwd = lwd)
  segments(max, 0, max, 1/(max - min), lty = 2, col = col, lwd = lwd)
  points(0, min, pch = 21, col = col, bg = "white")
  points(max, min, pch = 21, col = col, bg = "white")
}

#' Adapted from: https://r-coder.com/normal-distribution-r
#' Plot Exponential distribution
#' @param rate: lambda parameter
#' @param  lb: lower bound of the area
#' @param  ub: upper bound of the area
#' @param  acolor: color of the area
#' ...: additional arguments to be passed to the lines function
#' @export
exp_area <- function(rate = 1, lb, ub, acolor = "lightgray", ...) {
  x <- seq(0, 12/rate, 0.01)

  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }

  x2 <- seq(lb, ub, length = 100)
  plot(x, dexp(x, rate = rate), type = "n", ylab = "")

  y <- dexp(x2, rate = rate)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dexp(x, rate = rate), type = "l", ...)
}

#' Adapted from: https://r-coder.com/normal-distribution-r
#' Plot Normal distribution
#' @param mean: mean of the Normal variable
#' @param  sd: standard deviation of the Normal variable
#' @param  lb: lower bound of the area
#' @param  ub: upper bound of the area
#' @param  acolor: color of the area
#' ...: additional arguments to be passed to lines function
#' @export

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
}

#' Adapted from: https://r-coder.com/normal-distribution-r
#' Plot Chi-Square distribution
#'
#' @param df Degrees of freedom (default = 5)
#' @param  lb: lower bound of the area
#' @param  ub: upper bound of the area
#' @param  acolor: color of the area
#' ...: additional arguments to be passed to lines function
#' @export
#'
chisq_area <- function(df=5, lb, ub, acolor = "lightgray", ...) {
  x <- seq(0, qchisq(0.999, df), length = 100)

  if (missing(lb)) {
    lb <- min(x)
  }
  if (missing(ub)) {
    ub <- max(x)
  }

  x2 <- seq(lb, ub, length = 100)
  plot(x, dchisq(x, df), type = "n", ylab = "", xlim=c(0, quantile(x, 0.999)))
  plot(x, dchisq(x, df+5),add=TRUE,  type = "n", ylab = "", xlim=c(0, quantile(x, 0.999)))
  y <- dchisq(x2, df)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dchisq(x, df), type = "l", ...)
  lines(x, rep(0,100), type = "l")
}
