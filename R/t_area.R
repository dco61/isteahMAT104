#' Adapted from: https://r-coder.com/normal-distribution-r
#' Function t_area(): draw a Student distribution with shaded area
#'
#'@param df Degrees of freedom
#'@param lb lower bound of the area
#'@param ub upper bound of the area
#'@param acolor color of the area
#' ...: additional arguments to be passed to lines function
#'
#'@example
#'t_area(df=25, lb=0, ub=1.25, acolor="red")
#'@export
#'

t_area <- function(df=25, lb, ub, acolor = "lightgray", ...) {
  x <- seq(-5, 5, length = 100)

    lbf <- ubf <- TRUE
    if (missing(lb)) {
    lbf <- FALSE
    lb <- min(x)
  }
  if (missing(ub)) {
    ubf <- FALSE
    ub <- max(x)
  }

  x2 <- seq(lb, ub, length = 100)
  plot(x, dt(x, df), type = "n", ylab = "")

  y <- dt(x2, df)
  polygon(c(lb, x2, ub), c(0, y, 0), col = acolor)
  lines(x, dt(x, df), type = "l", ...)
  lines(x,rep(0,length(x)))
  if(!ubf){
    pr <- pt(lb, df, lower.tail=FALSE)
    cat("pr1=",pr)
  }else
  if(!lbf){
    pr <- pt(ub, df, lower.tail=TRUE)
    cat("pr2=",pr)
  }else
    pr <- pt(ub, df) - pt(lb, df)
    cat("pr3=",pr)
  text(-3, 0.35, paste("p = ", round(pr, 4)), cex=1.2)
}
