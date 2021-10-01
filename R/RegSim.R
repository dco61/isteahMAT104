#' Simple linear regression simulation
#'@param nrep scalar value: number of samples (default=10000)
#'@param n scalar value: sample size (default = 50)
#'@param xb scalar value: mean of independent variable (default = 20)
#'@param sx scalar value: standard deviation of independent variable (default=5)
#'@param yb scalar value: mean of dependent variable (default = 40)
#'@param sx scalar value: standard deviation of dependent variable (default=10)
#'@param r scalar value: correlation coefficient  (default=0.6)
#'@return list containing correlations, intercepts and slopes
#'@examples
#'  RegSim()
#'@export
RegSim <- function(nrep=10000, n=50, xb=20, sx=5, yb=40, sy=10, r=0.6){

  b0 <- vector()
  b1 <- vector()
  rxy <- vector()

  for(i in 1:nrep) {
    x1 = rnorm(n)
    y1 = r * x1 + sqrt(1-r^2)*rnorm(n)
    x <- x1*sx + xb
    y <- y1*sy + yb
    reg <- lm(y~x)
    rxy[i] <- cor(x,y)
    b0[i] <- reg$coefficients[1]
    b1[i] <- reg$coefficients[2]
  }
  out <- data.frame(rxy, b0, b1)
  return(out)
}
