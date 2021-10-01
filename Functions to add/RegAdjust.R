# From https://gist.github.com/tomhopper/8c204d978c4a0cbcb8c0

#example.R
# Example usage for model_fit_stats.R

# Set up some data
x <- seq(from=0, to=10, by=0.5)
y1 <- 2*x + rnorm(length(x))


# We want to compare two different linear models:
my.lm1 <- lm(y1 ~ sin(x))
my.lm2 <- lm(y1 ~ x)

# We will use plyr for this.
library(plyr)

# Now call model_fit_stats() for each lm model that
# we have, using ldply. This returns the results in
# a data frame that is easily used for further
# calculations.
ldply(list(my.lm1, my.lm2), model_fit_stats)

# adply() also works, though it should be less robust
# than ldply().
#adply(list(my.lm1, my.lm2), 1, model_fit_stats)

# model_fit_stats.R
#' @title Model Fit Statistics
#' @description Returns lm model fit statistics R-squared, adjucted R-squared,
#'      predicted R-squared and PRESS.
#'      Thanks to John Mount for his 6-June-2014 blog post, R style tip: prefer functions that return data frames" for
#'      the idea \link{http://www.win-vector.com/blog/2014/06/r-style-tip-prefer-functions-that-return-data-frames}
#' @return Returns a data frame with one row and a column for each statistic
#' @param linear.model A \code{lm()} model.
model_fit_stats <- function(linear.model) {
  r.sqr <- summary(linear.model)$r.squared
  adj.r.sqr <- summary(linear.model)$adj.r.squared
  pre.r.sqr <- pred_r_squared(linear.model)
  PRESS <- PRESS(linear.model)
  return.df <- data.frame(r.squared = r.sqr, adj.r.squared = adj.r.sqr, pred.r.squared = pre.r.sqr, press = PRESS)
  return(return.df)
}


# pred_r_squared.R
#' @title Predictive R-squared
#' @author Thomas Hopper
#' @description returns the predictive r-squared. Requires the function PRESS(), which returns
#'              the PRESS statistic.
#' @param linear.model A linear regression model (class 'lm'). Required.
#'
pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)

  return(pred.r.squared)
}


# PRESS.R
#' @title PRESS
#' @author Thomas Hopper
#' @description Returns the PRESS statistic (predictive residual sum of squares).
#'              Useful for evaluating predictive power of regression models.
#' @param linear.model A linear regression model (class 'lm'). Required.
#'
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)

  return(PRESS)
}
