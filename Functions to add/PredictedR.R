# From https://rpubs.com/RatherBit/102428


### short-hand: source("predictiveR2.R")

cat("These functions are helpful for calculating the predictive R-square of a model \n
    The functions are used essentially to compare different models \n
    The package plyr is necessary for the functions to work \n
    For models mod1 and mod2 \n
    digit: ldply(list(mod1, mod2), model_fit_stats) \n

    If you have just one model and you want to know the predictive R-square of your model [e.g, named 'fit'] \n
    just digit: ldply(list(fit), model_fit_stats) \n

    ")
## These functions are helpful for calculating the predictive R-square of a model
##
##  The functions are used essentially to compare different models
##
##  The package plyr is necessary for the functions to work
##
##  For models mod1 and mod2
##
##  digit: ldply(list(mod1, mod2), model_fit_stats)
##
##
##  If you have just one model and you want to know the predictive R-square of your model [e.g, named 'fit']
##
##  just digit: ldply(list(fit), model_fit_stats)
##
##
##
#####################################################################
###
### Functions to calculate Predictive R-squared
###
#####################################################################


### This calculate the PRESS (predictive residual sum of squares), the lower, the better

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


### This calculate the Predictive r-squared

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



### This calculate the R-squared, the adj-R-squared and the Predictive R-squared (from the functions above)

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
  ratio.adjr2.to.r2 <- (adj.r.sqr/r.sqr)
  pre.r.sqr <- pred_r_squared(linear.model)
  press <- PRESS(linear.model)
  return.df <- data.frame("R-squared" = r.sqr, "Adj R-squared" = adj.r.sqr,
                          "Ratio Adj.R2 to R2" = ratio.adjr2.to.r2, "Pred R-squared" = pre.r.sqr, PRESS = press)
  return(round(return.df,3))
}


### This will allow the testing (e.g., comparison of model1 and model2)

library(plyr)
## Warning: package 'plyr' was built under R version 3.1.2
# Now call model_fit_stats() for each lm model that
# we have, using ldply. This returns the results in
# a data frame that is easily used for further
# calculations.

# ldply(list(model1, model2), model_fit_stats)
When you run the script, you will be ready to test your model.

Let’s see an example by Tom Hopper.

#####################################################################
###
### Example usage for model_fit_stats.R
###
#####################################################################


# Set up some data
x <- seq(from=0, to=10, by=0.5)
y1 <- 2*x + rnorm(length(x))




# We want to compare two different linear models:
my.lm1 <- lm(y1 ~ sin(x))
my.lm2 <- lm(y1 ~ x)

# plot the two models: in black the linear one, in red the one using the sin

plot(y1 ~ x, pch=19, col="black", cex=1.2)
points(y1 ~ sin(x), pch=19, col="red", cex=1.2)
