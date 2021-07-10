

#'
#'Sample from a normal distribution, with random mean and random std. deviation
#'@param n scalar value: Number of observations to draw (default = 50)
#'@param moy scalar value: approximate mean of the distribution (default = 100)
#'@param et scalar value: approximate standard deviation of the distribution (default = 15)
#'@return vector of observations, quantitative values
#'@examples
#'x1 <- RandNorm() # use defaults
#'x2 <- RandNorm(25,50,8) # draw 25 observations from a distribution with mean
#'      randomly chosen between 50 +/- 1.96 Std. Err. of the mean, and a standard deviation
#'      randomly chosen from 8 +/- et/3
#'@export
RandNorm <- function(n = 50, moy = 100, et = 15) {
    sem <- et/sqrt(n)
    moypop <-runif(1, moy - 1.96 * sem, moy + 1.96 * sem)
    etpop <- runif(1, et - et/3, et + et/3)
    x <- rnorm(n, moypop, etpop)
    return(x)
}

