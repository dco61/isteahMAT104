#'
#'Sample from a binomial distribution, with random n and random p
#'@param N scalar value: Number of observations to draw (default = 50)
#'@param n scalar value: number of trials (default = 10)
#'@param p scalar value: approximate probability of success (default = 0.5)
#'@return vector of observations, integer values
#'@examples
#'  x1 <- RandBin() # use defaults
#'  x2 <- RandBin(25,5,0.7) # draw 25 observations of results of 5 trials, with p=0.7
#'@export
RandBin <- function(N = 50, n = 10, p = 0.5) {
    ppop <- runif(1, runif(1, 0, p), runif(1, p, 1))
    x <- rbinom(N, n, ppop)
    return(x)
}
