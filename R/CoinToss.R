#' Tossing a coin
#'
#'@param n number of coins to toss (integer value, default = 1)
#'@param p probability of a 'success' (Tail/Head), default = 0.5
#'@return either 'Pile' or 'Face'
#'@export
CoinToss <- function(n = 1, p = 0.5) {
    if (!testInt(n)) {
        stop("n must be an integer!")
    }
    if (p < 0 | p > 1) {
        stop("p must be between 0 and 1!")
    }
    coin <- c("Pile", "Face")
    x <- sample(coin, n, c(p, 1 - p), replace = TRUE)
    return(x)
}
