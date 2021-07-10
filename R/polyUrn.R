#' Urn
#'
#'@param n number of balls to draw from urn (integer value, default = 20)
#'@param r number of RED balls in urn (default = 5)
#'@param b number of BLUE balls in urn (default = 7)
#'@param g number of GREEN balls in urn (default = 10)
#'@param repl: with replacement (TRUE/FALSE)
#'@return vector of 3 elements: numbers of balls drawn from each color
polyUrn <- function(n = 20, r = 5, b = 7, g = 10, repl = FALSE) {
    if (!testInt(n)) {
        stop("n must be an integer!")
    }
    if (!testInt(r)) {
        stop("r must be an integer!")
    }
    if (!testInt(b)) {
        stop("b must be an integer!")
    }
    if (!testInt(g)) {
        stop("g must be an integer!")
    }
    if (n > (r + b + g)) {
        stop("Trying to draw more balls that urn content!")
    }
    urn <- c(rep("R", r), rep("B", b), rep("G", g))
    draw <- sample(urn, size = n, replace = repl)
    x <- c(sum(draw == "R"), sum(draw == "B"), sum(draw == "G"))
    return(x)
}
