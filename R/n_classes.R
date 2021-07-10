#'
#' Compute optimal number of bins for frequency tables or histogram (quantitative data)
#'@param x a numeric vector
#'@return Optimal number of bins
#'@examples
#'nbins <- n_classes(rnorm(200,50,8))
#'@export
n_classes <- function(x) {
    if (missing(x))
        stop("The x argument is required.")
    if (!is.vector(x))
        x <- as.vector(x)
    nc <- ceiling((length(x)^(1/3) * (max(x) - min(x)))/(2 * IQR(x)))
    return(nc)
}
