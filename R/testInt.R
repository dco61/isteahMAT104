#'
#' Verify that x is an integer value
#'
#'@param x integer value to check: Must be numeric
#'@return TRUE if integer, FALSE if not
testInt <- function(x) {
    if (!is.numeric(x)) {
        stop("Argument must be numeric")
    }
    t <- all.equal(x, as.integer(x))
    if (t == TRUE) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
