#'
#'Compute mode & modal frequency of vector (numeric/alphanumeric)
#'@param x a numeric or character vector
#'@return vector containing modal category and modal frequency
#'@examples
#'m1 <- ModeCat(sample(1:7,200,replace=TRUE))
#'m2 <- ModeCat(sample(LETTERS[1:7],200, replace=TRUE))
#'@export
ModeCat = function(x) {
  if(missing(x)) stop("The x argument is required.")
  if(!is.vector(x)) x <- as.vector(x)
  uniqx <- unique(x)
  m = uniqx[which.max(tabulate(match(x, uniqx)))]
  fm = sum(x==m)
  return(c(m,fm))
}
