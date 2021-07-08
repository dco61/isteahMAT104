#'
#'Compute mode of discrete or continuous numeric vector
#'@param x a numeric vector, discrete or continuous
#'@return mode for continuous data, or mode(s) for discrete data
#'@examples
#'m1 <- Mode(sample(1:7,200,replace=TRUE))
#'m2 <- Mode(rnorm(200,50,8))
#'@export
Mode <- function(x)
{
  ### Initial Checks
  if(missing(x)) stop("The x argument is required.")
  if(!is.vector(x)) x <- as.vector(x)
  x <- x[is.finite(x)]
  ### Amodal
  if(length(unique(x))<=1) return(NA)
  ### Discrete
  if(all(x == round(x))) {
    unique_val <- unique(x)
    counts <- vector()
    for (i in 1:length(unique_val)) {
      counts[i] <- length(which(x==unique_val[i]))
    }
    position <- c(which(counts==max(counts)))
    if (mean(counts)==max(counts))
      Mode <- 'Mode does not exist'
    else
      Mode <- unique_val[position]
  }
  ### Continuous (using kernel density)
  else {
    x <- as.vector(as.numeric(as.character(x)))
    kde <- density(x)
    Mode <- kde$x[kde$y == max(kde$y)][1]
  }
  return(Mode)
}
