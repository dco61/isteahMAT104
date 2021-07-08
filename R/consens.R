





#'
#'Compute consensus for ordinal data sets
#'@param x a ordinal data vector
#'@param tbl Logical TRUE/FALSE: Table data or Raw data (default)
#'@return consensus index (according to Tatsle & Wierman (2007))
#'@examples
#'cons <- consens(sample(1:7,200,replace=TRUE),tbl=FALSE)
#'@export
consens <- function(V,tbl=FALSE) {
  # input validation: is this a frequency vector?
  if(!tbl){V = table(V)}
  if (min(V) < 0) stop("Error: negative values found in frequency vector.")
  if (all(round(V,0) == V) == FALSE) stop("Error: not all values are whole numbers; not a frequency vector; multiply by 100?")

  P <- V/sum(V) # normalizing the vector
  pos <- 1:length(V)
  mx <- mean(expand(V))
  dx <- max(pos) - min(pos)
  cns <- 1 + sum(P * log2(1-abs(pos-mx)/dx))
  if (max(V)==sum(V)) cns <- 1
  return(cns)
}
