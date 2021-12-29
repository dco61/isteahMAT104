#' Fisher Transformation: r to z
#' Argument: Correlation
#' Output: Fisher's z
#' @param r = correlation
#' @export
r2z <- function(r){
  if(r<(-1) | r>1) stop("Corrélation invalide!")
  z <- 0.5 * log((1 + r)/(1 - r))
  return(z)
}
