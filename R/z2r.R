#' Fisher Transformation: z to r
#' Argument: z
#' Output: r
#' @param z = Fisher's zcorrelation
#' @export
z2r <- function(z){
  if(z<(-10) | z>10) stop("Z invalide!")
  r <- (exp(2 * z) - 1)/(1 + exp(2 * z))
  return(r)
}
