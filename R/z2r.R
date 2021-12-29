#' Fisher Transformation: z to r
#' Argument: z (can be a vector)
#' Output: r
#' @param z = Fisher's z
#' @export
z2r <- function(z){
  if((sum(z<(-10) | z>10))!=0) stop("Z invalide!")
  r <- (exp(2 * z) - 1)/(1 + exp(2 * z))
  return(r)
}
