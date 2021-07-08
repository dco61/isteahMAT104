#'
#'
#' Compute number of Permutations of n objects taken k at a time
#'
#' @param n Number of items to permute (numeric/alphanumeric)
#' @param k Number of items selected from set
#' @return Number of permutations of n objects taken k at a time
#' @examples
#' p1 <- perm.n.k(n=6,k=2)
#' p2 <- perm.n.k(10,4)
#' @export
perm.n.k <- function(n,k){
  if(!testInt(n)){stop("n must be an integer value!")}
  if(!testInt(k)){stop("k must be an integer value!")}
  if(k>n){stop("n must be greater or equal to k!")}
  prm <- factorial(n)/factorial(n-k)
  return(prm)
}
