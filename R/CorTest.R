#' Test d'hypothèse sur k corrélations
#' @param r = vecteur des Corrélations observées
#' @param n = vecteur des Tailles d'échantillons
#' @export

CorTest <- function(r, n){
  if(length(r)!=length(n)){
    stop:"Les arguments doivent être de même longueur!"}
  if(length(r)==1){
    stop:"Les arguments doivent être des vecteurs de longueur supérieure à 1!"}
  z <- r2z(r)
  if(length(r)==2){
      Z <- (z[1]-z[2]) / sqrt(sum(1/(n-3)))
      p <- 2* (1 - pnorm(Z)) # Test bilatéral
  }else{
      chi2 <- sum((n-3) * z^2) - (sum((n-3) * z)^2 / sum(n-3))
      dl <- length(r) - 1      
      p <- pchisq(chi2, dl, lower.tail=FALSE)
  }
  if(length(r)==2){
    cat("Z = ", Z, "   p = ", p)
  }else{
    cat("Chi2 = ", chi2, "   p = ", p)
  }
}
