#' Combinaison de  k corrélations
#' @param r = vecteur des Corrélations observées
#' @param n = vecteur des Tailles d'échantillons
#' @export

CorComb <- function(r, n){
  if(length(r)!=length(n)){
    stop:"Les arguments doivent être de même longueur!"}
  if(length(r)==1){
    stop:"Les arguments doivent être des vecteurs de longueur supérieure à 1!"}
  zw <- sum(r2z(r) * (n - 3)) / sum(n - 3)
  rho = (exp(2 * zw)-1) / (exp(2 * zw)+1)
  cat("Rho = ", rho)
}