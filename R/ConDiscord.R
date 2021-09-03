#' Calcul du nombre de concordances/discordances/égalités
#' dans un tableau de contingence (2 variables ordinales)
#' @param x Tableau de contingence bidimensionnel
#' x est un tableau (matrice, array, table ou data frame) bidimensionnel
#' @examples
#' x <- table(sample(1:3, 50, replace=TRUE), sample(1:5, 50, replace=TRUE) )
#' ConDiscord(x)
#' @export
#'
ConDiscord <- function(x){
  if(!is.array(x) &
     !is.matrix(x) &
     !is.data.frame(x) &
     !is.table(x)){stop:"L'argument présenté n'est pas du type requis!"}
  if(!length(dim(x)==2)){stop:"Tableau de contingence bidimensionnel requis!"}
  r_ndx = row(x)
  c_ndx = col(x)
  Nc <- sum(x * mapply(function(r, c){sum(x[(r_ndx > r) & (c_ndx > c)])}, r = r_ndx, c = c_ndx))
  Nd <- sum(x * mapply(function(r, c){sum(x[(r_ndx > r) & (c_ndx < c)])}, r = r_ndx, c = c_ndx) )
  Ntr <- sum(x * mapply(function(r, c){sum(x[(r_ndx == r)])}, r = r_ndx, c = c_ndx) )
  Ntc <- sum(x * mapply( function(r, c){sum(x[(c_ndx == c)])},r = r_ndx, c = c_ndx) )
  return(list(Nc=Nc, Nd=Nd, Ntr=Ntr, Ntc=Ntc))
}
