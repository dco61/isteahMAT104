# Calcul du nombre de concordances et de discordances
ConDisPairs <- function(x,y){
  n <- length(x)
  ix <- order(x)
  x <- x[ix]
  y <- y[ix]
  Nc <- sum(sapply(1:(n-1),function(i) sum(x[i]<x[(i+1):n] & y[i]<y[(i+1):n])))
  Nd <- sum(sapply(1:(n-1),function(i) sum(x[i]<x[(i+1):n] & y[i]>y[(i+1):n])))
  return(list(Nc=Nc,Nd=Nd))
}
