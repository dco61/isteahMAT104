#' Dice Roll
#'
#'@param n number of Dice to roll (integer value, default = 1)
#'@param p vector of probabilities of a 'success' (6 probs), default = 6X(1/6)
#'       if it is a single value, the probabilities are equals
#'@return Value of each die
DiceRoll = function(n=1,p=rep(1,6)){
  if(!testInt(n)){stop("n must be an integer!")}
  if(length(p)==1){p = rep(p,6)}
  if(!is.numeric(p)){stop("p must be between numeric!")}
  x = sample(1:6,n,p,replace=TRUE)
  return(x)
}
