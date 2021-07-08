#'
#'Dispersion measures for nominal data
#'@param x a numeric or character vector
#'@return vector containing VarRat,DMM,IVQ,IER,and ETM
#'@examples
#'m1 <- DispNominal(sample(1:7,200,replace=TRUE))
#'m2 <- DispNominal(sample(LETTERS[1:7],200, replace=TRUE))
#'@export
DispNominal = function(x){
  if(missing(x)) stop("The x argument is required.")
  if(!is.vector(x)) x <- as.vector(x)
  md = Mode(x)
  nomd = length(md)
  tbl = table(x)
  N = sum(tbl)
  k = length(tbl)
  VarRat = 1-nomd*max(tbl)/N
  xs = sort(tbl,descending=FALSE)
  DMM = 1-(sum(xs[k]-xs[1:k-1]))/(N*(k-1))
  IVQ = (k/(k-1))*(1-sum((tbl/N)**2))
  IER = -sum((tbl/sum(tbl))*log(tbl/sum(tbl)))/log(k)
  ETM = 1-sqrt(1/((k-1)*N^2)*sum((xs[k]-xs)^2))
  out = c(VarRat,DMM,IVQ,IER,ETM)
  return(out)
}
