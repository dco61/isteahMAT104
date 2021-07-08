#'
#'Generate a frequency table
#'@param x a numeric vector
#'@param bins number of intervals (bins)
#'@return data frame:
#'      Lower & Upper limits of intervals,
#'      Absolute frequencies,
#'      relative frequencies,
#'      cumulative frequencies,
#'      Cumulative/Relative frequencies
#'@examples
#'TblFreq(rnorm(200,50,8),n_classes(x))
#'@export
TblFreq = function(x,bins=20){
  hg = hist(x,bins)                 # Obtenir les paramètres de l'histogramme
  FreqAbs=hg$counts                 # Fréquences absolues
  FreqRel=prop.table(FreqAbs)       # Fréquences relatives
  FreqCum = cumsum(FreqAbs)         # Fréquences cumulatives
  FreqRelCum = cumsum(FreqRel)      # Fréquences Relatives et Cumulatives
  lim = hg$breaks[1:(length(hg$breaks))]-.5  # Limites exactes des intervalles
  LimInf=lim[1:(length(lim)-1)]
  LimSup=lim[2:(length(lim))]
  output = data.frame(LimInf,LimSup,FreqAbs,FreqRel,FreqCum,FreqRelCum)
  return(output)
}
