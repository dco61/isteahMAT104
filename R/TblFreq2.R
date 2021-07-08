#'
#'Generate a frequency table with various graphs
#'@param x a numeric vector
#'@param bins number of intervals (bins)
#'@return data frame:
#'      Lower & Upper limits of intervals,
#'      Absolute frequencies,
#'      relative frequencies,
#'      cumulative frequencies,
#'      Cumulative/Relative frequencies
#'      graphs (histogram, boxplot, cumulative, ogive, frequency polygon...)
#'@examples
#'TblFreq2(rnorm(200,50,8),n_classes(x))
#'@export
TblFreq2 = function(x, bins = 20) {
  # Fonction TblFreq2: x = vecteur de données; bins =
  # nombre d'intervalles
  hg = hist(x, bins, main = "Histogramme", xlab = "X")  # Obtenir les paramètres de l'histogramme
  MidPt = hg$mids
  FreqAbs = hg$counts  # Fréquences absolues
  FreqRel = prop.table(FreqAbs)  # Fréquences relatives
  FreqCum = cumsum(FreqAbs)  # Fréquences cumulatives
  FreqRelCum = cumsum(FreqRel)  # Fréquences Relatives et Cumulatives
  lim = hg$breaks[1:(length(hg$breaks))] - 0.5  # Limites exactes des intervalles
  LimInf = lim[1:(length(lim) - 1)]
  LimSup = lim[2:(length(lim))]
  hist(x, bins, main = "Histogramme et Polygone de Fréquences",
       xlab = "X")
  par(new = TRUE)
  # Polygone de fréquences
  lines(MidPt, FreqAbs, col = "red", lwd = 3)
  par(new = FALSE)
  plot(MidPt, FreqAbs, type = "l", main = "Polygone de Fréquences",
       xlab = "X", ylab = "Fréquence", col = "red", lwd = 3)
  boxplot(x, main = "Diagramme en Boîte", xlab = "X", range = 1.5,
          horizontal = TRUE, boxwex = 0.25)
  # Distribution Cumulative
  barplot(FreqCum, main = "Distribution de Fréquences Cumulatives",
          xlab = "X", ylab = "Fréquences Cumulatives", space = FALSE,
          names.arg = MidPt)
  plot(MidPt, FreqRelCum, type = "l", main = "Distribution des fréquences Relatives Cumulatives",
       xlab = "X", ylab = "Proportion", col = "red", lwd = 3)
  output = data.frame(LimInf, LimSup, FreqAbs, FreqRel, FreqCum,
                      FreqRelCum)
  return(output)
}
