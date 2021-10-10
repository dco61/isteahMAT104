
#' Calcul de puissance: Test d'hypothèse sur une moyenne unique
#' Arguments:
#' @param n scalaire: taille de l'échantillon (défaut: 30)
#' @param alpha scalaire: niveau de signification (défaut: 0.05)
#' @param H0 scalaire: valeur postulée sous H0 (défaut:0)
#' @param H1 scalaire: valeur proposée sous H1 (défaut: 2)
#' @param sig scalaire: Écart-Type de la population (défaut: 1)
#' @param lat scalaire [1, 2]: spécification d'un test uni- ou bilatéral
#' Sortie:  puissance
#' @export
PwrCalc <- function(n = 30,
                    alpha = .05,
                    H0 = 0,
                    H1 = 2,
                    sig = 1,
                    lat = 1) {
  if(!lat %in% c(1, 2)){stop: "Argument LAT doit être 1 ou 2 (Uni/Bilatéral"}
  # Déterminer la valeur critique (Z)
  z_alpha <- qnorm(p = alpha/lat,
                   mean = 0,
                   sd = 1,
                   lower.tail = FALSE)
  # Déterminer la moyenne critique
  y_bar_cv1 <- z_alpha*(sig/sqrt(n)) + H0
  if(lat==2){
    y_bar_cv2 <- -z_alpha*(sig/sqrt(n)) + H0}

  # Calcul de la puissance
  power <- pnorm(q = y_bar_cv1,
                 mean = H1,
                 sd = sig/sqrt(n),
                 lower.tail = FALSE)
  if(lat==2){
    power <- power + pnorm(q = y_bar_cv2,
                           mean = H1,
                           sd = sig/sqrt(n),
                           lower.tail = TRUE)
  }
  # Afficher la puissance
  power
}
