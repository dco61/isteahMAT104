#' somersD: Calcul du coefficient D de Somers
#'
#' Arguments:
#' table: Tableau de contingence bidimensionnel
#' conflev: Niveau de confiance, 0.95 par défaut
#' digits: Nombre de décimales, 3 par défaut
#'
#' Sortie: liste comprenant:
#'           - Tableau contenant pour VD=R et VD=C,
#'               - D de Somers
#'               - Erreur Standard de D
#'               - Limite inférieure de l'IC
#'               - Limite supérieure de l'IC
#'           - Nombre de concordances
#'           - Nombre de discordances
#'
#' @examples
#' x <- matrix(c(12, 4, 3, 5, 10, 6, 3, 5, 14),nrow=3, byrow=TRUE)
#' somersD(x)
#' @export
#'

somersD = function(tbl, conflev=0.95, digits=3){
  if(!length(dim(tbl))==2){stop("Tableau de contingence bidimensionnel nécessaire")}
    N <- sum(tbl)
    nr <- nrow(tbl)
    nc <- ncol(tbl)
    r_ndx = row(tbl)
    c_ndx = col(tbl)
    C <- sum(tbl * mapply(function(r, c){sum(tbl[(r_ndx > r) & (c_ndx > c)])},
                   r = r_ndx, c = c_ndx))
    D <- sum(tbl * mapply( function(r, c){sum(tbl[(r_ndx > r) & (c_ndx < c)])},
                    r = r_ndx, c = c_ndx) )

    Dc.r <- 2 * (C - D) / (N^2 - sum(rowSums(tbl)^2))
    Dr.c <- 2 * (C - D) / (N^2 - sum(colSums(tbl)^2))

  # Asymptotic standard error: sqrt(sigma2)
  sigma2r <- (4 * (nr^2-1) * (nc + 1)) / (9 * N * nr^2 * (nc - 1))
  sigma2c <- (4 * (nc^2-1) * (nr + 1)) / (9 * N * nc^2 * (nr - 1))
  SEDc <- sqrt(sigma2c)
  SEDr <- sqrt(sigma2r)

  pr2 <- 1 - (1 - conflev)/2
  CIr <- qnorm(pr2) * SEDr * c(-1, 1) + Dr.c
  CIc <- qnorm(pr2) * SEDc * c(-1, 1) + Dc.r

  ResR <- c(Dr.c, SEDr, CIr)
  ResC <- c(Dc.r, SEDc, CIc)

  res <- data.frame(rbind(ResR, ResC))
  colnames(res) <- c("Somers'D", "StdErr.D", "Lower", "Upper")
  rownames(res) <- c("Rows", "Columns")


  return(list(ResTbl=res, concord=C, discord=D))

}
