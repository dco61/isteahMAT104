#' Intervalle de confiance: corrélation transformée en Z (Fisher) - Simulation
#' Échantillons tirés d'une distribution normale
#' @param r = Corrélation au niveau de la population
#' @param n = Taille de l'échantillon
#' @param nrep = Nombre de réplications
#' @param nconf = Niveau de confiance
#' @export

Z_CI <- function(r=0.36, n=50, nrep=100000, conf=95){

  z=.5*log((1+r)/(1-r))
  se=1/sqrt(n-3)
  zvec=rnorm(nrep)*se+z
  low=(1-conf/100)/2
  upp=((1-conf/100)/2)+(conf/100)
  LL=quantile(zvec,low)
  UL=quantile(zvec,upp)
  LL4=format(LL,digits=4)
  UL4=format(UL,digits=4)

  gr1 <- hist(zvec,
              breaks = "FD",
              plot = FALSE)
  cuts <- cut(gr1$breaks, c(-Inf, LL, UL, Inf))
  plot(gr1,
       main = "Distribution Empirique de Zxy",
       col = c("red", "lightblue", "red")[cuts],
       freq=FALSE,
       xlab=paste(conf,'% IC (Empirique) ','LL',LL4,'  UL',UL4))
  abline(v=mean(zvec), lty=2, lwd=2, col="#CC0000")
  txt1 <- paste("Moyenne = ", round(mean(zvec), 3))
  txt2 <- paste("Err.Std = ", round(sd(zvec), 3))
  legend("topleft",
         legend = c(txt1, txt2),
         lty = 0:0,
         cex = 0.8,
         bg = "lightyellow")
}
