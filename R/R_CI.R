#' Intervalle de confiance d'une corrélation - Simulation
#' Échantillons tirés d'une distribution normale
#' @param r = Corrélation au niveau de la population
#' @param n = Taille de l'échantillon
#' @param nrep = Nombre de ré-échantillonnages
#' @param nconf = Niveau de confiance
#' @export

R_CI <- function(r=0.36, n=50, nrep=100000, conf=95){

z=.5*log((1+r)/(1-r))
se=1/sqrt(n-3)
zvec=rnorm(nrep)*se+z
ivec=(exp(2*zvec)-1)/(exp(2*zvec)+1)
low=(1-conf/100)/2
upp=((1-conf/100)/2)+(conf/100)
LL=quantile(ivec,low)
UL=quantile(ivec,upp)
LL4=format(LL,digits=4)
UL4=format(UL,digits=4)
LE=(exp(2*(.5*log((1+r)/(1-r))+se*qnorm(low,0,1)))-1)/(exp(2*(.5*log((1+r)/(1-r))+se*qnorm(low,0,1)))+1)
UE=(exp(2*(.5*log((1+r)/(1-r))-se*qnorm(low,0,1)))-1)/(exp(2*(.5*log((1+r)/(1-r))-se*qnorm(low,0,1)))+1)
LE4=format(LE,digits=4)
UE4=format(UE,digits=4)

gr1 <- hist(ivec,
            breaks = "FD",
            plot = FALSE)
cuts <- cut(gr1$breaks, c(-Inf, LL, UL, Inf))
plot(gr1,
     main = "Distribution Empirique de Rxy",
     col = c("red", "lightblue", "red")[cuts],
     freq=FALSE,
     xlab=paste("C[", LL4,"\u2264 \u03C1 \u2264 ",UL4,"] = ",conf, "% (Empir) \n C[",LE4,"\u2264 \u03C1 \u2264 ",UE4, "] = ",conf, "% (Théor)"), cex=0.8)
abline(v=mean(ivec), lty=2, lwd=2, col="#CC0000")
txt1 <- paste("Moyenne = ", round(mean(ivec), 3))
txt2 <- paste("Err.Std = ", round(sd(ivec), 3))
legend("topleft",
       legend = c(txt1, txt2),
       lty = 0:0,
       cex = 0.6,
       bg = "lightyellow")
}
