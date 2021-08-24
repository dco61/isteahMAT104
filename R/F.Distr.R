F.Distr <- function(f, df1, df2, alpha=.05){
  x <- seq(0,4,by=0.01)
  y <- df(x, df1, df2)
  right <- qf(1-alpha, df1, df2)
  plot(x, y, type="l",
       xaxt="n",ylab="Probabilité",
       xlab="Rapport F",
       axes=FALSE,
       ylim=c(0,max(y)*1.05),
       xlim=c(min(x),max(x)),
       frame.plot=FALSE)
  axis(1,at=c(0, round(right, 3), round(f, 3)),
       pos = c(0,0),
       labels=TRUE)
  axis(2)
  xReject <- c(seq(0,4,by=0.01))
  yReject <- df(xReject, df1, df2)
  yReject[xReject < right ] <- 0

  polygon(c(xReject, xReject[length(xReject)], xReject[1]),
      c(0, yReject , 0), col='lightblue')
  p <- pf(f, df1, df2, lower.tail=FALSE)
legend("topleft", paste("p = ", round(p, 4)))
arrows(f, .15, f, 0, length=0.1, col="red", lwd=2)
text(f, 0.25, "F \n calculé", cex=1.2)
arrows(right, .35, right, 0, length=0.1, col="red", lwd=2)
text(right, 0.45, "F \n critique", cex=1.2)
}
