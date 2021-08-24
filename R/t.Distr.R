t.Distr <- function(t, df, alpha, h1){
t=3.1
df=25
alpha=0.05
h1=-1
  x <- seq(-4,4,by=0.01)
  y <- dt(x, df)
  if(h1!=0){
    right <- qt(1-alpha, df)
    left <- qt(alpha, df)
  }else{
    right <- qt(1-alpha/2, df)
    left <- qt(alpha/2, df)
  }
  plot(x, y, type="l",
       xaxt="n",ylab="Probabilité",
       xlab=expression(paste('Distribution des  ',(bar(X) - bar(Y)),' supposant  ', H[0],'  vraie')),
       axes=FALSE,
       ylim=c(0,max(y)*1.05),
       xlim=c(min(x),max(x)),
       frame.plot=FALSE)
  axis(1,at=c(round(left, 3), 0, round(right, 3), round(t, 3)),
       pos = c(0,0),
       labels=TRUE)
  axis(2)
  xReject <- c(seq(-4,4,by=0.01))
  yReject <- dt(xReject, df)
  if(h1==-1){
    yReject[xReject > left ] <- 0}else{
      if(h1==1){yReject[xReject < right ] <- 0}else{
        if(h1==0){yReject[xReject > left & xReject < right] <- 0}
      }
    }
polygon(c(xReject,xReject[length(xReject)],xReject[1]),
      c(0, yReject , 0), col='lightblue')
if(h1!=0){
  p <- pt(abs(t), df, lower.tail=FALSE)
}else{
  p <- 2 * pt(abs(t), df, lower.tail=FALSE)
}
legend("topleft", paste("p = ", round(p, 4)))
arrows(t, .15, t, 0, length=0.1, col="red", lwd=2)
text(t, 0.17, "t calculé", cex=1.2)
}
