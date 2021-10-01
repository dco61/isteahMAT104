sideways.dnorm<-function(wx,wy,values=seq(-4,4,.1),magnify=4){
  # ... est constitué des moyennes et écart-types,
  # passés à la fonction dnorm
  #
  dens <- dnorm(x=values)
  x <- wx + dens * magnify
  y <- wy + values
  return(cbind(x,y))
}

par(mfrow=c(1,3))
x<-seq(-8,8,2)
y <- 1 + .5*x
fit <- lm(y~x)
plot(x,y, xlim=c(-10,10), ylim=c(-3, 7), pch=19,
     cex=1.4,col="red",  main="équation de régression")
abline(fit, lwd=3, col="blue")
plot(x,y, xlim=c(-10,10), ylim=c(-8, 10), pch=19, cex=1.4
     , main="Régression linéaire simple\navec erreurs gaussiennes"
     , col="red")
abline(fit, lwd=3, col="blue")
where.normal.x<-sort(x)
xx<-NULL
zz<-NULL
# where.normal.x <- c(-4,0,4)
for(i in 1:length(where.normal.x)){
  where.x <- where.normal.x[i]
  where.y <- predict(fit, newdata=data.frame(x=where.x))
  xy <- sideways.dnorm(where.x=where.x, where.y=where.y, magnify=4)
  lines(xy)
  abline(h=where.y, lty=2,col="pink")
  abline(v=where.x, lty=2)
  xx<-c(xx,rep(where.x,5))
  z<-where.y+rnorm(5)
  zz<-c(zz,z)
  aux<-cbind(rep(where.x,5),z)
  points(aux,col="blue")
}
plot(xx,zz,xlim=c(-10,10), ylim=c(-3, 7),main="les observations")

