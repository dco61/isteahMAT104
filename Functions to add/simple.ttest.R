simple.ttest <- function(x, mu0, alpha, method='two-sided'){
  n <- length(x)
  xbar <- mean(x)
  s <- sd(x)
  # calculate test statistic
  tstat <- (xbar-mu0)/(s/sqrt(n))

  # calculate critical value
  df <- n-1
  # for left-tailed test
  if (method=='left'){
    crit.val <- qt(p = 0.05, df = df, lower.tail = TRUE)
    #evaluate rejection region
    if (tstat < crit.val){
      reject = TRUE
    } else{
      reject = FALSE
    }
  }
  # for right-tailed test
  else if (method=='right'){
    crit.val <- qt(p = alpha, df = df, lower.tail = FALSE)
    #evaluate rejection region
    if (tstat > crit.val){
      reject = TRUE
    } else {
      reject = FALSE
    }
  }
  # for two-sided test (default)
  else {
    crit.val <- qt(p = alpha/2, df = df, lower.tail = FALSE)
    #evaluate rejection region
    if (abs(tstat) > abs(crit.val) | -abs(tstat) < -abs(crit.val)){
      reject = TRUE
    } else{
      reject = FALSE
    }
  }
  # print out summary and evaluation
  print(paste('Significance level:',alpha))
  print(paste('Degrees of freedom:',df))
  print(paste('Test statistic:',round(tstat,4)))
  print(paste('Critical value:',round(crit.val,4)))
  print(paste('Reject H0:',reject))
}




x <- seq(-3,3,by=0.01)
y <- dnorm(x,sd=stdDev)
# right <- qnorm(0.95,sd=stdDev)
left <- qnorm(0.05,sd=stdDev)
plot(x, y, type="l",
     xaxt="n",ylab="Probabilité",
     xlab=expression(paste('Distribution des ',bar(x),' supposant  ', H[0],'  vraie')),
     axes=FALSE,
     ylim=c(0,max(y)*1.05),
     xlim=c(min(x),max(x)),
     frame.plot=FALSE)
axis(1,at=c(-3,right,0,3),
     pos = c(0,0),
     labels=c(expression(''),
              expression(bar(x)[A]-bar(x)[C]),
              expression(mu[A]-mu[C]),
              expression('')))
axis(2)
xReject <- c(seq(-3,3,by=0.01))
yReject <- dnorm(xReject,sd=stdDev)
yReject[xReject > left ] <- 0 # & xReject < right
polygon(c(xReject,xReject[length(xReject)],xReject[1]),
        c(yReject,0, 0), col='red')
legend("topleft", paste("p = ", round(res$p.value, 4)))



t.Distr <- function(t, df, alpha, h1){

  x <- seq(-4,4,by=0.01)
  y <- dt(x, df)
  if(h1!=0){
    right <- qt(1-alpha, df, lower.tail=FALSE)
    left <- qt(alpha, df)
  }else{
    right <- qt(1-alpha/2, df, lower.tail=FALSE)
    left <- qt(alpha/2, df)
  }
  plot(x, y, type="l",
    xaxt="n",ylab="Probabilité",
    xlab=expression(paste('Distribution des  ',(bar(X) - bar(y)),' supposant  ', H[0],'  vraie')),
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
#  polygon(c(xReject,xReject[length(xReject)],xReject[1]),
#        c(0, yReject , 0), col='red')
#  legend("topleft", paste("p = ", round(pt(t, df), 4)))
}




Voir: https://www.uvm.edu/~statdhtx/StatPages/Randomization%20Tests/BootstCorr/bootstrapping_correlations.html
https://uoftcoders.github.io/studyGroup/lessons/r/resampling/lesson/
  http://users.stat.umn.edu/~helwig/notes/npboot-notes.html  ***********



  Bootstrap correlation:
  dat <- mtcars[, c(1, 3)]
N <- nrow(dat)
R <- 2500

cor.orig <- cor(dat)[1,2]
cor.boot <- NULL

for (i in 1:R) {
  idx <- sample.int(N, N, replace = TRUE)
  cor.boot[i] <- cor(dat[idx, ])[1,2]
}



