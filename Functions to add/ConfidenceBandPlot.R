# Functions to make SLR Confidence and Prediction Band Plots.
# Grant Brown, June 2013,  # 171-162

# Internal function used by ConfidenceBandPlot and PredictionBandPlot
BasicRegressionPlot = function(X,Y, main = "", xlab = "",ylab = "", plt = TRUE, matchScale = FALSE)
{

  regression = lm(Y ~ X)
  if (plt)
  {
    if (matchScale)
    {
      lower = min(min(X), min(Y)) - 0.2*max(sd(X), sd(Y))
      upper = max(max(X), max(Y)) + 0.2*max(sd(X), sd(Y))
      plot(X,Y,main = main,xlab=xlab,ylab=ylab, xlim = c(lower, upper), ylim = c(lower, upper))
    }
    else
    {
      plot(X,Y,main = main,xlab=xlab,ylab=ylab, xlim = c(min(X)-sd(X), max(X) + sd(X)), ylim = c(min(Y)-sd(Y), max(Y) + sd(Y)))
    }
    abline(reg=regression)
  }
  return(regression)
}
# Internal function used by ConfidenceBandPlot and PredictionBandPlot
BandPlot = function(X,Y,type = c("confidence", "prediction"),main="Regression + Confidence Band"
                    ,xlab="X",ylab="Y", col = "red", lty =2, lwd =1,
                    alpha=0.05, add = FALSE, matchScale = FALSE)
{


  if (! add)
  {
    regression = BasicRegressionPlot(X,Y,main,xlab,ylab,plt=TRUE,matchScale)
  }
  else
  {
    regression = BasicRegressionPlot(X,Y,plt=FALSE)
  }

  MSE = sum(regression$residuals^2)/regression$df.residual
  RSE = sqrt(MSE)

  n = length(Y)
  X0 = seq(min(X) - 10*sd(X), max(X) + 10*sd(X), length = 300)

  if (type == "confidence")
  {
    print("making confidence band plot")
    SE = RSE*sqrt(1/n + ((X0 - mean(X))^2)/(sum((X-mean(X))^2)))
  }
  else if (type == "prediction")
  {
    print("making prediction band plot")
    SE = RSE*sqrt(1 + 1/n + ((X0 - mean(X))^2)/(sum((X-mean(X))^2)))
  }
  else
  {
    print(paste("Unsupported type: ", type, sep = ""))
    print("Using Confidence Band")
  }

  LB = qt(alpha/2, df = n-2)*SE
  UB = qt(1-alpha/2,df=n-2)*SE
  Yhat = regression$coefficients[1] + regression$coefficients[2]*X0
  UB = UB + Yhat
  LB = LB + Yhat
  lines(X0, UB, lty=lty, col = col)
  lines(X0, LB, lty=lty, col = col)
  abline(h=0,lty=2,col="grey")
  abline(v=0,lty=2,col="grey")
}


# Make a Confidence Band Plot. The only required arguments are X and Y.
ConfidenceBandPlot = function(X,Y,main="Regression + Confidence Band",xlab="X",
                              ylab="Y", col = "red", lty =2, lwd =1,matchScale = FALSE,
                              alpha=0.05, add = FALSE
)
{
  BandPlot(X,Y,type = "confidence", main = main, xlab =xlab,ylab=ylab,col=col,
           lty = lty, lwd = lwd, alpha = alpha, add = add, matchScale = matchScale)
}

# Make a Prediction Band Plot. The only required arguments are X and Y.
PredictionBandPlot = function(X,Y,main="Regression + Prediction Band",xlab="X",
                              ylab="Y", col = "red", lty =2, lwd =1,matchScale = FALSE,
                              alpha=0.05, add = FALSE
)
{
  BandPlot(X,Y,type = "prediction", main = main, xlab =xlab,ylab=ylab,col=col,
           lty = lty, lwd = lwd, alpha =alpha, add = add, matchScale = matchScale)
}


MakeExamplePlots = function()
{
  # Simulate data for a regression
  X = seq(1,10, length = 100)
  Y = X + rnorm(length(X), 0, 5)
  # Open up a pdf device
  pdf(file = "Confidence and Prediction Bands.pdf")
  # Set up a grid of 2x2 plots
  par(mfrow = c(2,2))
  # Make a confidence band plot at alpha = 0.01
  ConfidenceBandPlot(X,Y, alpha = 0.01)

  # Make a prediction band plot at alpha = 0.01
  PredictionBandPlot(X,Y, alpha = 0.01)

  # Make a prediction + confidence band plot at alpha = 0.01
  ConfidenceBandPlot(X,Y, alpha = 0.01, main = "Regression + Confidence \n and Prediction Bands", lty = 1)
  PredictionBandPlot(X,Y, alpha = 0.01, add = TRUE, col = "blue")

  # Make a prediction + confidence band plot at alpha = 0.01 where the X and Y scale is identical.
  ConfidenceBandPlot(X,Y, alpha = 0.01, main = "Regression + Confidence \n and Prediction Bands", lty = 1,
                     matchScale = TRUE)
  PredictionBandPlot(X,Y, alpha = 0.01, add = TRUE, col = "blue")
  # Write the plots to the pdf and close the file
  dev.off()
}
