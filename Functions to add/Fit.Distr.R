## fitting a normal distribution
fitnormal <- function (x, exact = TRUE) {
  if (exact) {
    ################################################
    ## Exact inference based on likelihood theory ##
    ################################################
    ## minimum negative log-likelihood (maximum log-likelihood) estimator of `mu` and `phi = sigma ^ 2`
    n <- length(x)
    mu <- sum(x) / n
    phi <- crossprod(x - mu)[1L] / n  # (a bised estimator, though)
    ## inverse of Fisher information matrix evaluated at MLE
    invI <- matrix(c(phi, 0, 0, phi * phi), 2L,
                   dimnames = list(c("mu", "sigma2"), c("mu", "sigma2")))
    ## log-likelihood at MLE
    loglik <- -(n / 2) * (log(2 * pi * phi) + 1)
    ## return
    return(list(theta = c(mu = mu, sigma2 = phi), vcov = invI, loglik = loglik, n = n))
  }
  else {
    ##################################################################
    ## Numerical optimization by minimizing negative log-likelihood ##
    ##################################################################
    ## negative log-likelihood function
    ## define `theta = c(mu, phi)` in order to use `optim`
    nllik <- function (theta, x) {
      (length(x) / 2) * log(2 * pi * theta[2]) + crossprod(x - theta[1])[1] / (2 * theta[2])
    }
    ## gradient function (remember to flip the sign when using partial derivative result of log-likelihood)
    ## define `theta = c(mu, phi)` in order to use `optim`
    gradient <- function (theta, x) {
      pl2pmu <- -sum(x - theta[1]) / theta[2]
      pl2pphi <- -crossprod(x - theta[1])[1] / (2 * theta[2] ^ 2) + length(x) / (2 * theta[2])
      c(pl2pmu, pl2pphi)
    }
    ## ask `optim` to return Hessian matrix by `hessian = TRUE`
    ## use "..." part to pass `x` as additional / further argument to "fn" and "gn"
    ## note, we want `phi` as positive so box constraint is used, with "L-BFGS-B" method chosen
    init <- c(sample(x, 1), sample(abs(x) + 0.1, 1))  ## arbitrary valid starting values
    z <- optim(par = init, fn = nllik, gr = gradient, x = x, lower = c(-Inf, 0), method = "L-BFGS-B", hessian = TRUE)
    ## post processing ##
    theta <- z$par
    loglik <- -z$value  ## flip the sign to get log-likelihood
    n <- length(x)
    ## Fisher information matrix (don't flip the sign as this is the Hessian for negative log-likelihood)
    I <- z$hessian / n  ## remember to take average to get mean
    invI <- solve(I, diag(2L))  ## numerical inverse
    dimnames(invI) <- list(c("mu", "sigma2"), c("mu", "sigma2"))
    ## return
    return(list(theta = theta, vcov = invI, loglik = loglik, n = n))
  }
}

fit <- fitnormal(x)
hist(x, prob = TRUE)
curve(dnorm(x, fit$theta[1], sqrt(fit$theta[2])), add = TRUE, col = 2)
