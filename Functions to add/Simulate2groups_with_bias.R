# True Values
b0=2
b1=1.3
b2=5

# Simulation Set Points
N=1000
n <- 100
set.seed(42)

collector <- matrix(ncol = 3,nrow = N)
colnames(collector) <- c("b0_hat", "b1_hat", "b2_hat")
for(i in 1:N){

  # Generate Data
  x1 <- rnorm(n, mean = 1, sd = 1)
  x2 <- rnorm(n, mean = 1, sd = 1)
  y_hat <- b1 * x1 + b2 * x2 + b0

  # Add Noise
  y <- rnorm(n, y_hat, 1)

  # Fit Data
  fit <- lm(y ~ x1 + x2)

  # Store Results
  collector[i, ] <- fit$coefficients
}


# Graph to Show UnbiasedNess
par(mfrow = c(3,1))
hist(collector[,1], main =expression(hat(beta[0])),breaks = 30)
abline(v =b0, col = "red", lwd = 2)

hist(collector[,2], main =expression(hat(beta[1])),breaks = 30)
abline(v =b1, col = "red", lwd = 2)

hist(collector[,3], main =expression(hat(beta[2])),breaks = 30)
abline(v =b2, col = "red", lwd = 2)

# Biased Estimation: Biased estimates refer to the idea that in the long run (i.e. the expected value) the parameter estimates are different than the true parameter value. One way to do this is to say that that instead of the errors being drawn from a normal distribution, they are drawn from a t-distribution.

# True Values
b0=2
b1=1.3
b2=5

# Simulation Set Points
N=1000
n <- 100
set.seed(42)

collector <- matrix(ncol = 3,nrow = N)
colnames(collector) <- c("b0_hat", "b1_hat", "b2_hat")
for(i in 1:N){

  # Generate Data
  x1 <- rnorm(n, mean = 1, sd = 1)
  x2 <- rnorm(n, mean = 1, sd = 1)
  y_hat <- b1 * x1 + b2 * x2 + b0

  # Add Noise from a t-distribution using `rt`
  y <- rt(n, df = 3, ncp = y_hat)

  # Fit Data
  fit <- lm(y ~ x1 + x2)

  # Store Results
  collector[i, ] <- fit$coefficients
}

# Graph to Show UnbiasedNess
par(mfrow = c(3,1))
hist(collector[,1], main =expression(hat(beta[0])),breaks = 30)
abline(v =b0, col = "red", lwd = 2)

hist(collector[,2], main =expression(hat(beta[1])),breaks = 30)
abline(v =b1, col = "red", lwd = 2)

hist(collector[,3], main =expression(hat(beta[2])),breaks = 30)
abline(v =b2, col = "red", lwd = 2)



