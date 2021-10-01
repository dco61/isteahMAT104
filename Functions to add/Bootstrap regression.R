# From: https://towardsdatascience.com/bootstrap-regression-in-r-98bfe4ff5007

set.seed(2021)
n <- 1000
x <- rnorm(n)
y <- x + rnorm(n)
population.data <- as.data.frame(cbind(x, y))

sample.data <- population.data[sample(nrow(population.data), 20, replace = TRUE),]

population.model <- lm(y~x, population.data)
summary(population.model)

sample.model <- lm(y~x, sample.data)
summary(sample.model)

# Bootstrap Regression
# Containers for the coefficients
sample_coef_intercept <- NULL
sample_coef_x1 <- NULL

for (i in 1:10000) {
  #Creating a resampled dataset from the sample data
  sample_d = sample.data[sample(1:nrow(sample.data), nrow(sample.data), replace = TRUE), ]

  #Running the regression on these data
  model_bootstrap <- lm(y ~ x, data = sample_d)

  #Saving the coefficients
  sample_coef_intercept <-
    c(sample_coef_intercept, model_bootstrap$coefficients[1])

  sample_coef_x1 <-
    c(sample_coef_x1, model_bootstrap$coefficients[2])
}






# From https://theanlim.rbind.io/post/linear-regression-simulation-study-plotly-theanlim/
set.seed(001)
n = 1000; p = 2; sigma = 4;

# Betas
b0 = 10
b1 = 2
b2 = 0.5
beta = c(b0, b1,b2)

# Xs
x1 = runif(n, 0, 10)
x2 = rnorm(n, mean = 4, sd = 10)
xmat = cbind(rep(1,n), x1, x2)

# errors
eps = rnorm(n,0,sigma)

# calculate Y using linear model
y = xmat %*% beta + eps

#bind data
df_lm = as.data.frame(cbind(y,xmat))
colnames(df_lm) = c("y", "intercept", "x1", "x2")
head(df_lm)

library(plotly)
pcolor = rep("red", length(x1))
plot_ly(df_lm,
        x = ~x1,
        y = ~x2,
        z = ~y,
        type = "scatter3d",
        mode = "markers",
        marker = list(color = pcolor,
                      opacity = 0.4))
