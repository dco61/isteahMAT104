#' RandPop function: Generate N observations from a given distribution
#'
#' This function generates a vector of N observations from a specific
#' distribution. A maximum of 4 parameters can be supplied, as follows:
#' - distr: population distribution
#'       1 = Uniform with p1 = minimum, and p2 = maximum
#'       2 = Normal with p1 = mean, and p2 = standard deviation 
#'              Default selection, with mean = 100 and s.d. = 15
#'       3 = Exponential, with p1 = mu
#'       4 = F, with p1 = df1, and p2 = df2
#'       5 = Chi-Square with p1 = df1
#'       6 = Binomial with p1 = n, and p2 = p1
#'       7 = Multinomial with p1 = number of categories, and 
#'              p2 = vector of probabilities associated with each category
#'       8 = Poisson with p1 = lambda
#' - N: Size of the population (default = 10000)
#' - p1: first parameter of the distribution (see above)
#' - p2: Second parameter of the distribution, if any (see above)
#'
#' @export
RandPop <- function(distr = 2, N = 10000, p1 = 0, p2 = 1) {

    # Uniform distribution p1 = Minimum value p2 = Maximum value
    if (distr == 1) {
        popdata <- c(runif(N, p1, p2))
    }
    # Normal distribution p1 = mean p2 = standard deviation
    if (distr == 2) {
        popdata <- c(rnorm(N, p1, p2))
    }
    # Exponential distribution p1 = mu
    if (distr == 3) {
        popdata <- c(rexp(N, p1))
    }
    # F distribution p1 = df1 p2 = df2
    if (distr == 4) {
        popdata <- c(rf(N, p1, p2))
    }
    # Chi-Square distribution p1 = df1
    if (distr == 5) {
        popdata <- c(rchisq(N, p1))
    }
    # Binomial distribution p1 = n p2 = p
    if (distr == 6) {
        popdata <- c(rbinom(N, p1, p2))
    }
    # Multinomial distribution p1 = number of categories p2 = vector of probabilities
    # associated with each category
    if (distr == 7) {
        popdata <- c(rmultinom(N, p1, p2))
    }
    # Poisson distribution p1 = lambda
    if (distr == 8) {
        popdata <- c(rpois(N, p1))
    }
    par(mfrow = c(1, 1))
    hist(popdata, breaks = 40, freq = FALSE, main = "Distribution of the Population", xlab = "X")
    abline(v = mean(popdata), lty = 2, lwd = 3, col = "blue")

    return(popdata)
}
#'
#' This function draws any number of samples of size n from a population vector.
#' Parametera are:
#'\t\t- popdata = vector of population values
#'\t\t- N_Sample = number of sample to draw from the population data (default = 1000)
#'\t\t- n = sample size (default=25)
#' Output is a data frame containing:
#'\t\t- Samp_Means=means: sample means
#'\t\t- Samp_cil=cil: Sample confidence interval lower limits
#'\t\t- Samp_ciu=ciu,: Sample confidence interval upper limits
#'\t\t- Samp_Median=medians: Sample medians
#'\t\t- Samp_Var=variances: Sample variances
#'\t\t- Samp_SD=stdeviations: Sample Standard Deviations
#'\t\t- Samp_MAD=mean_abs_dev: Sample Mean Absolute Deviations
#' \t\t- Std_Error=std.err.mean: Sample Standard Error of the Means
#'\t\t- Samp_Skewness=skew: Sample Skewness
#' \t\t- Samp_Kurtosis=kurtos: Sample Kurtosis
#'\t\t- Samp_JB=jb: Sample Jarque-Bera Statistics
#'\t\t- Samp_Size=samp.size: Sample size
#'\t\t- popdata: Dataset from which samples have been drawn
#'
#' @export
SampDistr <- function(popdata, N_Sample = 1000, n = 25) {
    iter <- N_Sample
    #
    means <- rep(NA, iter)
    medians <- rep(NA, iter)
    variances <- rep(NA, iter)
    stdeviations <- rep(NA, iter)
    mean_abs_dev <- rep(NA, iter)
    std.err.mean <- rep(NA, iter)
    skew <- rep(NA, iter)
    kurtos <- rep(NA, iter)
    samp.size <- rep(NA, iter)
    cil <- rep(NA, iter)
    ciu <- rep(NA, iter)
    jb <- rep(NA, iter)
    #
    for (i in 1:iter) {
        d <- sample(popdata, n)
        means[i] <- mean(d)
        medians[i] <- median(d)
        variances[i] <- var(d)
        stdeviations[i] <- sd(d)
        mean_abs_dev[i] <- mad(d)
        std.err.mean[i] <- sqrt(variances[i]/n)
        ci <- CI(d)
        cil[i] <- ci[1]
        ciu[i] <- ci[3]
        skew[i] <- skewness(d)
        kurtos[i] <- kurtosis(d)
        jb[i] <- (n/6) * (skew[i]^2 + 0.25 * kurtos[i]^2)
        samp.size[i] <- n
    }
    Samp.Distr.Stats <- data.frame(Samp_Means <- means, Samp_cil <- cil, Samp_ciu <- ciu, Samp_Median <- medians,
        Samp_Var <- variances, Samp_SD <- stdeviations, Samp_MAD <- mean_abs_dev, Std_Error <- std.err.mean,
        Samp_Skewness <- skew, Samp_Kurtosis <- kurtos, Samp_JB <- jb, Samp_Size <- samp.size)
    Outlist <- list(SampStats <- Samp.Distr.Stats, PopData <- popdata)
    return(Outlist)
}
#'
#' Function to produce sampling distributions
#' Input consists of the sampling statistics and population data, as
#' generated from the SampDistr function
#' Output consists of a table summarizing the sampling distributions and
#' histograms of each distributions.
#' 
#' @export
SampleAnalysis <- function(DataIN) {
    popdata <- DataIN$PopData
    # Compute Population Parameters
    popmean <- mean(popdata)
    popsd <- sd(popdata)
    popvar <- var(popdata)
    popmed <- median(popdata)
    popmad <- mad(popdata)
    popskewness <- skewness(popdata)
    popkurtosis <- kurtosis(popdata)
    #
    hist(popdata, breaks = 20, freq = FALSE, main = "Population Distribution of X", xlab = "X")
    abline(v = popmean, lty = 2, lwd = 3, col = "blue")
    abline(v = popmed, lty = 3, lwd = 3, col = "green")
    legend("topright", c("Mean", "Median"), fill = c("blue", "green"))
    #
    samp.means <- DataIN$SampStats$Samp_Means
    samp.cil <- DataIN$SampStats$Samp_cil
    samp.ciu <- DataIN$SampStats$Samp_ciu
    samp.medians <- DataIN$SampStats$Samp_Median
    samp.variances <- DataIN$SampStats$Samp_Var
    samp.stdev <- DataIN$SampStats$Samp_SD
    samp.mad <- DataIN$SampStats$Samp_MAD
    samp.sem <- DataIN$SampStats$Std_Error
    samp.skewness <- DataIN$SampStats$Samp_Skewness
    samp.kurtosis <- DataIN$SampStats$Samp_Kurtosis
    samp.jb <- DataIN$SampStats$Samp_JB
    samp.n <- DataIN$SampStats$Samp_Size

    #
    popsem <- popsd/sqrt(as.integer(samp.n[1]))
    #
    distr.mean <- c(round(mean(samp.means), 3), round(sd(samp.means), 3), round(popmean, 3))
    #
    distr.median <- c(round(mean(samp.medians), 3), round(sd(samp.medians), 3), round(popmed,
        3))
    #
    distr.var <- c(round(mean(samp.variances), 3), round(sd(samp.variances), 3), round(popvar,
        3))
    #
    distr.stdev <- c(round(mean(samp.stdev), 3), round(sd(samp.stdev), 3), round(popsd, 3))
    #
    distr.mad <- c(round(mean(samp.mad), 3), round(sd(samp.mad), 3), round(popmad, 3))
    #
    distr.sem <- c(round(mean(samp.sem), 3), round(sd(samp.sem), 3), round(popsem, 3))
    #
    distr.skewness <- c(round(mean(samp.skewness), 3), round(sd(samp.skewness), 3), round(popskewness,
        3))
    #
    distr.kurtosis <- c(round(mean(samp.kurtosis), 3), round(sd(samp.kurtosis), 3), round(popkurtosis,
        3))
    #
    distr.jb <- c(round(mean(samp.jb), 3), round(sd(samp.jb), 3), 0)
    #
    col.names <- c("Mean", "Median", "Variance", "Std. Dev.", "MAD", "Std.Err.Mean", "Skewness",
        "Kurtosis")
    row.names <- c("Mean", "Std.Dev", "Population")
    sampling.distr.stats <- t(array(c(distr.mean, distr.median, distr.var, distr.stdev, distr.mad,
        distr.sem, distr.skewness, distr.kurtosis), dim = c(3, 8), dimnames = list(row.names,
        col.names)))
    # par(mfrow=c(2,1))
    hist(samp.means, breaks = 20, freq = FALSE, main = "Sampling Distribution of the Means",
        xlab = "Sample Means")
    abline(v = popmean, lty = 2, lwd = 3, col = "red")
    abline(v = distr.mean[1], lty = 3, lwd = 3, col = "blue")
    legend("topright", c("PopMean", "DistrMean"), fill = c("red", "blue"))
    #
    hist(samp.medians, breaks = 20, freq = FALSE, main = "Sampling distribution of Medians",
        xlab = "Sample Medians")
    abline(v = popmed, lty = 2, lwd = 3, col = "red")
    abline(v = distr.median[1], lty = 3, lwd = 3, col = "blue")
    # par(mfrow=c(4,1))
    hist(samp.variances, breaks = 20, freq = FALSE, main = "Sampling distribution of Variances",
        xlab = "Sample Variances")
    abline(v = popvar, lty = 2, lwd = 3, col = "red")
    abline(v = distr.var[1], lty = 3, lwd = 3, col = "blue")
    #
    hist(samp.stdev, breaks = 20, freq = FALSE, main = "Sampling distribution of Standard Deviations",
        xlab = "Sample Standard Deviations")
    abline(v = popsd, lty = 2, lwd = 3, col = "red")
    abline(v = distr.stdev[1], lty = 3, lwd = 3, col = "blue")
    #
    hist(samp.mad, breaks = 20, freq = FALSE, main = "Sampling distribution of Mean Abs Deviations",
        xlab = "Sample MAD")
    abline(v = popmad, lty = 2, lwd = 3, col = "red")
    abline(v = distr.mad[1], lty = 3, lwd = 3, col = "blue")
    # \t
    hist(samp.sem, breaks = 20, freq = FALSE, main = "Sampling distribution of Std. Error of the Mean",
        xlab = "Sample Std. Error of the Mean")
    abline(v = popsem, lty = 2, lwd = 3, col = "red")
    abline(v = distr.sem[1], lty = 3, lwd = 3, col = "blue")
    # \t
    hist(samp.skewness, breaks = 20, freq = FALSE, main = "Sampling distribution of Skewness",
        xlab = "Sample Skewness")
    abline(v = popskewness, lty = 2, lwd = 3, col = "red")
    abline(v = distr.skewness[1], lty = 3, lwd = 3, col = "blue")
    # \t
    hist(samp.kurtosis, breaks = 20, freq = FALSE, main = "Sampling distribution of Kurtosis",
        xlab = "Sample Kurtosis")
    abline(v = popkurtosis, lty = 2, lwd = 3, col = "red")
    abline(v = distr.kurtosis[1], lty = 3, lwd = 3, col = "blue")
    #
    hist(samp.jb, breaks = 20, freq = FALSE, main = "Sampling distribution of Jarque-Bera Statistic",
        xlab = "Sample JB Statistic")
    # \tabline(v=popkurtosis, lty=2, lwd=3, col='red')
    abline(v = distr.jb[1], lty = 3, lwd = 3, col = "blue")
    #
    return(sampling.distr.stats)
}
