

#'
#'Graph Sampling Distribution
#'
#'@param xb vector of sampling statistics
#'@param nc scalar value: Confidence level (default = 0.95)
#'@param parm character value: name of the estimated parameter (default = 'Mean')
#'@examples
#'xb <- rnorm(1000,100,2)
#'GraphDist(xb,nc=0.99,parm='Medians')
GraphDist <- function(xb, nc = 0.95, parm = "Means") {
    intc <- quantile(xb, c(0.5 * (1 - nc), nc + 0.5 * (1 - nc)))  # Calcul des percentiles
    cat("C[", round(intc[1], 3), "< ", parm, " < ", round(intc[2], 3), "] = ", 100 * nc, "%")
    gr1 <- hist(xb, breaks = "FD", plot = FALSE)
    cuts <- cut(gr1$breaks, c(-Inf, intc[1], intc[2], Inf))
    maintit <- paste("Distribution of", parm)
    plot(gr1, main = maintit, xlab = parm, col = c("red", "cadetblue1", "red")[cuts])
    txt1 <- paste("Mean    = ", round(mean(xb), 3))
    txt2 <- paste("StdErr = ", round(sd(xb), 3))
    legend("topleft", legend = c(txt1, txt2), lty = 0:0, cex = 0.8, bg = "lightgreen")
    txt3 <- paste("IC(", nc * 100, "%) = [", round(intc[1], 3), ",", round(intc[2], 3), "]")
    legend("topright", legend = c(txt3), lty = 0:0, cex = 0.8, bg = "cadetblue1")
}
