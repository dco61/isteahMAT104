PwrPlot <- function(mu0=0,
                    mu1=1.9,
                    sigma=5,
                    nsig=0.05,
                    n=25,
                    tail=1){
  alph <- nsig
  nalph <- 1 - alph
  bet <- pnorm(qnorm(nalph, mu0, sigma/sqrt(n)), mu1, sigma/sqrt(n))
  power <- 1 - bet
  sig0 <- sigma/sqrt(n)
  liminf <- qnorm(0.001, mu0, sig0)
  limsup <- qnorm(0.999, mu1, sig0)
  ggplot(data.frame(x = seq(liminf, limsup, length.out=100)), aes(x)) +
    stat_function(fun = dnorm, args = c(mu0, sig0), geom = 'area',
                  xlim = c(qnorm(1-alph/tail, mu0, sig0), limsup),
                  aes(fill = 'red')) +
    stat_function(fun = dnorm, args = c(mu0, sig0)) +
    stat_function(fun = dnorm, args = c(mu0, sig0), geom = 'area',
                  xlim = c(liminf, qnorm(1-alpha/tail, mu0, sig0)),
                  aes(fill = 'deepskyblue3')) +
    stat_function(fun = dnorm, args = c(mu0, sig0)) +
    stat_function(fun = dnorm, args = c(mu1, sig0)) +
    stat_function(fun = dnorm, args = c(mu1, sig0), geom = 'area',
                  xlim = c(qnorm(1-alph/tail, mu0, sig0), limsup),
                  aes(fill = 'green'), alpha=0.2) +
    stat_function(fun = dnorm, args = c(mu1, sig0), geom = 'area',
                  xlim = c(liminf, qnorm(1-alph/tail, mu0, sig0)),
                  aes(fill = 'chocolate3'), alpha=0.5) +
    scale_fill_identity(
      labels = c(paste("\u03B2=", round(bet, 3)),
                 paste("(1-\u03B1)=", nalph),
                 paste("(1-\u03B2)=", round(power, 3)),
                 paste("\u03B1=", alph)),
      guide = guide_legend(title="Probabilités", label.hjust = 0)) +
    theme(legend.title=element_text(size=15)) +
    annotate("text",
             size = 5,
             x = c(qnorm(0.05, mu0, sig0), qnorm(0.95, mu1, sig0)),
             y = c(dnorm(qnorm(0.25, mu0, sig0), mu0, sig0),
                   dnorm(qnorm(0.75, mu1, sig0), mu1, sig0)),
             label = c(expression(H[0], H[1])),
             colour = rep("black", 2)) +
    labs(y="Densité", x="Test") +
    theme(legend.position="right",
          legend.text = element_text(margin = margin(r = 10, unit = "pt")))+
    theme(legend.text = element_text(size = 10))

}
