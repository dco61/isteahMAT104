# Calcul du nombre de concordances et de discordances
ConDisPairs <- function(x,y){
  n <- length(x)
  ix <- order(x)
  x <- x[ix]
  y <- y[ix]
  Ntx <- sum(which(table(x) > 1))
  Nty <- sum(which(table(y) > 1))
  Nc <- sum(sapply(1:(n-1),function(i) sum(x[i]<x[(i+1):n] & y[i]<y[(i+1):n])))
  Nd <- sum(sapply(1:(n-1),function(i) sum(x[i]<x[(i+1):n] & y[i]>y[(i+1):n])))
  Nt <- sum(sapply(1:(n-1),function(i) sum(x[i]==x[(i+1):n] & y[i]==y[(i+1):n])))
  return(list(Nc=Nc, Nd=Nd, Nt=Nt, Ntx=Ntx, Nty=Nty))
}

# Liste de concordances et de discordances
ConTabl <- function(x,y){
  Nc <- vector()
  Nd <- vector()
  Nt <- vector()
  n <- length(x)
  ix <- order(x)
  x <- x[ix]
  y <- y[ix]
  Nc <- append(Nc, (sapply(1:(n-1),function(i) sum(x[i]<x[(i+1):n] & y[i]<y[(i+1):n]))))
  Nd <- append(Nd, (sapply(1:(n-1),function(i) sum(x[i]<x[(i+1):n] & y[i]>y[(i+1):n]))))
  Nt <- append(Nt, (sapply(1:(n-1),function(i) sum(x[i]==x[(i+1):n] & y[i]==y[(i+1):n]))))
  return(list(Nc=Nc, Nd=Nd, Nt=Nt))
}

# set.seed(150752)
n <- 12
r <- 0.6
# x <- rnorm(n)
# y <- r * x + sqrt(1-r**2) * rnorm(n)
x <- rbinom(n, 30, 0.5)
y <- round(r * x + sqrt(1-r**2) * rbinom(n, 30, .5))
x1 <- rank(x)                 # Rank X
x2 <- rank(y)                 # Rank Y
res <- survival::concordance(x1~x2)
dt <- data.frame(x1, x2)
dat2 <- dt[order(dt$x1), ]    # Order X
ID <- LETTERS[1:n]            # Add case labels
res2 <- ConTabl(x1, x2)
dat2 <- data.frame(ID, dat2$x1, dat2$x2, c(res2[[1]], NA), c(res2[[2]], NA), c(res2[[3]], NA))
colnames(dat2) <- c("ID", "Évaluateur 1", "Évaluateur 2", "Concordances", "Discordances", "Égalité")
kable(dat2)
cat("Nombre de Concordances ($n_C$) =", res[[1]])
cat("Nombre de Discordances ($n_D$) =", res[[2]])
cat("Nombre d'égalités      ($n_T$) =", res[[3]])
cat("Tau de Kendall = ", cor(x1, x2, method="kendall"))

