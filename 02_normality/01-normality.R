#' ---
#' title: "Data simulation to compare methods for testing normality assumption"
#' author: ""
#' date: "Last modified: 2025-09-15"
#' bibliography: ../lit.bib
#' ---

#' # Example ChickWeight

data("ChickWeight")

lattice::xyplot(weight ~ Time | Chick, data = ChickWeight,
                type = "l", layout = c(10, 5))

hist(ChickWeight$weight, freq = FALSE, border = "white", ylim = c(0, 0.008))
lines(density(ChickWeight$weight))

qqnorm(ChickWeight$weight)
qqline(ChickWeight$weight)

shapiro.test(ChickWeight$weight)
ks.test(ChickWeight$weight, "pnorm",
        mean = mean(ChickWeight$weight),
        sd = sd(ChickWeight$weight))
# --> mean and sd should *not* be estimated from the sample!

# Additional tests from R package `nortest`

nortest::ad.test(ChickWeight$weight)      # Anderson-Darling test for normality
nortest::cvm.test(ChickWeight$weight)     # Cramer-von Mises test for normality
nortest::lillie.test(ChickWeight$weight)  # Lilliefors (Kolmogorov-Smirnov) test for normality
nortest::pearson.test(ChickWeight$weight) # Pearson chi-square test for normality
nortest::sf.test(ChickWeight$weight)      # Shapiro-Francia test for normality


#' # Simulate skewed data
#'
# For the data generating process we take the funtion `fGarch::rsnorm()`. In
# order to change it easily, we still wrap it in `dgp()`.

dgp <- function(n, xi) {
  fGarch::rsnorm(n = n, mean = 0, sd = 1, xi = xi)
}

one_simulation <- function(n, xi) {
  y <- dgp(n = n, xi = xi)
  pval <- c(
    s = shapiro.test(y)$p.value,
    k = ks.test(y, "pnorm", mean = 0, sd = 1)$p.value
  )
  return(pval)
}

simulation_study <- function(niter, n, xi) {
  comb <- expand.grid(i = 1:niter, n = n, xi = xi)
  p <- mapply(one_simulation, n = comb$n, xi = comb$xi)
  comb$s <- p["s", ]
  comb$k <- p["k", ]
  comb
}

#+ cache = TRUE
res <- simulation_study(niter = 1000,
                        n = c(10, 50, 100, 200),
                        xi = seq(1, 10, 2))

lattice::xyplot(s + k ~ xi | factor(n), data = res,
                type = "a",,
                auto.key = list(space = "top", column = 2),
                xlab = expression(xi),
                ylab = "Mean p value",
                abline = list(h = c(0.05, 0.8), lty = 3))

# What Wikipedia says on this:
# https://de.wikipedia.org/wiki/Shapiro-Wilk-Test

