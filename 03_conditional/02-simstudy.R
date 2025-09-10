#' ---
#' title: "Data simulation to examine test properties"
#' author: ""
#' date: "Last modified: 2025-09-10"
#' bibliography: ../lit.bib
#' ---

# Load packages
library("lattice")

#' # Quality of point estimates

#' ## Data generating process

dgp <- function(npers, beta, normality){
  x <- runif(n = npers, min = 0, max = 5)
  if (normality) {
    err <- rnorm(n = npers, mean = 0, sd = 1)
  } else {
    err <- fGarch::rsnorm(n = npers, mean = 0, sd = 1, xi = 5)
    #err <- rchisq(n = npers, df = 1) |> scale()
  }
  y <- beta[1] + beta[2] * x + err
  dat <- data.frame(x = x, y = y)
  return(dat)
}

#' ## A single simulation run

one_simulation <- function(npers, beta, normality){
  dat <- dgp(npers = npers, beta = beta, normality = normality)
  model <- lm(y ~ x, data = dat)
  slope_est <- coef(model)[2]
  return(slope_est)
}

#' ## Complete simulation design

simulation_study <- function(niter, npers, beta, normality){
  prs <- expand.grid(i = 1:niter, npers = npers, 
                     normality = normality)
  slope_est <- rep(NA, nrow(prs))
  for(i in 1:nrow(prs)) {
    slope_est[i] <- one_simulation(npers = prs$npers[i],
                                   beta = beta,
                                   normality = prs$normality[i])
  }
  return(cbind(prs, slope_est))
}

#+ cache = TRUE
res <- simulation_study(niter = 1000,
                        #npers = c(20, 50, 100),
                        npers = c(10, 20, 30),
                        #npers = c(100, 500, 1000),
                        beta = c(1, 2.5),
                        normality = c(TRUE, FALSE))

aggregate(slope_est ~ npers + normality, data = res, FUN = mean)

bwplot(slope_est ~ factor(npers) | factor(normality), data = res,
       panel = function(...) {
         panel.bwplot(...)
         panel.abline(h = 2.5, lty = 2)
       }
)

#'
#' * Adjust the code above so that the return value of `one_simulation()` is the
#'   bias and not the estimated slope
#' * Should `simulation_study()` be changed as well?
#' * Create a plot showing the absolue bias for the different conditions of your
#'   simulation study
#' * How can you interpret the results?


#+ echo = FALSE, results = "hide", fig.show = "hide", cache = TRUE
# Data generating process stays the same, but we need to adjust the single
# simulation run.

# Single simulation run
one_simulation <- function(npers, beta, normality){
  dat <- dgp(npers = npers, beta = beta, normality = normality)
  model <- lm(y ~ x, data = dat)
  bias <- coef(model)[2] - beta[2]
  return(bias)
}

# Complete simulation design
simulation_study <- function(niter, npers, beta, normality){
  prs <- expand.grid(i = 1:niter, npers = npers, 
                     normality = normality)
  bias <- rep(NA, nrow(prs))
  for(i in 1:nrow(prs)) {
    bias[i] <- one_simulation(npers = prs$npers[i],
                              beta = beta,
                              normality = prs$normality[i])
  }
  return(cbind(prs, bias))
}

res <- simulation_study(niter = 1000,
                        npers = c(20, 50, 100),
                        beta = c(1, 2.5),
                        normality = c(TRUE, FALSE))

resm <- aggregate(bias ~ npers + normality, data = res, FUN = mean)
resm$sd <- aggregate(bias ~ npers + normality, data = res, FUN = sd)[, 3]
resm$n <- aggregate(bias ~ npers + normality, data = res, FUN = length)[, 3]
resm$se <- resm$sd / resm$n
resm$stand_bias <- resm$bias / resm$se
resm$perc_bias <- resm$bias / 2.5
resm$abs_bias <- aggregate(bias ~ npers + normality, data = res,
                           FUN = function(x) mean(abs(x)))[, 3]

xyplot(abs_bias ~ factor(npers), data = resm, groups = normality,
       xlab = "Number of observations",
       ylab = "Absolute bias",
       type = "b",
       auto.key = list(space = "top", columns = 2))

#' # Quality of confidence intervals
#'
#'
#' ## A single simulation run
#'

one_simulation <- function(npers, beta, normality){
  dat <- dgp(npers = npers, beta = beta, normality = normality)
  model <- lm(y ~ x, data = dat)
  res <- list()
  res[[1]] <- coef(model)[2]
  res[[2]] <- confint(model)[2, 1] < beta[2] & confint(model)[2, 2] > beta[2]
  return(res)
}

#' ## Complete simulation design

simulation_study <- function(niter, npers, beta, normality){
  prs <- expand.grid(i = 1:niter, npers = npers, 
                     normality = normality)
  slope_est <- rep(NA, nrow(prs))
  covered <- rep(NA, nrow(prs))
  for(i in 1:nrow(prs)) {
    res <- one_simulation(npers = prs$npers[i],
                          beta = beta,
                          normality = prs$normality[i])
    slope_est[i] <- res[[1]]
    covered[i] <- res[[2]]
  }
  return(cbind(prs, slope_est, covered))
}

#+ cache = TRUE
res <- simulation_study(niter = 1000,
                        npers = c(20, 50, 100),
                        beta = c(1, 2.5),
                        normality = c(TRUE, FALSE))

aggregate(cbind(slope_est, covered) ~ npers + normality, data = res, FUN = mean)

xyplot(covered ~ factor(npers), data = res, groups = normality,
     type = "a",
     abline = list(h = .95, lty = 2),
     xlab = "Number of observations",
     ylab = "Proportion of CIs covering true value",
     auto.key = list(space = "top", columns = 2),
     ylim = c(.7, 1)
)


#' ## Look at width of confidence intervals

one_simulation <- function(npers, beta, normality){
  dat <- dgp(npers = npers, beta = beta, normality = normality)
  model <- lm(y ~ x, data = dat)
  res <- confint(model)[2, ] |> diff()
  return(res)
}

simulation_study <- function(niter, npers, beta, normality){
  prs <- expand.grid(i = 1:niter, npers = npers, 
                     normality = normality)
  res <- rep(NA, nrow(prs))
  for(i in 1:nrow(prs)) {
    res[i] <- one_simulation(npers = prs$npers[i],
                             beta = beta,
                             normality = prs$normality[i])
  }
  return(cbind(prs, res))
}

#+ cache = TRUE
res <- simulation_study(niter = 1000,
                        npers = c(20, 50, 100),
                        beta = c(1, 2.5),
                        normality = c(TRUE, FALSE))

aggregate(res ~ npers + normality, data = res, FUN = mean)

xyplot(res ~ npers, data = res, groups = normality,
       auto.key = TRUE,
       #ylim = c(0, 1.5),
       xlab = "Number of observations",
       ylab = "Mean width of CIs",
       type = "a")


