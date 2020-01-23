### Fitting a Weibull model
library(simsurv)
set.seed(9911)
covs <- data.frame(id = 1:100, trt = stats::rbinom(100, 1L, 0.5))
s1 <- simsurv(lambdas = 0.1, gammas = 1.5, betas = c(trt = -0.5), x = covs, maxt = 5)
df <- merge(covs, s1)

library(survival)
S <- Surv(df$eventtime, df$status)



weibull_ll <- function(pars, data, S) {
  logp <- pars[1]
  beta <- pars[-1]
  ll <- S[, 2] * (logp + data %*% beta + (exp(logp) - 1) * log(S[, 1])) - exp(data %*% beta) * (S[, 1]^(exp(logp)))
  ll <- sum(ll)
  return(-ll)
}


weibull_ll(pars = c(0, 0, 0), data = model.matrix(~trt, data = df), S = S)

formula <- survival::Surv(time = df$eventtime, event = df$status) ~ trt

optim(par = c(0, 0, 0), fn = weibull_ll, data = model.matrix(~trt, data = df), S = S, method = "BFGS")

streg <- function(formula, data) {
  S <- eval(expr = formula[[2]], envir = data)
  formula[[2]] <- NULL
  data <- model.matrix(formula, data = data)
  init <- rep(0, ncol(data) + 1)
  optim(par = init, fn = weibull_ll, data = data, S = S, method = "BFGS", hessian = TRUE)
}
streg(Surv(eventtime, status) ~ trt, data = df)
