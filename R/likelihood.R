#' @keywords internal
exponential_ll <- function(pars, data, time, status) {
  beta <- pars
  ll <- status * (data %*% beta + log(time)) - exp(data %*% beta) * time
  ll <- sum(ll)
  return(-ll)
}

#' @keywords internal
weibull_ll <- function(pars, data, time, status) {
  beta <- pars[-length(pars)]
  logp <- pars[length(pars)]
  ll <- status * (logp + data %*% beta + (exp(logp) - 1) * log(time)) - exp(data %*% beta) * (time^(exp(logp)))
  ll <- sum(ll)
  return(-ll)
}

#' @keywords internal
gompertz_ll <- function(pars, data, time, status) {
  beta <- pars[-length(pars)]
  loggamma <- pars[length(pars)]
  ll <- status * (data %*% beta + exp(loggamma) * time) - exp(data %*% beta) / exp(loggamma) * (exp(exp(loggamma) * time) - 1)
  ll <- sum(ll)
  return(-ll)
}

#' @keywords internal
invweibull_ll <- function(pars, data, time, status) {
  beta <- pars[-length(pars)]
  logp <- pars[length(pars)]
  ll <- status * (logp + data %*% beta - (exp(logp) + 1) * log(time) - log(exp(exp(data %*% beta) * time^(-exp(logp))) - 1)) + log(1 - exp(-exp(data %*% beta) * time^(-exp(logp))))
  ll <- sum(ll)
  return(-ll)
}

#' @keywords internal
lognormal_ll <- function(pars, data, time, status) {
  beta <- pars[-length(pars)]
  logsigma <- pars[length(pars)]
  argof <- (log(time) - exp(data %*% beta)) / exp(logsigma)
  logh <- stats::dnorm(x = argof, log = TRUE) - logsigma - log(time) - stats::pnorm(q = argof, lower.tail = FALSE, log.p = TRUE)
  logS <- stats::pnorm(q = argof, lower.tail = FALSE, log.p = TRUE)
  ll <- status * logh + logS
  ll <- sum(ll)
  return(-ll)
}
