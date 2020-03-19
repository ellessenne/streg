#' @keywords internal
weibull_ll <- function(pars, data, S) {
  beta <- pars[-length(pars)]
  logp <- pars[length(pars)]
  ll <- S[, 2] * (logp + data %*% beta + (exp(logp) - 1) * log(S[, 1])) - exp(data %*% beta) * (S[, 1]^(exp(logp)))
  ll <- sum(ll)
  return(-ll)
}

#' @keywords internal
exponential_ll <- function(pars, data, S) {
  beta <- pars
  ll <- S[, 2] * (data %*% beta + log(S[, 1])) - exp(data %*% beta) * S[, 1]
  ll <- sum(ll)
  return(-ll)
}
