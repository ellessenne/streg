#' @title Extract Model Coefficients
#' @description `coef` extracts model coefficients from a `streg` model fit. `coefficients` is an alias for it.
#' @param object An object of class `streg` or `summary.streg`.
#' @param ... Not used.
#' @export
coef.streg <- function(object, ...) object$coefficients

#' @rdname coef.streg
#' @export
coef.summary.streg <- function(object, ...) coef.streg(object, ...)

#' @title Calculate Variance-Covariance Matrix for a `streg` Model Object
#' @description Returns the variance-covariance matrix of all estimated parameters of a fitted `streg` model.
#' @param object An object of class `streg` or `summary.streg`.
#' @param ... Not used.
#' @export
vcov.streg <- function(object, ...) object$vcov

#' @rdname vcov.streg
#' @export
vcov.summary.streg <- function(object, ...) vcov.streg(object, ...)

#' @title Extract Log-Likelihood
#' @description Extract log-likelihood of a `streg` model.
#' @param object An object of class `streg` or `summary.streg`.
#' @param ... Not used.
#' @export
logLik.streg <- function(object, ...) object$loglik

#' @rdname logLik.streg
#' @export
logLik.summary.streg <- function(object, ...) logLik.streg(object, ...)

#' @title Extract Log-Likelihood
#' @description Extract log-likelihood of a `streg` model.
#' @param object An object of class `streg` or `summary.streg`.
#' @param ... Not used.
#' @export
logLik.streg <- function(object, ...) object$loglik

#' @rdname logLik.streg
#' @export
logLik.summary.streg <- function(object, ...) logLik.streg(object, ...)

#' @title Akaike's Information Criterion
#' @description Extract Akaike's Information Criterion (AIC) or Bayesian Information Criterion (BIC) from fitted models of class `streg`.
#' @param object An object of class `streg` or `summary.streg`.
#' @param ... Not used.
#' @param k The penalty per parameter to be used; defaults to $k = 2$, corresponding to the classical AIC.
#' @export
AIC.streg <- function(object, ..., k = 2) -2 * object$loglik + k * length(stats::coef(object))
