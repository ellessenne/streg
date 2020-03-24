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
#' @importFrom stats AIC BIC
#' @param k The penalty per parameter to be used; defaults to $k = 2$, corresponding to the classical AIC.
#' @export
AIC.streg <- function(object, ..., k = 2) -2 * stats::logLik(object) + k * length(stats::coef(object))

#' @rdname AIC.streg
#' @export
AIC.summary.streg <- function(object, ...) AIC.streg(object, ...)

#' @rdname AIC.streg
#' @export
BIC.streg <- function(object, ...) AIC.streg(object = object, ..., k = log(object$n))

#' @rdname AIC.streg
#' @export
BIC.summary.streg <- function(object, ...) BIC.streg(object, ...)

#' @title Parametric Survival Models
#' @description Reports whether `x` is an object of class `streg` or `summary.streg`.
#' @param x An object to test.
#' @export
is.streg <- function(x) {
  inherits(x, "streg")
}

#' @rdname is.streg
#' @export
is.summary.streg <- function(x) inherits(x, "summary.streg")
