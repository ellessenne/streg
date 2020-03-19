#' @title Extract Model Coefficients
#' @description \code{coef} extracts model coefficients from a \code{streg} model fit. \code{coefficients} is an alias for it.
#' @param object An object of class \code{streg} or \code{summary.streg}.
#' @param ... Not used.
#' @export
coef.streg <- function(object, ...) object$coef

#' @rdname coef.streg
#' @export
coef.summary.streg <- function(object, ...) coef.streg(object, ...)

#' @title Calculate Variance-Covariance Matrix for a \code{streg} Model Object
#' @description Returns the variance-covariance matrix of all estimated parameters of a fitted \code{streg} model.
#' @param object An object of class \code{streg} or \code{summary.streg}.
#' @param ... Not used.
#' @export
vcov.streg <- function(object, ...) object$vcov

#' @rdname vcov.streg
#' @export
vcov.summary.streg <- function(object, ...) vcov.streg(object, ...)

#' @title Extract Log-Likelihood
#' @description Extract log-likelihood of a \code{streg} model.
#' @param object An object of class \code{streg} or \code{summary.streg}.
#' @param ... Not used.
#' @export
logLik.streg <- function(object, ...) object$loglik

#' @rdname logLik.streg
#' @export
logLik.summary.streg <- function(object, ...) logLik.streg(object, ...)
