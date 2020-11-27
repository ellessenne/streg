#' @title Print \code{streg} Fits
#' @description Print the coefficients from a \code{streg} fit.
#'
#' @param x An object of class \code{streg}.
#' @param digits The number of significant digits to use when printing.
#' @param ... Not used.
#'
#' @export
print.streg <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat(tools::toTitleCase(x$distribution), "regression -- log-relative hazard form\n\n")
  if (length(stats::coef(x))) {
    cat("Coefficients:\n")
    print.default(format(stats::coef(x), digits = digits), print.gap = 2L, quote = FALSE)
  } else {
    cat("No coefficients\n")
  }
  cat("\n")
  invisible(x)
}

#' @title Extract Model Coefficients
#' @description Extract model coefficients from a \code{streg} fit.
#'
#' @param object An object of class \code{streg}.
#' @param value Value of coefficients to replace.
#' @param ... Not used.
#' @importFrom stats coef
#' @importFrom rstpm2 "coef<-"
#'
#' @seealso [stats::coef()]
#' @seealso [stats::coefficients()]
#'
#' @export
coef.streg <- function(object, ...) {
  object$coefficients
}

#' @rdname coef.streg
# This S3 method is needed to support rstpm2::predictnl
"coef<-.streg" <- function(object, value) {
  object$coefficients <- value
  return(object)
}

#' @title Calculate Variance-Covariance Matrix for a Fitted Model Object
#' @description Returns the variance-covariance matrix of the main parameters of a fitted \code{streg} model.
#'
#' @param object An object of class \code{streg}.
#' @param ... Not used.
#' @importFrom stats vcov
#'
#' @seealso [stats::vcov()]
#' @export
vcov.streg <- function(object, ...) {
  object$vcov
}

#' @title Extract the Number of Observations from a Fit.
#' @description Extract the number of 'observations' from an \code{streg} model fit.
#'
#' @param object An object of class \code{streg}.
#' @param ... Not used.
#'
#' @seealso [stats::nobs()]
#' @importFrom stats nobs
#'
#' @export
nobs.streg <- function(object, ...) {
  object$n
}

#' @title Extract Log-Likelihood
#' @description Extract log-likelihood from a fitted \code{streg} model.
#'
#' @param object An object of class \code{streg}.
#' @param ... Not used.
#' @importFrom stats logLik
#'
#' @seealso [stats::logLik()]
#'
#' @export
logLik.streg <- function(object, ...) {
  out <- object$loglik
  dd <- diag(object$vcov)
  attr(out, "df") <- sum(!is.na(dd) & dd > 0)
  class(out) <- "logLik"
  return(out)
}

#' @title Parametric Survival Models
#' @description Reports whether `x` is an object of class `streg` or `summary.streg`.
#' @param x An object to test.
#'
#' @export
is.streg <- function(x) {
  inherits(x, "streg")
}

#' @rdname is.streg
#' @export
is.summary.streg <- function(x) inherits(x, "summary.streg")
