#' @title Summarizing \code{streg} Fits
#' @description These functions are all methods for class \code{streg} or \code{summary.streg} objects.
#' @param object An object of class \code{streg}
#' @param conf.int Significancy level for confidence intervals. Defaults to 0.95.
#' @param ... Not used.
#'
#' @export
summary.streg <- function(object, conf.int = 0.95, ...) {
  btab <- matrix(NA, ncol = 6, nrow = length(stats::coef(object)))
  colnames(btab) <- c("Estimate", "Std. Error", "z", "Pr(>|z|)", paste0("[", sprintf("%1.0f%%", 100 * conf.int), " Conf."), "Interval]")
  rownames(btab) <- names(stats::coef(object))
  btab[, 1] <- stats::coef(object)
  btab[, 2] <- sqrt(diag(stats::vcov(object)))
  btab[, 3] <- btab[, 1] / btab[, 2]
  btab[, 4] <- 2 * stats::pnorm(-abs(btab[, 3]))
  btab[, 5] <- btab[, 1] - stats::qnorm(1 - (1 - conf.int) / 2) * btab[, 2]
  btab[, 6] <- btab[, 1] + stats::qnorm(1 - (1 - conf.int) / 2) * btab[, 2]
  object$coefftable <- btab
  #
  structure(object, class = c("summary.streg", class(object)))
}

#' @param x An object of class \code{summary.streg}
#' @param digits The number of significant digits to use when printing.
#' @rdname summary.streg
#' @export
print.summary.streg <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  if (x$distribution == "invweibull") {
    ddd <- "Inverse Weibull"
  } else if (x$distribution == "lognormal") {
    ddd <- "log-Normal"
  } else {
    ddd <- tools::toTitleCase(x$distribution)
  }
  cat(ddd, "regression -- log-relative hazard form\n\n")
  cat("No. of subject =", x$n, "\n")
  cat("No. of failures =", x$nevent, "\n")
  cat("Time at risk =", x$time.at.risk, "\n\n")
  cat("Log likelihood =", stats::logLik(x), "\n\n")

  stats::printCoefmat(x$coefftable, digits = digits, na.print = "NA", cs.ind = c(1, 2, 5, 6), tst.ind = 3, zap.ind = 4)
  invisible(x)
}
