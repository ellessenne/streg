#' @export
summary.streg <- function(object, ...) {
  # Augment object
  out <- object
  # Create coeftable
  se <- sqrt(diag(stats::vcov(object)))
  est <- stats::coefficients(object)
  zval <- est / se
  coefficients <- cbind(
    Estimate = est,
    `Std. Error` = se,
    `z value` = zval,
    `Pr(>|z|)` = 2 * stats::pnorm(q = abs(zval), lower.tail = FALSE)
  )
  out$coeftable <- coefficients
  # Return an object of class summary.streg and streg (to inherit other S3 methods)
  structure(out, class = c("summary.streg", "streg"))
}

#' @export
print.summary.streg <- function(x, digits = max(3, getOption("digits") - 3), signif.stars = getOption("show.signif.stars"), ...) {
  cat(tools::toTitleCase(x$distribution), "regression -- log-relative hazard form\n\n")
  cat("N. of subjects \t=", x$n, "\n")
  cat("N. of failures \t=", x$n.events, "\n")
  cat("Time at risk \t=", x$time.at.risk, "\n\n")
  cat("Log likelihood \t=", stats::logLik(x), "\n\n")
  stats::printCoefmat(x$coeftable, digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
  cat("\n")
  invisible(x)
}
