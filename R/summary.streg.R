#' @export
summary.streg <- function(object, conf.int = 0.95, ...) {
  sout <- data.frame(
    coef = coef(object),
    stderr = sqrt(diag(vcov(object)))
  )
  sout$z <- sout$coef / sout$stderr
  sout$p <- stats::pnorm(q = sout$z)
  sout$lower <- sout$coef - stats::qnorm(p = 1 - (1 - conf.int) / 2) * sout$stderr
  sout$upper <- sout$coef + stats::qnorm(p = 1 - (1 - conf.int) / 2) * sout$stderr
  sout
}
