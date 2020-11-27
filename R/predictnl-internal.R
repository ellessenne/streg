#' @keywords internal
.predictnl_internal <- function(object, newdata, .t, .type) {
  if (.type == "xb") {
    out <- .xb(X = newdata, par = coef(object))
  } else {
    if (object$distribution == "exponential") {
      if (.type == "hazard") {
        out <- .exp_h(X = newdata, par = coef(object))
      } else if (.type == "surv") {
        out <- .exp_surv(X = newdata, t = .t, par = coef(object))
      }
    } else if (object$distribution == "weibull") {
      if (.type == "hazard") {
        out <- .wei_h(X = newdata, t = .t, par = coef(object))
      } else if (.type == "surv") {
        out <- .wei_surv(X = newdata, t = .t, par = coef(object))
      }
    } else if (object$distribution == "gompertz") {
      if (.type == "hazard") {
        out <- .gom_h(X = newdata, t = .t, par = coef(object))
      } else if (.type == "surv") {
        out <- .gom_surv(X = newdata, t = .t, par = coef(object))
      }
    }
  }
  ###
  dim(out) <- NULL
  return(out)
}
