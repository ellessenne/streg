#' @importFrom stats predict
#' @export
predict.streg <- function(object, newdata = NULL, type = "xb", se.fit = FALSE, ...) {
  # object <- fit2
  # newdata <- NULL
  # type <- "xb"
  # se.fit <- FALSE

  # Process type of prediction to obtain
  type <- match.arg(arg = type, choices = c("xb", "hazard", "surv"))
  # Process newdata
  if (is.null(newdata)) {
    if (is.null(object$x)) {
      stop("'predict' requires either a data.frame to predict values for via the 'newdata' argument or the original dataset (re-fit the model by setting 'x = TRUE')", call. = FALSE)
    } else {
      newdata <- object$x
    }
  }
  # Then, create matrix data as it is used by streg for estimation
  S <- .process_streg_formula(formula = object$formula, data = newdata, which = "y")
  start <- S[, which(grepl("^time|^start", colnames(S))), drop = FALSE]
  status <- S[, which(grepl("^status", colnames(S))), drop = FALSE]
  X <- .process_streg_formula(formula = object$formula, data = newdata, which = "x")
  # Now, it all depends on whether we want std.errors or not
  if (se.fit) {
    ### This relies on predictnl
    ### Needs to be implemented here
  } else {
    if (type == "xb") {
      out <- .xb(X = X, par = coef(object))
    } else {
      if (object$distribution == "exponential") {
        if (type == "hazard") {
          out <- .exp_h(X = X, par = coef(object))
        } else if (type == "surv") {
          out <- .exp_surv(X = X, t = start, par = coef(object))
        }
      } else if (object$distribution == "weibull") {
        if (type == "hazard") {
          out <- .wei_h(X = X, t = start, par = coef(object))
        } else if (type == "surv") {
          out <- .wei_surv(X = X, t = start, par = coef(object))
        }
      } else if (object$distribution == "gompertz") {

      }
    }
  }
  dim(out) <- NULL
  return(list(X, S, out))
}
