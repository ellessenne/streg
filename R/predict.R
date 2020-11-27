#' @importFrom stats predict
#' @export
predict.streg <- function(object, newdata = NULL, type = "xb", se.fit = FALSE, ...) {
  # object <- fit3
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
    preds <- rstpm2::predictnl(object, .predictnl_internal, newdata = X, .t = start, .type = type)
  } else {
    preds <- .predictnl_internal(object = object, newdata = X, .t = start, .type = type)
  }

  # Finally, return predictions
  return(preds)
}
