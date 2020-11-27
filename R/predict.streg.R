#' @title Predict Method for Parametric Proportional Hazards Survival Models
#'
#' @description Obtain model-based predictions from a parametric proportional hazards survival model fitted by [streg()].
#'
#' @param object A fitted [streg()] object.
#' @param newdata Optionally, a data frame in which to look for variables with which to predict.
#' If `NULL` (the default), predictions will be computed for the data used to fit the model.
#' In this scenario, passing an [streg()] model fitted with the argument `x = TRUE` is required.
#' @param type The type of prediction required.
#' The default `"xb"` is to return the linear predictor.
#' Other possible predictions are the hazard function (`type = "hazard"`) and the survival function (`type = "survival"`).
#' @param se.fit Logical switch indicating if standard errors are required.
#' @param ... Not used.
#'
#' @details If `newdata` is omitted the predictions are based on the data used for the fit.
#'
#' @return If `se.fit = FALSE`, a vector with the required predictions.
#'
#' If `se.fit = TRUE` a `data.frame` is returned with a column for the predictions (named `fit`) and a column for the fitted standard errors (named `se.fit`).
#'
#' @note Standard errors are estimated using the numerical delta method, as implemented in [rstpm2::predictnl()] --- which is equivalent, in principle --- to Stata's `predictnl` command.
#'
#' @importFrom stats predict
#'
#' @export
#'
#' @examples
#'
#' library(streg)
#' data("kva", package = "streg")
#'
#' # Fit a model
#' fit <- streg(Surv(failtime, event) ~ load + bearings, data = kva, distribution = "exp", x = TRUE)
#'
#' # Predict the hazard for each subject
#' predict(fit, type = "surv")
#'
#' # Predict the survival probability, including a standard error for the fitted value
#' predict(fit, type = "surv", se.fit = TRUE)
#'
#' # Predictions for a new dataset
#' nd <- data.frame(failtime = 30, event = 1, load = 10, bearings = 3)
#' predict(fit, newdata = nd, type = "surv")
#' predict(fit, newdata = nd, type = "surv", se.fit = TRUE)
predict.streg <- function(object, newdata = NULL, type = "xb", se.fit = FALSE, ...) {
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
