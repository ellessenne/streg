#' @export
predict.streg <- function(object, type = c("median", "mean", "hazard", "hr", "xb", "stdp", "surv", "csurv"), newdata = NULL, ...) {
  type <- match.arg(type)

  if (is.null(newdata)) {
    if (is.null(object$x)) stop("Fitting an 'streg' model with argument 'x = TRUE' is required when no 'newdata' is specified.", call. = TRUE)
    newdata <- object$x
  } else {
    Surv_terms <- unlist(strsplit(as.character(object$formula[[2]]), split = ","))
    Surv_terms <- Surv_terms[!grepl("Surv", Surv_terms)]
    if (!(Surv_terms[1] %in% names(newdata))) stop(paste0("A variable in 'newdata' representing time (probably '", Surv_terms[1], "') is required."), call. = FALSE)
    if (!(Surv_terms[2] %in% names(newdata))) newdata[[Surv_terms[2]]] <- 1
  }
  S <- eval(expr = object$formula[[2]], envir = newdata)
  X <- stats::model.matrix(object$formula[-2], data = newdata)

  predict_fun <- switch(type,
    "median" = .median,
    "mean" = .mean,
    "hazard" = .hazard,
    "hr" = .hr,
    "xb" = .xb,
    "surv" = .surv
  )
  out <- predict_fun(S = S, X = X, coefficients = object$coefficients, distribution = object$distribution)
  colnames(out) <- NULL
  return(out[, 1])
}

#' @keywords internal
.median <- function(S, X, coefficients, distribution) {
  stop("Prediction of type = 'median' not implemented yet.", call. = TRUE)
}

#' @keywords internal
.mean <- function(S, X, coefficients, distribution) {
  stop("Prediction of type = 'mean' not implemented yet.", call. = TRUE)
}

#' @keywords internal
.hazard <- function(S, X, coefficients, distribution) {
  t <- S[, which(grepl("^time|^start", colnames(S))), drop = FALSE]
  if (distribution == "exponential") {
    out <- exp(X %*% coefficients)
  } else if (distribution == "weibull") {
    out <- exp(coefficients["ln_p"]) * t^(exp(coefficients["ln_p"]) - 1) * exp(X %*% coefficients[colnames(X)])
  } else if (distribution == "gompertz") {
    out <- exp(exp(coefficients["ln_gamma"]) * t) * exp(X %*% coefficients[colnames(X)])
  } else if (distribution == "invweibull") {
    stop("Not implemented yet.", call. = TRUE)
  } else if (distribution == "lognormal") {
    stop("Not implemented yet.", call. = TRUE)
  }
  return(out)
}

#' @keywords internal
.hr <- function(S, X, coefficients, distribution) {
  out <- exp(.xb(S = S, X = X, coefficients = coefficients, distribution = distribution))
  return(out)
}

#' @keywords internal
.xb <- function(S, X, coefficients, distribution) {
  out <- X %*% coefficients[colnames(X)]
  return(out)
}

#' @keywords internal
.surv <- function(S, X, coefficients, distribution) {
  t <- S[, which(grepl("^time|^start", colnames(S))), drop = FALSE]
  if (distribution == "exponential") {
    out <- exp(-t)^exp(X %*% coefficients)
  } else if (distribution == "weibull") {
    out <- exp(-(t^exp(coefficients["ln_p"])))^exp(X %*% coefficients[colnames(X)])
  } else if (distribution == "gompertz") {
    lambda <- exp(coefficients["(Intercept)"])
    gamma <- exp(coefficients["ln_gamma"])
    coefficients <- coefficients[colnames(X)][-1]
    X <- X[, -1, drop = FALSE]
    out <- exp(-lambda / gamma * (exp(gamma * t) - 1))^exp(X %*% coefficients)
  } else if (distribution == "invweibull") {
    stop("Not implemented yet.", call. = TRUE)
  } else if (distribution == "lognormal") {
    stop("Not implemented yet.", call. = TRUE)
  }
  return(out)
}
