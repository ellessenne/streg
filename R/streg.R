#' @title Fitting Parametric Proportional Hazards Survival Models
#'
#' @description [streg()] is used to fit parametric proportional hazards survival models.
#'
#' @param formula A formula describing the model to be fitted.
#' The left-hand-side of the formula must be a [survival::Surv()] object, and (at the moment) only right censoring is supported.
#' @param data A data frame containing the variables in the model (as described by the model formula).
#' @param distribution A character string naming the distribution to be assumed for the baseline hazard function.
#' Possible values are `"exponential"`, `"weibull"`, and `"gompertz"` for exponential, Weibull, and Gompertz parametric survival regression models, respectively.
#' See 'Details' for more informations on each.
#' @param x Logical value indicating whether the model matrix used in the fitting process should be returned as components of the fitted object.
#' @param y Logical value indicating whether the response vector (the [survival::Surv()] object) used in the fitting process should be returned as components of the fitted object.
#' @param init An optional vector of starting values for the fitting process.
#' If `NULL` (the default), starting values will be obtained by (1) fitting the empty model for the parameters related to the distribution and (2) assuming all other coefficients start from a value of zero.
#' @param control A list of parameters for controlling the fitting process, which are passed to [stats::nlminb()].
#'
#' @details A general parametric proportional hazards survival model is defined as
#' \deqn{
#'  h(t | X, \theta, \beta) = h_0(t | \theta) \exp(X \beta)
#' }
#' where \eqn{X} represents model covariates, \eqn{\theta} represents any ancillary parameter, and \eqn{\beta} represents regression coefficients; \eqn{h_0(\cdot)} is the baseline hazard function.
#'
#' The exponential model assumes the following baseline hazard function:
#' \deqn{
#'  h_0(t | \theta) = \lambda
#' }
#' In practice, \eqn{\lambda} is incorporated in the linear predictor and modelled on the log-scale (and reported as the `(Intercept)` of the model).
#'
#' The Weibull model assumes the following baseline hazard function:
#' \deqn{
#'  h_0(t | \theta) = p \lambda t^{p - 1}
#' }
#' \eqn{\lambda} is incorporated in the linear predictor and modelled on the log-scale (and reported as the `(Intercept)` of the model); \eqn{p} is also modelled on the log-scale and reported as `ln_p`.
#'
#' Finally, the Gompertz model assumes the following baseline hazard function:
#' \deqn{
#'  h_0(t | \theta) = \lambda \exp(\gamma t)
#' }
#' \eqn{\lambda} is incorporated in the linear predictor and modelled on the log-scale (and reported as the `(Intercept)` of the model), \eqn{\gamma} is reported as `gamma` and not constrained to be strictly positive, as in Stata.
#'
#' @return An object of class `streg`.
#'
#' @export
#'
#' @examples
#'
#' library(streg)
#' data("kva")
#' fit <- streg(Surv(failtime, event) ~ load + bearings, data = kva, distribution = "exp", x = TRUE)
#' fit
streg <- function(formula, data, distribution = "exponential", x = FALSE, y = FALSE, init = NULL, control = list()) {
  # Extract call
  cl <- match.call()
  # Match distribution
  distribution <- match.arg(distribution, choices = c("exponential", "weibull", "gompertz"))
  # Process Surv component
  S <- .process_streg_formula(formula = formula, data = data, which = "y")
  start <- S[, which(grepl("^time|^start", colnames(S))), drop = FALSE]
  status <- S[, which(grepl("^status", colnames(S))), drop = FALSE]
  # Create model matrix
  X <- .process_streg_formula(formula = formula, data = data, which = "x")
  # Process starting values
  if (is.null(init)) {
    # Fit the empty model here to improve starting values, if not provided with starting values
    empty_formula <- stats::update(formula, ~ -.)
    empty_X <- stats::model.matrix(empty_formula[-2], data = data)
    init <- rep(1, ncol(empty_X) + as.numeric(distribution != "exponential"))
    f <- switch(distribution,
      "exponential" = TMB::MakeADFun(data = list(model = "ll_exp", data = empty_X, time = start, status = status), parameters = list(beta = init), silent = TRUE, DLL = "streg_TMBExports"),
      "weibull" = TMB::MakeADFun(data = list(model = "ll_wei", data = empty_X, time = start, status = status), parameters = list(beta = init[-length(init)], logp = init[length(init)]), silent = TRUE, DLL = "streg_TMBExports"),
      "gompertz" = TMB::MakeADFun(data = list(model = "ll_gom", data = empty_X, time = start, status = status), parameters = list(beta = init[-length(init)], gamma = init[length(init)]), silent = TRUE, DLL = "streg_TMBExports")
    )
    # Fit
    empty_fit <- stats::nlminb(start = f$par, objective = f$fn, gradient = f$gr, hessian = f$he)
    init <- rep(0, ncol(X) + as.numeric(distribution != "exponential"))
    init[1] <- empty_fit$par[1]
    if (length(empty_fit$par) > 1) init[length(init)] <- empty_fit$par[2]
  }
  names(init) <- switch(distribution,
    "exponential" = colnames(X),
    "weibull" = c(colnames(X), "ln_p"),
    "gompertz" = c(colnames(X), "gamma"),
  )
  # Pick correct likelihood function
  f <- switch(distribution,
    "exponential" = TMB::MakeADFun(data = list(model = "ll_exp", data = X, time = start, status = status), parameters = list(beta = init), silent = TRUE, DLL = "streg_TMBExports"),
    "weibull" = TMB::MakeADFun(data = list(model = "ll_wei", data = X, time = start, status = status), parameters = list(beta = init[-length(init)], logp = init[length(init)]), silent = TRUE, DLL = "streg_TMBExports"),
    "gompertz" = TMB::MakeADFun(data = list(model = "ll_gom", data = X, time = start, status = status), parameters = list(beta = init[-length(init)], gamma = init[length(init)]), silent = TRUE, DLL = "streg_TMBExports")
  )
  # Fit
  model.fit <- stats::nlminb(start = f$par, objective = f$fn, gradient = f$gr, hessian = f$he)
  # Hessian
  model.fit$hessian <- f$he(x = model.fit$par)
  # Fix names of Hessian matrix
  names(model.fit$par) <- names(init)
  colnames(model.fit$hessian) <- names(init)
  rownames(model.fit$hessian) <- names(init)

  # Create object to output
  out <- list()
  out$coefficients <- model.fit$par
  out$vcov <- solve(model.fit$hessian)
  # Add sum(log(t)) of uncensored observations to log-likelihood
  out$loglik <- -model.fit$objective
  out$n <- nrow(X)
  out$n.events <- sum(status)
  out$time.at.risk <- sum(start)
  out$distribution <- distribution
  out$convergence <- model.fit$convergence
  out$formula <- formula
  out$time.range <- range(start)
  if (x) out$x <- data
  if (y) out$y <- S
  out$call <- cl
  # Return an object of class streg
  structure(out, class = "streg")
}
