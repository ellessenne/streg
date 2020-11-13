#' @export
streg <- function(formula, data, distribution = "exponential", x = FALSE, y = FALSE, init = NULL, control = list()) {
  # Extract call
  cl <- match.call()
  # Match distribution
  distribution <- match.arg(distribution, choices = c("exponential", "weibull", "gompertz"))
  # Process Surv component
  S <- eval(expr = formula[[2]], envir = data)
  start <- S[, which(grepl("^time|^start", colnames(S))), drop = FALSE]
  status <- S[, which(grepl("^status", colnames(S))), drop = FALSE]
  # Create model matrix
  X <- stats::model.matrix(formula[-2], data = data)
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
  if (x) out$x <- data
  if (y) out$y <- S
  out$call <- cl
  # Return an object of class streg
  structure(out, class = "streg")
}
