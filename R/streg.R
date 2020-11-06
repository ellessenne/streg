#' @export
streg <- function(formula, data, distribution = "exponential", x = FALSE, y = FALSE, init = NULL, control = list()) {
  # Match distribution
  distribution <- match.arg(distribution, choices = c("exponential", "weibull", "gompertz"))
  # Process Surv component
  S <- eval(expr = formula[[2]], envir = data)
  start <- S[, which(grepl("^time|^start", colnames(S))), drop = FALSE]
  status <- S[, which(grepl("^status", colnames(S))), drop = FALSE]
  # Create model matrix
  .data <- stats::model.matrix(formula[-2], data = data)
  # Process starting values
  if (is.null(init)) {
    init <- rep(1, ncol(.data) + as.numeric(distribution != "exponential"))
    init[seq(ncol(.data))] <- 0
  }
  names(init) <- switch(distribution,
    "exponential" = colnames(.data),
    "weibull" = c(colnames(.data), "ln_p"),
    "gompertz" = c(colnames(.data), "gamma"),
  )
  # Pick correct likelihood function
  f <- switch(distribution,
    "exponential" = TMB::MakeADFun(data = list(model = "ll_exp", data = .data, time = start, status = status), parameters = list(beta = init), silent = TRUE, DLL = "streg_TMBExports"),
    "weibull" = TMB::MakeADFun(data = list(model = "ll_wei", data = .data, time = start, status = status), parameters = list(beta = init[-length(init)], logp = init[length(init)]), silent = TRUE, DLL = "streg_TMBExports"),
    "gompertz" = TMB::MakeADFun(data = list(model = "ll_gom", data = .data, time = start, status = status), parameters = list(beta = init[-length(init)], gamma = init[length(init)]), silent = TRUE, DLL = "streg_TMBExports")
  )
  # Fit
  model.fit <- stats::nlminb(start = f$par, objective = f$fn, gradient = f$gr, hessian = f$he)
  # Hessian
  model.fit$hessian <- f$he(x = model.fit$par)
  # Fix names
  names(model.fit$par) <- names(init)
  colnames(model.fit$hessian) <- names(init)
  rownames(model.fit$hessian) <- names(init)

  # Create object to output
  out <- list()
  out$coefficients <- model.fit$par
  out$vcov <- solve(model.fit$hessian)
  # Add sum(log(t)) of uncensored observations to log-likelihood
  out$loglik <- -model.fit$objective
  out$n <- nrow(.data)
  out$nevent <- sum(S[, 2])
  out$time.at.risk <- sum(S[, 1])
  out$distribution <- distribution
  out$convergence <- model.fit$convergence
  out$formula <- formula
  if (x) out$x <- data
  if (y) out$y <- S
  # Return an object of class streg
  structure(out, class = "streg")
}

#' @title Print \code{streg} Fits
#' @description Print the coefficients from a \code{streg} fit.
#'
#' @param x An object of class \code{streg}.
#' @param digits The number of significant digits to use when printing.
#' @param ... Not used.
#'
#' @export
print.streg <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  if (x$distribution == "invweibull") {
    ddd <- "Inverse Weibull"
  } else if (x$distribution == "lognormal") {
    ddd <- "log-Normal"
  } else {
    ddd <- tools::toTitleCase(x$distribution)
  }
  cat(ddd, "regression -- log-relative hazard form\n\n")
  if (length(stats::coef(x))) {
    cat("Coefficients:\n")
    print.default(format(stats::coef(x), digits = digits), print.gap = 2L, quote = FALSE)
  } else {
    cat("No coefficients\n")
  }
  cat("\n")
  invisible(x)
}
