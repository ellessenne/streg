#' @export
streg <- function(formula, data, distribution = "exp", method = "L-BFGS-B", x = FALSE, use.numDeriv = FALSE, optim.control = list()) {
  distribution <- match.arg(distribution, choices = c("exponential", "weibull"))
  S <- eval(expr = formula[[2]], envir = data)
  formula[[2]] <- NULL
  .data <- stats::model.matrix(formula, data = data)

  if (distribution == "exponential") {
    init <- rep(0, ncol(.data))
    names(init) <- colnames(.data)
    ll <- exponential_ll
  } else if (distribution == "weibull") {
    init <- rep(0, ncol(.data) + 1)
    names(init) <- c(colnames(.data), "ln_p")
    ll <- weibull_ll
  }

  # Fit
  model.fit <- stats::optim(par = init, fn = ll, data = .data, S = S, method = method, hessian = !use.numDeriv, control = optim.control)

  # Hessian
  if (use.numDeriv) {
    model.fit$hessian <- numDeriv::hessian(func = ll, x = model.fit$par, data = .data, S = S)
  }

  # Create object to output
  out <- list()
  out$coefficients <- model.fit$par
  out$vcov <- solve(model.fit$hessian)
  # Add sum(log(t)) of uncensored observations to log-likelihood
  out$loglik <- -model.fit$value + sum(log(S[S[, 2] == 1, 1]))
  out$n <- nrow(.data)
  out$nevent <- sum(S[, 2])
  out$time.at.risk <- sum(S[, 1])
  out$convergence <- model.fit$convergence
  out$formula <- formula
  if (x) out$x <- data

  # Return an object of class streg
  class(out) <- "streg"
  return(out)
}

#' @export
coef.streg <- function(object, ...) object$coef

#' @export
vcov.streg <- function(object, ...) object$vcov

#' @export
logLik.streg <- function(object, ...) object$loglik

#' @export
print.streg <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  if (length(coef(x))) {
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits),
      print.gap = 2L, quote = FALSE
    )
  } else {
    cat("No coefficients\n")
  }
  cat("\n")
  invisible(x)
}
