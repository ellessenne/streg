% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/streg.R
\name{streg}
\alias{streg}
\title{Fitting Parametric Proportional Hazards Survival Models}
\usage{
streg(
  formula,
  data,
  distribution = "exponential",
  x = FALSE,
  y = FALSE,
  init = NULL,
  control = list()
)
}
\arguments{
\item{formula}{A formula describing the model to be fitted.
The left-hand-side of the formula must be a \code{\link[survival:Surv]{survival::Surv()}} object, and (at the moment) only right censoring is supported.}

\item{data}{A data frame containing the variables in the model (as described by the model formula).}

\item{distribution}{A character string naming the distribution to be assumed for the baseline hazard function.
Possible values are \code{"exponential"}, \code{"weibull"}, and \code{"gompertz"} for exponential, Weibull, and Gompertz parametric survival regression models, respectively.
See 'Details' for more informations on each.}

\item{x}{Logical value indicating whether the model matrix used in the fitting process should be returned as components of the fitted object.}

\item{y}{Logical value indicating whether the response vector (the \code{\link[survival:Surv]{survival::Surv()}} object) used in the fitting process should be returned as components of the fitted object.}

\item{init}{An optional vector of starting values for the fitting process.
If \code{NULL} (the default), starting values will be obtained by (1) fitting the empty model for the parameters related to the distribution and (2) assuming all other coefficients start from a value of zero.}

\item{control}{A list of parameters for controlling the fitting process, which are passed to \code{\link[stats:nlminb]{stats::nlminb()}}.}
}
\value{
An object of class \code{streg}.
}
\description{
\code{\link[=streg]{streg()}} is used to fit parametric proportional hazards survival models.
}
\details{
A general parametric proportional hazards survival model is defined as
\deqn{
 h(t | X, \theta, \beta) = h_0(t | \theta) \exp(X \beta)
}
where \eqn{X} represents model covariates, \eqn{\theta} represents any ancillary parameter, and \eqn{\beta} represents regression coefficients; \eqn{h_0(\cdot)} is the baseline hazard function.

The exponential model assumes the following baseline hazard function:
\deqn{
 h_0(t | \theta) = \lambda
}
In practice, \eqn{\lambda} is incorporated in the linear predictor and modelled on the log-scale (and reported as the \code{(Intercept)} of the model).

The Weibull model assumes the following baseline hazard function:
\deqn{
 h_0(t | \theta) = p \lambda t^{p - 1}
}
\eqn{\lambda} is incorporated in the linear predictor and modelled on the log-scale (and reported as the \code{(Intercept)} of the model); \eqn{p} is also modelled on the log-scale and reported as \code{ln_p}.

Finally, the Gompertz model assumes the following baseline hazard function:
\deqn{
 h_0(t | \theta) = \lambda \exp(\gamma t)
}
\eqn{\lambda} is incorporated in the linear predictor and modelled on the log-scale (and reported as the \code{(Intercept)} of the model), \eqn{\gamma} is reported as \code{gamma} and not constrained to be strictly positive, as in Stata.
}
\examples{

library(streg)
data("kva")
fit <- streg(Surv(failtime, event) ~ load + bearings, data = kva, distribution = "exp", x = TRUE)
fit
}
