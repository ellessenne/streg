### Overall sample size
N <- 3e5

testthat::test_that("Compare fitted survival with simulated data, exponential model", {
  X <- rbinom(n = N, size = 1, prob = 0.5)
  beta <- -0.5
  lambda <- 0.5
  u <- runif(n = N)
  t <- -log(u) / (lambda * exp(X * beta))
  df <- data.frame(X, t, d = 1)
  fit <- streg::streg(survival::Surv(t, d) ~ X, data = df, distribution = "exponential", x = TRUE)
  df$fitted <- predict(fit, type = "surv")
  df$expected <- exp(-lambda * t)^(exp(X * beta))
  testthat::expect_equivalent(object = coef(fit), expected = c(log(lambda), beta), tolerance = 1e-2)
  testthat::expect_equivalent(object = df$fitted, expected = df$expected, tolerance = 1e-2)

  X <- rbinom(n = N, size = 1, prob = 0.5)
  beta <- 0.5
  lambda <- 1
  u <- runif(n = N)
  t <- -log(u) / (lambda * exp(X * beta))
  df <- data.frame(X, t, d = 1)
  fit <- streg::streg(survival::Surv(t, d) ~ X, data = df, distribution = "exponential", x = TRUE)
  df$fitted <- predict(fit, type = "surv")
  df$expected <- exp(-lambda * t)^(exp(X * beta))
  testthat::expect_equivalent(object = coef(fit), expected = c(log(lambda), beta), tolerance = 1e-2)
  testthat::expect_equivalent(object = df$fitted, expected = df$expected, tolerance = 1e-2)

  X <- rbinom(n = N, size = 1, prob = 0.5)
  beta <- 0
  lambda <- 3
  u <- runif(n = N)
  t <- -log(u) / (lambda * exp(X * beta))
  df <- data.frame(X, t, d = 1)
  fit <- streg::streg(survival::Surv(t, d) ~ X, data = df, distribution = "exponential", x = TRUE)
  df$fitted <- predict(fit, type = "surv")
  df$expected <- exp(-lambda * t)^(exp(X * beta))
  testthat::expect_equivalent(object = coef(fit), expected = c(log(lambda), beta), tolerance = 1e-2)
  testthat::expect_equivalent(object = df$fitted, expected = df$expected, tolerance = 1e-2)
})

testthat::test_that("Compare fitted hazard with simulated data, exponential model", {
  X <- rbinom(n = N, size = 1, prob = 0.5)
  beta <- -0.5
  lambda <- 0.5
  u <- runif(n = N)
  t <- -log(u) / (lambda * exp(X * beta))
  df <- data.frame(X, t, d = 1)
  fit <- streg::streg(survival::Surv(t, d) ~ X, data = df, distribution = "exponential", x = TRUE)
  df$fitted <- predict(fit, type = "hazard")
  df$expected <- lambda * exp(X * beta)
  testthat::expect_equivalent(object = coef(fit), expected = c(log(lambda), beta), tolerance = 1e-2)
  testthat::expect_equivalent(object = df$fitted, expected = df$expected, tolerance = 1e-2)

  X <- rbinom(n = N, size = 1, prob = 0.5)
  beta <- 0.5
  lambda <- 1
  u <- runif(n = N)
  t <- -log(u) / (lambda * exp(X * beta))
  df <- data.frame(X, t, d = 1)
  fit <- streg::streg(survival::Surv(t, d) ~ X, data = df, distribution = "exponential", x = TRUE)
  df$fitted <- predict(fit, type = "hazard")
  df$expected <- lambda * exp(X * beta)
  testthat::expect_equivalent(object = coef(fit), expected = c(log(lambda), beta), tolerance = 1e-2)
  testthat::expect_equivalent(object = df$fitted, expected = df$expected, tolerance = 1e-2)

  X <- rbinom(n = N, size = 1, prob = 0.5)
  beta <- 0
  lambda <- 3
  u <- runif(n = N)
  t <- -log(u) / (lambda * exp(X * beta))
  df <- data.frame(X, t, d = 1)
  fit <- streg::streg(survival::Surv(t, d) ~ X, data = df, distribution = "exponential", x = TRUE)
  df$fitted <- predict(fit, type = "hazard")
  df$expected <- lambda * exp(X * beta)
  testthat::expect_equivalent(object = coef(fit), expected = c(log(lambda), beta), tolerance = 1e-2)
  testthat::expect_equivalent(object = df$fitted, expected = df$expected, tolerance = 1e-2)
})
