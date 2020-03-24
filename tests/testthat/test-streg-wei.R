data("cancer", package = "streg")
f1 <- streg::streg(survival::Surv(studytime, died) ~ 1, data = cancer, distribution = "weibull")
f2 <- streg::streg(survival::Surv(studytime, died) ~ drug, data = cancer, distribution = "weibull")
f3 <- streg::streg(survival::Surv(studytime, died) ~ age, data = cancer, distribution = "weibull")
f4 <- streg::streg(survival::Surv(studytime, died) ~ drug + age, data = cancer, distribution = "weibull")

testthat::test_that("Compare likelihood with Stata", {
  testthat::expect_equivalent(object = logLik(f1), expected = -60.624022)
  testthat::expect_equivalent(object = logLik(f2), expected = -47.521229)
  testthat::expect_equivalent(object = logLik(f3), expected = -57.726859, tolerance = 1e-5)
  testthat::expect_equivalent(object = logLik(f4), expected = -42.662838, tolerance = 1e-5)
})

testthat::test_that("Compare coefficients with Stata", {
  testthat::expect_equivalent(object = coef(f1), expected = c(-3.798627, .1882428), tolerance = 1e-4)
  testthat::expect_equivalent(object = coef(f2), expected = c(-2.595273, -1.20743, .4660248), tolerance = 1e-4)
  testthat::expect_equivalent(object = coef(f3), expected = c(-8.524359, .081975, .2319948), tolerance = 1e-2)
  testthat::expect_equivalent(object = coef(f4), expected = c(-8.966805, -1.35337, .1109033, .5640338), tolerance = 1e-2)
})

testthat::test_that("Compare AIC with Stata", {
  testthat::expect_equivalent(object = AIC(f1), expected = 125.248, tolerance = 1e-6)
  testthat::expect_equivalent(object = AIC(f2), expected = 101.0425, tolerance = 1e-6)
  testthat::expect_equivalent(object = AIC(f3), expected = 121.4537, tolerance = 1e-5)
  testthat::expect_equivalent(object = AIC(f4), expected = 93.32568, tolerance = 1e-5)
})

testthat::test_that("Compare BIC with Stata", {
  testthat::expect_equivalent(object = BIC(f1), expected = 128.9904, tolerance = 1e-6)
  testthat::expect_equivalent(object = BIC(f2), expected = 106.6561, tolerance = 1e-6)
  testthat::expect_equivalent(object = BIC(f3), expected = 127.0673, tolerance = 1e-5)
  testthat::expect_equivalent(object = BIC(f4), expected = 100.8105, tolerance = 1e-5)
})
