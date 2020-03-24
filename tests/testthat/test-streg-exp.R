data("cancer", package = "streg")
f1 <- streg::streg(survival::Surv(studytime, died) ~ 1, data = cancer, distribution = "exp")
f2 <- streg::streg(survival::Surv(studytime, died) ~ drug, data = cancer, distribution = "exp")
f3 <- streg::streg(survival::Surv(studytime, died) ~ age, data = cancer, distribution = "exp")
f4 <- streg::streg(survival::Surv(studytime, died) ~ drug + age, data = cancer, distribution = "exp")

testthat::test_that("Compare likelihood with Stata", {
  testthat::expect_equivalent(object = logLik(f1), expected = -61.342985)
  testthat::expect_equivalent(object = logLik(f2), expected = -51.687419)
  testthat::expect_equivalent(object = logLik(f3), expected = -58.848198, tolerance = 1e-5)
  testthat::expect_equivalent(object = logLik(f4), expected = -48.837598, tolerance = 1e-5)
})

testthat::test_that("Compare coefficients with Stata", {
  testthat::expect_equivalent(object = coef(f1), expected = c(-3.178054))
  testthat::expect_equivalent(object = coef(f2), expected = c(-1.323385, -.9763093), tolerance = 1e-5)
  testthat::expect_equivalent(object = coef(f3), expected = c(-7.321192, .074351), tolerance = 1e-2)
  testthat::expect_equivalent(object = coef(f4), expected = c(-5.629056, -1.014594, .078479), tolerance = 1e-2)
  testthat::expect_equivalent(object = coef(f4), expected = c(-5.629056, -1.014594, .078479), tolerance = 1e-2)
})
