data("cancer", package = "streg")
f1 <- streg::streg(survival::Surv(studytime, died) ~ 1, data = cancer, distribution = "gom")
f2 <- streg::streg(survival::Surv(studytime, died) ~ drug, data = cancer, distribution = "gom")
f3.pre <- streg::streg(survival::Surv(studytime, died) ~ age, data = cancer, distribution = "wei")
f3 <- streg::streg(survival::Surv(studytime, died) ~ age, data = cancer, distribution = "gom", init = c(coef(f3.pre)[-length(coef(f3.pre))], coef(f2)["ln_gamma"]))
f4.pre <- streg::streg(survival::Surv(studytime, died) ~ drug + age, data = cancer, distribution = "wei")
f4 <- streg::streg(survival::Surv(studytime, died) ~ drug + age, data = cancer, distribution = "gom", init = c(coef(f4.pre)[-length(coef(f4.pre))], coef(f3)["ln_gamma"]))

testthat::test_that("Compare likelihood with Stata", {
  testthat::expect_equivalent(object = logLik(f1), expected = -60.971068)
  testthat::expect_equivalent(object = logLik(f2), expected = -47.008701)
  testthat::expect_equivalent(object = logLik(f3), expected = -58.214648, tolerance = 1e-5)
  testthat::expect_equivalent(object = logLik(f4), expected = -42.140706, tolerance = 1e-4)
})

testthat::test_that("Compare coefficients with Stata", {
  testthat::expect_equivalent(object = coef(f1), expected = c(-3.383262, log(.0174893)), tolerance = 1e-5)
  testthat::expect_equivalent(object = coef(f2), expected = c(-1.553472, -1.331773, log(.0759198)), tolerance = 1e-6)
  testthat::expect_equivalent(object = coef(f3), expected = c(-7.883695, .0796139, log(.0228651)), tolerance = 1e-2)
  testthat::expect_equivalent(object = coef(f4), expected = c(-7.759638, -1.505049, .1127103, log(.0947948)), tolerance = 1e-2)
})

testthat::test_that("Compare AIC with Stata", {
  testthat::expect_equivalent(object = AIC(f1), expected = 125.9421, tolerance = 1e-6)
  testthat::expect_equivalent(object = AIC(f2), expected = 100.0174, tolerance = 1e-6)
  testthat::expect_equivalent(object = AIC(f3), expected = 122.4293, tolerance = 1e-5)
  testthat::expect_equivalent(object = AIC(f4), expected = 92.28141, tolerance = 1e-4)
})

testthat::test_that("Compare BIC with Stata", {
  testthat::expect_equivalent(object = BIC(f1), expected = 129.6845, tolerance = 1e-6)
  testthat::expect_equivalent(object = BIC(f2), expected = 105.631, tolerance = 1e-6)
  testthat::expect_equivalent(object = BIC(f3), expected = 128.0429, tolerance = 1e-5)
  testthat::expect_equivalent(object = BIC(f4), expected = 99.76622, tolerance = 1e-4)
})
