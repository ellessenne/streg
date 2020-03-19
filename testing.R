### Testing
devtools::load_all()
data("kva", package = "streg")

summary(streg(Surv(failtime, event) ~ load + bearings, data = kva, distribution = "weibull"))
