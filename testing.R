### Testing
devtools::document()
devtools::load_all()
data("kva")
fit2 <- streg(Surv(failtime, event) ~ load + bearings, data = kva, distribution = "wei", x = TRUE)
predict.streg(fit2, type = "xb")
