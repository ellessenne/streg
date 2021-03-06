### Testing
devtools::document()
devtools::load_all()
data("kva")
fit1 <- streg(Surv(failtime, event) ~ load + bearings, data = kva, distribution = "exp", x = TRUE)
predict(fit1, type = "s")
predict(fit1, type = "s", se.fit = T)
fit2 <- streg(Surv(failtime, event) ~ load + bearings, data = kva, distribution = "wei", x = TRUE)
predict(fit2, type = "s")
predict(fit2, type = "s", se.fit = T)
fit3 <- streg(Surv(failtime, event) ~ load + bearings, data = kva, distribution = "gom", x = TRUE)
predict(fit3, type = "s")
predict(fit3, type = "s", se.fit = T)
