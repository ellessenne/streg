### Testing
devtools::load_all()

fff <- Surv(failtime, event) ~ load + bearings
fit <- streg(fff, data = kva, distribution = "exp")
summary(fit)

summary(streg(Surv(studytime, died) ~ 1, data = cancer, dist = "gom"))
