### Testing
devtools::load_all()

fff <- Surv(failtime, event) ~ load + bearings
fit <- streg(fff, data = kva, distribution = "exp")
summary(fit)

summary(streg(Surv(studytime, died) ~ 1, data = cancer, dist = "logn"))









library(simsurv)
set.seed(9911)

# Weibull model
covs <- data.frame(id = seq(100000), trt = stats::rbinom(100000, 1L, 0.5))
s1 <- simsurv(lambdas = 0.1, gammas = 1.5, betas = c(trt = -0.5), x = covs, maxt = 5, dist = "weibull")
df <- merge(covs, s1)
summary(streg(Surv(eventtime, status) ~ trt, data = df, dist = "wei"))

# Exponential model
covs <- data.frame(id = seq(100000), trt = stats::rbinom(100000, 1L, 0.5))
s1 <- simsurv(lambdas = 0.1, betas = c(trt = -0.5), x = covs, maxt = 5, dist = "exponential")
df <- merge(covs, s1)
summary(streg(Surv(eventtime, status) ~ trt, data = df, dist = "wei"))

# Gompertz model
covs <- data.frame(id = seq(100000), trt = stats::rbinom(100000, 1L, 0.5))
s1 <- simsurv(lambdas = 0.1, gammas = 1.5, betas = c(trt = -0.5), x = covs, maxt = 5, dist = "gompertz")
df <- merge(covs, s1)
summary(streg(Surv(eventtime, status) ~ trt, data = df, dist = "gom"))
