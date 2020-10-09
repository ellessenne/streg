### Testing
devtools::load_all()
library(simsurv)
set.seed(9911)

covs <- data.frame(id = seq(1e6), trt = stats::rbinom(1000, 1L, 0.5))
s1 <- simsurv(lambdas = 0.1, betas = c(trt = -0.5), x = covs, maxt = 5, dist = "exp")
df <- merge(covs, s1)

fit <- streg(Surv(eventtime, status) ~ trt, data = df, distribution = "exp")



nd <- data.frame(trt = 1)
predict(fit, type = "surv", newdata = nd)
