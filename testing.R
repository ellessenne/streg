### Testing
devtools::load_all()
library(simsurv)
set.seed(9911)
N = 1e5
covs <- data.frame(id = seq(N), trt = stats::rbinom(N, 1L, 0.5), age = runif(N, 40, 60))
s1 <- simsurv(lambdas = 0.1, betas = c(trt = -0.5, age = 0.01), x = covs, maxt = 5, dist = "exp")
s2 <- simsurv(lambdas = 0.1, gammas = 1.5, betas = c(trt = -0.5, age = 0.01), x = covs, maxt = 5, dist = "wei")
df1 <- merge(covs, s1)
df2 <- merge(covs, s2)

streg(Surv(eventtime, status) ~ trt + age, data = df1, distribution = "exp")
streg(Surv(eventtime, status) ~ trt + age, data = df2, distribution = "wei")


