### Testing
TMBtools::export_models()
devtools::load_all()
library(simsurv)
set.seed(9911)
N <- 1e3
covs <- data.frame(id = seq(N), trt = stats::rbinom(N, 1L, 0.5), age = runif(N, 40, 60))
s1 <- simsurv(lambdas = 0.1, betas = c(trt = -0.5, age = 0.01), x = covs, maxt = 5, dist = "exp")
s2 <- simsurv(lambdas = 0.1, gammas = 1.5, betas = c(trt = -0.5, age = 0.01), x = covs, maxt = 5, dist = "wei")
s3 <- simsurv(lambdas = 0.2, gammas = 0.5, betas = c(trt = -0.5, age = 0.01), x = covs, maxt = 5, dist = "gom")
df1 <- merge(covs, s1)
df2 <- merge(covs, s2)
df3 <- merge(covs, s3)

streg(Surv(eventtime, status) ~ trt + age, data = df1, distribution = "exp")
streg(Surv(eventtime, status) ~ trt + age, data = df2, distribution = "wei")
streg(Surv(eventtime, status) ~ trt + age, data = df3, distribution = "gom")
