### Testing
devtools::load_all()
library(simsurv)
set.seed(9911)
N <- 1e3
covs <- data.frame(id = seq(N), trt = stats::rbinom(N, 1L, 0.5), age = runif(N, 40, 60))
s1 <- simsurv(lambdas = 0.1, betas = c(trt = -0.5, age = 0.01), x = covs, maxt = 5, dist = "exp")
df1 <- merge(s1, covs)

fit <- streg(Surv(eventtime, status) ~ trt + age, data = df1, distribution = "exp")

asd <- summary(fit)
asd

coef(fit)
coefficients(fit)
vcov(fit)
logLik(fit)
nobs(fit)
update(fit, Surv(eventtime, status) ~ age + I(age^2))
confint(fit, parm = "age")
# see e.g.: methods(class = "lm")
