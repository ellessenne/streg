### Testing
devtools::load_all()
data("cancer", package = "streg")

str(streg(Surv(studytime, died) ~ age + drug, data = cancer, distribution = "weibull"))
