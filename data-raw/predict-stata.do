cd "~/R-dev/streg/"

set more off
webuse cancer, clear

streg age drug, dist(exp)
predict mean_exp, mean
predict median_exp, median
predict surv_exp, surv
predict h_exp, hazard
predict hr_exp, hr
predict xb_exp, xb

streg age drug, dist(wei)
predict mean_wei, mean
predict median_wei, median
predict surv_wei, surv
predict h_wei, hazard
predict hr_wei, hr
predict xb_wei, xb

streg age drug, dist(gom)
predict median_gom, median
predict surv_gom, surv
predict h_gom, hazard
predict hr_gom, hr
predict xb_gom, xb

compress
save data-raw/predict-stata, replace
