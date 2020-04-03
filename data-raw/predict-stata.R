### Code to prepare internal df for testing: 'predict-stata'
### Dataset was created using 'predict-stata.do' in the 'data-raw' folder
library(haven)
library(usethis)

## Read data
predict_stata <- haven::read_dta(file = "data-raw/predict-stata.dta")
predict_stata <- as.data.frame(predict_stata)


## Remove all Stata-related attributes
attr(predict_stata, "label") <- NULL
for (var in colnames(predict_stata)) {
  attr(predict_stata[[deparse(as.name(var))]], "label") <- NULL
  attr(predict_stata[[deparse(as.name(var))]], "format.stata") <- NULL
}

## Re-order columns
predict_stata[, c("studytime", "died", "drug", "age", "_st", "_d", "_t", "_t0")] <- NULL

## Save file to be used in the package
usethis::use_data(predict_stata, overwrite = TRUE, internal = TRUE)
