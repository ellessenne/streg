### Code to prepare 'cancer'
library(haven)
library(usethis)

## Read data
cancer <- haven::read_dta(file = "http://www.stata-press.com/data/r16/cancer.dta")
cancer <- as.data.frame(cancer)

## Remove all Stata-related attributes
attr(cancer, "label") <- NULL
for (var in colnames(cancer)) {
  attr(cancer[[deparse(as.name(var))]], "label") <- NULL
  attr(cancer[[deparse(as.name(var))]], "format.stata") <- NULL
}

## Re-order columns
cancer <- cancer[, c("studytime", "died", "drug", "age")]

## Save file to be used in the package
usethis::use_data(cancer, overwrite = TRUE)
