### Code to prepare 'hip'
library(haven)
library(usethis)

## Read data
hip <- haven::read_dta(file = "http://www.stata-press.com/data/r16/hip3.dta")
hip <- as.data.frame(hip)

## Remove all Stata-related attributes
attr(hip, "label") <- NULL
for (var in colnames(hip)) {
  attr(hip[[deparse(as.name(var))]], "label") <- NULL
  attr(hip[[deparse(as.name(var))]], "format.stata") <- NULL
}

## Removes _st variables
hip <- hip[, !grepl("^_", names(hip))]

## Save file to be used in the package
usethis::use_data(hip, overwrite = TRUE)
