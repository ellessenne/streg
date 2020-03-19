### Code to prepare 'kva'
library(haven)
library(usethis)

## Read data
kva <- haven::read_dta(file = "http://www.stata-press.com/data/r16/kva.dta")

## Add column with event indicator (all are events)
kva$event <- 1

## Remove all Stata-related attributes
attr(kva, "label") <- NULL
for (var in colnames(kva)) {
  attr(kva[[deparse(as.name(var))]], "label") <- NULL
  attr(kva[[deparse(as.name(var))]], "format.stata") <- NULL
}

## Save file to be used in the package
usethis::use_data(kva, overwrite = TRUE)
