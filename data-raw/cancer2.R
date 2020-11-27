### Code to prepare 'cancer2'
library(haven)
library(usethis)

## Read data
cancer2 <- haven::read_dta(file = "http://www.stata-press.com/data/r16/cancer.dta")
cancer2 <- as.data.frame(cancer2)

## Remove all Stata-related attributes
cancer2 <- haven::zap_formats(cancer2)
cancer2 <- haven::zap_label(cancer2)
cancer2 <- haven::zap_labels(cancer2)
cancer2 <- haven::zap_formats(cancer2)
cancer2 <- haven::zap_formats(cancer2)

## Re-order columns
cancer2 <- cancer2[, c("studytime", "died", "drug", "age")]

## Save file to be used in the package
usethis::use_data(cancer2, overwrite = TRUE)
