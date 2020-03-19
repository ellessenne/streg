#' @title Generator Experiment
#' @description Generator experiment data, imported from Stata 16.
#' @format A data frame with 12 rows and 4 variables:
#' * `failtime` Time until failure (in hours);
#' * `event` Event indicator variable;
#' * `load` Overload (in KVA);
#' * `bearings` Binary variable for _Has new bearings?_.
#' @references http://www.stata-press.com/data/r16/kva.dta
#' @examples
#' data("kva", package = "streg")
"kva"

#' @title Patient Survival in Drug Trial
#' @description Patient survival in drug trial data, imported from Stata 16.
#' @format A data frame with 48 rows and 4 variables:
#' * `studytime` Months to death or end of follow-up;
#' * `died` Event indicator variable, `died = 1` if a patient died;
#' * `drug` Drug type, with `drug = 1` being placebo;
#' * `age` Age of a patient at baseline.
#' @references http://www.stata-press.com/data/r16/cancer.dta
#' @examples
#' data("cancer", package = "streg")
"cancer"
