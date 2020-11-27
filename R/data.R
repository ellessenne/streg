#' @title Generator Experiment
#' @description Generator experiment data, imported from Stata 16.
#' @format A data frame with 12 rows and 4 variables:
#' * `failtime` Time until failure (in hours);
#' * `event` Event indicator variable;
#' * `load` Overload (in KVA);
#' * `bearings` Binary variable for _Has new bearings?_.
#' @references http://www.stata-press.com/data/r16/kva.dta
#' @keywords datasets
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
#' @keywords datasets
#' @examples
#' data("cancer2", package = "streg")
"cancer2"

#' @title Hip Fracture Study
#' @description Hip fracture study data, imported from Stata 16.
#' @format A data frame with 206 rows and 8 variables:
#' * `id` Patient ID;
#' * `time0` Begin of time interval;
#' * `time1` End of time interval;
#' * `fracture` Fracture event;
#' * `protect` Wears device;
#' * `age` Age at enrollment;
#' * `calcium` Blood calcium level;
#' * `male` 1 if male.
#' @references http://www.stata-press.com/data/r16/hip3.dta
#' @keywords datasets
#' @examples
#' data("hip", package = "streg")
"hip"
