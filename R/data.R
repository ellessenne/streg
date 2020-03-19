#' @title Generator Experiment
#' @format A data frame with 12 rows and 4 variables:
#' * `failtime` Time until failure (in hours);
#' * `event` Event indicator variable;
#' * `load` Overload (in KVA);
#' * `bearings` Binary variable for _Has new bearings?_.
#' @references http://www.stata-press.com/data/r16/kva.dta
#' @examples
#' data("kva", package = "streg")
#' @export
"kva"

#' @title Patient Survival in Drug Trial
#' @format A data frame with 48 rows and 4 variables:
#' * `studytime` Months to death or end of follow-up;
#' * `died` Event indicator variable, `died = 1` if a patient died;
#' * `drug` Drug type, with `drug = 1` being placebo;
#' * `age` Age of a patient at baseline.
#' @references http://www.stata-press.com/data/r16/cancer.dta
#' @examples
#' data("cancer", package = "streg")
#' @export
"cancer"
