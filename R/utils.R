#' @keywords internal
.process_streg_formula <- function(formula, data, which) {
  if (which == "y") {
    out <- eval(expr = formula[[2]], envir = data)
  } else if (which == "x") {
    out <- stats::model.matrix(formula[-2], data = data)
  }
  return(out)
}
