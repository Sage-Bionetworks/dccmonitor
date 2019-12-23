#' @title Percent of passing validation results
#'
#' @description Gives the overall percentage of validation
#' results that are passing.
#'
#' @param results A list of `dccvalidator` results,
#'   which are of the type `check_pass`, `check_fail`,
#'   or `check_warn`.
#' @return Percent of validation checks passed, rounded
#'   to the nearest whole number.
percent_pass_validation <- function(results) {
  passed <- purrr::map(
    results,
    function(x) {
      inherits(x, "check_pass")
    }
  )
  percent_pass <- (Reduce("+", passed) / length(results)) * 100
  percent_pass <- round(percent_pass)
  percent_pass
}
