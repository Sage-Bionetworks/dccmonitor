#' Percent of passing validation results
#'
#' Gives the overall percentage of validation
#' results that are passing.
#'
#' @inheritParams results
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
