#' @title Percent of passing validation results
#'
#' @description Gives the overall percentage of validation
#' results that are passing.
#'
#' @param results A list of `dccvalidator` results,
#'   which are of the type `check_pass`, `check_fail`,
#'   or `check_warn`.
#' @return Percent of validation checks passed, rounded
#'   to the nearest whole number. Returns 0 if `results` is `NULL` or if
#'   all checks in `results` are `NULL`.
percent_pass_validation <- function(results) {
  if (is.null(results)) {
    return(0)
  }
  # Only want to check percent pass for checks done
  null_checks <- purrr::map_lgl(results, function(x) {
    is.null(x)
  })
  results <- results[!null_checks]
  if (length(results) == 0) {
    return(0)
  }
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
