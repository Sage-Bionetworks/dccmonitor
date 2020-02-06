#' @title Fix character list
#'
#' @description Fix a list that should be all characters and
#'   should have `NA` fields, not `NaN` fields.
#'
#' @param data A list to fix.
#' @return The fixed character vector.
#' @keywords internal
fix_list <- function(data) {
  data <- as.character(data)
  fixed <- purrr::map(
    data,
    function(x) {
      if (x == "NaN") {
        x <- NA
      }
      x
    }
  )
  unlist(fixed)
}

#' @title Get most recent time
#'
#' @description Get the most recent time from within
#' a list of POSIX times in the form of seconds since epoch.
#'
#' @param times Vector of numeric times in seconds since epoch.
#' @return The most recent time in `times`.
get_most_recent_time <- function(times) {
  if (length(times) <= 1) {
    return(times)
  }
  most_recent <- times[1]
  for (date_time in times) {
    if (date_time > most_recent) {
      most_recent <- date_time
    }
  }
  most_recent
}
