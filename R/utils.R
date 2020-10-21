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

#' @importFrom magrittr %>%
magrittr::`%>%`
