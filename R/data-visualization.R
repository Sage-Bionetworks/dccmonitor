#' Visualize data types
#'
#' Visualize the data class types, including
#' missing data, using the visdat and ggplot
#' packages.
#'
#' @param data Dataframe or tibble with file data.
visualize_data_types <- function(data) {
  visdat::vis_dat(data) +
    ggplot2::theme(text = ggplot2::element_text(size = 16))
}

#' Get summary of data
#'
#' Get a summary of the data using the skimr
#' package, along with a custom function that appends an
#' extra column with a string showing each value for
#' a given variable and the number of times the value
#' appears. Returns a tibble with the data.
#'
#' @param data Dataframe or tibble with file data.
#' @return Tibble with summary information.
data_summary <- function(data) {
  data_sum <- skimr::skim_to_wide(data)
  data_sum <- tibble::add_column(data_sum, `value (# occurrences)` = NA)
  for (var in data_sum$variable) {
    var_col <- which(names(data) == var)
    data_sum$`value (# occurrences)`[data_sum$variable == var] <-
      summarize_values(data[, var_col])
  }
  data_sum
}

#' Summarize values present
#'
#' Get a list of values present and the number of times
#' each variable appeared.
#'
#' @param values The values to summarize in a list.
#' @return String with the form "value1 (2), value2 (4)",
#'   where the value is given with the number of
#'   occurrences in parenthesis.
summarize_values <- function(values) {
  val_sum <- list()
  for (value in unique(values)) {
    x_appeared <- length(which(values == value))
    val_sum <- append(
      val_sum,
      glue::glue("{value} ({x_appeared})")
    )
  }
  val_sum_string <- glue::glue_collapse(val_sum, sep = ", ")
  val_sum_string
}
