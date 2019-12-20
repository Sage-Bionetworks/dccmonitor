#' @title Visualize data types
#'
#' @description Visualize the data class types, including
#' missing data, using the visdat and ggplot
#' packages.
#'
#' @param data Dataframe or tibble with file data.
visualize_data_types <- function(data) {
  if (!inherits(data, "tbl_df") && !inherits(data, "data.frame")) {
    return(NULL)
  }
  visdat::vis_dat(data) +
    ggplot2::theme(text = ggplot2::element_text(size = 16))
}

#' @title Get summary of data
#'
#' @description Get a summary of the data using the skimr
#' package, along with a custom function that appends an
#' extra column with a string showing each value for
#' a given variable and the number of times the value
#' appears. Returns a tibble with the data.
#'
#' @param data Dataframe or tibble with file data.
#' @return Tibble with summary information.
data_summary <- function(data) {
  if (!inherits(data, "tbl_df") && !inherits(data, "data.frame")) {
    return(NULL)
  }
  data_sum <- tibble::as_tibble(skimr::skim_to_wide(data))
  # Cut out excess info from skimr results
  data_sum <- data_sum[, c(
    "variable",
    "type",
    "missing",
    "complete",
    "n",
    "min",
    "max",
    "empty",
    "n_unique"
  )]
  data_sum <- tibble::add_column(data_sum, value_occurrence = NA)
  for (var in data_sum$variable) {
    var_col <- which(names(data) == var)
    data_sum$value_occurrence[data_sum$variable == var] <-
      summarize_values(data[[var_col]])
  }
  data_sum
}

#' @title Summarize values present
#'
#' @description Get a list of values present and
#' the number of times each variable appeared.
#'
#' @param values The values to summarize in a list.
#' @return String with the form "value1 (2), value2 (4)",
#'   where the value is given with the number of
#'   occurrences in parenthesis.
summarize_values <- function(values) {
  if (is.null(values)) {
    return(NULL)
  }
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
