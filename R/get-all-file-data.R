#' @title Get data for all files in table
#'
#' @description Downloads all the file data and attaches
#' to table as a tibble column.
#'
#' @param fileview Tibble with columns 'id' and 'metadataType',
#'   at minimum, and one row per file. 'id' is the synId
#'   for the file.
#' @param syn Synapse client object.
#' @return Original tibble with extra column 'file_data' of
#'   tibbles, holding the data from each file.
get_all_file_data <- function(fileview, syn) {
  data <- purrr::map2(
    fileview$id,
    fileview$metadataType,
    function(id, type) {
      if (!is.na(type)) {
        get_data(id, type, syn)
      } else {
        type
      }
    }
  )
  fileview <- tibble::add_column(fileview, file_data = data)
  fileview
}

#' @title Get data from file
#'
#' @description Downloads a synapse file and returns data
#' inside as a tibble.
#'
#' @param id The synId for the file.
#' @param meta_type The metadata type of the file.
#'   "manifest" assumes tab-delimited text file;
#'   all other types currently assumed to be csv file.
#' @param syn Synapse client object.
#' @return Data from file in tibble.
get_data <- function(id, meta_type, syn) {
  data <- NULL
  file_info <- syn$get(id)

  if (meta_type == "manifest") {
    data <- utils::read.table(
      file_info$path,
      sep = "\t",
      header = TRUE,
      na.strings = "",
      stringsAsFactors = FALSE
    )
  } else {
    data <- utils::read.csv(
      file_info$path,
      na.strings = "",
      stringsAsFactors = FALSE
    )
  }

  return(tibble::as_tibble(data))
}
