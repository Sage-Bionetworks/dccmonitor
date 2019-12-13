#' Get data for all files in table
#'
#' Downloads all the file data and attaches
#' to table as a tibble column.
#'
#' @param fileview Tibble with columns 'id' and 'metadataType',
#'   at minimum, and one row per file. 'id' is the synId
#'   for the file.
#' @return Original tibble with extra column 'file_data' of
#'   tibbles, holding the data from each file.
get_all_file_data <- function(fileview) {
  data <- purrr::map2(
    fileview$id,
    fileview$metadataType,
    function(id, type) {
      if (!is.na(type)) {
        get_data(id, type)
      } else {
        type
      }
    }
  )
  fileview <- tibble::add_column(fileview, file_data = data)
  fileview
}

#' Get the templates for all files in table
#'
#' Gathers all the template synIds from the config file for each file
#' and attaches to table as a new column.
#'
#' @param fileview Tibble with columns 'metadataType', 'species',
#'   and 'assay', at minimum, and one row per file.
#' @return Original tibble with extra column 'template',
#'   holding the template synId for each file.
get_all_file_templates <- function(fileview) {
  fileview <- tibble::add_column(fileview, template = NA)
  for (file_index in seq_len(nrow(fileview))) {
    template <- get_template(
      fileview$metadataType[file_index],
      fileview$species[file_index],
      fileview$assay[file_index]
    )
    if (!is.na(template)) {
      fileview$template[file_index] <- template
    }
  }
  fileview
}
