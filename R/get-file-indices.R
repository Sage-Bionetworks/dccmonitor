#' Get named file indices by metadataType
#'
#' Get the file indices for `study_view` based on metadataType
#' and return as list with file metadataType as name of the
#' elements.
#'
#' @export
#' @inheritParams get_file_indices
#' @return Named list of indices from `study_view` for which
#'   the metadataType exists; `NULL` if none exist.
get_file_indices_named <- function(study_view, meta_types) {
  file_indices <- get_file_indices(study_view, meta_types)
  file_indices
}

#' Get file indices by metadataType
#'
#' Get the file indices for `study_view` based on
#' metadataType.
#'
#' @export
#' @inheritParams get_file_indices
#' @return Vector of indices of `study_view` for which
#'   the metadataType exists; `NULL` if none exist.
get_file_indices_vector <- function(study_view, meta_types) {
  file_indices <- get_file_indices(study_view, meta_types)
  names(file_indices) <- NULL
  unlist(file_indices)
}

#' Get file indices by metadataType
#'
#' Get the file indices for `study_view` based on
#' metadataType.
#'
#' @inheritParams num_individuals
#' @param meta_types List of metadataTypes.
#' @return Vector of indices of `study_view` for which
#'   the metadataType exists; `NULL` if none exist.
get_file_indices <- function(study_view, meta_types) {
  file_indices <- purrr::map(
    meta_types,
    function(x) {
      which(study_view$metadataType == x)
    }
  )
  names(file_indices) <- meta_types
  # flatten removes integer(0) values for indices
  file_indices <- purrr::flatten(file_indices)
  if (length(file_indices) > 0) {
    return(file_indices)
  } else {
    return(NULL)
  }
}
