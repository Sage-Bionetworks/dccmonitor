#' Get number of individuals
#'
#' Get the number of unique individuals for a study
#' from the metadata files.
#'
#' @param study_view The file view for a study, with the
#'   columns 'file_data', where each element is a tibble
#'   with the data, and metadataType, where each element
#'   is the metadataType for the file.
#' @return Number of unique individual ids.
num_individuals <- function(study_view) {
  files_of_interest <- c("manifest", "individual", "biospecimen")
  file_indices <- get_file_indices(study_view, files_of_interest)

  individuals <- list()
  for (index in file_indices) {
    individuals <- c(individuals, study_view$file_data[[index]]$individualID)
  }
  length(unique(individuals))
}

#' Get number of specimens
#'
#' Get the number of unique specimens for a study
#' from the metadata files.
#'
#' @inheritParams num_individuals
#' @return Number of unique specimen ids.
num_specimens <- function(study_view) {
  files_of_interest <- c("manifest", "biospecimen", "assay")
  file_indices <- get_file_indices(study_view, files_of_interest)

  specimens <- list()
  for (index in file_indices) {
    specimens <- c(specimens, study_view$file_data[[index]]$specimenID)
  }
  length(unique(specimens))
}

#' Get file indices by metadataType
#'
#' Get the file indices for `study_view` based on
#' metadataType.
#'
#' @inheritParams num_individuals
#' @param meta_types List of metadataTypes.
#' @return List of indices of `study_view` for which
#'   the metadataType exists.
get_file_indices <- function(study_view, meta_types) {
  file_indices <- purrr::map(
    meta_types,
    function(x) {
      which(study_view$metadataType == x)
    }
  )
  # flatten removes integer(0) values for indices
  file_indices <- purrr::flatten(file_indices)
  file_indices
}
