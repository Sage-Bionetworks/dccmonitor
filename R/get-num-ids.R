#' Get number of individuals
#'
#' Get the number of unique individuals for a study
#' from the metadata files. Checks for ids in the manifest,
#' individual metadata, and biospecimen metadata.
#'
#' @param study_view The file view for a study, with the
#'   columns 'file_data', where each element is a tibble
#'   with the data, and metadataType, where each element
#'   is the metadataType for the file.
#' @return Number of unique individual ids or 0 if no metadataTypes
#'   individual, biospecimen, manifest exist in `study_view`.
num_individuals <- function(study_view) {
  files_of_interest <- c("manifest", "individual", "biospecimen")
  file_indices <- get_file_indices(study_view, files_of_interest)

  if (!is.null(file_indices)) {
    individuals <- list()
    for (index in file_indices) {
      individuals <- c(individuals, study_view$file_data[[index]]$individualID)
    }
    num_individuals <- length(unique(individuals))
  } else {
    num_individuals <- 0
  }
  num_individuals
}

#' Get number of specimens
#'
#' Get the number of unique specimens for a study
#' from the metadata files. Checks for ids in the manifest,
#' biospecimen metadata, and assay metadata.
#'
#' @inheritParams num_individuals
#' @return Number of unique specimen ids or 0 if no metadataTypes
#'   manifest, biospecimen, assay exist in `study_view`.
num_specimens <- function(study_view) {
  files_of_interest <- c("manifest", "biospecimen", "assay")
  file_indices <- get_file_indices(study_view, files_of_interest)

  if (!is.null(file_indices)) {
    specimens <- list()
    for (index in file_indices) {
      specimens <- c(specimens, study_view$file_data[[index]]$specimenID)
    }
    num_specimens <- length(unique(specimens))
  } else {
    num_specimens <- 0
  }
  num_specimens
}
