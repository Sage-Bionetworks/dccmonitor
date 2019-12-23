#' @title Get the number of metadata files
#'
#' @description Get the number of metadata files in a study.
#'
#' @param study_view The file view for the study files.
#'   At minimum, the study_view is a dataframe or tibble
#'   with `metadataType` as a column name. Any `NA` in
#'   this column is assumed to be documentation files,
#'   all others are assumed to be metadata files.
#' @param num_docs Number of document files that are in
#'   the `study_view`. If `NULL`, will call
#'   `num_doc_files()` to get the value. This is here in case
#'   that the number of document files is known already.
#' @return The number of metadata files present in the `study_view`.
num_meta_files <- function(study_view, num_docs = NULL) {
  if (is.null(num_docs)) {
    num_docs <- num_doc_files(study_view)
  }
  num_files <- nrow(study_view) - num_docs
  num_files
}

#' @title Get number of files in manifest
#'
#' @description Get the number of unique files in manifest.
#'
#' @inheritParams num_individuals
#' @return The number of unique files in manifest or
#'   0 if the  manifest is not in the `study_view`.
num_manifest_files <- function(study_view) {
  file_index <- get_file_indices(study_view, "manifest")[[1]]
  if (length(file_index) > 0) {
    # Manifest file should exist in set
    num_files <- length(unique(study_view$file_data[[file_index]]$path))
  } else {
    num_files <- 0
  }
  num_files
}

#' Get the number of documentation files
#'
#' @description Get the number of documentation files
#' in a study.
#'
#' @inheritParams num_meta_files
#' @return The number of documentation files present in
#'   the `study_view`.
num_doc_files <- function(study_view) {
  num_docs <- sum(is.na(study_view$metadataType))
  num_docs
}
