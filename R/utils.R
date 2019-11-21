#' Get synapse fileview
#'
#' Download a synapse fileview and return as a tibble.
#'
#' @param fileview_id The synId for the full metadata Fileview.
#' @return Tibble with the Fileview information.
get_all_studies_table <- function(fileview_id) {
  # Get the table in my team directory & make into a dataframe
  syntable <- synapser::synTableQuery(sprintf("SELECT * FROM %s", fileview_id))
  tibble::as_tibble(syntable$asDataFrame())
}

#' Get data from file
#'
#' Downloads a synapse file and returns data inside as a tibble.
#'
#' @param id The synId for the file.
#' @param meta_type The metadata type of the file.
#'   "manifest" assumes tab-delimited text file;
#'   all other types currently assumed to be csv file.
#' @return Data from file in tibble.
get_data <- function(id, meta_type) {
  data <- NULL
  file_info <- synapser::synGet(id)

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

#' Gets the template synId
#'
#' Gets the template synId from the config file.
#'
#' @param metadata_type The metadata type, or NA for manifest.
#' @param species The species.
#' @param assay_type The type of assay.
#' @return synId for the template in Synapse.
get_template <- function(metadata_type, species = NA, assay_type = NA) {
  template <- switch(
    metadata_type,
    manifest = config::get("templates")$manifest_template,
    individual = {
      if (species != "human") {
        config::get("templates")$individual_templates[["animal"]]
      } else {
        config::get("templates")$individual_templates[[species]]
      }
    },
    biospecimen = {
      if (species != "drosophila") {
        config::get("templates")$biospecimen_templates[["general"]]
      } else {
        config::get("templates")$biospecimen_templates[[species]]
      }
    },
    assay = config::get("templates")$assay_templates[[assay_type]]
  )
  template
}

#' Get the number of metadata files
#'
#' Get the number of metadata files in a study.
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

#' Get the number of documentation files
#'
#' Get the number of documentation files in a study.
#'
#' @inheritParams num_meta_files
#' @return The number of documentation files present in
#'   the `study_view`.
num_doc_files <- function(study_view) {
  num_docs <- sum(is.na(study_view$metadataType))
  num_docs
}
