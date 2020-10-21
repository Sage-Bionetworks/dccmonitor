#' @title Get synapse fileview
#'
#' @description Download a synapse fileview and return as a tibble.
#'
#' @param fileview_id The synId for the full metadata Fileview.
#' @param syn Synapse client object.
#' @return Tibble with the Fileview information.
get_all_studies_table <- function(fileview_id, syn) {
  syntable <- syn$tableQuery(sprintf("SELECT * FROM %s", fileview_id))
  fileview <- tibble::as_tibble(syntable$asDataFrame())
  # Some columns end up as lists with NaN; fix these
  fileview$assay <- fix_list(fileview$assay)
  fileview$species <- fix_list(fileview$species)
  fileview$metadataType <- fix_list(fileview$metadataType)
  # File currentVersion should be an integer
  fileview$currentVersion <- as.integer(fileview$currentVersion)
  fileview
}

#' @title Get study table with most recent metadata files
#'
#' @description Filter fileview to get table with most recent metadata files.
#' The table will have all documentation files, but will only keep the most
#' recent version of files for the following metadata types: biospecimen,
#' manifest, assay, individual. Assumes that the fileview is for a single
#' study, but ultimately will keep most recent file information for each
#' metadata type, without filtering by study. If want to also filter by a
#' study name, specify name in `study_name` parameter.
#'
#' @param fileview The fileview.
#' @param study_name Name of study. If NULL, does not filter by study to
#'   return study specific table.
filter_study_table_latest <- function(fileview, study_name = NULL) {
  if (!is.null(study_name)) {
    fileview <- fileview[fileview$study == study_name, ]
  }
  metadata_types <- unique(
    fileview$metadataType[!is.na(fileview$metadataType)]
  )
  # Remove all "old" metadata files
  for (type in metadata_types) {
    file_indices <- get_file_indices_vector(fileview, type)
    if (length(file_indices) > 1) {
      times <- fileview$modifiedOn[file_indices]
      times <- times[!is.na(times)]
      most_recent_time <- max(times)
      to_remove <- intersect(
        file_indices,
        which(fileview$modifiedOn != most_recent_time)
      )
      if (length(to_remove) > 0) {
        fileview <- fileview[-to_remove, ]
      }
    }
  }
  fileview
}
