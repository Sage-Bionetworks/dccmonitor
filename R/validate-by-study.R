#' @title Validate all studies
#'
#' @description Validate metadata and manifest files for every study
#' in consortium. Returns a named list of validation results, where the name
#' refers to the study name. The validation checks are done via
#' `dccvalidator::check_all()`.
#'
#' @param fileview The fileview table.
#' @param annotations A data frame of annotation definitions.
#'   Must contain at least three columns: key, value, and columnType.
#' @param syn Synapse client object.
#' @return Named list of check results, where the names of the list refer
#'   to the study. Returns NULL if no studies in fileview.
validate_all_studies <- function(fileview, annotations, syn) {

  # Get the list of study names
  study_names <- unique(fileview$study)

  if (length(study_names) == 0) {
    return(NULL)
  }
  # Run validation checks on each set of study files
  full_results <- purrr::map(
    study_names,
    function(name) {
      study_table <- filter_study_table_latest(fileview, name)
      results <- validate_study(
        study_table = study_table,
        annotations = annotations,
        syn = syn,
        study = name
      )
      results
    }
  )
  names(full_results) <- study_names
  full_results
}

#' @title Validate a study
#'
#' @description Run validation checks for a single study and return the
#' results. Validation checks are run via `dccvalidator::check_all()`. It
#' is assumed that there is, at most, one row per metadataType in the
#' `study_table` passed in.
#'
#' @importFrom dccvalidator check_all
#' @param study_table Tibble with fileview information for
#'   a single study. Expected columns are: 'metadataType',
#'   'file_data' (tibble column with data for each file), 'assay',
#'   'species'. Should have, at most, one row per metadataType.
#' @param annotations A data frame of annotation definitions.
#'   Must contain at least three columns: 'key', 'value', and 'columnType'.
#' @param syn Synapse client object.
#' @param study Name of the study.
#' @return Named list of check results. Returns `NULL` if there are no rows.
validate_study <- function(study_table, annotations, syn, study) {

  # Check that there is one row per metadata type
  # If not, add "dummy" row
  # Required by dccvalidator::check_all()
  required <- c("manifest", "assay", "biospecimen", "individual")
  if (!any(required %in% study_table$metadataType)) {
    return(NULL)
  }
  if (!all(required %in% study_table$metadataType)) {
    present_types <- which(required %in% study_table$metadataType)
    for (type in required[-present_types]) {
      study_table <- tibble::add_row(
        study_table,
        metadataType = type
      )
    }
  } # else all present and it's okay to do checks
  results <- dccvalidator::check_all(
    data = study_table,
    annotations = annotations,
    syn = syn,
    study = study
  )
  results
}
