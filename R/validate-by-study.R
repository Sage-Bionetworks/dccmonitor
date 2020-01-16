#' @title Validate all studies
#'
#' @description Validate metadata and manifest files for every study
#' in consortium. Returns the fileview table with a new 'results'
#' column that contains a list of validation results for each
#' file.
#'
#' @param fileview The fileview table.
#' @param annotations A data frame of annotation definitions.
#'   Must contain at least three columns: key, value, and columnType.
#' @param syn Synapse client object.
#' @return The fileview table with a new column, 'results', with
#'   a list of validation results for every file.
validate_all_studies <- function(fileview, annotations, syn) {

  # Get the list of study names
  study_names <- unique(fileview$study)

  full_results <- tibble::tibble()
  # Run validation checks on each set of study files
  for (name in study_names) {
    study_table <- fileview[fileview$study == name, ]
    results <- validate_study(study_table, annotations, syn)
    full_results <- rbind(
      full_results,
      results
    )
  }
  full_results
}

#' @title Validate a study
#'
#' @description Run validation checks for a single study and appends
#' the results to the study_table in a new 'results' column. The results
#' are in list form with specific names for the tests.
#'
#' @param study_table Tibble with fileview information for
#'   a single study. Expected columns are: 'metadataType',
#'   'file_data' (tibble column with data for each file), 'assay',
#'   'species', and 'template'.
#' @param annotations A data frame of annotation definitions.
#'   Must contain at least three columns: 'key', 'value', and 'columnType'.
#' @param syn Synapse client object.
#' @return study_table with a new column, 'results', that holds the
#'   result lists for each file in the table.
validate_study <- function(study_table, annotations, syn) {

  # Add column to study_table for results lists
  study_table <- tibble::add_column(study_table, results = list(0))
  study_meta_types <- study_table$metadataType[!is.na(study_table$metadataType)]
  file_indices <- get_file_indices_named(study_table, study_meta_types)
  # Currently assuming only a single metadataType per study
  for (type in study_meta_types) {
    if (type == "manifest") {
      manifest_results <- validate_manifest(
        study_table$file_data[[file_indices$manifest]],
        study_table$template[[file_indices$manifest]],
        annotations = annotations,
        syn
      )
      meta_files_in_manifest <- dccvalidator::check_files_manifest(
        study_table$file_data[[file_indices$manifest]],
        c(
          study_table$name[file_indices$individual],
          study_table$name[file_indices$assay],
          study_table$name[file_indices$biospecimen]
        ),
        success_msg = "Manifest file contains all metadata files",
        fail_msg = "Manifest file does not contain all metadata files"
      )
      manifest_results[["meta_files_in_manifest"]] <- meta_files_in_manifest
      study_table$results[[file_indices$manifest]] <- I(manifest_results)
    } else if (type == "assay") {
      assay_results <- validate_assay_meta(
        study_table$file_data[[file_indices$assay]],
        study_table$template[[file_indices$assay]],
        annotations = annotations,
        syn
      )
      if ("biospecimen" %in% study_table$metadataType) {
        assay_biosp_ids <- dccvalidator::check_specimen_ids_match(
          study_table$file_data[[file_indices$biospecimen]],
          study_table$file_data[[file_indices$assay]],
          "biospecimen",
          "assay",
          bidirectional = FALSE
        )
        assay_results[["assay_biosp_ids"]] <- assay_biosp_ids
      }
      study_table$results[[file_indices$assay]] <- I(assay_results)
    } else if (type == "biospecimen") {
      biosp_results <- validate_biospecimen_meta(
        study_table$file_data[[file_indices$biospecimen]],
        study_table$template[[file_indices$biospecimen]],
        annotations,
        syn
      )
      if ("manifest" %in% study_table$metadataType) {
        biosp_manifest_ids <- dccvalidator::check_specimen_ids_match(
          study_table$file_data[[file_indices$biospecimen]],
          study_table$file_data[[file_indices$manifest]],
          "biospecimen",
          "manifest",
          bidirectional = FALSE
        )
        biosp_results[["biosp_manifest_ids"]] <- biosp_manifest_ids
      }
      study_table$results[[file_indices$biospecimen]] <- I(biosp_results)
    } else if (type == "individual") {
      indiv_results <- validate_individual_meta(
        study_table$file_data[[file_indices$individual]],
        study_table$template[[file_indices$individual]],
        annotations,
        syn
      )
      if ("manifest" %in% study_table$metadataType) {
        indiv_manifest_ids <- dccvalidator::check_indiv_ids_match(
          study_table$file_data[[file_indices$individual]],
          study_table$file_data[[file_indices$manifest]],
          "individual",
          "manifest",
          bidirectional = FALSE
        )
        indiv_results[["indiv_manifest_ids"]] <- indiv_manifest_ids
      }
      if ("biospecimen" %in% study_table$metadataType) {
        indiv_biosp_ids <- dccvalidator::check_indiv_ids_match(
          study_table$file_data[[file_indices$individual]],
          study_table$file_data[[file_indices$biospecimen]],
          "individual",
          "biospecimen",
          bidirectional = FALSE
        )
        indiv_results[["indiv_biosp_ids"]] <- indiv_biosp_ids
      }
      study_table$results[[file_indices$individual]] <- I(indiv_results)
    }
  }
  study_table
}
