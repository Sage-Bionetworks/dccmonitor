#' Validate all studies
#'
#' Validate metadata and manifest files for every study in
#' consortium. Returns the fileview table with a new 'results'
#' column that contains a list of validation results for each
#' file.
#'
#' @param fileview The fileview table.
#' @param annotations A data frame of annotation definitions.
#'   Must contain at least three columns: key, value, and columnType.
#' @return The fileview table with a new column, 'results', with
#'   a list of validation results for every file.
validate_all_studies <- function(fileview, annotations) {

  # Get the list of study names
  study_names <- unique(fileview$study)

  full_results <- tibble::tibble()
  # Run validation checks on each set of study files
  for (name in study_names) {
    study_table <- fileview[fileview$study == name, ]
    results <- validate_study(study_table, annotations)
    full_results <- rbind(
      full_results,
      results
    )
  }
  full_results
}

#' Validate a study
#'
#' Run validation checks for a single study and appends the results
#' to the study_table in a new 'results' column. The results
#' are in list form with specific names for the tests.
#'
#' @param study_table Dataframe with fileview information for
#'   a single study.
#' @param annotations A data frame of annotation definitions.
#'   Must contain at least three columns: key, value, and columnType.
#' @return study_table with a new column, 'results', that holds the
#'   result lists for each file in the table.
validate_study <- function(study_table, annotations) {

  # Add column to study_table for results lists
  study_table <- tibble::add_column(study_table, results = list(0))

  # Currently assuming only a single metadataType per study
  for (type in study_table$metadataType) {
    if (type == "manifest") {
      # Must be NA, which is what the manifest should be
      # Note that if more types come in, this would default incorrectly
      manifest_index <- which(study_table$metadataType == "manifest")
      manifest_results <- validate_manifest(
        study_table$file_data[[manifest_index]],
        study_table$template[[manifest_index]],
        annotations = annotations
      )
      study_table$results[[manifest_index]] <- I(manifest_results)
    } else if (type == "assay") {
      assay_index <- which(study_table$metadataType == "assay")
      assay_results <- validate_assay_meta(
        study_table$file_data[[assay_index]],
        study_table$template[[assay_index]],
        annotations = annotations
      )
      if ("biospecimen" %in% study_table$metadataType) {
        assay_biosp_ids <- dccvalidator::check_specimen_ids_match(
          study_table$file_data[[assay_index]],
          study_table$file_data[[
            which(study_table$metadataType == "biospecimen")
          ]],
          "assay",
          "biospecimen",
          bidirectional = FALSE
        )
        assay_results <- c(assay_results, assay_biosp_ids = assay_biosp_ids)
      }
      study_table$results[[assay_index]] <- I(assay_results)
    } else if (type == "biospecimen") {
      biosp_index <- which(study_table$metadataType == "biospecimen")
      biosp_results <- validate_biospecimen_meta(
        study_table$file_data[[biosp_index]],
        study_table$template[[biosp_index]],
        annotations
      )
      if ("manifest" %in% study_table$metadataType) {
        biosp_manifest_ids <- dccvalidator::check_specimen_ids_match(
          study_table$file_data[[biosp_index]],
          study_table$file_data[[
            which(study_table$metadataType == "manifest")
          ]],
          "biospecimen",
          "manifest",
          bidirectional = FALSE
        )
        biosp_results <- c(
          biosp_results,
          biosp_manifest_ids = biosp_manifest_ids
        )
      }
      study_table$results[[biosp_index]] <- I(biosp_results)
    } else if (type == "individual") {
      indiv_index <- which(study_table$metadataType == "individual")
      indiv_results <- validate_individual_meta(
        study_table$file_data[[indiv_index]],
        study_table$template[[indiv_index]],
        annotations
      )
      if ("manifest" %in% study_table$metadataType) {
        indiv_manifest_ids <- dccvalidator::check_indiv_ids_match(
          study_table$file_data[[indiv_index]],
          study_table$file_data[[
            which(study_table$metadataType == "manifest")
          ]],
          "individual",
          "manifest",
          bidirectional = FALSE
        )
        indiv_results <- c(
          indiv_results,
          indiv_manifest_ids = indiv_manifest_ids
        )
      }
      if ("biospecimen" %in% study_table$metadataType) {
        indiv_biosp_ids <- dccvalidator::check_indiv_ids_match(
          study_table$file_data[[indiv_index]],
          study_table$file_data[[
            which(study_table$metadataType == "biospecimen")
          ]],
          "individual",
          "biospecimen",
          bidirectional = FALSE
        )
        indiv_results <- c(
          indiv_results,
          indiv_biosp_ids = indiv_biosp_ids
        )
      }
      study_table$results[[indiv_index]] <- I(indiv_results)
    }
  }
  study_table
}
