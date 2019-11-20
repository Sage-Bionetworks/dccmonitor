#' Validation of manifest
#'
#' Validates the manifest according to specific
#' requirements and returns list of results.
#'
#' @param manifest Manifest data in dataframe.
#' @param template The assay template.
#' @param annotations A data frame of annotation definitions.
#'   Must contain at least three columns: key, value, and columnType.
#' @return List of condition objects indicating whether the manifest
#'   has passed each validation check.
validate_manifest <- function(manifest, template, annotations) {
  # Do checks
  missing_cols_manifest <- dccvalidator::check_cols_manifest(
    manifest,
    template
  )
  annotation_keys_manifest <- dccvalidator::check_annotation_keys(
    manifest,
    annotations,
    whitelist_keys = c("path", "parent")
  )
  annotation_values_manifest <- dccvalidator::check_annotation_values(
    manifest,
    annotations
  )
  empty_cols_manifest <- dccvalidator::check_cols_empty(
    manifest,
    success_msg = "No columns are empty in the manifest",
    fail_msg = "Some columns are empty in the manifest"
  )
  complete_cols_manifest <- dccvalidator::check_cols_complete(
    manifest,
    required_cols = c(
      "consortium",
      "study",
      "grant",
      "fileFormat",
      "parent"
    ),
    success_msg = "All required columns are complete in the manifest",
    fail_msg = "Some required columns are incomplete in the manifest"
  )

  # Gather check info
  manifest_results <- list(
    missing_cols = missing_cols_manifest,
    annot_keys = annotation_keys_manifest,
    annot_values = annotation_values_manifest,
    empty_cols = empty_cols_manifest,
    complete_cols = complete_cols_manifest
  )
  return(manifest_results)
}

#' Validation of assay metadata
#'
#' Validates the assay metadata according to specific
#' requirements and returns list of results.
#'
#' @param assay Assay metadata in dataframe.
#' @param template The assay template.
#' @param annotations A data frame of annotation definitions.
#'   Must contain at least three columns: key, value, and columnType.
#' @return List of condition objects indicating whether the assay
#'   has passed each validation check.
validate_assay_meta <- function(assay, template, annotations) {
  # Do checks
  missing_cols_assay <- dccvalidator::check_cols_assay(
    assay,
    template
  )
  annotation_values_assay <- dccvalidator::check_annotation_values(
    assay,
    annotations,
    whitelist_keys = c("specimenID"),
    success_msg = "All values in the assay metadata are valid",
    fail_msg = "Some values in the assay metadata are invalid"
  )
  empty_cols_assay <- dccvalidator::check_cols_empty(
    assay,
    success_msg = "No columns are empty in the assay metadata",
    fail_msg = "Some columns are empty in the assay metadata"
  )
  complete_cols_assay <- dccvalidator::check_cols_complete(
    assay,
    required_cols = c("specimenID"),
    success_msg = "All required columns are complete in the assay metadata", # nolint
    fail_msg = "Some required columns are incomplete in the assay metadata" # nolint
  )

  # Gather check info
  assay_results <- list(
    missing_cols = missing_cols_assay,
    annot_values = annotation_values_assay,
    empty_cols = empty_cols_assay,
    complete_cols = complete_cols_assay
  )
  assay_results
}

#' Validation of biospecimen metadata
#'
#' Validates the biospecimen metadata according to specific
#' requirements and returns list of results.
#'
#' @param biospecimen Biospecimen metadata in dataframe.
#' @param annotations A data frame of annotation definitions.
#'   Must contain at least three columns: key, value, and columnType.
#' @param template The biospecimen template.
#' @return List of condition objects indicating whether the manifest
#'   has passed each validation check.
validate_biospecimen_meta <- function(biospecimen, template, annotations) {
  # Do checks
  missing_cols_biosp <- dccvalidator::check_cols_biospecimen(
    biospecimen,
    template
  )
  annotation_values_biosp <- dccvalidator::check_annotation_values(
    biospecimen,
    annotations,
    whitelist_keys = c("specimenID", "individualID"),
    success_msg = "All values in the biospecimen metadata are valid",
    fail_msg = "Some values in the biospecimen metadata are invalid"
  )
  duplicate_specimen_ids <- dccvalidator::check_specimen_ids_dup(biospecimen)
  empty_cols_biosp <- dccvalidator::check_cols_empty(
    biospecimen,
    success_msg = "No columns are empty in the biospecimen metadata",
    fail_msg = "Some columns are empty in the biospecimen metadata"
  )
  complete_cols_biosp <- dccvalidator::check_cols_complete(
    biospecimen,
    required_cols = c("individualID", "specimenID"),
    success_msg = "All required columns are complete in the biospecimen metadata", # nolint
    fail_msg = "Some required columns are incomplete in the biospecimen metadata" # nolint
  )

  # Gather check info
  biosp_results <- list(
    missing_cols = missing_cols_biosp,
    annot_values = annotation_values_biosp,
    dup_ids = duplicate_specimen_ids,
    empty_cols = empty_cols_biosp,
    complete_cols = complete_cols_biosp
  )
  biosp_results
}

#' Validation of individual metadata
#'
#' Validates the individual metadata according to specific
#' requirements and returns list of results.
#'
#' @param individual Individual metadata in dataframe.
#' @param annotations A data frame of annotation definitions.
#'   Must contain at least three columns: key, value, and columnType.
#' @param template The individual template.
#' @return List of condition objects indicating whether the manifest
#'   has passed each validation check.
validate_individual_meta <- function(individual, template, annotations) {
  # Do checks
  missing_cols_indiv <- dccvalidator::check_cols_individual(
    individual,
    template
  )
  annotation_values_indiv <- dccvalidator::check_annotation_values(
    individual,
    annotations,
    whitelist_keys = c("individualID"),
    success_msg = "All values in the individual metadata are valid",
    fail_msg = "Some values in the individual metadata are invalid"
  )
  duplicate_indiv_ids <- dccvalidator::check_indiv_ids_dup(individual)
  empty_cols_indiv <- dccvalidator::check_cols_empty(
    individual,
    success_msg = "No columns are empty in the individual metadata",
    fail_msg = "Some columns are empty in the individual metadata"
  )
  complete_cols_indiv <- dccvalidator::check_cols_complete(
    individual,
    required_cols = c("individualID"),
    success_msg = "All required columns are complete in the individual metadata", # nolint
    fail_msg = "Some required columns are incomplete in the individual metadata" # nolint
  )

  # Gather check info
  indiv_results <- list(
    missing_cols = missing_cols_indiv,
    annot_values = annotation_values_indiv,
    dup_ids = duplicate_indiv_ids,
    empty_cols = empty_cols_indiv,
    complete_cols = complete_cols_indiv
  )
  indiv_results
}
