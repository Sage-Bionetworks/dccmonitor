#' Gathering data and validating metadata files
#'
#' This function is here as an example workflow, for now.
#' It shows how the functions in this package work together
#' to get the information needed to monitor data curation
#' status for studies.
#'
#' This function will most likely be adjusted, improved,
#' and eventually refactored during the switch to being
#' part of a shiny app.
#'
#' Currently assumes user is registered with Synapse, and
#' has access to the fileview and files needed.
main <- function() {
  Sys.setenv(R_CONFIG_ACTIVE = "amp-ad") # Replace "default" with your config

  # Should have check for team membership!
  synapser::synLogin(silent = TRUE)

  # Get the metadata fileview in team directory & make into a dataframe
  fileview_id <- config::get("metadata_fileview")
  fileview <- get_all_studies_table(fileview_id)

  # Download annotation definitions
  annotations <- dccvalidator::get_synapse_annotations(
    synID = config::get("annotations_table")
  )

  fileview <- get_all_file_templates(fileview)
  fileview <- get_all_file_data(fileview)
  # Runs checks on all files within all studies
  fileview <- validate_all_studies(fileview, annotations)
}
