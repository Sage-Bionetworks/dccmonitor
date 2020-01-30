#' @title
#'
#' @description
#'
#' @param id Module id.
#' @param keys List of annotation keys present in dataset.
edit_annotations_ui <- function(id) {
  ns <- NS(id)

  shinyWidgets::multiInput(
    "annot_keys",
    label = "Choose Annotation Keys",
    choices = ""
  )
}

#' @title
#'
#' @description
#'
#' @inheritParams study_overview_server
edit_annotations_server <- function(input, output, session, fileview) {
  # Table with all annotation keys/values
}
