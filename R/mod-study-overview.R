#' @title UI for the study overview module
#'
#' @description Creates the UI for the study overview
#' module.
#'
#' @importFrom dccvalidator results_boxes_ui
#' @param id Id for the module
study_overview_ui <- function(id, study) {
  ns <- NS(id)
    fluidRow(
      div(
        id = ns(id),
        column(
          10,
          offset = 1,
        box(
          title = study,
          width = NULL,
          status = "primary",
          solidHeader = TRUE,
          tabsetPanel(
            tabPanel(
              "Validation",
              br(),
              uiOutput(ns("infobox_ui")),
              reactable::reactableOutput(ns("infotable")),
              br(),
              dccvalidator::with_busy_indicator_ui(
                actionButton(ns("validate"), "Validate")
              ),
              br(),
              tabsetPanel(
                tabPanel(
                  "Validation Results",
                  br(),
                  dccvalidator::results_boxes_ui(ns("results"))
                ),
                tabPanel(
                  "Data Summary",
                  br(),
                  dccvalidator::file_summary_ui(ns("summary"))
                )
              )
            ),
            tabPanel(
              "Annotations",
              br(),
              edit_annotations_ui(ns("annots"))
            )
          )
        )
      )
    )
  )
}

#' @title Server for the study overview module
#'
#' @description Server for the study overview module.
#'
#' @importFrom dccvalidator results_boxes_server
#' @inheritParams app_server
#' @param fileview The fileview for a specific study.
#' @param annotations A dataframe of annotation definitions.
#' @param syn Synapse client object.
#' @param synapseclient Synapse client.
#' @param annots_folder Synapse folder ID to store generated annotation csvs in.
#' @param study Name of the study.
study_overview_server <- function(id, fileview, annotations, annots_folder,
                                  syn, synapseclient, study) {
  moduleServer(
    id,
    function(input, output, session) {
      stat_values <- reactiveValues(
        num_files = num_meta_files(fileview()),
        num_docs = num_doc_files(fileview()),
        success_rate = 0
      )

      output$infobox_ui <- renderUI({
        get_info_box_ui(
          stat_values$num_docs,
          stat_values$num_files,
          stat_values$success_rate
        )
      })

      output$infotable <- reactable::renderReactable({
        reactable::reactable(
          create_info_table(fileview(), syn),
          resizable = TRUE
        )
      })
      study_view <- isolate(get_all_file_data(fileview(), syn))
      # File summary module setup
      file_types_present <- unique(
        study_view$metadataType[
          !is.na(study_view$metadataType)
          ]
      )
      if (length(file_types_present) > 0) {
        req(study_view)
        file_indices <- get_file_indices_named(
          study_view,
          file_types_present
        )
        file_list <- reactive({
          purrr::map(file_indices, function(index) {
            tibble::as_tibble(study_view$file_data[[index]])
          })
        })
        callModule(dccvalidator::file_summary_server, "summary", file_list)
      }

      # Annotations module
      edit_annotations_server(
        id = "annots",
        fileview = study_view,
        annots_folder = annots_folder,
        syn = syn,
        synapseclient = synapseclient
      )

      # Validate button
      observeEvent(input$validate, {
        req(study_view)
        dccvalidator::with_busy_indicator_server("validate", {
          all_results <- validate_study(
            study_table = study_view[!is.na(study_view[["metadataType"]]), ],
            annotations = annotations,
            syn = syn,
            study = study
          )
          if (length(all_results) > 0) {
            stat_values$success_rate <- percent_pass_validation(all_results)
          }
          callModule(
            dccvalidator::results_boxes_server,
            id = "results",
            session = session,
            results = all_results
          )
        })
      })
    }
  )
}

#' @title Get info box UI
#'
#' @description Creates the html for the info box UI.
#'
#' @param num_docs Number of documentation files.
#' @param num_meta Number of metadata files.
#' @param percent_success Integer value between 0 and 100 representing the
#'   success rate for validation.
get_info_box_ui <- function(num_docs, num_meta, percent_success) {
  fluidRow(
    valueBox(
      num_docs,
      "# Documents",
      width = 3,
      icon = icon("file-alt"),
      color = box_docs_color(num_docs)
    ),
    valueBox(
      num_meta,
      "# Metadata Files",
      width = 3,
      icon = icon("table"),
      color = box_meta_color(num_meta)
    ),
    valueBox(
      percent_success,
      "% Success",
      width = 3,
      icon = icon("percentage"),
      color = box_success_color(percent_success)
    )
  )
}
