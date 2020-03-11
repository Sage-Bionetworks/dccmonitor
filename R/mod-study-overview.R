#' @title UI for the study overview module
#'
#' @description Creates the UI for the study overview
#' module.
#'
#' @importFrom dccvalidator results_boxes_ui
#' @param id Id for the module
study_overview_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    id,
    fluidPage(
      div(
        id = id,
        box(
          title = id,
          width = 12,
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
study_overview_server <- function(input, output, session,
                                  fileview, annotations, annots_folder,
                                  syn, synapseclient) {
  session <- getDefaultReactiveDomain()

  stat_values <- reactiveValues(
    num_files = num_meta_files(fileview()),
    num_docs = num_doc_files(fileview()),
    success_rate = 0
  )

  observe({
    output$infobox_ui <- renderUI({
      get_info_box_ui(
        stat_values$num_docs,
        stat_values$num_files,
        stat_values$success_rate
      )
    })
  })

  data <- reactiveValues(
    study_view = fileview()
  )

  observe({
    output$infotable <- reactable::renderReactable({
      reactable::reactable(
        create_info_table(fileview(), syn),
        resizable = TRUE
      )
    })
    data$study_view <- get_all_file_data(fileview(), syn)
    # File summary module setup
    file_types_present <- unique(
      data$study_view$metadataType[
        !is.na(data$study_view$metadataType)
      ]
    )
    if (length(file_types_present) > 0) {
      file_indices <- get_file_indices_named(
        data$study_view,
        file_types_present
      )
      file_list <- reactive({
        purrr::map(file_indices, function(index) {
          tibble::as_tibble(data$study_view$file_data[[index]])
        })
      })
      callModule(dccvalidator::file_summary_server, "summary", file_list)
    }

    # Annotations module
    data$study_view <- get_all_file_data(fileview(), syn)
    callModule(
      edit_annotations_server,
      "annots",
      data$study_view,
      annots_folder,
      syn = syn,
      synapseclient = synapseclient
    )

    # Validate button
    observeEvent(input$validate, {
      dccvalidator::with_busy_indicator_server("validate", {
        data$all_results <- validate_study(data$study_view, annotations, syn)
        if (length(data$all_results) > 0) {
          stat_values$success_rate <- percent_pass_validation(data$all_results)
        }
      })
    })
  })

  # Populate validation resulst
  observeEvent(data$all_results, {
    callModule(
      dccvalidator::results_boxes_server,
      id = "results",
      session = session,
      results = data$all_results
    )
  })
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
