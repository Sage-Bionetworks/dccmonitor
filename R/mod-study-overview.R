#' @title UI for the study overview module
#'
#' @description Creates the UI for the study overview
#' module.
#'
#' @param id Id for the module
study_overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      id = id,
      box(
        title = id,
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        uiOutput(ns("infobox_ui")),
        tableOutput(ns("infotable")),
        tabsetPanel(
          tabPanel(
            "Validation Results",
            br(),
            dccvalidator::results_boxes_ui(ns("results"))
          ),
          tabPanel(
            "Data Summary",
            br(),
            selectInput(
              ns("file_to_summarize"),
              label = "Choose file to view",
              choices = ""
            ),
            tabsetPanel(
              tabPanel(
                "File Overview",
                plotOutput(ns("datafilevisdat"))
              ),
              tabPanel(
                "File Details",
                br(),
                tableOutput(ns("data_details"))
              )
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
#' @inheritParams app_server
#' @param fileview The fileview for a specific study.
#' @param annotations A dataframe of annotation definitions.
#' @param syn Synapse client object.
study_overview_server <- function(input, output, session,
                                  fileview, annotations, syn) {
  session <- getDefaultReactiveDomain()

  stat_values <- reactiveValues(
    num_files = num_meta_files(fileview()),
    num_docs = num_doc_files(fileview()),
    success_rate = 0
  )

  data <- reactiveValues(
    study_view = fileview()
  )
  observe({
    output$infotable <- renderTable({
      create_info_table(fileview(), syn)
    })

    updateSelectInput(
      session = session,
      inputId = "file_to_summarize",
      label = "Choose file to view",
      choices = unique(
        data$study_view$metadataType[
          !is.na(data$study_view$metadataType)
        ]
      )
    )

    temp <- get_all_file_data(fileview(), syn)
    data$study_view <- validate_study(temp, annotations, syn)
    data$all_results <- purrr::flatten(
      data$study_view$results[which(!is.na(data$study_view$metadataType))]
    )
    if (length(data$all_results) > 0) {
      stat_values$success_rate <- percent_pass_validation(data$all_results)
    }
  })

  observeEvent(data$all_results, {
    callModule(
      dccvalidator::results_boxes_server,
      id = "results",
      session = session,
      results = data$all_results
    )

    output$infobox_ui <- renderUI({
      fluidRow(
        valueBox(
          stat_values$num_docs,
          "# Documents",
          width = 3,
          icon = icon("file-alt"),
          color = box_docs_color(stat_values$num_docs)
        ),
        valueBox(
          stat_values$num_files,
          "# Metadata Files",
          width = 3,
          icon = icon("table"),
          color = box_meta_color(stat_values$num_files)
        ),
        valueBox(
          stat_values$success_rate,
          "% Success",
          width = 3,
          icon = icon("percentage"),
          color = box_success_color(stat_values$success_rate)
        )
      )
    })
  })

  observeEvent(input$file_to_summarize, {
    data_index <- which(data$study_view$metadataType == input$file_to_summarize)
    output$datafilevisdat <- renderPlot({
      visualize_data_types(data$study_view$file_data[[data_index]])
    })
    output$data_details <- renderTable({
      data_summary(data$study_view$file_data[[data_index]])
    })
  })
}
