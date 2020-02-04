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
                reactable::reactableOutput(ns("data_details"))
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

    files_present <- unique(
      data$study_view$metadataType[
        !is.na(data$study_view$metadataType)
      ]
    )
    if (length(files_present) > 0) {
      updateSelectInput(
        session = session,
        inputId = "file_to_summarize",
        label = "Choose file to view",
        choices = files_present
      )
    }

    temp <- get_all_file_data(fileview(), syn)
    callModule(edit_annotations_server, "annots", temp)
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
    if (input$file_to_summarize != "") {
      data_index <- which(
        data$study_view$metadataType == input$file_to_summarize
      )
      output$datafilevisdat <- renderPlot({
        visualize_data_types(data$study_view$file_data[[data_index]])
      })
      output$data_details <- reactable::renderReactable({
        reactable::reactable(
          data_summary(data$study_view$file_data[[data_index]]),
          highlight = TRUE,
          searchable = TRUE,
          resizable = TRUE,
          columns = list(
            variable = reactable::colDef(
              name = "Variable",
              width = 125
            ),
            type = reactable::colDef(
              name = "Type",
              width = 75
            ),
            missing = reactable::colDef(
              name = "Missing",
              maxWidth = 75
            ),
            complete = reactable::colDef(
              name = "Complete",
              maxWidth = 75
            ),
            n = reactable::colDef(
              name = "n",
              maxWidth = 75
            ),
            min = reactable::colDef(
              name = "Min",
              maxWidth = 75
            ),
            max = reactable::colDef(
              name = "Max",
              maxWidth = 75
            ),
            empty = reactable::colDef(
              name = "# Empty",
              maxWidth = 75
            ),
            n_unique = reactable::colDef(
              name = "# Unique",
              maxWidth = 75
            ),
            value_occurrence = reactable::colDef(
              name = "Value (# Occurrences)"
            )
          )
        )
      })
    }
  })
}
