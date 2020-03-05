#' @title Edit annotations module UI
#'
#' @description UI function for the edit annotations module.
#'
#' @param id Module id.
edit_annotations_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      h4("Instructions"),
      # nolint start
      tags$ol(
        tags$li("Click on 'Get Annotations' to join all metadata to the manifest. See note below."),
        tags$li("Select the desired annotation columns and verify annotations do not contain PII/PHI."),
        tags$li("Below the annotations table, give the name to download the file as and click on 'Download' to download the annotations as a csv file locally.")
      ),
      p("Note: All metadata files listed in the table above are joined to the manifest. Verify that only files relevant to the current data release are present."),
      # nolint end
      dccvalidator::with_busy_indicator_ui(
        actionButton(ns("get_annots"), "Get annotations")
      ),
      br(),
      br(),
      shinyWidgets::multiInput(
        ns("annot_keys"),
        label = "Choose Annotation Keys",
        choices = ""
      ),
      br(),
      h4("Annotations table"),
      reactable::reactableOutput(ns("annot_table")),
      textOutput(ns("no_annots")),
      br(),
      textInput(ns("file_name"), "Enter file name to download as:"),
      shinyjs::disabled(
        dccvalidator::with_busy_indicator_ui(
          downloadButton(ns("download"), "Download")
        )
      )
    )
  )
}

#' @title Edit annotations module server
#'
#' @description Server function for the edit annotations module.
#'
#' @inheritParams study_overview_server
edit_annotations_server <- function(input, output, session, fileview) {
  session <- getDefaultReactiveDomain()

  all_metadata <- NULL
  observeEvent(input$get_annots, {
    dccvalidator::with_busy_indicator_server("get_annots", {
      all_metadata <<- combine_all_metadata(fileview)
      annot_keys <- names(all_metadata)
      disallowed_keys <- config::get("annotation_keys")
      annot_keys_subset <- setdiff(annot_keys, disallowed_keys)

      if (!is.null(all_metadata)) {
        shinyWidgets::updateMultiInput(
          session = session,
          "annot_keys",
          label = "Choose Annotation Keys",
          choices = annot_keys,
          selected = annot_keys_subset
        )
      }
    })
  })

  observe({
    if (is.null(input$annot_keys)) {
      output$annot_table <- reactable::renderReactable(NULL)
    } else {
      metadata_subset <- all_metadata[, input$annot_keys]
      output$annot_table <- reactable::renderReactable({
        reactable::reactable(
          metadata_subset,
          highlight = TRUE,
          searchable = TRUE,
          resizable = TRUE,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(10, 25, 50, 100),
          outlined = TRUE
        )
      })
    }
  })

  observe({
    if (input$file_name == "" || is.null(input$file_name) || input$annot_keys == "" || is.null(input$annot_keys)) { # nolint
      shinyjs::disable("download")
    } else {
      shinyjs::enable("download")
    }
  })
  dccvalidator::with_busy_indicator_server("download", {
    output$download <- downloadHandler(
      filename = function() {
        paste0(input$file_name, ".csv")
      },
      content = function(file) {
        metadata_subset <- all_metadata[, input$annot_keys]
        utils::write.csv(metadata_subset, file, row.names = FALSE)
      }
    )
  })
}
