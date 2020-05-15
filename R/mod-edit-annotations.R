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
        tags$li("Click on 'Get Annotations' to join all metadata to the manifest."),
        tags$li("Select the desired annotation columns and verify annotations do not contain PII/PHI."),
        tags$li("Below the annotations table, give the name to upload the file as and click on 'Upload' to save the annotations as a csv file in Synapse.")
      ),
      h4("Important:"),
      tags$ul(
        tags$li("In order to save to Synapse, the annotations file is first downloaded to a local, temporary file. Double-check that PII/PHI data has not been included."),
        tags$li("All metadata files listed in the table above are joined to the manifest. Verify that only files relevant to the current data release are present.")
      ),
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
      textInput(ns("file_name"), "Enter file name to upload as:"),
      shinyjs::disabled(
        dccvalidator::with_busy_indicator_ui(
          actionButton(ns("upload"), "Upload")
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
edit_annotations_server <- function(input, output, session,
                                    fileview, annots_folder,
                                    syn, synapseclient) {
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
          pageSizeOptions = c(1, 5, 10, 25, 50, 100),
          outlined = TRUE
        )
      })
    }
  })

  # Require important info to be able to push upload button
  observe({
    if (input$file_name == "" || is.null(input$file_name)
        || input$annot_keys == "" || is.null(input$annot_keys)) {
      shinyjs::disable("upload")
    } else {
      shinyjs::enable("upload")
    }
  })

  # Upload handling
  observeEvent(input$upload, {
    dccvalidator::with_busy_indicator_server("upload", {
      metadata_subset <- all_metadata[, input$annot_keys]
      temp <- tempfile(pattern = input$file_name, fileext = ".csv")
      utils::write.csv(metadata_subset, file = temp, row.names = FALSE)
      file_to_upload <- synapseclient$File(
        temp,
        parent = annots_folder,
        name = input$file_name,
        annotations = list(
          study = unique(fileview$study[!is.na(fileview$study)])
        )
      )
      syn$store(file_to_upload)
    })
  })
}
