#' @title App UI
#'
#' @description Create the UI component of the dccmonitor
#' Shiny app.
#'
#' @import shiny
#' @import shinydashboard
#' @param request Shiny request
#' @export
mod_main_ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    dashboardHeader(
      title = "Metadata Validation Monitor",
      titleWidth = "100%"
    ),

    dashboardSidebar(disable = TRUE),

    dashboardBody(
      # Add resources in www
      golem_add_external_resources(),
      shinyjs::useShinyjs(),
      fluidRow(
        column(
          10,
          offset = 1,
          box(
            shinyjs::disabled(
              selectInput(
                inputId = ns("study_name"),
                label = "Current Studies",
                choices = "",
                selected = ""
              )
            ),
            width = NULL
          )
        )
      ),
      fluidRow(
        column(
          12,
          uiOutput(ns("study_overview"))
        )
      )
    )
  )
}

mod_main_server <- function(id, syn) {
  moduleServer(
    id,
    function(input, output, session) {
      shiny::req(inherits(syn, "synapseclient.client.Synapse") & logged_in(syn))

      # Check if user is in correct consortium team (needed to access
      # project), and if they are a certified user.
      user <- syn$getUserProfile()
      membership <- dccvalidator::check_team_membership(
        teams = get_golem_config("teams"),
        user = user,
        syn = syn
      )
      certified <- dccvalidator::check_certified_user(user$ownerId, syn = syn)
      dccvalidator::report_unsatisfied_requirements(
        membership,
        certified,
        syn = syn
      )

      ## If pass checks, do stuff
      if (inherits(membership, "check_pass") &
          inherits(certified, "check_pass")) {
        # Add folder to upload annotations to if doesn't exist already
        annots_folder <- try({
          new_folder <- synapse$Folder(
            name = user$userName,
            parent = get_golem_config("annotations_storage")
          )
          syn$store(new_folder)
        })

        # Download annotation definitions
        annotations <- purrr::map_dfr(
          get_golem_config("annotations_table"),
          dccvalidator::get_synapse_annotations,
          syn = syn
        )

        # Should be in config
        fileview_id <- get_golem_config("consortium_fileview")
        # Get the Fileview in team directory & make into a dataframe
        fileview <- get_all_studies_table(fileview_id, syn)

        # Setup study server functions
        studies <- sort(unique(fileview$study))
        updateSelectInput(
          inputId = "study_name",
          label = "Current Studies",
          choices = c("", studies),
          selected = ""
        )
        shinyjs::enable("study_name")

        observeEvent(input$study_name, {
          if (input$study_name != "") {
            output$study_overview <- renderUI(
              study_overview_ui(session$ns(input$study_name), input$study_name)
            )
            view <- reactive({
              filter_study_table_latest(fileview, input$study_name)
            })
            study_overview_server(
              id = input$study_name,
              fileview = view,
              annotations = annotations,
              annots_folder = annots_folder,
              syn = syn,
              synapseclient = synapse,
              study = input$study_name
            )
          }
        })
      }
    }
  )
}
