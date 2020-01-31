#' @title App server
#'
#' @description Create the server-side component of the dccmonitor Shiny app.
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom dccvalidator check_team_membership check_certified_user
#'   report_unsatisfied_requirements
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @export
app_server <- function(input, output, session) {
  syn <- synapse$Synapse()
  session$sendCustomMessage(type = "readCookie", message = list())

  # Show message if user is not logged in to synapse
  unauthorized <- observeEvent(input$authorized, {
    showModal(
      modalDialog(
        title = "Not logged in",
        HTML("You must log in to <a target=\"_blank\" href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.") # nolint
      )
    )
  })

  # Do stuff if authorized
  observeEvent(input$cookie, {
    syn$login(sessionToken = input$cookie)

    # Check if user is in AMP-AD Consortium team (needed to access
    # project), and if they are a certified user.
    user <- syn$getUserProfile()
    membership <- dccvalidator::check_team_membership(
      teams = config::get("teams"),
      user = user,
      syn = syn
    )
    certified <- dccvalidator::check_certified_user(user$ownerId, syn = syn)
    dccvalidator:::report_unsatisfied_requirements(
      membership,
      certified,
      syn = syn
    )

    if (inherits(membership, "check_pass")) {
      # Download annotation definitions
      annotations <- purrr::map_dfr(
        config::get("annotations_table"),
        dccvalidator::get_synapse_annotations,
        syn = syn
      )

      # Should be in config
      fileview_id <- config::get("consortium_fileview")
      # Get the Fileview in team directory & make into a dataframe
      fileview <- get_all_studies_table(fileview_id, syn)
      fileview <- get_all_file_templates(fileview)

      output$all_studies <- renderUI({
        set_up_ui(fileview)
      })

      # Setup study server functions
      studies <- unique(fileview$study)
      for (study in studies) {
        view <- reactive({
          filter_study_table_latest(fileview, study)
        })
        callModule(
          study_overview_server,
          study,
          session = getDefaultReactiveDomain(),
          fileview = view,
          annotations = annotations,
          syn = syn
        )
      }
    }
  })
}

#' @title Setup UI for all studies
#'
#' @description Setup UI for all studies in the fileview.
#'
#' @param fileview A data frame with at least
#'   one column, `study`.
set_up_ui <- function(fileview) {
  studies <- unique(fileview$study)
  html_studies <- ""
  for (study in studies) {
    html_studies <- c(html_studies, study_overview_ui(study))
  }
  html_studies
}
