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
    dccvalidator::report_unsatisfied_requirements(
      membership,
      certified,
      syn = syn
    )

    if (inherits(membership, "check_pass")) {
      # Add folder to upload annotations to if doesn't exist already
      annots_folder <- try({
        new_folder <- synapse$Folder(
          name = user$userName,
          parent = config::get("annotations_storage")
          )
        syn$store(new_folder)
      })

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

      # Setup study server functions
      studies <- unique(fileview$study)
      purrr::walk(studies, function(study) {
        insertTab(
          inputId = "studies",
          study_overview_ui(study),
          target = "Start",
          position = "after"
        )
        view <- reactive({
          filter_study_table_latest(fileview, study)
        })
        callModule(
          study_overview_server,
          study,
          session = getDefaultReactiveDomain(),
          fileview = view,
          annotations = annotations,
          annots_folder = annots_folder,
          syn = syn,
          synapseclient = synapse,
          study = study
        )
      })
    }
  })
}
