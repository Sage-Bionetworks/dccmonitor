#' @title App server
#'
#' @description Create the server-side component of the dccmonitor Shiny app.
#'
#' @import shiny
#' @import shinydashboard
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @export
app_server <- function(input, output, session) {
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
    synapser::synLogin(sessionToken = input$cookie)

    # Check if user is in AMP-AD Consortium team (needed to access
    # project), and if they are a certified user.
    user <- synapser::synGetUserProfile()
    membership <- dccvalidator:::check_team_membership(
      teams = config::get("teams"),
      user = user
    )


    ## Need to use non-forced list of names
    studynames <- c("WayCool", "TotesCool", "SuperCool")

    # Should be in config
    fileview_id <- config::get("consortium_fileview")
    # Get the Fileview in team directory & make into a dataframe
    fileview <- get_all_studies_table(fileview_id)
    fileview <- get_all_file_templates(fileview)
    view_waycool <- reactive({fileview[which(fileview$study == "WayCool"), ]})
    view_supercool <- reactive({fileview[which(fileview$study == "SuperCool"), ]})
    view_totescool <- reactive({fileview[which(fileview$study == "TotesCool"), ]})

    # Download annotation definitions
    annotations <- dccvalidator::get_synapse_annotations()

    callModule(
      study_overview_server,
      "WayCool",
      session = getDefaultReactiveDomain(),
      fileview = view_waycool,
      annotations = annotations
    )
    callModule(
      study_overview_server,
      "SuperCool",
      session = getDefaultReactiveDomain(),
      fileview = view_supercool,
      annotations = annotations
    )
    callModule(
      study_overview_server,
      "TotesCool",
      session = getDefaultReactiveDomain(),
      fileview = view_totescool,
      annotations = annotations
    )
  })
}
