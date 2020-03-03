#' @title App UI
#'
#' @description Create the UI component of the dccmonitor
#' Shiny app.
#'
#' @import shiny
#' @import shinydashboard
#' @param request Shiny request
#' @export
app_ui <- function(request) {

  tagList(
    # Add resources in www
    golem_add_external_resources(),

    dashboardPage(
      dashboardHeader(
        title = "Metadata Validation Monitor",
        titleWidth = "100%"
      ),

      dashboardSidebar(disable = TRUE),

      dashboardBody(
        navlistPanel(
          id = "studies",
          tabPanel(
            "Start",
            h1("Welcome to dccmonitor"),
            p("The app is loaded when all the studies appear as tabs in the sidebar.") # nolint
          )
        )
      )
    )
  )
}

#' @import shiny
#' @keywords internal
golem_add_external_resources <- function() {
  addResourcePath(
    "www", system.file("app/www", package = "dccmonitor")
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
    tags$script(src = "www/readCookie.js")
  )
}
