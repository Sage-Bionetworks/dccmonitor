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
  dashboardPage(
    dashboardHeader(
      title = "Metadata Validation Monitor",
      titleWidth = "100%"
    ),

    dashboardSidebar(
      sidebarMenu(
        menuItem("Validation Status", tabName = "validation")
      )
    ),

    dashboardBody(

      # Add resources in www
      golem_add_external_resources(),

      tags$div(
        # Validation tab UI
        tabItem(
          tabName = "validation",

          # Use shinyjs
          shinyjs::useShinyjs(),

          # UI for all studies
          uiOutput("all_studies")
        ),
        class = "tab-content"
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
