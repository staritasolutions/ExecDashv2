#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      header = dashboardHeader(title = "Executive Dashboard"),
      sidebar = dashboardSidebar(id = "sidebar"),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(
        layout_column_wrap(
          width = 1/2,
          card(full_screen = TRUE, card_header("Monthly Lead Breakdown"),
               card_body(mod_leads_overview_tab_ui("leads_overview"))),
          card(full_screen = TRUE, card_header("Conversion Metrics"),
               card_body(p("This is the body")))
        )
    ))
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ExecDashv2"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
