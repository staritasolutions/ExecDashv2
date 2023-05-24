#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @import bslib
#' @noRd

theme_set(theme_minimal())

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      dark = NULL,
      # removes light/dark mode toggle
      header = dashboardHeader(
        title = "Executive Dashboard",
        fixed = FALSE,
        skin = "light"
      ),
      sidebar = dashboardSidebar(
        id = "sidebar",
        expandOnHover = FALSE,
        minified = FALSE,
        collapsed = FALSE,

        sidebarMenu(
          id = "sidebarmenu",
          flat = TRUE,
          childIndent = TRUE,
          sidebarHeader("CRM"),
          menuItem("Leads Overview", tabName = "leadsoverview"),
          menuItem("School Comparison", tabName = "schoolcomparison"),
          menuItem("ROI", tabName = "roi"),
          menuItem("Start Dates", tabName = "startdates"),
          sidebarHeader("Freedom"),
          menuItem("Executive Veiw", tabName = "executiveview")
        )
      ),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(),
      body = dashboardBody(tabItems(
        tabItem(tabName = "leadsoverview",
                mod_leads_overview_tab_ui("leads_overview")
                ),
        tabItem(
          tabName = "schoolcomparison",
          card(
            full_screen = TRUE,
            card_header("Monthly Lead Breakdown"),
            card_body(p("some other content")),
            card_body(p("This is another part of the body."))
          )
        ),

        tabItem(tabName = "roi", mod_roi_tab_ui("roi")),
        tabItem(tabName = "startdates", mod_start_date_tab_ui("start_date")),
        tabItem(tabName = "executiveview", mod_freedom_executive_tab_ui("freedom_executive"))
      ))
    )
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
