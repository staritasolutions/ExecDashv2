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
          menuItem("Executive View", tabName = "freedomexecview"),
          menuItem("School View", tabName = "freedomschoolview"),
          sidebarHeader("Meevo"),
          menuItem("Executive View", tabName = "meevoexecview"),
          menuItem("School View", tabName = "meevoschoolview"),
          sidebarHeader("Learning Leader"),
          menuItem("Rebooking", tabName = "llrebooking"),
          menuItem("Take Home", tabName = "lltakehome"),
          menuItem("Services", tabName = "llservices")

        )
      ),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(
        right = "App by Starita Solutions LLC",
        left = "For support email support@StaritaSolutions.com"
      ),
      body = dashboardBody(tabItems(
        tabItem(tabName = "leadsoverview",
                mod_leads_overview_tab_ui("leads_overview")
                ),
        tabItem(
          tabName = "schoolcomparison",
          mod_school_comp_tab_ui("school_comparison")
        ),

        tabItem(
          tabName = "roi",
          mod_roi_tab_ui("roi")
          ),
        tabItem(tabName = "startdates", fluidRow("Tab4 content")),
        tabItem(tabName = "startdates", mod_start_date_tab_ui("start_date")),
        tabItem(tabName = "freedomexecview", mod_freedom_executive_tab_ui("freedom_executive")),
        tabItem(tabName = "freedomschoolview", mod_freedom_school_tab_ui("freedom_school")),
        tabItem(tabName = "meevoexecview", mod_meevo_executive_tab_ui("meevo_executive")),
        tabItem(tabName = "meevoschoolview", mod_meevo_school_tab_ui("meevo_school")),
        tabItem(tabName = "llrebooking", mod_ll_rebooking_tab_ui("rebooking")),
        tabItem(tabName = "lltakehome", mod_ll_takehome_tab_ui("takehome")),
        tabItem(tabName = "llservices", mod_ll_services_tab_ui("services"))


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
