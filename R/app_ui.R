#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @import bslib
#' @noRd
#' @include app_server.R

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
        tags$img(src = "www/Paul_Mitchell_logo.svg.png", height = "22px", width = "182px"),
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
          #sidebarHeader("CRM"),
          menuItem("CRM", tabName = "crm",
                   icon = icon(name = "calendar", lib = "font-awesome"),
                   menuSubItem(
                     text = "Leads Overview",
                     tabName = "leadsoverview"
                   ),
                   menuSubItem(
                     text = "School Comparison",
                     tabName = "schoolcomparison"
                   ),
                   menuSubItem(
                     text = "ROI",
                     tabName = "roi"
                   ),
                   menuSubItem(
                     text = "Start Dates",
                     tabName = "startdates"
                   )
                   ),
          menuItem("Freedom", tabName = "freedom",
                   icon = icon(name = "user-clock", lib = "font-awesome"),
                   menuSubItem(
                     text = "Executive View",
                     tabName = "freedomexecview"
                   ),
                   menuSubItem(
                     text = "School View",
                     tabName = "freedomschoolview"
                   )
                   ),
          menuItem("Meevo", tabName = "meevo",
                   icon = icon(name = "scissors", lib = "font-awesome"),
                   menuSubItem(
                     text = "Executive View",
                     tabName = "meevoexecview"
                   ),
                   menuSubItem(
                     text = "School View",
                     tabName = "meevoschoolview"
                   )
                   ),
          menuItem("Learning Leader", tabName = "ll",
                   icon = icon(name = "chalkboard-user", lib = "font-awesome"),
                   menuSubItem(
                     text = "Rebooking",
                     tabName = "llrebooking"
                   ),
                   menuSubItem(
                     text = "Take Home",
                     tabName = "lltakehome",
                   ),
                   menuSubItem(
                     text = "Services",
                     tabName = "llservices"
                   )
                   )

        )
      ),
      controlbar = dashboardControlbar(),
      footer = dashboardFooter(
        right = "App by Starita Solutions LLC",
        left = "For support email support@StaritaSolutions.com",
        fixed = FALSE
      ),
      body = dashboardBody(tabItems(
        tabItem(
          tabName = "leadsoverview",
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
        tabItem(
          tabName = "startdates",
          mod_start_date_tab_ui("startdates")
          ),
        tabItem(
          tabName = "startdates",
          mod_start_date_tab_ui("start_date")
          ),
        tabItem(
          tabName = "freedomexecview",
          mod_freedom_executive_tab_ui("freedom_executive")
          ),
        tabItem(
          tabName = "freedomschoolview",
          mod_freedom_school_tab_ui("freedom_school")
          ),
        tabItem(
          tabName = "meevoexecview",
          mod_meevo_executive_tab_ui("meevo_executive")
          ),
        tabItem(
          tabName = "meevoschoolview",
          mod_meevo_school_tab_ui("meevo_school")
          ),
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
