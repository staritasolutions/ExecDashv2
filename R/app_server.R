#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
app_server <- function(input, output, session) {
  # Your application server logic

  mod_leads_overview_tab_server("leads_overview")

  mod_school_comp_tab_server("school_comparison")

  mod_roi_tab_server("roi")

  mod_start_date_tab_server("startdates")

  mod_freedom_executive_tab_server("freedom_executive")

  mod_freedom_school_tab_server("freedom_school")

  mod_meevo_executive_tab_server("meevo_executive")

  mod_meevo_school_tab_server("meevo_school")


  observe(print(input$sidebarmenu))
  observe(print(input$card_monthlyleads$maximized))

}
