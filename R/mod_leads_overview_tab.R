#' leads_overview_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textOutput renderText tableOutput renderTable
#' @importFrom DT renderDT DTOutput
#'
mod_leads_overview_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_date_select_ui(ns("date")),
    mod_general_select_ui(ns("school"), "Schools", crm, "School Name"),
    mod_crm_metric_select_ui(ns("metric")),
    h3("Table"),
    DTOutput(ns("table")),
    textOutput(ns("text"))
  )
}

#' leads_overview_tab Server Functions
#'
#' @noRd
mod_leads_overview_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    date1 <- mod_date_select_server("date")
    general1 <- mod_general_select_server("school")
    metric1 <- mod_crm_metric_select_server("metric")
    output$text <- renderText({
      text <- date1$start()
      text})
    df <- filter_data(crm, general1, date1)
    L2P <- conversion(df, "L2P", "Date Submitted", "Prospective Date", TRUE)
    output$table <- DT::renderDT({
      DT::datatable(
        L2P(),
        options = list(
          pageLength = 10,  # Number of rows per page
          pagingType = "full_numbers"  # Pagination type (optional)
        )
      )
    })
  })
}

## To be copied in the UI
# mod_leads_overview_tab_ui("leads_overview_tab_1")

## To be copied in the server
# mod_leads_overview_tab_server("leads_overview_tab_1")
