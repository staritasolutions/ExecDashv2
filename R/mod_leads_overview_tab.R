#' leads_overview_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textOutput renderText tableOutput renderTable
#' @importFrom DT renderDataTable dataTableOutput
#'
mod_leads_overview_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_date_select_ui(ns("date")),
    mod_general_select_ui(ns("school"), "Schools", crm, "School Name"),
    mod_general_select_ui(ns("lead_type"), "Lead Type", crm, "lead_type"),
    mod_general_select_ui(ns("program"), "Program", crm, "program_final"),
    mod_crm_metric_select_ui(ns("metric")),
    h3("Table"),
    DT::dataTableOutput(ns("table1")),
    mod_conversions_table_ui(ns("conversions_table_1")),
    mod_monthly_leads_graph_ui(ns("monthly_leads_graph_1"))
  )
}

#' leads_overview_tab Server Functions
#'
#' @noRd
mod_leads_overview_tab_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    date1 <- mod_date_select_server("date")
    school1 <- mod_general_select_server("school")
    lead_type1 <- mod_general_select_server("lead_type")
    program1 <- mod_general_select_server("program")
    metric1 <- mod_crm_metric_select_server("metric")
    filtered_crm <- filter_data(crm, school1, lead_type1, program1, date1)
    mod_conversions_table_server("conversions_table_1", filtered_crm)
    mod_monthly_leads_graph_server("monthly_leads_graph_1", filtered_crm)
  })
}

## To be copied in the UI
# mod_leads_overview_tab_ui("leads_overview_tab_1")

## To be copied in the server
# mod_leads_overview_tab_server("leads_overview_tab_1")
