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
#' @import ggiraph
#'
mod_leads_overview_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(

      bs4Card(mod_date_select_ui(ns("date1")),
              mod_general_select_ui(ns("school1"), "Schools", crm, "School Name"),
              mod_general_select_ui(ns("lead_type1"), "Lead Type", crm, "lead_type"),
              mod_general_select_ui(ns("program1"), "Program", crm, "program_final"))

    ),

    fluidRow(

      bs4Card(mod_conversions_table_ui(ns("conversions_table_1"))),

      bs4Card(mod_monthly_leads_graph_ui(ns("monthly_leads_graph_1")))

    ),

    fluidRow(

      bs4Card(
        mod_crm_metric_select_ui(ns("metric1")),
        mod_date_select_ui(ns("date2")),
        mod_general_select_ui(ns("school2"), "Schools", crm, "School Name"),
        mod_general_select_ui(ns("lead_type2"), "Lead Type", crm, "lead_type"),
        mod_general_select_ui(ns("program2"), "Program", crm, "program_final")
      )

    ),

    fluidRow(

      bs4Card(mod_quarterly_metrics_graph_ui(ns("quarterly_metrics_graph_1"))),

      bs4Card(mod_yearly_metrics_graph_ui(ns("yearly_metrics_graph_1")))

    )
  )
}

#' leads_overview_tab Server Functions
#'
#' @noRd
mod_leads_overview_tab_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    date1 <- mod_date_select_server("date1")
    school1 <- mod_general_select_server("school1")
    lead_type1 <- mod_general_select_server("lead_type1")
    program1 <- mod_general_select_server("program1")
    leads_filtered_crm <- filter_data(data = crm,
                                      school = school1,
                                      lead_type = lead_type1,
                                      program = program1,
                                      date = date1)
    mod_conversions_table_server("conversions_table_1", leads_filtered_crm)
    mod_monthly_leads_graph_server("monthly_leads_graph_1", leads_filtered_crm)

    # Metrics over time section

    metric1 <- mod_crm_metric_select_server("metric1")
    date2 <- mod_date_select_server("date2")
    school2 <- mod_general_select_server("school2")
    lead_type2 <- mod_general_select_server("lead_type2")
    program2 <- mod_general_select_server("program2")
    metrics_filtered_data <- filter_data_with_metric(crm, school2, lead_type2, program2, metric1, date2)
    mod_quarterly_metrics_graph_server("quarterly_metrics_graph_1", metrics_filtered_data, metric1)
    mod_yearly_metrics_graph_server("yearly_metrics_graph_1", metrics_filtered_data, metric1)


  })
}

## To be copied in the UI
# mod_leads_overview_tab_ui("leads_overview_tab_1")

## To be copied in the server
# mod_leads_overview_tab_server("leads_overview_tab_1")
