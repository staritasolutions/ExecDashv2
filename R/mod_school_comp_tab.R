#' school_comp_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_school_comp_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_date_select_ui(ns("date1")),
    mod_general_select_ui(ns("school1"), "Schools", crm, "School Name"),
    mod_general_select_ui(ns("lead_type1"), "Lead Type", crm, "lead_type"),
    mod_general_select_ui(ns("program1"), "Program", crm, "program_final"),
    mod_crm_metric_select_ui(ns("metric1")),
    mod_school_comp_graph_ui(ns("school_comp_graph_1")),
    mod_school_comp_table_ui(ns("school_comp_table_1")),
    mod_school_comp_table_PMAE_ui(ns("school_comp_table_2"))
  )
}

#' school_comp_tab Server Functions
#'
#' @noRd
mod_school_comp_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    date1 <- mod_date_select_server("date1")
    school1 <- mod_general_select_server("school1")
    lead_type1 <- mod_general_select_server("lead_type1")
    program1 <- mod_general_select_server("program1")
    metric1 <- mod_crm_metric_select_server("metric1")
    graph_data <- filter_data_with_metric(crm, school1, lead_type1, program1, metric1, date1)
    mod_school_comp_graph_server("school_comp_graph_1", graph_data, metric1)
    table_data <- filter_data(crm, school1, lead_type1, program1, "Lead", date1)
    mod_school_comp_table_server("school_comp_table_1", table_data)
    mod_school_comp_table_PMAE_server("school_comp_table_2", crm, school1, lead_type1, program1, date1)

  })
}

## To be copied in the UI
# mod_school_comp_tab_ui("school_comp_tab_1")

## To be copied in the server
# mod_school_comp_tab_server("school_comp_tab_1")
