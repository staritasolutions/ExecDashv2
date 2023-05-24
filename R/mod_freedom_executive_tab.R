#' freedom_executive_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom scales percent
mod_freedom_executive_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_general_select_ui(ns("program1"), "Program", attendance, "revised_program"),
    mod_date_select_ui(ns("date1")),
    mod_attendance_by_school_graph_ui(ns("attendance_by_school_graph_1")),
    mod_hours_at_drop_graph_ui(ns("hours_at_drop_graph_1")),
    mod_active_table_ui(ns("active_table_1")),
    mod_general_select_ui(ns("school2"), "School", scorecard, "School"),
    mod_general_select_ui(ns("program2"), "Program", scorecard, "Program"),
    mod_date_select_ui(ns("date2")),
    mod_scorecard_table_ui(ns("scorecard_table_1"))


  )
}

#' freedom_executive_tab Server Functions
#'
#' @noRd
mod_freedom_executive_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    program1 <- mod_general_select_server("program1")
    date1 <- mod_date_select_server("date1")
    mod_attendance_by_school_graph_server("attendance_by_school_graph_1", attendance, program1, date1)
    mod_hours_at_drop_graph_server("hours_at_drop_graph_1", ad_hoc, program1, date1)
    mod_active_table_server("active_table_1", ad_hoc)
    school2 <- mod_general_select_server("school2")
    program2 <- mod_general_select_server("program2")
    date2 <- mod_date_select_server("date2")
    mod_scorecard_table_server("scorecard_table_1", scorecard, school2, program2, date2)


  })
}

## To be copied in the UI
# mod_freedom_executive_tab_ui("freedom_executive_tab_1")

## To be copied in the server
# mod_freedom_executive_tab_server("freedom_executive_tab_1")
