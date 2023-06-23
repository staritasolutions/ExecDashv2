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
    fluidRow(
      bs4Card(
        title = strong("CRM - School Comparison", style = "font-size:25px;"),
        id = "card_schoolcomparison",
        width = 12,
        "Welcome to the CRM school comparison page. This box contains the controls
        needed for the table and graph below.",
        fluidRow(
          column(3, mod_date_select_ui(ns("date1"))),
          column(3, mod_general_select_ui(ns("school1"), "Schools", crm, "School Name")),
          column(3, mod_general_select_ui(ns("program1"), "Program", crm, "program_final")),
          column(3, mod_general_select_ui(ns("lead_type1"), "Lead Type", crm, "lead_type"))
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = strong("School Comparisons Table"),
        id = "card_schoolcomptable",
        width = 12,
        maximizable = TRUE,
        mod_school_comp_table_ui(ns("school_comp_table"))
      )
    ),

    fluidRow(
      bs4Card(
        id = "card_metricselect",
        width = 12,
        fluidRow(
          column(6, mod_crm_metric_select_ui(ns("metric1"))),
          column(6,
                 "Use this metric selector to further adjust the plot below that
                 shows the school comparisons detailed at the top of the page.")
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = strong("School Comparisons Graph"),
        id = ns("card_schoolcompgraph"),
        width = 12,
        maximizable = TRUE,
        uiOutput(ns("graph_schoolcomp"))
      )
    )
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

    table_data <- filter_data(crm, school1, lead_type1, program1, "Lead", date1)
    mod_school_comp_table_server("school_comp_table", table_data)

    ## card_schoolcompgraph
    ### UI
    max_schoolcompgraph <- reactive({input$card_schoolcompgraph$maximized})

    output$graph_schoolcomp <- renderUI({
      if(max_schoolcompgraph()) {
        fluidRow(
          column(12,
                 mod_school_comp_graph_ui(ns("school_comp_graph_max")))
        )
      } else {
        fluidRow(
          column(12,
                 mod_school_comp_graph_ui(ns("school_comp_graph_min")))
        )
      }
    })

    ### Server
    graph_data <- filter_data_with_metric(crm, school1, lead_type1, program1, metric1, date1)
    mod_school_comp_graph_server("school_comp_graph_max", graph_data, metric1, maximized = TRUE)
    mod_school_comp_graph_server("school_comp_graph_min", graph_data, metric1, maximized = FALSE)

  })
}

## To be copied in the UI
# mod_school_comp_tab_ui("school_comp_tab_1")

## To be copied in the server
# mod_school_comp_tab_server("school_comp_tab_1")
