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

    fluidRow(
      bs4Card(
        title = "Freedom - Executive View",
        id = "card_schoolcomparison",
        width = 12,
        "Welcome to the Freedom Executive View. Here you will find an overview of
        each of your school's data found on the Freedom platform. The controls in
        this box apply to the Attendance bar chart and the Hours at Drop Distribution
        rain cloud plots. Only the Program filter applies to the Currently Attending
        & LOAs table. The Date Range filter will not affect the table because it
        is focused on current status only.",
        fluidRow(
          column(6,
                 mod_date_select_ui(ns("date1"))
                 ),
          column(6,
                 mod_general_select_ui(ns("program1"), "Program", attendance, "revised_program")
                 )
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = "Attendance by School",
        id = ns("card_schoolattend"),
        width = 4,
        maximizable = TRUE,
        height = 550,
        uiOutput(ns("graph_schoolattend"))
      ),
      bs4Card(
        title = "Hours at Drop Distribution",
        id = ns("card_hoursatdrop"),
        width = 4,
        maximizable = TRUE,
        height = 550,
        uiOutput(ns("graph_hoursatdrop"))
      ),
      bs4Card(
        title = "Currently Attending & LOAs",
        id = "card_currenttable",
        width = 4,
        maximizable = TRUE,
        height = 550,
        mod_active_table_ui(ns("active_table_1"))
      )
    ),

    fluidRow(
      bs4Card(
        title = "Enrollment Scorecard Inputs",
        id = "card_ersinputs",
        width = 12,
        "Below are the controls and filters available for the Enrollment Scorecard.
        This is the same Enrollment Scorecard as on the PMAE tool, but specific
        to your schools here for ease of access.",
        fluidRow(
          column(4,
                 mod_date_select_ui(ns("date2"))
                 ),
          column(4,
                 mod_general_select_ui(ns("school2"), "School", scorecard, "School")
          ),
          column(4,
                 mod_general_select_ui(ns("program2"), "Program", scorecard, "Program")
                 )
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = "Enrollment Scorecard",
        id = "card_ers",
        width = 12,
        maximizable = TRUE,
        mod_scorecard_table_ui(ns("scorecard_table_1"))
      )
    )


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

    ## card_schoolattend
    ### UI
    max_schoolattend <- reactive({input$card_schoolattend$maximized})

    output$graph_schoolattend <- renderUI({
      if(max_schoolattend()) {
        fluidRow(
          column(
            12,
            mod_attendance_by_school_graph_ui(ns("attendance_by_school_graph_max"))
          )
        )
      } else {
        fluidRow(
          column(
            12,
            mod_attendance_by_school_graph_ui(ns("attendance_by_school_graph_min"))
          )
        )
      }
    })

    ### Server
    mod_attendance_by_school_graph_server("attendance_by_school_graph_max", attendance, program1, date1, maximized = TRUE)
    mod_attendance_by_school_graph_server("attendance_by_school_graph_min", attendance, program1, date1, maximized = FALSE)

    ## card_hoursatdrop
    ### UI
    max_hoursatdrop <- reactive({input$card_hoursatdrop$maximized})

    output$graph_hoursatdrop <- renderUI({
      if(max_hoursatdrop()) {
        fluidRow(
          column(
            12,
            mod_hours_at_drop_graph_ui(ns("hours_at_drop_graph_max"))
          )
        )
      } else {
        fluidRow(
          column(
            12,
            mod_hours_at_drop_graph_ui(ns("hours_at_drop_graph_min"))
          )
        )
      }
    })

    ### Server
    mod_hours_at_drop_graph_server("hours_at_drop_graph_max", ad_hoc, program1, date1, maximized = TRUE)
    mod_hours_at_drop_graph_server("hours_at_drop_graph_min", ad_hoc, program1, date1, maximized = FALSE)

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
