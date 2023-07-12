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
        title = strong("Freedom - Executive View", style = "font-size:25px;"),
        id = "card_schoolcomparison",
        width = 12,
        fluidRow(
          em(paste0("Data last updated: ", Sys.Date()), style = "margin-bottom: 10px;")
        ),
        fluidRow(
          column(6,
                 mod_date_select_ui(ns("date1"), start = Sys.Date() - months(6), end = Sys.Date())
                 ),
          column(6,
                 uiOutput(ns("program_ui1"))
                 )
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = strong("Attendance by School"),
        id = ns("card_schoolattend"),
        width = 4,
        maximizable = TRUE,
        height = 550,
        uiOutput(ns("graph_schoolattend"))
      ),
      bs4Card(
        title = strong("Hours at Drop Distribution"),
        id = ns("card_hoursatdrop"),
        width = 4,
        maximizable = TRUE,
        height = 550,
        uiOutput(ns("graph_hoursatdrop"))
      ),
      bs4Card(
        title = strong("Currently Attending & LOAs"),
        id = "card_currenttable",
        width = 4,
        maximizable = TRUE,
        height = 550,
        mod_active_table_ui(ns("active_table_1"))
      )
    ),

    fluidRow(
      bs4Card(
        title = strong("Enrollment Scorecard Inputs"),
        id = "card_ersinputs",
        width = 12,
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
        title = strong("Enrollment Scorecard"),
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
    date1 <- mod_date_select_server("date1")

    output$program_ui1 <- renderUI({
      mod_general_select_ui(ns("program1"), "Program", attendance, "revised_program")
    })

    program1 <- mod_general_select_server("program1")

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
