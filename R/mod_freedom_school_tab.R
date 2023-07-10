#' freedom_school_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_freedom_school_tab_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Card(
        title = "Freedom - School View",
        id = "card_schoolviewinfo",
        width = 12,
        "Welcome to the Freedom School View tab. Here you will find information
        about individual schools and students within each school. The controls in
        this card modify the attendance graph.",
        fluidRow(
          column(3,
                 mod_date_select_ui(ns("date1"), start = Sys.Date() - 35)
                 ),
          column(3,
                 uiOutput(ns("school_ui1"))
                 ),
          column(3,
                 uiOutput(ns("program_ui1"))
                 ),
          column(3,
                 prettyRadioButtons(ns("attendance_graph_choice"),
                                    "Select Graph",
                                    choices = c("Attendance", "Attendance %"),
                                    selected = "Attendance",
                                    shape = "curve",
                                    bigger = TRUE,
                                    animation = "smooth"
                 )
                 )

        )
      )
    ),

    fluidRow(
      bs4Card(
        title = "Attendance",
        id = "card_attendance",
        width = 12,
        mod_school_attendance_graphs_ui(ns("school_attendance_graphs_1"))
      )
    ),

    fluidRow(bs4Card(
      title = "Adhoc Table Information",
      id = "card_adhocinfo",
      width = 12,
      fluidRow(
        column(
          4,
          mod_general_select_ui(ns("program2"), "Program", ad_hoc, "revised_program")
        ),
        column(
          8,
          "This card contains information about the adhoc table and the boxes above
          the adhoc table. The program filter on the left helps filter down the
          adhoc table and the info boxes."
        )
      )
    )),

    fluidRow(
      bs4Card(
        title = "Adhoc Table",
        id = "card_adhoc",
        width = 12,
        maximizable = TRUE,
        mod_ad_hoc_boxes_ui(ns("ad_hoc_boxes_1")),
        mod_ad_hoc_table_ui(ns("ad_hoc_table_1"))
      )
    )

  )
}

#' freedom_school_tab Server Functions
#'
#' @noRd
mod_freedom_school_tab_server <- function(id, attendance){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    program1 <- mod_general_select_server("program1")
    date1 <- mod_date_select_server("date1")
    school1 <- mod_general_select_server("school1")


    output$school_ui1 <- renderUI({
      mod_general_select_ui(ns("school1"), "School", attendance, "bus_name")
    })


    output$program_ui1 <- renderUI({
      mod_general_select_ui(ns("program1"), "Program", attendance, "revised_program")
    })

    choice <- reactive({
      input$attendance_graph_choice
    })
    mod_school_attendance_graphs_server("school_attendance_graphs_1", attendance, school1, program1, date1, choice)
    program2 <- mod_general_select_server("program2")

    fp_data <- reactive({
      ad_hoc %>%
        filter(bus_name %in% school1()) %>%
        filter(revised_program %in% program2()) %>%
        filter(`Attend stat` %in% c("Currently Attending",
                                    "Leave of Absence")) %>%
        mutate(`Days Until Grad` = `Rev grad` - Sys.Date(),
               `Attendance %` = round(`Tot hrs`/ `Sched hrs`, 2),
               Balance = as.numeric(sub("\\(", "-", gsub("\\)|\\$|\\,","", Balance))),
               PAvg = PAvg/100,
               WAvg = WAvg/100,
               School = bus_name,
               Program = revised_program) %>%
        select(Acct,
               School,
               Program,
               `Tot hrs`,
               `Attendance %`,
               Balance,
               `Days Until Grad`,
               `Days Absent`,
               PAvg,
               WAvg
        ) %>%
        arrange(`Attendance %`)
    })

    mod_ad_hoc_boxes_server("ad_hoc_boxes_1", fp_data)
    mod_ad_hoc_table_server("ad_hoc_table_1", fp_data)

  })
}

## To be copied in the UI
# mod_freedom_school_tab_ui("freedom_school_tab_1")

## To be copied in the server
# mod_freedom_school_tab_server("freedom_school_tab_1")
