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
    mod_general_select_ui(ns("school1"), "School", attendance, "bus_name"),
    mod_general_select_ui(ns("program1"), "Program", attendance, "revised_program"),
    mod_date_select_ui(ns("date1"), start = Sys.Date()-35),
    prettyRadioButtons(ns("attendance_graph_choice"),
                       "Select Graph",
                       choices = c("Attendance", "Attendance %"),
                       selected = "Attendance",
                       shape = "curve",
                       bigger = TRUE,
                       animation = "smooth"
    ),
    mod_school_attendance_graphs_ui(ns("school_attendance_graphs_1")),
    mod_general_select_ui(ns("program2"), "Program", ad_hoc, "revised_program"),
    mod_ad_hoc_boxes_ui(ns("ad_hoc_boxes_1")),
    mod_ad_hoc_table_ui(ns("ad_hoc_table_1"))


  )
}

#' freedom_school_tab Server Functions
#'
#' @noRd
mod_freedom_school_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    program1 <- mod_general_select_server("program1")
    date1 <- mod_date_select_server("date1")
    school1 <- mod_general_select_server("school1")
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
