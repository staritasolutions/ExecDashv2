#' meevo_executive_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_meevo_executive_tab_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Card(
        title = strong("Meevo - Executive View", style = "font-size:25px;"),
        id = "card_execview",
        width = 12,
        fluidRow(
          em(paste0("Data last updated: ", Sys.Date()), style = "margin-bottom: 10px;")
        ),
        fluidRow(
          column(
            4,
            mod_date_select_ui(ns("date1"))
          )
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = strong("Service Sales by School"),
        id = "card_servsales",
        width = 6,
        maximizable = FALSE,
        mod_service_sales_by_school_graph_ui(ns("service_sales_by_school_graph_1"))
      ),
      bs4Card(
        title = strong("Take Home Sales by School"),
        id = "card_takehomesales",
        width = 6,
        maximizable = FALSE,
        mod_takehome_by_school_graph_ui(ns("takehome_by_school_graph_1"))
      )
    ),

    fluidRow(
      bs4Card(
        title = strong("Metrics Over Time Information"),
        id = "card_metricsinfo",
        width = 12,
        maximizable = FALSE,
        fluidRow(
          column(
            6,
            uiOutput(ns("school_ui"))
          ),
          column(
            6,
            pickerInput(ns("metric"),
                        choices = c("Guests/FP",
                                    "Avg Total Ticket",
                                    "Take Home $/Guest",
                                    "Bottles/Guest",
                                    "Service $/Guest"),
                        multiple = FALSE,
                        selected = "Guests/FP",
                        options = pickerOptions(actionsBox = TRUE,
                                                liveSearch = TRUE,
                                                selectedTextFormat = "static",
                                                title = "Metric"))
          )
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = strong("Metrics Over Time Graph"),
        id = "card_metricsovertime",
        width = 9,
        maximizable = FALSE,
        mod_metrics_over_time_graph_ui(ns("metrics_over_time_graph_1"))
      ),
      bs4Card(
        title = strong("Date Range Average"),
        id = "card_summarytable",
        width = 3,
        maximizable = FALSE,
        mod_meevo_metrics_table_ui(ns("meevo_metrics_table_1"))
      )
    )
  )
}

#' meevo_executive_tab Server Functions
#'
#' @noRd
mod_meevo_executive_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    date1 <- mod_date_select_server("date1")
    filtered_meevo <- reactive ({
      meevo %>% filter(Date >= date1$start() &
                         Date <= date1$end())  %>%
        group_by(School) %>%
        summarize(Service = sum(`Total Service (Including Add On)`),
                  `Take Home` = sum(`Total Retail (Incl. Ageless Serums)`)) %>%
        mutate(service_tooltip = paste0("$", format(round(Service, 2), big.mark = ",")),
               takehome_tooltip = paste0("$", format(round(`Take Home`, 2), big.mark = ",")),
               School = School)
    })
    mod_service_sales_by_school_graph_server("service_sales_by_school_graph_1", filtered_meevo)
    mod_takehome_by_school_graph_server("takehome_by_school_graph_1", filtered_meevo)
    output$school_ui <- renderUI({
      mod_general_select_ui(ns("school"), "Schools", meevo, "School", 3)
    })
    school <- mod_general_select_server("school")
    mod_metrics_over_time_graph_server("metrics_over_time_graph_1", meevo, school, date1, reactive({input$metric}))
    mod_meevo_metrics_table_server("meevo_metrics_table_1", meevo, school, date1, reactive({input$metric}))



  })
}

## To be copied in the UI
# mod_meevo_executive_tab_ui("meevo_executive_tab_1")

## To be copied in the server
# mod_meevo_executive_tab_server("meevo_executive_tab_1")
