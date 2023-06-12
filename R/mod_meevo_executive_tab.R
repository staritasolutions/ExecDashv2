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
        title = "Meevo - Executive View",
        id = "card_execview",
        width = 12,
        fluidRow(
          column(
            4,
            mod_date_select_ui(ns("date1"))
          ),
          column(
            8,
            "Welcome to the Meevo Executive View. Here you will find information
            about all your schools in one place. The Date Range filter in this box
            will adjust all the graphs and tables in this tab."
          )
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = "Service Sales by School",
        id = "card_servsales",
        width = 6,
        maximizable = FALSE,
        mod_service_sales_by_school_graph_ui(ns("service_sales_by_school_graph_1"))
      ),
      bs4Card(
        title = "Take Home Sales by School",
        id = "card_takehomesales",
        width = 6,
        maximizable = FALSE,
        mod_takehome_by_school_graph_ui(ns("takehome_by_school_graph_1"))
      )
    ),

    fluidRow(
      bs4Card(
        title = "Metrics Over Time Information",
        id = "card_metricsinfo",
        width = 12,
        maximizable = FALSE,
        fluidRow(
          "These are additional controls for the Metrics Over Time Graph and the
            Date Range Totals Table. Use these filters in addition to the Date Range
            filter in the card at the top of the page to adjust the graph and table
            below.",
          column(
            6,
            mod_general_select_ui(ns("school"), "Schools", meevo, "School", 3)
          ),
          column(
            6,
            pickerInput(ns("metric"),
                        label = "Metric",
                        choices = c("Guests/FP",
                                    "Avg Total Ticket",
                                    "Take Home $/Guest",
                                    "Bottles/Guest",
                                    "Service $/Guest"),
                        multiple = FALSE,
                        selected = "Guests/FP",
                        options = pickerOptions(actionsBox = TRUE,
                                                liveSearch = TRUE))
          )
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = "Metrics Over Time Graph",
        id = "card_metricsovertime",
        width = 9,
        maximizable = FALSE,
        mod_metrics_over_time_graph_ui(ns("metrics_over_time_graph_1"))
      ),
      bs4Card(
        title = "Date Range Summary",
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
    school <- mod_general_select_server("school")
    mod_metrics_over_time_graph_server("metrics_over_time_graph_1", meevo, school, date1, reactive({input$metric}))
    mod_meevo_metrics_table_server("meevo_metrics_table_1", meevo, school, date1, reactive({input$metric}))



  })
}

## To be copied in the UI
# mod_meevo_executive_tab_ui("meevo_executive_tab_1")

## To be copied in the server
# mod_meevo_executive_tab_server("meevo_executive_tab_1")
