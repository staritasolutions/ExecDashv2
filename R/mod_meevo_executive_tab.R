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
    mod_date_select_ui(ns("date1")),
    mod_service_sales_by_school_graph_ui(ns("service_sales_by_school_graph_1")),
    mod_takehome_by_school_graph_ui(ns("takehome_by_school_graph_1")),
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
                                        liveSearch = TRUE)),
    mod_general_select_ui(ns("school"), "Schools", meevo, "School", 3),
    mod_date_select_ui(ns("date2")),
    mod_metrics_over_time_graph_ui(ns("metrics_over_time_graph_1")),
    mod_meevo_metrics_table_ui(ns("meevo_metrics_table_1"))

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
    date2 <- mod_date_select_server("date2")
    mod_metrics_over_time_graph_server("metrics_over_time_graph_1", meevo, school, date2, reactive({input$metric}))
    mod_meevo_metrics_table_server("meevo_metrics_table_1", meevo, school, date2, reactive({input$metric}))



  })
}

## To be copied in the UI
# mod_meevo_executive_tab_ui("meevo_executive_tab_1")

## To be copied in the server
# mod_meevo_executive_tab_server("meevo_executive_tab_1")
