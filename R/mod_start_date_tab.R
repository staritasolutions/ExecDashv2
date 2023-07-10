#' start_date_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_start_date_tab_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Card(
        title = "CRM - Start Dates",
        id = "card_startdateinfo",
        width = 12,
        "Welcome to the CRM Start Dates tab. Here you will find information on the
        start dates your schools have, filterable by date and school.",
        fluidRow(
          column(6,
                 mod_date_select_ui(ns("date1"), start = Sys.Date(), end = Sys.Date() + 183)
                 ),
          column(6,
                 uiOutput(ns("school1_ui"))
                 )
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = "Start Date Table",
        id = "card_startdatetable",
        width = 12,
        maximizable = TRUE,
        mod_start_date_table_ui(ns("start_date_table_1"))
      )
    )
  )
}

#' start_date_tab Server Functions
#'
#' @noRd
mod_start_date_tab_server <- function(id, crm){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    date1 <- mod_date_select_server("date1")
    output$school1_ui <- renderUI({
      mod_general_select_ui(ns("school1"), "Schools", crm, "School Name")
    })
    school1 <- mod_general_select_server("school1")
    start_df <- reactive({
      crm %>% filter(`School Name` %in% school1()) %>%
        filter(`Start Date` >= date1$start() &
                 `Start Date` <= date1$end())
    })
    mod_start_date_table_server("start_date_table_1", start_df)

  })
}

## To be copied in the UI
# mod_start_date_tab_ui("start_date_tab_1")

## To be copied in the server
# mod_start_date_tab_server("start_date_tab_1")
