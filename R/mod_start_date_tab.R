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
    mod_date_select_ui(ns("date1"), start = Sys.Date(), end = Sys.Date() + 183),
    mod_general_select_ui(ns("school1"), "Schools", crm, "School Name"),
    mod_start_date_table_ui(ns("start_date_table_1"))
  )
}

#' start_date_tab Server Functions
#'
#' @noRd
mod_start_date_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    date1 <- mod_date_select_server("date1")
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
