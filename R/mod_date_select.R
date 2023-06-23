#' date_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_date_select_ui <- function(id, start = floor_date(Sys.Date(), unit = "year"), end = NULL){
  ns <- NS(id)
  tagList(
    dateRangeInput(ns("date"),
                   label = NULL,
                   start = start,
                   end = end,
                   format = "M d, yyyy")
  )
}

#' date_select Server Functions
#'
#' @noRd
mod_date_select_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    return(
      list(
        start  = reactive({input$date[1]}),
        end = reactive({input$date[2]})
      )
    )

  })
}

## To be copied in the UI
# mod_date_select_ui("date_select_1")

## To be copied in the server
# mod_date_select_server("date_select_1")
