#' crm_metric_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_crm_metric_select_ui <- function(id){
  ns <- NS(id)
  tagList(
    pickerInput(ns("Metric"),
                choices = c("Lead",
                            "Prospect",
                            "Tour",
                            "Application",
                            "Enrolled",
                            "Active"),
                multiple = FALSE,
                selected = "Lead",
                options = pickerOptions(actionsBox = TRUE,
                                        liveSearch = TRUE,
                                        selectedTextFormat = "static",
                                        title = "Metric"))
  )
}

#' crm_metric_select Server Functions
#'
#' @noRd
mod_crm_metric_select_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    return(
      reactive({input$Metric})
    )
  })
}

## To be copied in the UI
# mod_crm_metric_select_ui("crm_metric_select_1")

## To be copied in the server
# mod_crm_metric_select_server("crm_metric_select_1")
