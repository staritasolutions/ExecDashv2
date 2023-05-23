#' roi_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_roi_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' roi_graph Server Functions
#'
#' @noRd 
mod_roi_graph_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_roi_graph_ui("roi_graph_1")
    
## To be copied in the server
# mod_roi_graph_server("roi_graph_1")
