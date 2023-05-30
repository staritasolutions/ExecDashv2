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
 
  )
}
    
#' meevo_executive_tab Server Functions
#'
#' @noRd 
mod_meevo_executive_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_meevo_executive_tab_ui("meevo_executive_tab_1")
    
## To be copied in the server
# mod_meevo_executive_tab_server("meevo_executive_tab_1")
