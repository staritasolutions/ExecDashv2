#' meevo_school_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_meevo_school_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' meevo_school_tab Server Functions
#'
#' @noRd 
mod_meevo_school_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_meevo_school_tab_ui("meevo_school_tab_1")
    
## To be copied in the server
# mod_meevo_school_tab_server("meevo_school_tab_1")
