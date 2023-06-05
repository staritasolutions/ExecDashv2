#' roi_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyWidgets
#'
mod_roi_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Card(
      maximizable = TRUE,
      width = 12,
      fluidPage(
        titlePanel(
          "ROI Lead Calculator"
        ),
        sidebarLayout(
          sidebarPanel(
            mod_date_select_ui(ns("date1")),
            mod_general_select_ui(ns("school1"), "Schools", crm, "School Name"),
            currencyInput("budget",
                          "Input Your Monthly Budget",
                          value = 1000,
                          format = "NorthAmerican",
                          align = "left")
          ),
          mainPanel(
            mod_roi_graph_ui(ns("roi_graph_1"))
          )
        )
      )
    )

  )
}

#' roi_tab Server Functions
#'
#' @noRd
mod_roi_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_roi_graph_server("roi_graph_1")

  })
}

## To be copied in the UI
# mod_roi_tab_ui("roi_tab_1")

## To be copied in the server
# mod_roi_tab_server("roi_tab_1")
