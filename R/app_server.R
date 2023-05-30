#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
app_server <- function(input, output, session) {
  # Your application server logic
  mod_leads_overview_tab_server("leads_overview")
  mod_school_comp_tab_server("school_comp_tab_1")

  observe(print(input$sidebarmenu))
  observe(print(input$card_monthlyleads$maximized))

  # observeEvent(input$card_monthlyleads$maximized, {
  #   # updatebs4Card(id = "card_monthlyleads",
  #   #               action = "toggleMaximize",
  #   #               )
  #
  #   print("box is maximized")
  # })

  observeEvent(input$card_monthlyleads$maximized, {
    if(input$card_monthlyleads$maximized) {
      updatebs4Card(id = "card_monthlyleads",
                    action = "update",
                    session = shiny::getDefaultReactiveDomain(),
                    fluidRow(p("Hello this is a new UI")))
    }

  })


}
