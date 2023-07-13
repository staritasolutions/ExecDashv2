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
      title = strong("CRM - ROI Lead Calculator", style = "font-size:25px;"),
      maximizable = TRUE,
      width = 12,
      fluidPage(
        # titlePanel(
        #   "ROI Lead Calculator"
        # ),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            mod_date_select_ui(ns("date1")),
            uiOutput(ns("school1_ui")),
            shinyWidgets::currencyInput(ns("budget"),
                          "Input Your Monthly Budget",
                          value = 1000,
                          format = "NorthAmerican",
                          align = "left")
          ),
          mainPanel(
            width = 9,
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
mod_roi_tab_server <- function(id, crm){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    date1 <- mod_date_select_server("date1")
    output$school1_ui <- renderUI({
      mod_general_select_ui(ns("school1"), "Schools", crm, "School Name")
    })
    school1 <- mod_general_select_server("school1")
    roi_data <- reactive ({
      crm %>%
        filter(`School Name` %in% school1()) %>%
        mutate(Date = floor_date(Lead, "month")) %>%
        filter(Date >= date1$start() &
                 Date <= date1$end()) %>%
        group_by(Date) %>%
        summarize(`Cost / Lead` = as.numeric(input$budget)/n()) %>%
        ungroup() %>%
        mutate(tooltip =
                 paste0(month(Date, label = TRUE), " ", year(Date), "\n",
                        "$", round(`Cost / Lead`, 2)))
    })

    mod_roi_graph_server("roi_graph_1", roi_data)

  })
}

## To be copied in the UI
# mod_roi_tab_ui("roi_tab_1")

## To be copied in the server
# mod_roi_tab_server("roi_tab_1")
