#' service_sales_overtime_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_service_sales_overtime_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("monthly_services"))

  )
}

#' service_sales_overtime_graph Server Functions
#'
#' @noRd
mod_service_sales_overtime_graph_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    p1 <- reactive({
      ggplot(data = data(), aes_string(x = "Date")) +
        geom_col_interactive(aes(y = prev_year_service, tooltip = service_tooltip), fill = "#0090e1", alpha = .5) +
        #geom_col(aes(y = Service), width = .8) +
        geom_col_interactive(aes(y = Service, tooltip = service_tooltip), width = 18, fill = "#005481") +
        labs(y = "Service Sales ") +
        theme(legend.position = "top",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(x = NULL,
             y = NULL,
             subtitle = paste0("Sales: $", format(round(sum(data()$Service)), big.mark = ","),
                               "\n","Previous Year Sales: $", format(round(sum(data()$prev_year_service)),big.mark = ","),
                               "\n", "Percentage Change: ", percent(round((sum(data()$Service)-sum(data()$prev_year_service))/sum(data()$prev_year_service),2)))) +
        scale_y_continuous(labels=scales::dollar_format()) +
        scale_colour_identity()

    })

    output$monthly_services<- renderGirafe({
      girafe(ggobj = p1())
    })

  })
}

## To be copied in the UI
# mod_service_sales_overtime_graph_ui("service_sales_overtime_graph_1")

## To be copied in the server
# mod_service_sales_overtime_graph_server("service_sales_overtime_graph_1")
