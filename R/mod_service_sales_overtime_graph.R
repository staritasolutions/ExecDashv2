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

    perc_change_col <- reactive({
      if ((sum(data()$Service) - sum(data()$prev_year_service)) / sum(data()$prev_year_service) < 0) {
        "firebrick"
      } else {
        "forestgreen"
      }
    })





    cur_sales <- reactive({format(round(sum(data()$Service)), big.mark = ',')})
    prev_sales <- reactive({format(round(sum(data()$prev_year_service)), big.mark = ',')})

    custom_percent_format <- function(x) {
      formatted <- percent(x)
      if (x >= 0) {
        formatted <- paste0("+", formatted)
      }
      formatted
    }

    perc_change <- reactive({custom_percent_format(round((sum(data()$Service) - sum(data()$prev_year_service)) /
                                   sum(data()$prev_year_service), 2))})

    p1 <- reactive({
      ggplot(data = data(), aes_string(x = "Date")) +
        geom_col_interactive(aes(y = prev_year_service, tooltip = service_tooltip), fill = "#0090e1", alpha = .5) +
        #geom_col(aes(y = Service), width = .8) +
        geom_col_interactive(aes(y = Service, tooltip = service_tooltip), width = 18, fill = "#005481") +
        labs(y = "Service Sales ") +
        theme(legend.position = "top",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.subtitle = element_markdown(size = 16, hjust = 1)) +
        labs(x = NULL,
             y = NULL,
             subtitle = glue::glue(
               '<span style = "color:#005481">**Sales:**</span> $',
               '{cur_sales()}',
               '<br>',
               '<span style = "color:#0090e1">**Previous Year Sales:**</span> $',
               '{prev_sales()}',
               '<br>',
               '<span style = "color: {perc_change_col()}">**Percentage Change:**</span> ',
               '{perc_change()}'
               )) +
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
