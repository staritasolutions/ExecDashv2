#' service_sales_by_school_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_service_sales_by_school_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("plot_service"))

  )
}

#' service_sales_by_school_graph Server Functions
#'
#' @noRd
mod_service_sales_by_school_graph_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    p1 <- reactive({
      ggplot(data = data(), aes_string(x = "School",
                                           y = "Service")) +
        geom_col_interactive(aes(tooltip = service_tooltip),
                             fill = "#009e73") +
        labs(y = "Service Sales ") +
        theme(legend.position = "top",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(x = NULL,
             y = NULL,) +
        scale_y_continuous(labels=scales::dollar_format()) +
        scale_colour_identity()

    })

    output$plot_service<- renderGirafe({
      girafe(ggobj = p1())
    })


  })
}

## To be copied in the UI
# mod_service_sales_by_school_graph_ui("service_sales_by_school_graph_1")

## To be copied in the server
# mod_service_sales_by_school_graph_server("service_sales_by_school_graph_1")
