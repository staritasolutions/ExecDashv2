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
    girafeOutput(ns("plot_roi"))

  )
}

#' roi_graph Server Functions
#'
#' @noRd
mod_roi_graph_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    p3 <- reactive({
      ggplot(data(), aes(
        x = Date,
        y = `Cost / Lead`
      )) +
        geom_line(linewidth = 2, alpha = 0.5) +
        geom_point_interactive(aes(tooltip = tooltip), size = 5) +
        labs(x = NULL)
    })

    output$plot_roi <- renderGirafe({
      girafe(ggobj = p3(),
             height = 5,
             width = 10)
    })

  })
}

## To be copied in the UI
# mod_roi_graph_ui("roi_graph_1")

## To be copied in the server
# mod_roi_graph_server("roi_graph_1")
