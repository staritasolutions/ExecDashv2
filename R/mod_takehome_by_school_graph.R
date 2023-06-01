#' takehome_by_school_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_takehome_by_school_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("plot_take_home"))

  )
}

#' takehome_by_school_graph Server Functions
#'
#' @noRd
mod_takehome_by_school_graph_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    p1 <- reactive({
      ggplot(data = data(), aes_string(x = "School",
                                           y = "`Take Home`")) +
        geom_col_interactive(aes(tooltip = takehome_tooltip),
                             fill = "#0072b2") +
        labs(y = "Take Home Sales ") +
        theme(legend.position = "top",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(x = NULL,
             y = NULL,) +
        scale_y_continuous(labels=scales::dollar_format()) +
        scale_colour_identity()

    })

    output$plot_take_home<- renderGirafe({
      girafe(ggobj = p1())
    })

  })
}

## To be copied in the UI
# mod_takehome_by_school_graph_ui("takehome_by_school_graph_1")

## To be copied in the server
# mod_takehome_by_school_graph_server("takehome_by_school_graph_1")
