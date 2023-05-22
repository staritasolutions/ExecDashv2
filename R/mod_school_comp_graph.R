#' school_comp_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import lubridate
#' @import ggiraph
mod_school_comp_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("plot_schoolcomp"))
  )
}

#' school_comp_graph Server Functions
#'
#' @noRd
mod_school_comp_graph_server <- function(id, data, metric){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    school_comp_df <- reactive({
      data() %>%
        mutate(Date = floor_date(.data[[metric()]], "month")) %>%
        group_by(Date, `School Name`) %>%
        summarize(
          Metric = n()
        ) %>%
        ungroup() %>%
        mutate(tooltip = paste0(month(Date, label = TRUE), " ", year(Date), "\n",
                                metric(), ": ",Metric))

    })

    p2 <- reactive ({
      school_comp_df() %>%
        ggplot(aes(
          x = Date,
          y = Metric,
          color = as.factor(`School Name`),
          group = as.factor(`School Name`),
          tooltip = tooltip
        )) +
        geom_line(linewidth = 3, linetype = "dashed", alpha = 0.5) +
        geom_point_interactive(size = 8) +
        labs(y = str_to_title(metric()),
             color = "School") +
        theme_minimal(base_size = 16) +
        theme(legend.position = "top")

    })

    output$plot_schoolcomp <- renderGirafe({
      girafe(ggobj = p2(),
             height = 8,
             width = 18) %>%
        girafe_options(opts_sizing(rescale = FALSE))
    })

  })
}

## To be copied in the UI
# mod_school_comp_graph_ui("school_comp_graph_1")

## To be copied in the server
# mod_school_comp_graph_server("school_comp_graph_1")
