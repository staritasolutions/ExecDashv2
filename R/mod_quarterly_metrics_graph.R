#' quarterly_metrics_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom lubridate quarter year
mod_quarterly_metrics_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("plot_quarterly"))
  )
}

#' quarterly_metrics_graph Server Functions
#'
#' @noRd
mod_quarterly_metrics_graph_server <- function(id, data, metric){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    quarterly_df <- reactive({
      data() %>%
        mutate(Quarter = quarter(.data[[metric()]]),
               Year = year(.data[[metric()]])
        ) %>%
        group_by(Quarter, Year) %>%
        summarize(Metric = n()) %>%
        ungroup() %>%
        mutate(tooltip =
            paste0(
              "Q",
              Quarter,
              " ",
              Year,
              "\n",
              metric(),": ",
              format(Metric, big.mark = ","))
        )
    })

    p1_3 <- reactive({
      ggplot(
        data = quarterly_df(),
        aes(
          x = Quarter,
          y = Metric,
          group = as.factor(Year),
          fill = as.factor(Year),
          tooltip = tooltip
        )
      ) +
        geom_col_interactive(position = "dodge", width = 0.7) +
        labs(x = NULL,
             y = str_to_title(metric())) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top",
              legend.title = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank())
    })

    output$plot_quarterly <- renderGirafe({
      girafe(ggobj = p1_3(),
             height = 8,
             width = 8) %>%
        girafe_options(opts_sizing(rescale = FALSE))
    })
  })
}

## To be copied in the UI
# mod_quarterly_metrics_graph_ui("quarterly_metrics_graph_1")

## To be copied in the server
# mod_quarterly_metrics_graph_server("quarterly_metrics_graph_1")
