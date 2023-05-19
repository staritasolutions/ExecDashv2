#' yearl_metrics_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_yearly_metrics_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("plot_yearly"))
  )
}

#' yearl_metrics_graph Server Functions
#'
#' @noRd
mod_yearly_metrics_graph_server <- function(id, data, metric){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    yearly_df <- reactive({
      data() %>%
        mutate(Date = floor_date(.data[[metric()]], "month")) %>%
        group_by(Date) %>%
        summarize(Metric = n()) %>%
        ungroup() %>%
        mutate(tooltip =
                 paste0(
                   month(Date, label = TRUE),
                   " ",
                   year(Date),
                   "\n",
                   metric(),": ",
                   format(Metric, big.mark = ","))
        )
    })

    p1_3 <- reactive({
      ggplot(
        data = yearly_df(),
        aes(
          x = month(Date, label = TRUE),
          y = Metric,
          group = as.factor(year(Date)),
          fill = as.factor(year(Date)),
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

    output$plot_yearly <- renderGirafe({
      girafe(ggobj = p1_3(),
             height = 8,
             width = 8) %>%
        girafe_options(opts_sizing(rescale = FALSE))
    })
  })
}

## To be copied in the UI
# mod_yearly_metrics_graph_ui("yearly_metrics_graph_1")

## To be copied in the server
# mod_yearly_metrics_graph_server("yearly_metrics_graph_1")
