#' takehome_overtime_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_takehome_overtime_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("monthly_take_home"))

  )
}

#' takehome_overtime_graph Server Functions
#'
#' @noRd
mod_takehome_overtime_graph_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    perc_change_col <- reactive({
      if ((sum(data()$`Take Home`)-sum(data()$prev_year_take_home))/sum(data()$prev_year_take_home) < 0) {
        "firebrick"
      } else {
        "forestgreen"
      }
    })

    cur_takehome <- reactive({format(round(sum(data()$`Take Home`)), big.mark = ",")})
    prev_takehome <- reactive({format(round(sum(data()$prev_year_take_home)),big.mark = ",")})
    perc_change <- reactive({percent(round((sum(data()$`Take Home`)-sum(data()$prev_year_take_home))/
                                             sum(data()$prev_year_take_home),2))})


    p1 <- reactive({
      ggplot(data = data(), aes_string(x = "Date")) +
        geom_col_interactive(aes(y = prev_year_take_home, tooltip = take_home_tooltip), fill = "#0090e1", alpha = .5) +
        #geom_col(aes(y = Service), width = .8) +
        geom_col_interactive(aes(y = `Take Home`, tooltip = take_home_tooltip), width = 18, fill = "#005481") +
        labs(y = "Take Home") +
        theme(legend.position = "top",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.subtitle = element_markdown(size = 16, hjust = 1)) +
        labs(
          x = NULL,
          y = NULL,
          subtitle = glue::glue(
            '<span style = "color:#005481">**Take Home:**</span> $',
            '{cur_takehome()}',
            '<br>',
            '<span style = "color:#0090e1">**Previous Year Take Home:**</span> $',
            '{prev_takehome()}',
            '<br>',
            '<span style = "color: {perc_change_col()}">**Percentage Change:**</span> ',
            '{perc_change()}'
          )
        ) +
        scale_y_continuous(labels=scales::dollar_format()) +
        scale_colour_identity()

    })

    output$monthly_take_home<- renderGirafe({
      girafe(ggobj = p1())
    })

  })
}

## To be copied in the UI
# mod_takehome_overtime_graph_ui("takehome_overtime_graph_1")

## To be copied in the server
# mod_takehome_overtime_graph_server("takehome_overtime_graph_1")
