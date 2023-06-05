#' monthly_leads_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import lubridate
#'
mod_monthly_leads_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("plot_leads"))
  )
}

#' monthly_leads_graph Server Functions
#'
#' @noRd
mod_monthly_leads_graph_server <- function(id, data, maximized = FALSE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    plot_data <- reactive({
      data() %>% mutate(Lead = floor_date(Lead, unit = "month")) %>%
        group_by(Lead, lead_type) %>%
        summarize(leads = n()) %>%
        rename(Date = Lead) %>%
        ungroup() %>%
        group_by(Date) %>%
        mutate(Total = sum(leads)) %>%
        mutate(tooltip = paste0(month(Date, label = TRUE), " ", year(Date), "\n",
                                lead_type, ": ", format(leads, big.mark = ","), "\n",
                                "Total: ", format(Total, big.mark = ",")))
    })

    # Plot adjustments
    if (maximized) {
      width <- 12
      legend_pos <- "right"
    } else {
      width <- 8
      legend_pos <- "none"
    }

    p1 <- reactive({
      ggplot(data = plot_data(), aes_string(x = "Date",
                                              y = "leads")) +
        geom_col_interactive(aes(fill = lead_type,
                                 tooltip = tooltip),
                             width = 15) +
        labs(y = "Leads") +
        guides(fill = guide_legend(title = "Lead Type")) +
        theme(legend.position = legend_pos,
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(x = NULL)
    })

    output$plot_leads <- renderGirafe({
      girafe(ggobj = p1(),
             width = width) %>%
        girafe_options(opts_tooltip(zindex = 9999))
    })
  })
}

## To be copied in the UI
# mod_monthly_leads_graph_ui("monthly_leads_graph_1")

## To be copied in the server
# mod_monthly_leads_graph_server("monthly_leads_graph_1")
