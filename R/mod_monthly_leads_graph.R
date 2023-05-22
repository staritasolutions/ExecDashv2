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
mod_monthly_leads_graph_server <- function(id, data){
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
    p1 <- reactive({
      ggplot(data = plot_data(), aes_string(x = "Date",
                                              y = "leads")) +
        geom_col_interactive(aes(fill = lead_type,
                                 tooltip = tooltip),
                             width = 15) +
        labs(y = "Leads") +
        guides(fill = guide_legend(title = "Lead Type")) +
        theme(legend.position = "top",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(x = NULL)
    })

    output$plot_leads <- renderGirafe({
      girafe(ggobj = p1(),
             width = 8)
    })
  })
}

## To be copied in the UI
# mod_monthly_leads_graph_ui("monthly_leads_graph_1")

## To be copied in the server
# mod_monthly_leads_graph_server("monthly_leads_graph_1")
