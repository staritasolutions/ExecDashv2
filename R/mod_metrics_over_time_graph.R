#' metrics_over_time_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metrics_over_time_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("plot_meevo_metrics"))


  )
}

#' metrics_over_time_graph Server Functions
#'
#' @noRd
mod_metrics_over_time_graph_server <- function(id, data, school, date, metric){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    meevo_metrics <- reactive ({
      data %>%
        filter(Date >= date$start() & Date <= date$end()) %>%
        filter(School %in% school()) %>%
        mutate(Date = floor_date(Date, "week")) %>%
        group_by(School, Date) %>%
        summarize(`Guests/FP` = round(sum(`# Unique Clients Buying Servs.`)/sum(`Emps with Serv. Sales`),2),
                  `Avg Total Ticket` = round(sum(`Total Sales`)/sum(`# Unique Clients Buying Servs.`),2),
                  `Take Home $/Guest` = round(sum(`Total Retail (Incl. Ageless Serums)`)/sum(`# Unique Clients Buying Servs.`),2),
                  `Bottles/Guest` = round(sum(`Retail Items Sold`)/sum(`# Unique Clients Buying Servs.`),2),
                  `Service $/Guest` = round(sum(`Total Service (Including Add On)`)/sum(`# Unique Clients Buying Servs.`),2)) %>%
        mutate(tooltip = case_when(metric() == "Guests/FP" ~
                                     paste0("Date: ", Date, "\n", "Guests/FP: ", `Guests/FP`),
                                   metric() == "Avg Total Ticket" ~
                                     paste0("Date: ", Date, "\n", "Avg Total Ticket: $",`Avg Total Ticket`),
                                   metric() == "Take Home $/Guest" ~
                                     paste0("Date: ", Date, "\n", "Take Home $/Guest: $", `Take Home $/Guest`),
                                   metric() == "Bottles/Guest" ~
                                     paste0("Date: ", Date, "\n", "Bottles/Guest: ", `Bottles/Guest`),
                                   metric() == "Service $/Guest" ~
                                     paste0("Date: ", Date, "\n", "Service $/Guest: $", `Service $/Guest`),
                                   TRUE ~ "Error")) %>%
        ungroup()
    })

    p1 <- reactive ({
      ggplot(data = meevo_metrics(),
             aes(x = Date, y = .data[[metric()]], color = School)) +
        geom_line(size = 2) +
        geom_point_interactive(aes(tooltip = tooltip, color = School),
                               size = 4) +
        labs(x = "Date") +
        scale_y_continuous(limits=(c(0,NA))) +
        theme(legend.position = "top")
    })

    output$plot_meevo_metrics<- renderGirafe({
      girafe(ggobj = p1(),
             width = 8)
    })

  })
}

## To be copied in the UI
# mod_metrics_over_time_graph_ui("metrics_over_time_graph_1")

## To be copied in the server
# mod_metrics_over_time_graph_server("metrics_over_time_graph_1")
