#' conversions_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import gt
#' @import tidyverse
mod_conversions_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    gt_output(ns("conversions_table"))
  )
}

#' conversions_table Server Functions
#'
#' @noRd
mod_conversions_table_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # conversions grouped by lead_type
    L2P <- conversion(data, "L2P", "Lead", "Prospect", FALSE)
    P2T <- conversion(data, "P2T", "Prospect", "Tour", FALSE)
    T2A <- conversion(data, "T2A", "Tour", "Application", FALSE)
    A2E <- conversion(data, "A2E", "Application", "Enrolled", FALSE)
    E2Act <- conversion(data, "E2Act", "Enrolled", "Active", FALSE)
    L2Act <- conversion(data, "L2Act", "Lead", "Active", FALSE)

    lead_total <- reactive({
        data() %>% group_by(lead_type) %>%
          summarize(lead_total = n())
    })

    df_list <- reactive({
      list(L2P(), P2T(), T2A(), A2E(), E2Act(), L2Act(), lead_total())
    })

    grouped_by_lead_type <- reactive ({
      df_list() %>% reduce(full_join, by='lead_type') %>%
        mutate(`% of Total` = lead_total / sum(lead_total)) %>%
        select(lead_type, L2P, P2T, T2A, A2E, E2Act, L2Act, lead_total, `% of Total`)
    })

    # conversions totals

    L2P_t <- conversion(data, "L2P", "Lead", "Prospect", TRUE)
    P2T_t <- conversion(data, "P2T", "Prospect", "Tour", TRUE)
    T2A_t <- conversion(data, "T2A", "Tour", "Application", TRUE)
    A2E_t <- conversion(data, "A2E", "Application", "Enrolled", TRUE)
    E2Act_t <- conversion(data, "E2Act", "Enrolled", "Active", TRUE)
    L2Act_t <- conversion(data, "L2Act", "Lead", "Active", TRUE)
    lead_total_t <- reactive({
        data() %>%
          summarize(lead_type = "Total", lead_total = n())
      })

    df_list_t <- reactive({
      list(L2P_t(), P2T_t(), T2A_t(), A2E_t(), E2Act_t(), L2Act_t(), lead_total_t())
    })

    totals <- reactive ({
      df_list_t() %>% reduce(full_join, by='lead_type') %>%
        mutate(`% of Total` = 1) %>%
        select(lead_type, L2P, P2T, T2A, A2E, E2Act, L2Act, lead_total, `% of Total`)
    })

    conversions_table <- reactive ({
      grouped_by_lead_type() %>% rbind(totals()) %>%
        rename("Lead Type" = lead_type,
               "Lead Total" = lead_total)
    })

    output$conversions_table <- render_gt({
      expr = conversions_table() %>%
        gt() %>%
        fmt_percent(columns = c(L2P:L2Act, `% of Total`), decimals = 0) %>%
        data_color(columns = L2P:L2Act,
                   palette = c("red", "white", "blue")
                   #,domain = c(0,1)
                   ) %>%
        fmt_number(columns = "Lead Total", decimals = 0)

  })


  })
}



## To be copied in the UI
# mod_conversions_table_ui("conversions_table_1")

## To be copied in the server
# mod_conversions_table_server("conversions_table_1")
