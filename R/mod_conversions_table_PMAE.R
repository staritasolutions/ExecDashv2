#' conversions_table_PMAE UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_conversions_table_PMAE_ui <- function(id){
  ns <- NS(id)
  tagList(
    gt_output(ns("conversions_table"))

  )
}

#' conversions_table_PMAE Server Functions
#'
#' @noRd
mod_conversions_table_PMAE_server <- function(id, data, school, lead_type, program, date){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    L2P <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Lead", date),
                           filter_data(crm, school, lead_type, program, "Prospect", date),
                           "L2P")
    P2T <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Prospect", date),
                           filter_data(crm, school, lead_type, program, "Tour", date),
                           "P2T")
    T2A <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Tour", date),
                           filter_data(crm, school, lead_type, program, "Application", date),
                           "T2A")
    A2E <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Application", date),
                           filter_data(crm, school, lead_type, program, "Enrolled", date),
                           "A2E")
    E2Act <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Enrolled", date),
                             filter_data(crm, school, lead_type, program, "Active", date),
                             "E2Act")
    L2Act <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Lead", date),
                             filter_data(crm, school, lead_type, program, "Active", date),
                             "L2Act")
    lead_data <- filter_data(crm, school, lead_type, program, "Lead", date)
    lead_total <- reactive ({
      lead_data() %>%
        group_by(lead_type) %>%
        summarize(lead_total = n())
    })

    df_list <- reactive({
      list(L2P(), P2T(), T2A(), A2E(), E2Act(), L2Act(), lead_total())
    })

    grouped_by_lead_type <- reactive ({
      df_list() %>% reduce(full_join, by='lead_type') %>%
        replace(is.na(.), 0) %>%
        mutate(`% of Total` = lead_total / sum(lead_total)) %>%
        select(lead_type, L2P, P2T, T2A, A2E, E2Act, L2Act, lead_total, `% of Total`)
    })

    # conversions totals

    L2P_t <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Lead", date),
                           filter_data(crm, school, lead_type, program, "Prospect", date),
                           "L2P", TRUE)
    P2T_t <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Prospect", date),
                           filter_data(crm, school, lead_type, program, "Tour", date),
                           "P2T", TRUE)
    T2A_t <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Tour", date),
                           filter_data(crm, school, lead_type, program, "Application", date),
                           "T2A", TRUE)
    A2E_t <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Application", date),
                           filter_data(crm, school, lead_type, program, "Enrolled", date),
                           "A2E", TRUE)
    E2Act_t <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Enrolled", date),
                             filter_data(crm, school, lead_type, program, "Active", date),
                             "E2Act", TRUE)
    L2Act_t <- PMAE_conversion(filter_data(crm, school, lead_type, program, "Lead", date),
                             filter_data(crm, school, lead_type, program, "Active", date),
                             "L2Act", TRUE)
    lead_total_t <- reactive({
      lead_data() %>%
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
      grouped_by_lead_type() %>% rbind(totals())
    })

    output$conversions_table <- render_gt(
      conversions_table()
    )


  })
}

## To be copied in the UI
# mod_conversions_table_PMAE_ui("conversions_table_PMAE_1")

## To be copied in the server
# mod_conversions_table_PMAE_server("conversions_table_PMAE_1")
