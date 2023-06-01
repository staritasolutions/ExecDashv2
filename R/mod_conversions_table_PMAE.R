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

    lead_df <- filter_data(data, school, lead_type, program, "Lead", date)
    prospect_df <- filter_data(data, school, lead_type, program, "Prospect", date)
    tour_df <- filter_data(data, school, lead_type, program, "Tour", date)
    application_df <- filter_data(data, school, lead_type, program, "Application", date)
    enrolled_df <- filter_data(data, school, lead_type, program, "Enrolled", date)
    active_df <- filter_data(data, school, lead_type, program, "Active", date)

    L2P <- PMAE_conversion(lead_df,
                           prospect_df,
                           "L2P")
    P2T <- PMAE_conversion(prospect_df,
                           tour_df,
                           "P2T")
    T2A <- PMAE_conversion(tour_df,
                           application_df,
                           "T2A")
    A2E <- PMAE_conversion(application_df,
                           enrolled_df,
                           "A2E")
    E2Act <- PMAE_conversion(enrolled_df,
                             active_df,
                             "E2Act")
    L2Act <- PMAE_conversion(lead_df,
                             active_df,
                             "L2Act")
    lead_total <- reactive ({
      lead_df() %>%
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

    L2P_t <- PMAE_conversion(lead_df,
                             prospect_df,
                           "L2P", TRUE)
    P2T_t <- PMAE_conversion(prospect_df,
                             tour_df,
                           "P2T", TRUE)
    T2A_t <- PMAE_conversion(tour_df,
                             application_df,
                           "T2A", TRUE)
    A2E_t <- PMAE_conversion(application_df,
                             enrolled_df,
                           "A2E", TRUE)
    E2Act_t <- PMAE_conversion(enrolled_df,
                               active_df,
                             "E2Act", TRUE)
    L2Act_t <- PMAE_conversion(lead_df,
                               active_df,
                             "L2Act", TRUE)
    lead_total_t <- reactive({
      lead_df() %>%
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
