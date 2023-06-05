#' school_comp_table_PMAE UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_school_comp_table_PMAE_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("table_schoolcomp"))
  )
}

#' school_comp_table_PMAE Server Functions
#'
#' @noRd
mod_school_comp_table_PMAE_server <- function(id, data, school, lead_type, program, date){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    lead_df <- filter_data(data, school, lead_type, program, "Lead", date)
    prospect_df <- filter_data(data, school, lead_type, program, "Prospect", date)
    tour_df <- filter_data(data, school, lead_type, program, "Tour", date)
    application_df <- filter_data(data, school, lead_type, program, "Application", date)
    enrolled_df <- filter_data(data, school, lead_type, program, "Enrolled", date)
    active_df <- filter_data(data, school, lead_type, program, "Active", date)

    L2P <- PMAE_school_conversion(lead_df,
                                  prospect_df,
                           "L2P")
    P2T <- PMAE_school_conversion(prospect_df,
                                  tour_df,
                           "P2T")
    T2A <- PMAE_school_conversion(tour_df,
                                  application_df,
                           "T2A")
    A2E <- PMAE_school_conversion(application_df,
                                  enrolled_df,
                           "A2E")
    E2Act <- PMAE_school_conversion(enrolled_df,
                                    active_df,
                             "E2Act")
    L2Act <- PMAE_school_conversion(lead_df,
                                    active_df,
                             "L2Act")
    lead_total <- reactive({
      lead_df() %>% group_by(`School Name`) %>%
        summarize(`Lead Total` = n())
    })

    df_list <- reactive({
      list(L2P(), P2T(), T2A(), A2E(), E2Act(), L2Act(), lead_total())
    })

    final_df <- reactive ({
      df_list() %>% reduce(full_join, by="School Name") %>%
        select(`School Name`, L2P, P2T, T2A, A2E, E2Act, L2Act, `Lead Total`)
    })


    output$table_schoolcomp <- DT::renderDT({
      datatable(final_df(),
                rownames = FALSE,
                options = list(
                  pageLength = 15,
                  columnDefs = list(list(
                    className = 'dt-center',
                    targets = 1:4
                  ))
                ))  %>%
        formatPercentage(c(2,3,4,5,6,7))
    })

  })
}

## To be copied in the UI
# mod_school_comp_table_PMAE_ui("school_comp_table_PMAE_1")

## To be copied in the server
# mod_school_comp_table_PMAE_server("school_comp_table_PMAE_1")
