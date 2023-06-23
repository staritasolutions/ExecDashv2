#' school_comp_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import DT
mod_school_comp_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("table_schoolcomp"))
  )
}

#' school_comp_table Server Functions
#'
#' @noRd
mod_school_comp_table_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # conversions grouped by school
    L2P <- school_conversion(data, "L2P", "Lead", "Prospect", FALSE)
    P2T <- school_conversion(data, "P2T", "Prospect", "Tour", FALSE)
    T2A <- school_conversion(data, "T2A", "Tour", "Application", FALSE)
    A2E <- school_conversion(data, "A2E", "Application", "Enrolled", FALSE)
    E2Act <- school_conversion(data, "E2Act", "Enrolled", "Active", FALSE)
    L2Act <- school_conversion(data, "L2Act", "Lead", "Active", FALSE)
    lead_total <- reactive({
      data() %>% group_by(`School Name`) %>%
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
                  dom = "t",
                  columnDefs = list(list(
                    className = 'dt-center',
                    targets = 1:7
                  ))
                ))  %>%
        formatPercentage(c(2,3,4,5,6,7))
    })

  })
}

## To be copied in the UI
# mod_school_comp_table_ui("school_comp_table_1")

## To be copied in the server
# mod_school_comp_table_server("school_comp_table_1")
