#' start_date_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_start_date_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("start_date_table"))

  )
}

#' start_date_table Server Functions
#'
#' @noRd
mod_start_date_table_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
      table_df <- reactive({
        data() %>%
          group_by(`School Name`, `Start Date Name`, `Start Date`) %>%
          summarize(Lead = sum(ifelse(`Workflow Status` == "Lead", 1, 0)),
                    Prospect = sum(ifelse(`Workflow Status` == "Prospect", 1, 0)),
                    Tour = sum(ifelse(`Workflow Status` == "Tour", 1, 0)),
                    Application = sum(ifelse(`Workflow Status` == "Application", 1, 0)),
                    Enrolled = sum(ifelse(`Workflow Status` == "Contracted", 1, 0)),
                    Active = sum(ifelse(`Workflow Status` == "Enrolled", 1, 0))) %>%
          arrange(`Start Date`)
      })

      output$start_date_table <- DT::renderDT({

        datatable(
          table_df(),
          rownames = FALSE,
          filter = "none",
          options = list(
            pageLength = 15,
            dom = "ltp",
            columnDefs = list(list(
              className = 'dt-center',
              targets = 0:8
            ))
          ))

      })

  })
}

## To be copied in the UI
# mod_start_date_table_ui("start_date_table_1")

## To be copied in the server
# mod_start_date_table_server("start_date_table_1")
