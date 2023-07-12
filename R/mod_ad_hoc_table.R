#' ad_hoc_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ad_hoc_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    DTOutput(ns("fp_table"))

  )
}

#' ad_hoc_table Server Functions
#'
#' @noRd
mod_ad_hoc_table_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$fp_table <- DT::renderDT({

      datatable(
        data(),
        rownames = FALSE,
        filter = "none",
        options = list(
          pageLength = 15,
          dom = "tlp",
          columnDefs = list(list(
            className = 'dt-center',
            targets = 0:8
          ))
        )) %>%
        formatStyle(
          'Attendance %',
          color = styleInterval(c('.89'), c('red','grey85'))) %>%
        formatStyle(
          'Balance',
          color = styleInterval(c(.001), c('grey85', 'red'))) %>%
        formatStyle(
          'Days Until Grad',
          color = styleInterval(c(90), c('red', 'grey85'))
        ) %>%
        formatStyle(
          'PAvg',
          color = styleInterval(c('.7'), c('red','grey85'))) %>%
        formatStyle(
          'WAvg',
          color = styleInterval(c('.7'), c('red','grey85'))) %>%
        formatStyle(
          'Days Absent',
          color = styleInterval(c(6), c('grey85', 'red'))) %>%
        formatPercentage(c(5,9,10)) %>%
        formatCurrency(c(6))

    })


  })
}

## To be copied in the UI
# mod_ad_hoc_table_ui("ad_hoc_table_1")

## To be copied in the server
# mod_ad_hoc_table_server("ad_hoc_table_1")
