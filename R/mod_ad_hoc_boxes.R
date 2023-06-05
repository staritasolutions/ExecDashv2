#' ad_hoc_boxes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ad_hoc_boxes_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4InfoBoxOutput(ns("low_grades")),
    bs4InfoBoxOutput(ns("low_attendance")),
    bs4InfoBoxOutput(ns("balance")),


  )
}

#' ad_hoc_boxes Server Functions
#'
#' @noRd
mod_ad_hoc_boxes_server <- function(id, fp_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$low_grades <- renderbs4InfoBox({
      bs4InfoBox(
        "Low Academic Average: ",
        fp_data() %>%
          filter((PAvg < .7 | WAvg < .7) & `Tot hrs` >= 250) %>%
          nrow(),
        icon = icon("list"),
        color = "primary",
        width = 3,
        elevation = 3
      )
    })

    output$low_attendance <- renderbs4InfoBox({
      bs4InfoBox(
        "Low Attendance %: ",
        fp_data() %>%
          filter(`Attendance %` < .9) %>%
          nrow(),
        icon = icon("users"),
        color = "success",
        width = 3,
        elevation = 3
      )
    })

    output$balance <- renderbs4InfoBox({
      bs4InfoBox(
        "FP's with Balance: ",
        fp_data() %>%
          filter(Balance > 0) %>%
          nrow(),
        icon = icon("dollar-sign"),
        color = "danger",
        width = 3,
        elevation = 3
      )
    })

  })
}

## To be copied in the UI
# mod_ad_hoc_boxes_ui("ad_hoc_boxes_1")

## To be copied in the server
# mod_ad_hoc_boxes_server("ad_hoc_boxes_1")
