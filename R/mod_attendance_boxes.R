#' ad_hoc_boxes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_attendance_boxes_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             bs4InfoBoxOutput(ns("act_hrs"),
                              width = 12)
      ),
      column(4,
             bs4InfoBoxOutput(ns("sched_hrs"),
                              width = 12)
      ),
      column(4,
             bs4InfoBoxOutput(ns("pct_hrs"),
                              width = 12)
      ),
    )





  )
}

#' ad_hoc_boxes Server Functions
#'
#' @noRd
mod_attendance_boxes_server <- function(id, data, school, program, date){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    weekly_attendance_df2 <- reactive ({
      data %>%
        mutate(Beginning_Date = floor_date(Beginning_Date, unit = "week")) %>%
        filter(bus_name %in% school()) %>%
        filter(revised_program %in% program()) %>%
        filter(Beginning_Date >= date$start() & Beginning_Date <= date$end()) %>%
        group_by(Beginning_Date)  %>%
        summarize(Actual_Hours = sum(Actual_Hours),
                  Scheduled_Hours = sum(Scheduled_Hours),
                  `Attendance %` = sum(Actual_Hours)/sum(Scheduled_Hours)) %>%
        mutate(color_text = case_when(
          `Attendance %` < .9 ~ "#ff817e",
          TRUE ~ "white"
        )) %>%
        ungroup()

    })

    output$act_hrs <- renderbs4InfoBox({
      bs4InfoBox(
        "Total Actual Hours: ",
        format(round(sum(weekly_attendance_df2()$Actual_Hours)), big.mark = ","),
        icon = icon("a"),
        color = "navy",
        width = 3,
        elevation = 3
      )
    })

    output$sched_hrs <- renderbs4InfoBox({
      bs4InfoBox(
        "Total Scheduled Hours: ",
        format(round(sum(weekly_attendance_df2()$Scheduled_Hours)),big.mark = ","),
        icon = icon("s"),
        color = "lightblue",
        width = 3,
        elevation = 3
      )
    })

    output$pct_hrs <- renderbs4InfoBox({
      bs4InfoBox(
        "Total Attendance %: ",
        percent(round(sum(weekly_attendance_df2()$Actual_Hours)/sum(weekly_attendance_df2()$Scheduled_Hours),2)),
        icon = icon("percent"),
        color = "gray",
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
