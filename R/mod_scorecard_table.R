#' scorecard_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_scorecard_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("scorecard"))
  )
}

#' scorecard_table Server Functions
#'
#' @noRd
mod_scorecard_table_server <- function(id, data, school, program, date){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    df1 <- reactive({
      data %>%
        filter(School %in% school()) %>%
        filter(Program %in% program()) %>%
        filter(Date >= date$start() & Date <= date$end()) %>%
        group_by(Date) %>%
        summarise(`Start Enrollment` = sum(`Start Enrollment`),
                  `New Starts` = sum(`New Starts`),
                  Grads = sum(Grads),
                  Drops = sum(Drops),
                  `End Enrollment` = sum(`End Enrollment`),
                  Reenroll = sum(Reenroll),
                  `New LOAs` = sum(`New LOAs`),
                  `LOA Ended` = sum(`LOA Ended`),
                  `Current LOAs` = sum(`Current LOAs`),
                  `Actual Hours` = sum(`Actual Hours`),
                  `Scheduled Hours` = sum(`Scheduled Hours`),
                  Attendance = `Actual Hours` / `Scheduled Hours`) %>%
        ungroup() %>%
        arrange(Date)

    })

    full_df <- reactive({

      totals_df <- df1() %>%
        summarise(across(where(is.numeric), sum))

      bind_rows(df1(), totals_df) %>%
        mutate(Date = paste0(month(Date, label = TRUE), " ", year(Date))) %>%
        mutate(Attendance = formattable::percent(Attendance, accuracy = 1)) %>%
        mutate_at(c(2:12), ~prettyNum(., big.mark = ",")) %>%
        mutate(Date = case_when(Date == "NA NA" ~ "Totals",
                                TRUE ~ Date))
    })

    # First table
    output$scorecard <- DT::renderDT({

      datatable(
        full_df(),
        rownames = FALSE,
        filter = "none",
        options = list(
          pageLength = 15,
          dom = "ltp",
          columnDefs = list(list(
            className = 'dt-center',
            targets = 0:12
          ))
        )) %>%
        formatStyle(columns = c(2:6),
                    backgroundColor = "#4C86A825"
                    #border = 'solid #4C86A8'
        ) %>%
        formatStyle(
          1,
          target = 'row',
          fontWeight = styleEqual("Totals", "bold")
        ) %>%
        formatStyle(
          columns = c(2,6,10,13),
          valueColumns = "Date",
          backgroundColor = styleEqual("Totals", "#FFFFFF"),
          color = styleEqual("Totals", "#FFFFFF")
        ) %>%
        formatPercentage(c(13))

    })

  })
}

## To be copied in the UI
# mod_scorecard_table_ui("scorecard_table_1")

## To be copied in the server
# mod_scorecard_table_server("scorecard_table_1")
