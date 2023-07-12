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
        summarise(`Start Census` = sum(`Start Enrollment`),
                  `New Starts` = sum(`New Starts`),
                  Grads = sum(Grads),
                  Drops = sum(Drops),
                  `End Census` = sum(`End Enrollment`),
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

      totals_df <- tibble(
        Date = "Totals",
        `Start Census` = (df1()$`Start Census`)[1],
        `New Starts` = sum(df1()$`New Starts`),
        Grads = sum(df1()$Grads),
        Drops = sum(df1()$Drops),
        `End Census` = df1()$`End Census`[length(df1()$`End Census`)],
        Reenroll = sum(df1()$Reenroll),
        `New LOAs` = sum(df1()$`New LOAs`),
        `LOA Ended` = sum(df1()$`LOA Ended`),
        `Current LOAs` = df1()$`Current LOAs`[length(df1()$`Current LOAs`)],
        `Actual Hours` = sum(df1()$`Actual Hours`),
        `Scheduled Hours` = sum(df1()$`Scheduled Hours`)
      ) %>%
        mutate(Attendance = `Actual Hours` / `Scheduled Hours`)

      df1() %>%
        mutate(Date = paste0(month(Date, label = TRUE), " ", year(Date))) %>%
        bind_rows(totals_df) %>%
        mutate(Attendance = formattable::percent(Attendance, accuracy = 1)) %>%
        mutate_at(c(2:12), ~prettyNum(., big.mark = ","))
    })

    # First table
    output$scorecard <- DT::renderDT({

      datatable(
        full_df(),
        rownames = FALSE,
        filter = "none",
        options = list(
          pageLength = 15,
          dom = "t",
          columnDefs = list(list(
            className = 'dt-center',
            targets = 0:12
          ))
        )) %>%
        formatStyle(columns = c(2:6),
                    backgroundColor = "#e2f5ff"
                    #border = 'solid #4C86A8'
        ) %>%
        formatStyle(
          1,
          target = 'row',
          fontWeight = styleEqual("Totals", "bold")
        ) %>%
        formatPercentage(c(13))

    })

  })
}

## To be copied in the UI
# mod_scorecard_table_ui("scorecard_table_1")

## To be copied in the server
# mod_scorecard_table_server("scorecard_table_1")
