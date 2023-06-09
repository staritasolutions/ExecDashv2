#' meevo_metrics_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom scales dollar
mod_meevo_metrics_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    gt_output(ns("meevo_metrics_table"))
  )
}

#' meevo_metrics_table Server Functions
#'
#' @noRd
mod_meevo_metrics_table_server <- function(id, data, school, date, metric){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    meevo_metrics_table_df <- reactive({
      data %>%
        filter(Date >= date$start() & Date <= date$end()) %>%
        filter(School %in% school()) %>%
        group_by(School) %>%
        summarize(`Guests/FP` = round(sum(`# Unique Clients Buying Servs.`)/sum(`Emps with Serv. Sales`),2),
                  `Avg Total Ticket` = dollar(round(sum(`Total Sales`)/sum(`# Unique Clients Buying Servs.`),2)),
                  `Take Home $/Guest` = dollar(round(sum(`Total Retail (Incl. Ageless Serums)`)/sum(`# Unique Clients Buying Servs.`),2)),
                  `Bottles/Guest` = round(sum(`Retail Items Sold`)/sum(`# Unique Clients Buying Servs.`),2),
                  `Service $/Guest` = dollar(round(sum(`Total Service (Including Add On)`)/sum(`# Unique Clients Buying Servs.`),2))) %>%
        ungroup() %>% select(School, .data[[metric()]]) #%>%
      # mutate(`\`Avg Total Ticket\`` = paste0("$", `\`Avg Total Ticket\``),
      #        `\`Take Home $/Guest\`` = paste0("$", `\`Take Home $/Guest\``),
      #        `\`Service $/Guest\`` = paste0("$", `\`Service $/Guest\``))
    })

    output$meevo_metrics_table <- render_gt(
      gt(meevo_metrics_table_df(), rowname_col = "School") %>%
        cols_align(align = "center", columns = 1:2) %>%
        tab_options(column_labels.padding.horizontal = px(30),
                    column_labels.hidden = FALSE) %>%
        tab_header(title = "Date Range Average")
    )

  })
}

## To be copied in the UI
# mod_meevo_metrics_table_ui("meevo_metrics_table_1")

## To be copied in the server
# mod_meevo_metrics_table_server("meevo_metrics_table_1")
