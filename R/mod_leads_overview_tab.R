#' leads_overview_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textOutput renderText tableOutput renderTable
#' @importFrom DT renderDataTable dataTableOutput
#' @import ggiraph
#'
mod_leads_overview_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(width = 12,
        mod_date_select_ui(ns("date1")),
        mod_general_select_ui(ns("school1"), "Schools", crm, "School Name"),
        mod_general_select_ui(ns("lead_type1"), "Lead Type", crm, "lead_type"),
        mod_general_select_ui(ns("program1"), "Program", crm, "program_final")
      )

    ),

    fluidRow(
      bs4Card(maximizable = TRUE,
              height = 600,
              mod_conversions_table_ui(ns(
                "conversions_table_1"
              ))),

      bs4Card(title = "Monthly Leads",
              id = "card_monthlyleads",
              maximizable = TRUE,
              height = 600,
              mod_monthly_leads_graph_ui(ns(
                "monthly_leads_graph_1"
              )))

    ),

    fluidRow(

      # MAXIMIZING EXAMPLES

      bs4Card(
        title = "My Card",
        id = ns("mycard"),
        width = 6,
        status = "primary",
        maximizable = TRUE,
        uiOutput(ns("card_content"))
      ),

      bs4Card(
        title = "Monthly Leads2",
        id = ns("card_monthlyleads2"),
        width = 6,
        status = "primary",
        maximizable = TRUE,
        uiOutput(ns("card_content2"))
      )

    ),

    fluidRow(
      bs4Card(
        mod_crm_metric_select_ui(ns("metric1")),
        mod_crm_metric_select_ui(ns("metric2"))
      )

    ),

    fluidRow(
      bs4Card(maximizable = TRUE,
              mod_quarterly_metrics_graph_ui(ns(
                "quarterly_metrics_graph_1"
              ))),

      bs4Card(maximizable = TRUE,
              mod_yearly_metrics_graph_ui(ns(
                "yearly_metrics_graph_1"
              )))

    )
  )
}

#' leads_overview_tab Server Functions
#'
#' @noRd
mod_leads_overview_tab_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # First Row
    date1 <- mod_date_select_server("date1")
    school1 <- mod_general_select_server("school1")
    lead_type1 <- mod_general_select_server("lead_type1")
    program1 <- mod_general_select_server("program1")
    leads_filtered_crm <- filter_data(data = crm,
                                      school = school1,
                                      lead_type = lead_type1,
                                      program = program1,
                                      date = date1)

    # Second Row
    mod_conversions_table_server("conversions_table_1", leads_filtered_crm)
    mod_monthly_leads_graph_server("monthly_leads_graph_1", leads_filtered_crm)

    # MAXIMIZED CONTENT

    max <- reactive({input$mycard$maximized})

    output$card_content <- renderUI({
      if (max()) {
        fluidRow(
          column(12,
                 #plotOutput("maximized_plot")
                 girafeOutput(ns("maximized_plot"))
          )
        )
      } else {
        fluidRow(
          column(6, "This is the default content.")
        )
      }
    })

    output$maximized_plot <- ggiraph::renderGirafe({
        gg <- data.frame(x = rnorm(100)) %>%
          ggplot(aes(x = x)) +
          geom_histogram()

        ggiraph(code = print(gg))
    })

    max2 <- reactive({input$card_monthlyleads2$maximized})

    output$card_content2 <- renderUI({
      if (max2()) {
        fluidRow(
          column(12,
                 mod_monthly_leads_graph_ui(ns("monthly_leads_graph_2"))
                 )
        )
      } else {
        fluidRow(
          column(12,
                 mod_monthly_leads_graph_ui(ns("monthly_leads_graph_3")))
        )
      }
    })

    mod_monthly_leads_graph_server("monthly_leads_graph_2", leads_filtered_crm)
    mod_monthly_leads_graph_server("monthly_leads_graph_3", leads_filtered_crm)



    # Metrics over time section

    metric1 <- mod_crm_metric_select_server("metric1")
    metric2 <- mod_crm_metric_select_server("metric2")
    metrics_filtered_data1 <- filter_data_with_metric(crm, school1, lead_type1, program1, metric1, date1)
    metrics_filtered_data2 <- filter_data_with_metric(crm, school1, lead_type1, program1, metric2, date1)
    mod_quarterly_metrics_graph_server("quarterly_metrics_graph_1", metrics_filtered_data1, metric1)
    mod_yearly_metrics_graph_server("yearly_metrics_graph_1", metrics_filtered_data2, metric2)


  })
}

## To be copied in the UI
# mod_leads_overview_tab_ui("leads_overview_tab_1")

## To be copied in the server
# mod_leads_overview_tab_server("leads_overview_tab_1")
