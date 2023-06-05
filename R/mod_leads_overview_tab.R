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
      bs4Card(title = "CRM - Leads Overview",
              id = "card_leadsoverview",
              width = 12,
              "Welcome to the Leads Overview page. This box contains the controls
              needed for the graphs and table below. Some of the boxes show additional
              information or details when maximized.",
              fluidRow(
                column(6, mod_date_select_ui(ns("date1"))),
                column(6, mod_general_select_ui(ns("school1"), "Schools", crm, "School Name"))
              ),
              fluidRow(
                column(6, mod_general_select_ui(ns("lead_type1"), "Lead Type", crm, "lead_type")),
                column(6, mod_general_select_ui(ns("program1"), "Program", crm, "program_final"))
              )
      )

    ),

    fluidRow(
      bs4Card(title = "Conversion Metrics",
              id = "card_convmetrics",
              maximizable = TRUE,
              height = 500,
              mod_conversions_table_ui(ns("conversions_table_1"))
              ),

      bs4Card(title = "Monthly Leads",
              id = ns("card_monthlyleads"),
              maximizable = TRUE,
              height = 500,
              uiOutput(ns("graph_monthlyleads"))
              )

    ),

    # ### COMMENTED OUT HERE ARE MAXIMIZE EXAMPLES
    #
    # fluidRow(
    #
    #   # MAXIMIZING EXAMPLES
    #
    #   bs4Card(
    #     title = "My Card",
    #     id = ns("mycard"),
    #     width = 6,
    #     status = "primary",
    #     maximizable = TRUE,
    #     uiOutput(ns("card_content"))
    #   ),
    #
    #   bs4Card(
    #     title = "Monthly Leads2",
    #     id = ns("card_monthlyleads2"),
    #     width = 6,
    #     status = "primary",
    #     maximizable = TRUE,
    #     uiOutput(ns("card_content2"))
    #   )
    #
    # ),

    fluidRow(
      bs4Card(
        title = "",
        id = "card_metric",
        width = 12,
        fluidRow(
          column(6,
                 mod_crm_metric_select_ui(ns("metric1"))
                 ),
          column(6,
                 "Use this metric selector to further adjust the plots below that
                 show comparisons in addition to the selectors and toggles at the
                 top of the page."
          )
        )
      )

    ),

    fluidRow(

      bs4Card(title = "YOY Quarterly Comparison",
              id = ns("card_quarterlycomp"),
              maximizable = TRUE,
              width = 6,
              uiOutput(ns("graph_quarterlycomp"))),

      bs4Card(title = "YOY Monthly Comparison",
              id = ns("card_monthlycomp"),
              maximizable = TRUE,
              width = 6,
              uiOutput(ns("graph_monthlycomp")))

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

    # MAXIMIZED CONTENT

    ## card_monthlyleads
    ### UI
    max_monthlyleads <- reactive({input$card_monthlyleads$maximized})

    output$graph_monthlyleads <- renderUI({
      if(max_monthlyleads()) {
        fluidRow(
          column(12,
                 mod_monthly_leads_graph_ui(ns("monthly_leads_graph_max"))
                 )
        )
      } else {
        fluidRow(
          column(12,
                 mod_monthly_leads_graph_ui(ns("monthly_leads_graph_min")))
        )
      }
    })

    ### Server
    mod_monthly_leads_graph_server("monthly_leads_graph_max", leads_filtered_crm, maximized = TRUE)
    mod_monthly_leads_graph_server("monthly_leads_graph_min", leads_filtered_crm, maximized = FALSE)

    # ### COMMENTED OUT HERE ARE MAXIMIZE EXAMPLES
    #
    # ## card_mycard
    # ### UI
    # max <- reactive({input$mycard$maximized})
    #
    # output$card_content <- renderUI({
    #   if (max()) {
    #     fluidRow(
    #       column(12,
    #              #plotOutput("maximized_plot")
    #              girafeOutput(ns("maximized_plot"))
    #       )
    #     )
    #   } else {
    #     fluidRow(
    #       column(6, "This is the default content.")
    #     )
    #   }
    # })
    #
    # ### Server
    # output$maximized_plot <- ggiraph::renderGirafe({
    #     gg <- data.frame(x = rnorm(100)) %>%
    #       ggplot(aes(x = x)) +
    #       geom_histogram()
    #
    #     ggiraph(code = print(gg))
    # })
    #
    # ## card_monthlyleads2
    # ### UI
    # max2 <- reactive({input$card_monthlyleads2$maximized})
    #
    # output$card_content2 <- renderUI({
    #   if (max2()) {
    #     fluidRow(
    #       column(12,
    #              mod_monthly_leads_graph_ui(ns("monthly_leads_graph_2"))
    #              )
    #     )
    #   } else {
    #     fluidRow(
    #       column(12,
    #              mod_monthly_leads_graph_ui(ns("monthly_leads_graph_3")))
    #     )
    #   }
    # })
    #
    # ### Server
    # mod_monthly_leads_graph_server("monthly_leads_graph_2", leads_filtered_crm, maximized = TRUE)
    # mod_monthly_leads_graph_server("monthly_leads_graph_3", leads_filtered_crm, maximized = FALSE)

    ## card_quarterlycomp
    ### UI
    max_quarterlycomp <- reactive({input$card_quarterlycomp$maximized})

    output$graph_quarterlycomp <- renderUI({
      if (max_quarterlycomp()) {
        fluidRow(
          column(12,
                 mod_quarterly_metrics_graph_ui(ns("quarterly_metrics_graph_max"))
                 )
        )
      } else {
        fluidRow(
          column(12,
                 mod_quarterly_metrics_graph_ui(ns("quarterly_metrics_graph_min"))
          )
        )
      }
    })

    ### Server
    metric1 <- mod_crm_metric_select_server("metric1")
    metrics_filtered_data <- filter_data_with_metric(crm, school1, lead_type1, program1, metric1, date1)

    mod_quarterly_metrics_graph_server("quarterly_metrics_graph_max", metrics_filtered_data, metric1, maximized = TRUE)
    mod_quarterly_metrics_graph_server("quarterly_metrics_graph_min", metrics_filtered_data, metric1, maximized = FALSE)

    ## card_monthlycomp
    ### UI
    max_monthlycomp <- reactive({input$card_monthlycomp$maximized})

    output$graph_monthlycomp <- renderUI({
      if (max_monthlycomp()) {
        fluidRow(
          column(12,
                 mod_yearly_metrics_graph_ui(ns("yearly_metrics_graph_max"))
                 )
        )
      } else {
        fluidRow(
          column(12,
                 mod_yearly_metrics_graph_ui(ns("yearly_metrics_graph_min"))
                 )
        )
      }
    })

    ### Server
    mod_yearly_metrics_graph_server("yearly_metrics_graph_max", metrics_filtered_data, metric1, maximized = TRUE)
    mod_yearly_metrics_graph_server("yearly_metrics_graph_min", metrics_filtered_data, metric1, maximized = FALSE)





    # Metrics over time section


    metric2 <- mod_crm_metric_select_server("metric2")
    metrics_filtered_data2 <- filter_data_with_metric(crm, school1, lead_type1, program1, metric1, date1)


  })
}

## To be copied in the UI
# mod_leads_overview_tab_ui("leads_overview_tab_1")

## To be copied in the server
# mod_leads_overview_tab_server("leads_overview_tab_1")
