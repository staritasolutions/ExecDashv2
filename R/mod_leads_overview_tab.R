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
#' @import lubridate
#'
mod_leads_overview_tab_ui <- function(id) {
  ns <- NS(id)

  # This stopped working for some reason
  # custom_start <- ifelse(month(Sys.Date()) < 7,
  #                        Sys.Date() - months(6),
  #                        floor_date(Sys.Date(), unit = "year"))

  if(month(Sys.Date()) < 7) {
    custom_start <- Sys.Date() - months(6)
  } else {
    custom_start <- floor_date(Sys.Date(), unit = "year")
  }

  tagList(
    fluidRow(
      bs4Card(title = strong("CRM - Leads Overview", style = "font-size:25px;"),
              id = "card_leadsoverview",
              width = 12,
              fluidRow(
                em(paste0("Data last updated: ", Sys.Date()), style = "margin-bottom: 10px;")
              ),
              fluidRow(
                column(3, mod_date_select_ui(ns("date1"),
                                             start = custom_start,
                                             end = Sys.Date())),
                column(3, uiOutput(ns("school1_ui")) ),
                column(3, uiOutput(ns("lead_type1_ui"))),
                column(3, uiOutput(ns("program1_ui")))
              ),
              style = "border-radius: class='rounded-9';"
      )

    ),

    fluidRow(
      bs4Card(title = strong("Conversion Metrics"),
              id = "card_convmetrics",
              maximizable = TRUE,
              height = 550,
              mod_conversions_table_ui(ns("conversions_table_1"))
              ),

      bs4Card(title = strong("Monthly Leads"),
              id = ns("card_monthlyleads"),
              maximizable = TRUE,
              height = 550,
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
                 offset = 3,
                 uiOutput(ns("year_ui")))
        ),
        fluidRow(
          column(3, mod_crm_metric_select_ui(ns("metric1"))),
          column(3, uiOutput(ns("school2_ui"))),
          column(3, uiOutput(ns("lead_type2_ui"))),
          column(3, uiOutput(ns("program2_ui")))
        )
      )

    ),

    fluidRow(

      bs4Card(title = strong("YOY Quarterly Comparison"),
              id = ns("card_quarterlycomp"),
              maximizable = TRUE,
              width = 5,
              uiOutput(ns("graph_quarterlycomp"))),

      bs4Card(title = strong("YOY Monthly Comparison"),
              id = ns("card_monthlycomp"),
              maximizable = TRUE,
              width = 7,
              uiOutput(ns("graph_monthlycomp")))

    )
  )
}

#' leads_overview_tab Server Functions
#'
#' @noRd
mod_leads_overview_tab_server <- function(id, crm){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # First Row
    date1 <- mod_date_select_server("date1")
    output$school1_ui <- renderUI ({
      mod_general_select_ui(ns("school1"), "Schools", crm, "School Name")
    })
    school1 <- mod_general_select_server("school1")
    output$lead_type1_ui <- renderUI({
      mod_general_select_ui(ns("lead_type1"), "Lead Type", crm, "lead_type")
    })
    lead_type1 <- mod_general_select_server("lead_type1")
    output$program1_ui <- renderUI ({
      mod_general_select_ui(ns("program1"), "Program", crm, "program_final")
    })
    program1 <- mod_general_select_server("program1")
    leads_filtered_crm <- filter_data(data = crm,
                                      school = school1,
                                      lead_type = lead_type1,
                                      program = program1,
                                      metric = "Lead",
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

    ## Inputs needed for remaining graphs
    output$school2_ui <- renderUI ({
      mod_general_select_ui(ns("school2"), "Schools", crm, "School Name")
    })
    school2 <- mod_general_select_server("school2")
    output$lead_type2_ui <- renderUI({
      mod_general_select_ui(ns("lead_type2"), "Lead Type", crm, "lead_type")
    })
    lead_type2 <- mod_general_select_server("lead_type2")
    output$program2_ui <- renderUI ({
      mod_general_select_ui(ns("program2"), "Program", crm, "program_final")
    })
    program2 <- mod_general_select_server("program2")

    output$year_ui <- renderUI({
      sliderInput(
        ns("slider1"),
        label = "",
        min = min(year(crm$Lead), na.rm = TRUE),
        max = max(year(crm$Lead), na.rm = TRUE),
        value = c(year(Sys.Date()) - 2,
                  year(Sys.Date())),
        sep = "",
        ticks = FALSE
      )
    })

    custom_date <- list(start = NA, end = NA)
    custom_date$start <- reactive({paste0(input$slider1[1], "-01-01")})
    custom_date$end <- reactive({paste0(input$slider1[2], "-12-31")})


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


    metrics_filtered_data <- filter_data_with_metric(crm, school2, lead_type2, program2, metric1, custom_date)

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
