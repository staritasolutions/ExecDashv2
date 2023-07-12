#' meevo_school_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_meevo_school_tab_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Card(
        title = strong("Meevo - School View", style = "font-size:25px;"),
        id = "card_meevoschool",
        width = 12,
        maximizable = FALSE,
        fluidRow(
          em(paste0("Data last updated: ", Sys.Date()), style = "margin-bottom: 10px;")
        ),
        fluidRow(
          column(
            6,
            uiOutput(ns("school_ui"))
          ),
          column(
            6,
            mod_date_select_ui(ns("date1"), start = "2023-01-01")
          )
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = strong("Monthly Service Sales"),
        id = "card_servicesales",
        width = 6,
        maximizable = FALSE,
        mod_service_sales_overtime_graph_ui(ns("service_sales_overtime_graph_1"))
      ),
      bs4Card(
        title = strong("Monthly Take Home"),
        id = "card_takehome",
        width = 6,
        maximizable = FALSE,
        mod_takehome_overtime_graph_ui(ns("takehome_overtime_graph_1"))
      )
    )

  )
}

#' meevo_school_tab Server Functions
#'
#' @noRd
mod_meevo_school_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$school_ui <- renderUI({
      mod_general_select_ui(ns("school"), "Schools", meevo, "School", 1, FALSE)
    })
    school <- mod_general_select_server("school")
    date1 <- mod_date_select_server("date1")


# Make Monthly Sales and TH dataframe -------------------------------------

    this_year_monthly_sales_df <- reactive({
      meevo %>%
        filter(Date >= date1$start() & Date <= date1$end()) %>%
        filter(School == school()) %>%
        mutate(Date = floor_date(Date, "month")) %>%
        group_by(Date, School) %>%
        summarize(Service = sum(`Total Service (Including Add On)`),
                  `Take Home` = sum(`Total Retail (Incl. Ageless Serums)`)) %>%
        mutate(service_tooltip = paste0("$", format(round(Service, 2), big.mark = ",")),
               takehome_tooltip = paste0("$", format(round(`Take Home`, 2), big.mark = ",")),
               Month = as.character(month(Date, label = TRUE))) %>%
        ungroup()
    })

    prev_year_monthly_sales_df <- reactive({
      meevo %>%
        filter(Date >= date1$start()-years(1) & Date <= date1$end()-years(1)) %>%
        filter(School == school()) %>%
        mutate(prev_year_date = floor_date(Date, "month")) %>%
        group_by(prev_year_date, School) %>%
        summarize(prev_year_service = sum(`Total Service (Including Add On)`),
                  prev_year_take_home = sum(`Total Retail (Incl. Ageless Serums)`)) %>%
        mutate(join_date = prev_year_date+years(1)) %>%
        ungroup()
    })

    monthly_sales_df <- reactive({
      this_year_monthly_sales_df() %>%
        left_join(prev_year_monthly_sales_df(), by = c("Date" = "join_date",
                                                       "School" = "School")) %>%
        mutate(Month = month(Date),
               service_tooltip = paste0(format(Date, "%B"), "\n",
                                        year(prev_year_date), ": $", format(round(prev_year_service, 2), big.mark = ","), "\n",
                                        year(Date), ": $", format(round(Service, 2), big.mark = ",")),
               take_home_tooltip = paste0(format(Date, "%B"), "\n",
                                          year(prev_year_date), ": $", format(round(prev_year_take_home, 2), big.mark = ","), "\n",
                                          year(Date), ": $", format(round(`Take Home`, 2), big.mark = ",")))
    })

    mod_service_sales_overtime_graph_server("service_sales_overtime_graph_1", monthly_sales_df)
    mod_takehome_overtime_graph_server("takehome_overtime_graph_1", monthly_sales_df)

  })
}

## To be copied in the UI
# mod_meevo_school_tab_ui("meevo_school_tab_1")

## To be copied in the server
# mod_meevo_school_tab_server("meevo_school_tab_1")
