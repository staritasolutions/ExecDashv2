#' ll_services_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ll_services_tab_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Card(
        title = strong("Learning Leader - Services", style = "font-size:25px;"),
        id = "card_servinfo",
        maximizable = FALSE,
        width = 12,
        fluidRow(
          em(paste0("Data last updated: ", Sys.Date()), style = "margin-bottom: 10px;")
        ),
        fluidRow(
          column(
            width = 6,
            uiOutput(ns("school1_ui"))
          ),
          column(
            width = 6,
            mod_date_select_ui(ns("date"), start = floor_date(Sys.Date(), unit = "year"))
          )
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = strong("Services Table"),
        id = "card_servtable",
        maximizable = TRUE,
        width = 12,
        fluidRow(
          column(
            4,
            p("", style = "margin-top: 30px;"),
            uiOutput(ns("ll_ui"))
          ),
          column(
            8,
            sliderInput(
              ns("minguests"),
              "Minimum Guests Serviced",
              min = 1,
              max = 100,
              value = 20
            ),
          )
        ),
        fluidRow(
          DTOutput(ns("table"))
        )
      )
    ),

    fluidRow(
      bs4Card(
        title = strong("Services Plot"),
        id = "card_servplt",
        maximizable = TRUE,
        width = 12,
        fluidRow(
          column(
            width = 3,
            uiOutput(ns("ll2_ui"))
          ),
          column(
            width = 3,
            pickerInput(
              ns("metric"),
              choices = c("Service $ / Guest",
                          "Services/Guest",
                          "Average Ticket $"),
              selected = "Service $ / Guest",
              multiple = FALSE,
              options = pickerOptions(actionsBox = TRUE,
                                      liveSearch = TRUE,
                                      selectedTextFormat = "static",
                                      title = "Metric")
            )
          )
        ),
        fluidRow(
          girafeOutput(ns("plot"))
        )
      )
    )

  )
}

#' ll_services_tab Server Functions
#'
#' @noRd
mod_ll_services_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$school1_ui <- renderUI ({
      mod_general_select_ui(ns("school"), "School", learning_leader, "Campus")
    })

    school <- mod_general_select_server("school")
    date <- mod_date_select_server("date")

    output$ll_ui <- renderUI ({
      pickerInput(
        ns("ll"),
        #label = p("", style = "margin-top: 30px;"),
        choices = sort(unique(learning_leader$`Learning Leader`)),
        selected = sort(unique(learning_leader$`Learning Leader`)),
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE,
                                liveSearch = TRUE,
                                selectedTextFormat = "static",
                                title = "Learning Leader")
      )
    })

    observe({
      update_df <- learning_leader %>%
        filter(`Date Start` >= date$start() &
                 `Date Start` <= date$end()) %>%
        filter(Campus %in% school())

      updatePickerInput(session,
                        inputId = "ll",
                        choices = sort(unique(update_df$`Learning Leader`)),
                        selected = sort(unique(update_df$`Learning Leader`)))
    })

    observe({
      updateSliderInput(session,
                        inputId = "minguests",
                        value = (interval(date$start(), date$end()) %/% months(1) + 1) * 30,
                        max = (interval(date$start(), date$end()) %/% months(1) + 1) * 50
      )
    })

    output$ll2_ui <- renderUI ({
      pickerInput(
        ns("ll2"),
        choices = sort(unique(learning_leader$`Learning Leader`)),
        selected = sort(unique(learning_leader$`Learning Leader`))[2],
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE,
                                liveSearch = TRUE,
                                selectedTextFormat = "static",
                                title = "Learning Leader")
      )
    })

    observe({
      update_df <- learning_leader %>%
        filter(`Date Start` >= date$start() &
                 `Date Start` <= date$end()) %>%
        filter(Campus %in% school())

      updatePickerInput(session,
                        inputId = "ll2",
                        choices = sort(unique(update_df$`Learning Leader`)),
                        selected = sort(unique(update_df$`Learning Leader`))[2])
    })

    table_df <- reactive({
      learning_leader %>%
        filter(`Date Start` >= date$start() &
                 `Date Start` <= date$end()) %>%
        filter(Campus %in% school()) %>%
        filter(`Learning Leader` %in% input$ll) %>%
        group_by(`Learning Leader`, Campus) %>%
        summarize(Guests = sum(Clients),
                  `Service $` = sum(`Service $`),
                  `Total Services` = sum(`Total Services`)) %>%
        ungroup() %>%
        mutate(`Serv $ / Guest` = round(`Service $` / Guests,2),
               `Services / Guest` = round(`Total Services` / Guests, 2)) %>%
        filter(Guests >= input$minguests)
    })

    output$table <- DT::renderDT({
      datatable(table_df())
    })

    plot_df <- reactive({
      learning_leader %>%
        filter(`Date Start` >= date$start() &
                 `Date Start` <= date$end()) %>%
        filter(Campus %in% school()) %>%
        filter(`Learning Leader` %in% input$ll2) %>%
        rename(Date = `Date Start`) %>%
        mutate(`Service $ / Guest` = round(`Service $ / Guest`, 2),
          `Services/Guest` = round(`Services/Guest`, 2),
          `Average Ticket $` = round(`Average Ticket $`, 2)
        ) %>%
        mutate(
          tooltip = paste0(
            "Date: ",
            Date,
            "\n",
            "Learning Leader: ",
            `Learning Leader`,
            "\n",
            input$metric,
            .data[[input$metric]]
          )
        )
    })

    p <- reactive({
      ggplot(data = plot_df(), aes(x = Date,
                                   y = .data[[input$metric]],
                                   color = as.factor(`Learning Leader`),
                                   group = as.factor(`Learning Leader`),
                                   tooltip = tooltip)) +
        geom_line(linewidth = 1) +
        geom_point_interactive(size = 3) +
        theme(
          legend.position = "top"
        ) +
        labs(color = "",
             x = NULL)
    })

    output$plot <- renderGirafe({
      girafe(ggobj = p(),
             width = 12,
             height = 4)
    })

  })
}

## To be copied in the UI
# mod_ll_services_tab_ui("ll_services_tab_1")

## To be copied in the server
# mod_ll_services_tab_server("ll_services_tab_1")
