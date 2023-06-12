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
    mod_general_select_ui(ns("school"), "School", learning_leader, "Campus"),
    mod_date_select_ui(ns("date"), start = "2023-01-01"),
    pickerInput(
      ns("ll"),
      "Learning Leader",
      choices = sort(unique(learning_leader$`Learning Leader`)),
      selected = sort(unique(learning_leader$`Learning Leader`)),
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE,
                              liveSearch = TRUE)
    ),
    sliderInput(
      ns("minguests"),
      "Minimum Guests Serviced",
      min = 1,
      max = 100,
      value = 20
    ),
    pickerInput(
      ns("ll2"),
      "Learning Leader",
      choices = sort(unique(learning_leader$`Learning Leader`)),
      selected = sort(unique(learning_leader$`Learning Leader`))[2],
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE,
                              liveSearch = TRUE)
    ),
    pickerInput(
      ns("metric"),
      "Metric",
      choices = c("Service $ / Guest",
                  "Services/Guest",
                  "Average Ticket $"),
      selected = "Service $ / Guest",
      multiple = FALSE,
      options = pickerOptions(actionsBox = TRUE,
                              liveSearch = TRUE)
    ),
    DTOutput(ns("table")),
    girafeOutput(ns("plot"))

  )
}

#' ll_services_tab Server Functions
#'
#' @noRd
mod_ll_services_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    school <- mod_general_select_server("school")
    date <- mod_date_select_server("date")

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
        labs(color = "")
    })

    output$plot <- renderGirafe({
      girafe(ggobj = p(),
             width = 12)
    })

  })
}

## To be copied in the UI
# mod_ll_services_tab_ui("ll_services_tab_1")

## To be copied in the server
# mod_ll_services_tab_server("ll_services_tab_1")
