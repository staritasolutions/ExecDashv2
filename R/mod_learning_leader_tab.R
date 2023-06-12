#' learning_leader_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_learning_leader_tab_ui <- function(id){
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
      ns("metric"),
      "Metric",
      choices = c("Rebooking %",
                  "TH $ / Guest",
                  "Bottles/Guest",
                  "Service $ / Guest",
                  "Services/Guest",
                  "Average Ticket $"),
      selected = "Rebooking %",
      multiple = FALSE,
      options = pickerOptions(actionsBox = TRUE,
                              liveSearch = TRUE)
    ),
    DTOutput(ns("table")),
    girafeOutput(ns("plot"))


  )
}

#' learning_leader_tab Server Functions
#'
#' @noRd
mod_learning_leader_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    school <- mod_general_select_server("school")
    date <- mod_date_select_server("date")
    ll <- mod_general_select_server("ll")

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

    table_df <- reactive({
      learning_leader %>%
        filter(`Date Start` >= date$start() &
                 `Date Start` <= date$end()) %>%
        filter(Campus %in% school()) %>%
        filter(`Learning Leader` %in% input$ll) %>%
        group_by(`Learning Leader`, Campus) %>%
        summarize(Guests = sum(Clients),
                  Rebooked = sum(Rebooked),
                  `TH $` = sum(`TH $`),
                  `Total Bottles` = sum(`Total Bottles`),
                  `Service $` = sum(`Service $`),
                  `Total Services` = sum(`Total Services`)) %>%
        ungroup() %>%
        mutate(`Serv $ / Guest` = round(`Service $` / Guests,2),
               `Services / Guest` = round(`Total Services` / Guests, 2),
               `Rebooking %` = round(Rebooked / Guests,2),
               `TH / Guest` = round(`TH $` / Guests, 2),
               `Bottle / Guest` = round(`Total Bottles` / Guests, 2)) %>%
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
        filter(`Learning Leader` %in% input$ll) %>%
        rename(Date = `Date Start`) %>%
        mutate(
          `Rebooking %` = round(`Rebooking %`, 2) * 100,
          `TH $ / Guest` = round(`TH $ / Guest`, 2),
          `Bottles/Guest` = round(`Bottles/Guest`, 2),
          `Service $ / Guest` = round(`Service $ / Guest`, 2),
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
# mod_learning_leader_tab_ui("learning_leader_tab_1")

## To be copied in the server
# mod_learning_leader_tab_server("learning_leader_tab_1")
