#' ll_rebooking_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ll_rebooking_tab_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Card(
        title = strong("Learning Leader - Rebooking", style = "font-size:25px;"),
        id = "card_rebookinfo",
        maximizable = FALSE,
        width = 12,
        fluidRow(
          em(paste0("Data last updated: ", Sys.Date()), style = "margin-bottom: 10px;")
        ),
        fluidRow(
          column(
            width = 6,
            uiOutput(ns("school_ui"))
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
        title = strong("Rebooking Table"),
        id = "card_rebooktable",
        maximizable = TRUE,
        width = 12,
        fluidRow(
          column(
            width = 4,
            p("", style = "margin-top: 30px;"),
            uiOutput(ns("ll_ui"))
          ),
          column(
            width = 8,
            sliderInput(
              ns("minguests"),
              "Minimum Guests Serviced",
              min = 1,
              max = 100,
              value = 20
            )
          )
        ),
        fluidRow(
          DTOutput(ns("table"))
        )
      )
      ),
    fluidRow(
      bs4Card(
        title = strong("Rebooking Plot"),
        id = "card_rebookplt",
        maximizable = TRUE,
        width = 12,
        fluidRow(
          column(
            width = 3,
            uiOutput(ns("ll2_ui"))
          )
        ),
        fluidRow(
          girafeOutput(ns("plot"))
        )
      )
    )

  )
}

#' ll_rebooking_tab Server Functions
#'
#' @noRd
mod_ll_rebooking_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$school_ui <- renderUI ({
      mod_general_select_ui(ns("school"), "School", learning_leader, "Campus")
    })
    school <- mod_general_select_server("school")
    date <- mod_date_select_server("date")

    output$ll_ui <- renderUI ({
      pickerInput(
        ns("ll"),
        choices = sort(unique(learning_leader$`Learning Leader`)),
        selected = sort(unique(learning_leader$`Learning Leader`)),
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          selectedTextFormat = "static",
          title = "Learning Leader"
        )
      )
    })

    output$ll2_ui <- renderUI({
      pickerInput(
        ns("ll2"),
        choices = sort(unique(learning_leader$`Learning Leader`)),
        selected = sort(unique(learning_leader$`Learning Leader`))[2],
        multiple = TRUE,
        options = pickerOptions(
          actionsBox = TRUE,
          liveSearch = TRUE,
          selectedTextFormat = "static",
          title = "Learning Leader"
        ))
    })

    observe({
      update_df <- learning_leader %>%
        filter(`Date Start` >= date$start() &
                 `Date Start` <= date$end()) %>%
        filter(Campus %in% school())

      pickerInput(
        ns("ll"),
        #label = p("", style = "margin-top: 30px;"),
        choices = sort(unique(learning_leader$`Learning Leader`)),
        selected = sort(unique(learning_leader$`Learning Leader`)),
        multiple = TRUE
      )
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
                  Rebooked = sum(Rebooked)) %>%
        ungroup() %>%
        mutate(`Rebooking %` = round(Rebooked / Guests,2) * 100) %>%
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
        mutate(
          `Rebooking %` = round(`Rebooking %`, 2)
        ) %>%
        mutate(
          tooltip = paste0(
            "Date: ",
            Date,
            "\n",
            "Learning Leader: ",
            `Learning Leader`,
            "\n",
            "Rebooking %: ",
            `Rebooking %` * 100
          )
        )
    })

    p <- reactive({
      ggplot(data = plot_df(), aes(x = Date,
                                   y = `Rebooking %`,
                                   color = as.factor(`Learning Leader`),
                                   group = as.factor(`Learning Leader`),
                                   tooltip = tooltip)) +
        geom_line(linewidth = 1) +
        geom_point_interactive(size = 3) +
        labs(color = "",
             y = NULL,
             x = NULL) +
        theme(
          legend.position = "top"
        ) +
        scale_y_continuous(labels = scales::percent)
    })

    output$plot <- renderGirafe({
      girafe(ggobj = p(),
             width = 12,
             height = 4) %>%
        girafe_options(opts_tooltip(zindex = 9999))
    })

  })
}

## To be copied in the UI
# mod_ll_rebooking_tab_ui("ll_rebooking_tab_1")

## To be copied in the server
# mod_ll_rebooking_tab_server("ll_rebooking_tab_1")
