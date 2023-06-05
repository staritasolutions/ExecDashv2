#' hours_at_drop_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggiraph
mod_hours_at_drop_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(width = 6,
                    girafeOutput(ns("plot_hours_at_drop"))),
             column(width = 6,
                    girafeOutput(ns("plot_hours_at_drop2"))))
  )
}

#' hours_at_drop_graph Server Functions
#'
#' @noRd
mod_hours_at_drop_graph_server <- function(id, data, program, date){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filtered_data <- reactive ({
      data %>%
        mutate(
          revised_program = case_when(
            revised_program == "Cosmetology_FT" |
              revised_program == "Cosmetology_PT" ~ "Cosmetology",
            TRUE ~ revised_program
          )
        ) %>%
        filter(revised_program %in% program()) %>%
        filter(Start >= date$start() &
                 Start <= date$end()) %>%
        filter(`Attend stat` == "Withdrawn" & `Tot hrs` > 0)
    })


# old density plot --------------------------------------------------------


    density_scaling_constant <- reactive ({
      density(filtered_data()$`Tot hrs`)$y
    })

    xd <- reactive ({
      data.frame(density(filtered_data()$`Tot hrs`)[c("x", "y")])

    })

    quantiles <- reactive({
      quantile(filtered_data()$`Tot hrs`)
    })

    p1 <- reactive({
      ggplot(xd(), mapping = aes(x, y)) +

        geom_area(data = subset(xd(), x < quantiles()[2]), fill = "#e69f00", alpha = .3) +
        geom_area(data = subset(xd(), x < quantiles()[3] & x > quantiles()[2]), fill = "#009e73", alpha = .3) +
        geom_area(data = subset(xd(), x < quantiles()[4] & x > quantiles()[3]), fill = "#0072b2", alpha = .3) +
        geom_area(data = subset(xd(), x > quantiles()[4]), fill = "#cc79a7", alpha = .3) +
        geom_line() +
        geom_text(data = filtered_data(), mapping =
                    aes(
                      x = quantiles()[3],
                      label = paste0("Average: ", round(quantiles()[3])),
                      y = 0.00005
                    ),
                  angle = 90,
                  vjust = -1,
                  hjust = max(density_scaling_constant())/10
        ) +
        labs(x = NULL,
             subtitle = paste0("Average: ", round(quantiles()[3]), " | Count: ", nrow(filtered_data())),
             y = NULL,
             caption = "\n *This graph shows generally when students drop,split into quartiles. \n (Ex: 25% of students that have dropped, drop in the blue range, \n and the next 25% drop in the pink range etc)") +
        theme(axis.text.y=element_blank(),
              plot.caption = element_text(hjust = 0.5, face = "italic"))

    })


    output$plot_hours_at_drop<- renderGirafe({
      girafe(ggobj = p1(),
             width = 5)
    })

    p2 <- reactive({
      ggplot(filtered_data()) +
        geom_histogram(mapping = aes(x = `Tot hrs`),
                       binwidth = 1)

    })


# new rain cloud plot -----------------------------------------------------


    p3 <- reactive({
      ggplot(filtered_data(),
             aes(x = bus_name,
                 y = `Tot hrs`,
                 fill = bus_name)) +
        PupillometryR::geom_flat_violin(position = position_nudge(x = 0.20)) +
        geom_point(aes(color = bus_name),
                   alpha = .5,
                   position = position_jitter(width = 0.1, height = 0)) +
        coord_flip() +
        labs(title = "Hours at Drop Distribution")
    })
    output$plot_hours_at_drop2 <- renderGirafe({
      girafe(ggobj = p3(),
             width = 5)
    })


  })
}

## To be copied in the UI
# mod_hours_at_drop_graph_ui("hours_at_drop_graph_1")

## To be copied in the server
# mod_hours_at_drop_graph_server("hours_at_drop_graph_1")
