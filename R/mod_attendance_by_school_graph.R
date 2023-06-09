#' attendance_by_school_graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggiraph
mod_attendance_by_school_graph_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("plot_attendance"))

  )
}

#' attendance_by_school_graph Server Functions
#'
#' @noRd
mod_attendance_by_school_graph_server <- function(id, data, program, date, maximized = FALSE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    school_attendance_df <- reactive({
      data %>%
        filter(revised_program %in% program()) %>%
        filter(Beginning_Date >= date$start() &
                 Beginning_Date <= date$end()) %>%
        group_by(bus_name) %>%
        summarize(`Actual Hours`= sum(Actual_Hours),
                  `Scheduled Hours` = sum(Scheduled_Hours),
                  `Attendance %` = sum(Actual_Hours)/sum(Scheduled_Hours)) %>%
        mutate(color_text = case_when(
          `Attendance %` < .9 ~ "#ff6865",
          TRUE ~ "white"
        ),
        `Attendance %` = percent(round(`Attendance %`, 2))) %>%
        ungroup() %>%
        # pivot_longer(cols = `Actual Hours`:`Scheduled Hours`,
        #              names_to = "hours_type",
        #              values_to = "hours") %>%
        mutate(tooltip = paste0(
          "School: ", bus_name, "\n",
          "Actual Hours: ", format(round(`Actual Hours`), big.mark = ","), "\n",
          "Scheduled Hours: ", format(round(`Scheduled Hours`), big.mark = ",")
        ))
    })

    scaling_constant <- reactive({
      max(school_attendance_df()$hours)

    })

    # Plot adjustments
    if (maximized) {
      width <- 16
      height <- 10
      legend_position <- "right"
      axis_text <- 0
      label_size <- 10
    } else {
      width <- 8
      height <- 10
      legend_position <- "top"
      axis_text <- 90
      label_size <- 5
    }

    p <- reactive({
      ggplot(data = school_attendance_df(), aes_string(x = "bus_name")) +
        geom_col_interactive(aes(y = `Scheduled Hours`, tooltip = tooltip),
                             fill = "#0090e1", alpha = .5) +
        geom_col_interactive(aes(y = `Actual Hours`, tooltip = tooltip),
                             fill = "#005481", width = 0.6) +
        # guides(fill = guide_legend(title = "Hour Type")) +
        theme(plot.caption = element_text(hjust = 0.5, face = "italic"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(x = NULL,
             y = NULL,
             caption = " \n *Schools under 90% attendance are marked red. \n \n \n") +
        scale_y_continuous(labels = scales::comma) +
        geom_text(data = school_attendance_df() %>%
                     group_by(bus_name) %>%
                     slice_head(n = 1),
                   aes(label = `Attendance %`, color = color_text, y = max(`Scheduled Hours`) * 0.03),
                   size = label_size)+
        scale_colour_identity() +
        theme(text = element_text(size = 20),
              axis.text.x = element_text(angle = axis_text))
    })

    output$plot_attendance<- renderGirafe({
      girafe(ggobj = p(),
             width = width,
             height = height) %>%
        girafe_options(opts_tooltip(zindex = 9999))
    })

  })
}

## To be copied in the UI
# mod_attendance_by_school_graph_ui("attendance_by_school_graph_1")

## To be copied in the server
# mod_attendance_by_school_graph_server("attendance_by_school_graph_1")
