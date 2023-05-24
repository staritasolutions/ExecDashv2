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
mod_attendance_by_school_graph_server <- function(id, data, program, date){
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
          `Attendance %` < .9 ~ "red",
          TRUE ~ "black"
        ),
        `Attendance %` = percent(round(`Attendance %`, 2))) %>%
        ungroup() %>%
        pivot_longer(cols = `Actual Hours`:`Scheduled Hours`,
                     names_to = "hours_type",
                     values_to = "hours") %>%
        mutate(tooltip = paste0(
          "School: ", bus_name, "\n", "Hours: ", format(round(hours), big.mark = ",")
        ))
    })

    scaling_constant <- reactive({
      max(school_attendance_df()$hours)

    })

    p <- reactive({
      ggplot(data = school_attendance_df(), aes_string(x = "bus_name",
                                                       y = "hours",
                                                       fill = "hours_type")) +
        geom_col_interactive(aes(tooltip = tooltip),
                             position = "dodge") +
        labs(y = "Hours",
             x = "School") +
        guides(fill = guide_legend(title = "Hour Type")) +
        theme(legend.position = "top",
              plot.caption = element_text(hjust = 0.5, face = "italic"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()) +
        labs(x = NULL,
             caption = " \n *Schools under 90% attendance are marked red. \n \n \n") +
        scale_y_continuous(labels = scales::comma) +
        geom_label(data = school_attendance_df() %>%
                     group_by(bus_name) %>%
                     slice_head(n = 1),
                   aes(label = `Attendance %`, color = color_text,y = 0),fill = "white")+
        scale_colour_identity()

    })

    output$plot_attendance<- renderGirafe({
      girafe(ggobj = p(),
             width = 6)
    })

  })
}

## To be copied in the UI
# mod_attendance_by_school_graph_ui("attendance_by_school_graph_1")

## To be copied in the server
# mod_attendance_by_school_graph_server("attendance_by_school_graph_1")
