#' school_attendance_graphs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggtext
mod_school_attendance_graphs_ui <- function(id){
  ns <- NS(id)
  tagList(
    girafeOutput(ns("plot_weekly_attendance"))

  )
}

#' school_attendance_graphs Server Functions
#'
#' @noRd
mod_school_attendance_graphs_server <- function(id, data, school, program, date, choice){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    weekly_attendance_df <- reactive ({
      data %>%
        mutate(Beginning_Date = floor_date(Beginning_Date, unit = "week")) %>%
        filter(bus_name %in% school()) %>%
        filter(revised_program %in% program()) %>%
        filter(Beginning_Date >= date$start() & Beginning_Date <= date$end()) %>%
        group_by(Beginning_Date)  %>%
        summarize(Actual_Hours = sum(Actual_Hours),
                  Scheduled_Hours = sum(Scheduled_Hours),
                  `Attendance %` = sum(Actual_Hours)/sum(Scheduled_Hours)) %>%
        mutate(color_text = case_when(
          `Attendance %` < .895 ~ "#ff6865",
          TRUE ~ "white"
        )) %>%
        ungroup() %>%
        pivot_longer(cols = Actual_Hours:Scheduled_Hours,
                     names_to = "hours_type",
                     values_to = "hours")

    })

    # Attendance % plot

    p2<- reactive ({
      ggplot(data = weekly_attendance_df() %>%
               group_by(Beginning_Date) %>%
               slice_head(n = 1) %>%
               mutate(tooltip = paste0("Week: ", Beginning_Date, "\n",
                                       "Attendance: ", percent(round(`Attendance %`,2)))),
             aes_string(x = "Beginning_Date", y = "`Attendance %`")) +
        geom_hline(yintercept = .9, linetype = "dashed", color = "#009e73",
                   size = 1.5, alpha = 0.8) +
        geom_line(color = "#005481", size = 1.5, alpha = 0.5) +
        geom_point_interactive(aes(tooltip = tooltip),
                               color = "#005481",
                               size = 5) +
        geom_label(y = .90, x = date$end()-7, color = "#009e73", label = "90%") +
        labs(x = NULL,
             y = NULL) +
        scale_y_continuous(labels = scales::percent,
                           limits = c(.5,NA))
    })



    weekly_attendance_df2 <- reactive ({
      data %>%
        mutate(Beginning_Date = floor_date(Beginning_Date, unit = "week")) %>%
        filter(bus_name %in% school()) %>%
        filter(revised_program %in% program()) %>%
        filter(Beginning_Date >= date$start() & Beginning_Date <= date$end()) %>%
        group_by(Beginning_Date)  %>%
        summarize(Actual_Hours = sum(Actual_Hours),
                  Scheduled_Hours = sum(Scheduled_Hours),
                  `Attendance %` = sum(Actual_Hours)/sum(Scheduled_Hours)) %>%
        mutate(color_text = case_when(
          `Attendance %` < .9 ~ "#ff817e",
          TRUE ~ "white"
        )) %>%
        ungroup()

    })

    p1<- reactive({
      ggplot(data = weekly_attendance_df2() %>%
               mutate(`Attendance %` = percent(`Attendance %`)) %>%
               mutate(tooltip = paste0("Week: ", Beginning_Date, "\n",
                                       "Hours: ", format(round(Actual_Hours), big.mark = ","), "\n",
                                       "Scheduled Hours: ", format(round(Scheduled_Hours), big.mark = ","))))+
        geom_col(aes(x = Beginning_Date, y = Scheduled_Hours), fill = "#0090e1", alpha = .5) +
        geom_col(aes(x = Beginning_Date, y = Actual_Hours), fill = "#005481", width = 4)+
        geom_col_interactive(aes(x = Beginning_Date, y = Scheduled_Hours, tooltip = tooltip), fill = NA) +
        labs(y = "Hours") +
        theme(legend.position = "top",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              plot.subtitle = element_text(hjust = 1, face = "italic"),
              plot.caption = element_text(color = "#ff817e", size = 12),
              plot.title = element_markdown(hjust = 0.5)) +
        labs(x = NULL,
             y = NULL,
             title = "<span style='color:#005481;font-size:22px'>**Actual Hours**</span>
             within
             <span style='color:#0090e1;font-size:22px'>**Scheduled Hours**</span>",
             caption = "\n
             *Weeks with less than 90% attendance") +
        geom_text(data = weekly_attendance_df() %>%
                    group_by(Beginning_Date) %>%
                    slice_head(n = 1),
                  aes(x = Beginning_Date,  y = max(hours)*.03, label = percent(round(`Attendance %`,2)), color = color_text)) +
        scale_colour_identity()

    })

    graph_choice <- reactive ({
      if (choice() == "Attendance")
        p1()
      else p2()
    })

    output$plot_weekly_attendance<- renderGirafe({
      girafe(ggobj = graph_choice(),
             width = 12)
    })

  })
}

## To be copied in the UI
# mod_school_attendance_graphs_ui("school_attendance_graphs_1")

## To be copied in the server
# mod_school_attendance_graphs_server("school_attendance_graphs_1")
