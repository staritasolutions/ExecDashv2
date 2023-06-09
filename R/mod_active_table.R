#' active_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import gt
mod_active_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    gt_output(ns("active_table"))

  )
}

#' active_table Server Functions
#'
#' @noRd
mod_active_table_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    active_df <- data %>%
        filter(`Attend stat` == "Currently Attending") %>%
        group_by(bus_name) %>%
        summarize(Active = n())

    loa_df <- data %>%
        filter(`Attend stat` == "Leave of Absence") %>%
        group_by(bus_name) %>%
        summarize(LOA = n())

    active_table_df <- active_df %>%
        left_join(loa_df, by = c("bus_name" = "bus_name")) %>%
        rename(School = bus_name) %>%
        mutate(LOA = ifelse(is.na(LOA), 0, LOA))


    output$active_table <- render_gt(
      gt(active_table_df %>%
           select(School, Active, LOA)) %>%
        cols_align(align = "left", columns = 1) %>%
        cols_align(align = "center", columns = Active:LOA) %>%
        # tab_options(column_labels.padding.horizontal = px(30)) %>%
        opt_stylize(style = 1, color = "gray", add_row_striping = TRUE) %>%
        tab_options(
          table.border.top.style = "hidden",
          table.width = pct(100),
          data_row.padding = px(15)
        ) %>%
        tab_style(
          style = list(
            cell_text(align = "left")
          ),
          locations = cells_column_labels(
            columns = School
          )
        )
    )

  })
}

## To be copied in the UI
# mod_active_table_ui("active_table_1")

## To be copied in the server
# mod_active_table_server("active_table_1")
