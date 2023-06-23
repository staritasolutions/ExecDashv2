#' general_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput pickerOptions
mod_general_select_ui <- function(id, title, data, metric, selected_count = NULL, select_multiple = TRUE){
  ns <- NS(id)
  default_selected <- if (is.null(selected_count)) {
    unique(data[[metric]])
  } else {
    unique(data[[metric]])[seq_len(selected_count)]
  }

  tagList(
    pickerInput(
      ns("list"),
      choices = sort(unique(data[[metric]])),
      multiple = select_multiple,
      selected = default_selected,
      options = pickerOptions(actionsBox = TRUE,
                              liveSearch = TRUE,
                              selectedTextFormat = "static",
                              title = title)
    )
  )
}

#' general_select Server Functions
#'
#' @noRd
mod_general_select_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    return(
      reactive({input$list})
    )
  })
}

## To be copied in the UI
# mod_general_select_ui("general_select_1")

## To be copied in the server
# mod_general_select_server("general_select_1")
