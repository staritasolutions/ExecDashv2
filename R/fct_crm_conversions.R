#' crm_conversions
#'
#' @description A fct function that creates conversion metrics for crm data
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

filter_data <- function(data, schools, date) {
  df <- reactive ({
    data %>%
      filter(`School Name` %in% schools()) %>%
      filter(`Date Submitted` >= date$start() &
               `Date Submitted` <= date$end())
  })
  #filter(lead_type %in% lead_type) %>%
    #filter(program %in% program)
  df
}

conversion <- function(data, conversion, metric1, metric2, is_total = FALSE) {
  if(is_total) {
    df <- reactive ({
      data() %>%
      summarize(!!conversion := sum(ifelse(!is.na(.data[[metric2]]), 1, 0))/sum(ifelse(!is.na(.data[[metric1]]), 1, 0)))
    })
  }
    else {
  df <- reactive ({
    data() %>%
    group_by(lead_type) %>%
    summarize(!!conversion := sum(ifelse(!is.na(.data[[metric2]]), 1, 0))/sum(ifelse(!is.na(.data[[metric1]]), 1, 0)))
  })
  }
  return(df)
}




