#' crm_conversions
#'
#' @description A fct function that creates conversion metrics for crm data
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

filter_data <- function(data, schools, lead_type, program, metric, date) {
  df <- reactive ({
    data %>%
      filter(`School Name` %in% schools()) %>%
      filter(.data[[metric]] >= date$start() &
               .data[[metric]] <= date$end()) %>%
  filter(lead_type %in% lead_type()) %>%
  filter(program %in% program())
  })
  return(df)
}

filter_data_with_metric <- function(data, schools, lead_type, program, metric, date) {
  df <- reactive ({
    data %>%
      filter(`School Name` %in% schools()) %>%
      filter(.data[[metric()]] >= date$start() &
               .data[[metric()]] <= date$end()) %>%
      filter(lead_type %in% lead_type()) %>%
      filter(program %in% program())
  })
  return(df)
}

conversion <- function(data, conversion, metric1, metric2, is_total = FALSE) {
  if(is_total) {
    df <- reactive ({
      data() %>%
      summarize(lead_type = "Total",
                !!conversion := sum(ifelse(!is.na(.data[[metric2]]), 1, 0))/sum(ifelse(!is.na(.data[[metric1]]), 1, 0)))
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

school_conversion <- function(data, conversion, metric1, metric2, is_total = FALSE) {
  if(is_total) {
    df <- reactive ({
      data() %>%
        summarize(`School Name` = "Total",
                  !!conversion := sum(ifelse(!is.na(.data[[metric2]]), 1, 0))/sum(ifelse(!is.na(.data[[metric1]]), 1, 0)))
    })
  }
  else {
    df <- reactive ({
      data() %>%
        group_by(`School Name`) %>%
        summarize(!!conversion := sum(ifelse(!is.na(.data[[metric2]]), 1, 0))/sum(ifelse(!is.na(.data[[metric1]]), 1, 0)))
    })
  }
  return(df)
}

PMAE_conversion <- function(data1, data2, metric, is_total = FALSE) {

  if(is_total) {
    grouped_df1 <- reactive ({
      data1() %>%
        summarize(lead_type = "Total",
                  count1 = n())
    })
    grouped_df2 <- reactive ({
      data2() %>%
        summarize(lead_type = "Total",
                  count2 = n())
    })
  }
  else {
    grouped_df1 <- reactive ({
      data1() %>% group_by(lead_type) %>%
        summarize(count1 = n())
    })
    grouped_df2 <- reactive ({
      data2() %>% group_by(lead_type) %>%
        summarize(count2 = n())
    })

  }
  combined_df <- reactive ({
    grouped_df1() %>%
      left_join(grouped_df2(), by = "lead_type") %>%
      mutate(!!metric := count2 / count1) %>%
      select(lead_type, !!metric)
  })
  return(combined_df)
}

PMAE_school_conversion <- function(data1, data2, metric, is_total = FALSE) {
  if(is_total) {
    df1 <- reactive ({
      data1() %>%
        summarize(`School Name` = "Total",
                  count1 = n())
    })
    df2 <- reactive ({
      data2() %>%
        summarize(`School Name` = "Total",
                  count2 = n())
    })
    combined_df <- reactive({
      df1() %>%
        left_join(df2(), by = "School Name") %>%
        mutate(!!metric := count2/count1) %>%
        select(`School Name`, !!metric)

    })
  }
  else {
    df1 <- reactive ({
      data1() %>%
        group_by(`School Name`) %>%
        summarize(count1 = n())
    })
    df2 <- reactive ({
      data2() %>%
        group_by(`School Name`) %>%
        summarize(count2 = n())
    })

    combined_df <- reactive({
      df1() %>%
        left_join(df2(), by = "School Name") %>%
        mutate(!!metric := count2/count1) %>%
        select(`School Name`, !!metric)

    })

  }
  return(combined_df)
}




