## code to prepare `learning_leader` dataset goes here

learning_leader <- read_csv("data-raw/learning_leader.csv") %>%
  mutate(`Date Start` = as.Date(`Date Start`))

usethis::use_data(learning_leader, overwrite = TRUE)
