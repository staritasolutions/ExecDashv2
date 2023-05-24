## code to prepare `scorecard` dataset goes here

scorecard <- read_csv("data-raw/scorecard_data.csv") %>%
  filter(School %in% c("Boise",
                       "Nampa",
                       "Provo",
                       "Richland",
                       "Spokane",
                       "Twin Falls",
                       "Cour D'Alene"))

usethis::use_data(scorecard, overwrite = TRUE)
