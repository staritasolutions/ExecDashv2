## code to prepare `crm` dataset goes here
crm <- read_csv("data-raw/full_crm.csv")

usethis::use_data(crm, overwrite = TRUE)
