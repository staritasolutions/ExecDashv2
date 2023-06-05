## code to prepare `meevo` dataset goes here

meevo <- read_csv("data-raw/meevo_data.csv") %>%
  mutate(Date = as.Date(Date))

usethis::use_data(meevo, overwrite = TRUE)
