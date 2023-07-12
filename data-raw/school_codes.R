## code to prepare `school_codes` dataset goes here

school_codes <- read_csv("data-raw/school_codes.csv")

usethis::use_data(school_codes, overwrite = TRUE)
