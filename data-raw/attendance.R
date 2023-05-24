## code to prepare `attendance` dataset goes here

program_df <- read_csv("data-raw/program.csv")
school_codes <- read_csv("data-raw/school_codes.csv")

attendance <- read_csv("data-raw/attendance.csv") %>%
  left_join(school_codes,
            by = c("Kiss_ID" = "kiss_id")) %>%
  left_join(program_df, by = c("Program" = "Program")) %>%
  filter(bus_name %in% c("Boise",
                         "Nampa",
                         "Provo",
                         "Richland",
                         "Spokane",
                         "Twin Falls",
                         "Cour D'Alene"))


usethis::use_data(attendance, overwrite = TRUE)
