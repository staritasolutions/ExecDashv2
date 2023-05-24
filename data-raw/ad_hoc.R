## code to prepare `ad_hoc` dataset goes here

program_df <- read_csv("data-raw/program.csv")

ad_hoc <- read_csv("data-raw/northstar_adhocs.csv") %>%
  mutate(Start = mdy(Start),
         `Rev grad` = mdy(`Rev grad`),
         LDA = mdy(LDA),
         Drop = mdy(Drop),
         `Leave start` = mdy(`Leave start`),
         `Leave end` = mdy(`Leave end`),
         Reenrolled = mdy(Reenrolled)) %>%
  left_join(program_df, by = c("Program" = "Program")) %>%
  mutate(revised_program = case_when(`Wk hrs` < 24 & revised_program == "Cosmetology"~ "Cosmetology_PT",
                                     `Wk hrs` >= 24 & revised_program == "Cosmetology" ~ "Cosmetology_FT",
                                     TRUE ~ revised_program),
         date_pulled = floor_date(Sys.Date(), "week")) %>%
  mutate(`Days Absent` = difftime(date_pulled, LDA, units = "days"))

usethis::use_data(ad_hoc, overwrite = TRUE)
