## code to prepare `crm` dataset goes here
crm_raw <- read_csv("data-raw/full_crm.csv")

first_touch <- crm_raw$`URL Source` %>% str_split("\\[")
first_touch <- sapply(first_touch, function(x) ifelse(length(x) >= 2, x[2], NA))
first_touch <- first_touch %>% str_split("\"")
first_touch <- sapply(first_touch, function(x) ifelse(length(x) >= 2, x[2], NA))

subdomains_df <- readxl::read_excel("data-raw/PMAE_subdomains.xlsx") %>%
  mutate(all_subdomains = ifelse(str_detect(subdomain, ", |,"), str_split(subdomain, ", |,"), subdomain))

subdomains <- unlist(subdomains_df$all_subdomains)
subdomains <- paste(subdomains, collapse = "|")

crm <- crm_raw %>%
  mutate(program = ifelse(
    `Course of Interest` == "Intereset Not Set",
    Program,
    `Course of Interest`
  )) %>%
  mutate(
    program_final = case_when(
      str_detect(program, regex("instr|teach|lead", ignore_case = TRUE))
      ~ "Instructor",
      str_detect(program, regex("cos", ignore_case = TRUE))
      ~ "Cosmetology",
      str_detect(program, regex("nail|man", ignore_case = TRUE))
      ~ "Nails",
      str_detect(program, regex("est|make|skin|eye", ignore_case = TRUE))
      ~ "Skin",
      str_detect(program, regex("barb", ignore_case = TRUE))
      ~ "Barber",
      str_detect(program, regex("hair|braid", ignore_case = TRUE))
      ~ "Hairstyling",
      TRUE ~ "Cosmetology"
    )
  ) %>%
  mutate(first_touch = first_touch) %>%
  mutate(
    lead_type = case_when(

      # Non-Digital
      is.na(first_touch) &
        `Online Source` %in% c("", "Online Source Not Set") ~ "Non-Digital",

      # Other
      `Online Source` %in% c(
        "Employment Opportunities",
        "Florida Bright Futures Scholarship",
        "HS/CareerFair",
        "Other",
        "Transfer"
      ) ~ "Other",

      # Paid - Lead Generators
      is.na(first_touch) &
        `Online Source` %in% c("BSD", "Higher Level Education") ~ "Paid - Lead Generators",

      # Paid - Social Media
      str_detect(first_touch, "/social/") |
        (is.na(first_touch) &
           `Online Source` %in% c("Barber/Cosmo Transfer Program",
                                  "Barbering Evening Program",
                                  "Facebook",
                                  "GFF - Social - Barbering",
                                  "GFF - Social - Cosmetology",
                                  "GFF - Social - Esthetics",
                                  "GFF - Social - Nails",
                                  "Kent State Partnership Social",
                                  "PPC Facebook Local Cosmetology",
                                  "PPC Facebook Local Makeup",
                                  "UDNA - Social - Southern Utah",
                                  "UDNA 3-Day Cosmetology (Facebook)",
                                  "UDNA Barber Transfer",
                                  "UDNA Barbering LookALike",
                                  "UDNA Barbering Social Ads",
                                  "UDNA Barbering Veterans",
                                  "UDNA Combined Cosmetology",
                                  "UDNA Cosmetology Multiple Schedules",
                                  "UDNA Cosmetology Social Ads",
                                  "UDNA Cosmetology TCT Dedicated Lead Funnel",
                                  "UDNA Cosmetology/Esthetics/Barbering",
                                  "UDNA Crossover Specials (Social)",
                                  "UDNA Esthetics (Makeup Emphasis)",
                                  "UDNA Esthetics 4-Day FT",
                                  "UDNA Esthetics Social Ads",
                                  "UDNA Facebook Ads",
                                  "UDNA Facebook Nails/Manicuring",
                                  "UDNA GeoFence",
                                  "UDNA High School Scholarship",
                                  "UDNA Makeup Social Ads",
                                  "UDNA Makeup Standalone Program",
                                  "UDNA Military Geofence",
                                  "UDNA Nails Social Ads",
                                  "UDNA Part Time",
                                  "UDNA Part Time Evening ",
                                  "UDNA Social Barbering TCT",
                                  "UDNA Social Cosmetology TCT ",
                                  "UDNA Social Cosmetology TCT Dedicated Lead Funnel",
                                  "UDNA Storybrand (Annie) Social Ads",
                                  "UDNA StoryBrand Barbering",
                                  "UDNA StoryBrand Cosmetology",
                                  "UDNA StoryBrand Esthetics",
                                  "UDNA Veterans",
                                  "UNDA Crossover Specials",
                                  "20th Anniversary Scholarship - PPC - FB & IG Combined",
                                  "Facebook Ad - National Scholarship",
                                  "Instagram Ad - National Scholarship",
                                  "National - NOH Fall 2019 - Facebook PPC",
                                  "National - NOH Fall 2019 - Instagram PPC",
                                  "National - NOH Fall 2020 - PPC Combined",
                                  "National - NOH Fall 2021 - PPC Combined",
                                  "National - NOH Spring 2019 - Facebook PPC",
                                  "National - NOH Spring 2019 - Instagram PPC",
                                  "National - NOH Spring 2021 - PPC Combined",
                                  "National - Social Media Campaign",
                                  "National Facebook PPC - Esthetics",
                                  "National Scholarship - PPC - Facebook & Instagram Combined"
           )) ~ "Paid - Social Media",

      # Paid - Search PPC
      str_detect(first_touch, "/ppc/|/PPC/|/directoryLanding") |
        (is.na(first_touch) &
           `Online Source` %in% c(
             "AdWords Combined Landing",
             "CombinedLandingPage",
             "GFF - PPC - Barbering",
             "GFF - PPC - Cosmetology",
             "GFF - PPC - Esthetics",
             "GFF - PPC - Nails",
             "Google Ad Words",
             "Local AdWords Manicuring/Nails",
             "Local AdWords Utah Combined",
             "Local Google AdWords - Cosmetology",
             "Local Google AdWords - Esthetics",
             "UDNA Barbering",
             "UDNA Cosmetology",
             "UDNA Cosmetology TCT",
             "UDNA Esthetics",
             "UDNA Makeup",
             "UDNA Nails/Manicuring",
             "UDNA Sacramento Combined",
             "Google Ad Words Directory",
             "Pandora 2018",
             "Paul Mitchell Directory 2019 v2"
           )
        ) ~ "Paid - Search PPC",

      # Website - High School + Events
      str_detect(first_touch, "high-school|events") |
        (is.na(first_touch) & `Online Source` == "High School Campaign") ~
        "Website - High School + Events",

      # Website - School Microsite Walk-in
      str_detect(first_touch, "/walk-in") |
        (is.na(first_touch) & `Online Source` == "School Website Walk-In Form") ~
        "Website - School Microsite Walk-in",

      # Website - School Microsite
      str_detect(first_touch, subdomains) |
        (is.na(first_touch) & `Online Source` %in% c(
          "Get Started Website Form ",
          "Kent State Partnership",
          "Live Chat",
          "Octavo-Annapolis-Apply Now",
          "Octavo-Annapolis-Request Info",
          "Octavo-Frederick-Apply Now",
          "Octavo-Frederick-Request Info",
          "Online Application",
          "School Microsite Request a Tour Form",
          "School Website - Getting Started Page",
          "School Website - Home page form",
          "School Website - Programs Page",
          "School Website Banner Form",
          "School Website Footer Form",
          "Website Home Page",
          "Events",
          "Off Site Digital Entries")) ~ "Website - School Microsite",

      # Website - Top-Level Campaign
      str_detect(first_touch, "/national-open-house") |
        (is.na(first_touch) & `Online Source` %in% c(
          "20th Anniversary Scholarship - Main Landing Page",
          "ABS Chicago",
          "Caper 15",
          "National - NOH Fall 2019 - Facebook Organic",
          "National - NOH Fall 2019 - Main Landing Page",
          "National - NOH Fall 2020 - Main Landing Page",
          "National - NOH Fall 2021 - Main Landing Page",
          "National - NOH Spring 2019 - Facebook Organic",
          "National - NOH Spring 2019 - Instagram Organic",
          "National - NOH Spring 2019 - Main Landing Page",
          "National - NOH Spring 2020 - Main Landing Page",
          "National - NOH Spring 2021 - Main Landing Page",
          "National Facebook Organic - Cosmetology",
          "National Instagram Organic - Cosmetology",
          "National Scholarship - Landing Page",
          "Preview Day 2017",
          "YouTube National Campaign"
        )) ~ "Website - Top-Level Campaign",

      # Website - Top-Level
      str_detect(first_touch, "paulmitchell.edu/|/") |
        (is.na(first_touch) & `Online Source` %in% c(
          "Corporate Website",
          "Corporate Website Banner Form",
          "Corporate Website Footer Form")) ~ "Website - Top-Level",

      # OR ELSE
      TRUE ~ "Website - Top-Level"
    )
  ) %>%
  # Change a bunch of columns to be a date type
  mutate(dplyr::across(`Created TIME`:`Start Date`, ~ lubridate::date(lubridate::ymd_hms(.x))))

usethis::use_data(crm, overwrite = TRUE)
