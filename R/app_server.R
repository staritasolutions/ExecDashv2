#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import odbc
#' @import AzureStor
#' @import polished
#' @noRd
#'
app_server <- function(input, output, session) {
  # Your application server logic



  azure_db_connection <- dbConnect(drv = odbc::odbc(),
                             driver = "/opt/microsoft/msodbcsql18/lib64/libmsodbcsql-18.2.so.2.1",
                             server = "ss-solutions-server.database.windows.net",
                             database = "CorporateScorecardData",
                             uid = "staritasolutions",
                             pwd = "jje&2023FTW")

  attendance_df_raw <- azure_db_connection %>% tbl("ScorecardAttendance") %>% filter(KissID %in% c("5514", "1680", "4800", "7394" ,"5810", "7631" ,"5502")) %>% collect()
  dbDisconnect(azure_db_connection)
  attendance <- attendance_df_raw %>%
    mutate(BeginningDate = mdy(BeginningDate),
           EndDate = mdy(EndDate)) %>%
    rename(Beginning_Date = BeginningDate,
           End_Date = EndDate,
           Kiss_ID = KissID,
           Actual_Hours = ActualHours,
           Scheduled_Hours = ScheduledHours) %>%
    select(-(id)) %>% left_join(school_codes, by = c("Kiss_ID" = "kiss_id")) %>%
    left_join(program, by = "Program")

  #get storage endpoint
  endp <- storage_endpoint("https://ersdata.blob.core.windows.net",
                           key = "Nc6Avd2msMkfsLFTGMO24YT94BcYjffF0Pw/AnVZnmcS2fJqthqeEMnbjcMfeqQZwPbWLhoSsT/B+AStTjmBjQ==")
  #get specific container
  cont <- storage_container(endp, "ersdata")

  # Get proper adhoc file

  # store temp file from that container
  fname1 <- tempfile()
  storage_download(cont, "meevo/full_meevo.csv", fname1, overwrite = TRUE)

  meevo <- data.table::fread(fname1) %>%
    filter(School %in% c("Boise", "Nampa", "Provo", "Richland", "Spokane", "CDA", "Twin Falls")) %>%
    mutate(Date = as.Date(Date))

  fname2 <- tempfile()
  storage_download(cont, "meevo/north_star_ll.csv", fname2, overwrite = TRUE)

  learning_leader <- data.table::fread(fname2)


  fname3 <- tempfile()
  storage_download(cont, "crm/northstar_crm.csv", fname3)

  crm <- data.table::fread(fname3)

  mod_leads_overview_tab_server("leads_overview", crm)

  mod_school_comp_tab_server("school_comparison", crm)

  mod_roi_tab_server("roi", crm)

  mod_start_date_tab_server("startdates", crm)

  mod_freedom_executive_tab_server("freedom_executive", attendance)

  mod_freedom_school_tab_server("freedom_school", attendance)

  mod_meevo_executive_tab_server("meevo_executive", meevo)

  mod_meevo_school_tab_server("meevo_school", meevo)

  mod_ll_rebooking_tab_server("rebooking", learning_leader)

  mod_ll_takehome_tab_server("takehome")

  mod_ll_services_tab_server("services")

  observe(print(input$sidebarmenu))
  observe(print(input$card_monthlyleads$maximized))

}

