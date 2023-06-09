# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "date_select", with_test = TRUE) # Name of the module
golem::add_module(name = "general_select", with_test = TRUE) # Name of the module
golem::add_module(name = "crm_metric_select", with_test = TRUE)
golem::add_module(name = "leads_overview_tab", with_test = TRUE) # Name of the module
golem::add_module(name = "conversions_table", with_test = TRUE)
golem::add_module(name = "conversions_table_PMAE", with_test = TRUE)
golem::add_module(name = "monthly_leads_graph", with_test = TRUE)
golem::add_module(name = "quarterly_metrics_graph", with_test = TRUE)
golem::add_module(name = "yearly_metrics_graph", with_test = TRUE)
golem::add_module(name = "school_comp_graph", with_test = TRUE)
golem::add_module(name = "school_comp_tab", with_test = TRUE)
golem::add_module(name = "school_comp_table", with_test = TRUE)
golem::add_module(name = "school_comp_table_PMAE", with_test = TRUE)
golem::add_module(name = "roi_tab", with_test = TRUE)
golem::add_module(name = "roi_graph", with_test = TRUE)
golem::add_module(name = "start_date_tab", with_test = TRUE)
golem::add_module(name = "start_date_table", with_test = TRUE)
golem::add_module(name = "freedom_executive_tab", with_test = TRUE)
golem::add_module(name = "attendance_by_school_graph", with_test = TRUE)
golem::add_module(name = "hours_at_drop_graph", with_test = TRUE)
golem::add_module(name = "active_table", with_test = TRUE)
golem::add_module(name = "scorecard_table", with_test = TRUE)
golem::add_module(name = "freedom_school_tab", with_test = TRUE)
golem::add_module(name = "school_attendance_graphs", with_test = TRUE)
golem::add_module(name = "ad_hoc_boxes", with_test = TRUE)
golem::add_module(name = "ad_hoc_table", with_test = TRUE)
golem::add_module(name = "meevo_executive_tab", with_test = TRUE)
golem::add_module(name = "meevo_school_tab", with_test = TRUE)
golem::add_module(name = "service_sales_by_school_graph", with_test = TRUE)
golem::add_module(name = "takehome_by_school_graph", with_test = TRUE)
golem::add_module(name = "metrics_over_time_graph", with_test = TRUE)
golem::add_module(name = "meevo_metrics_table", with_test = TRUE)
golem::add_module(name = "service_sales_overtime_graph", with_test = TRUE)
golem::add_module(name = "takehome_overtime_graph", with_test = TRUE)
golem::add_module(name = "learning_leader_tab", with_test = TRUE)
golem::add_module(name = "ll_rebooking_tab", with_test = TRUE)
golem::add_module(name = "ll_takehome_tab", with_test = TRUE)
golem::add_module(name = "ll_services_tab", with_test = TRUE)




## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = TRUE)
golem::add_utils("helpers", with_test = TRUE)
golem::add_fct("crm_conversions", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "crm", open = FALSE)
usethis::use_data_raw(name = "attendance", open = FALSE)
usethis::use_data_raw(name = "ad_hoc", open = FALSE)
usethis::use_data_raw(name = "scorecard", open = FALSE)
usethis::use_data_raw(name = "meevo", open = FALSE)
usethis::use_data_raw(name = "learning_leader", open = FALSE)
usethis::use_data_raw(name = "school_codes", open = FALSE)
usethis::use_data_raw(name = "revised_program", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("ExecDashv2")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

# Packages
usethis::use_package("bs4Dash")
usethis::use_package("tidyverse")
usethis::use_package("gt")
usethis::use_package("ggiraph")
usethis::use_package("scales")
usethis::use_package("tidyverse", type = "depends")
usethis::use_package("shinyWidgets")
usethis::use_package("bs4Dash")
usethis::use_package("gt")
usethis::use_package("bslib")
usethis::use_package("scales")
usethis::use_package("PupillometryR")
usethis::use_package("ggpp")
usethis::use_package("DT")
usethis::use_package("lubridate")
usethis::use_package("spelling")
usethis::use_package("tcltk")
usethis::use_package("odbc")
usethis::use_package("AzureStor")
usethis::use_package("data.table")
usethis::use_package("polished")
usethis::use_package("ggtext")
