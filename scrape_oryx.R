#!/usr/bin/env Rscript
#' @title scrape_oryx
#' @description A simple R script for extracting tabular data from Oryx' excellent
#'   post detailing materiel lost by all sides in the [Russian invasion of
#'   Ukraine](https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html).
#'
#'
#' @author Daniel Scarnecchia
#'

# Setup
library(renv)
renv::restore()
library(rvest)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(magrittr)
library(tibble)
library(stringr)
library(readr)
library(glue)
library(logr)
library(ggplot2)
library(scales)
library(ggthemes)
source("R/functions.R")
source("R/scrape_data.R")
source("R/totals_by_type.R")
source("R/per_event.R")
source("R/daily_count.R")

tmp <-
  file.path("outputfiles", sprintf("scrape_oryx_%s.log", format(Sys.time(), "%Y%m%dT%H%M%S")))
lf <- logr::log_open(tmp)
today <- format(Sys.Date(), "%Y-%m-%d")


totals_by_system <- scrape_data() %>%
  readr::write_csv(., file = glue::glue("outputfiles/totals_by_system_{today}.csv"))

#' Write Event Tables
create_event_tables(totals_by_system, status)

totals_by_system_wide <- total_by_system_wide(totals_by_system) %>%
  readr::write_csv(.,
                   file = glue::glue("outputfiles/totals_by_system_wide_{today}.csv"))

total_by_type <- totals_by_type() %>%
  readr::write_csv(., file = glue::glue("outputfiles/totals_by_type_{today}.csv"))

daily_count <- daily_count() %>%
  readr::write_csv(., file = "outputfiles/daily_count.csv")

knitr::knit("index.Rmd")

logr::log_code()
logr::log_close()

writeLines(readLines(lf))
