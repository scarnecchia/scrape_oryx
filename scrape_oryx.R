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
source("R/functions.R")
source("R/scrape_data.R")
source("R/totals_by_type.R")

tmp <-
  file.path("outputfiles", sprintf("scrape_oryx_%s.log", format(Sys.time(), "%Y%m%dT%H%M%S")))
lf <- logr::log_open(tmp)
today <- format(Sys.Date(), "%Y-%m-%d")

totals_by_system <- scrape_data() %>%
  readr::write_csv(., file = glue::glue("outputfiles/totals_by_system_{today}.csv"))

totals_by_system_wide <- totals_by_system %>%
  dplyr::select(country, system, status) %>%
  dplyr::group_by(country, system, status) %>%
  dplyr::summarise(count = n()) %>%
  tidyr::pivot_wider(names_from=status, values_from=count) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)),
  total = destroyed + captured + damaged + abandoned) %>%
  readr::write_csv(.,
                   file = glue::glue("outputfiles/totals_by_system_wide_{today}.csv"))

total_by_type <- totals_by_type() %>%
  readr::write_csv(., file = glue::glue("outputfiles/totals_by_type_{today}.csv"))

logr::log_code()
logr::log_close()

writeLines(readLines(lf))
