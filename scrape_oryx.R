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


tmp <- file.path("outputfiles", sprintf("scrape_oryx_%s.log", format(Sys.time(), "%Y%m%dT%H%M%S")))
lf <- log_open(tmp)
today <- format(Sys.Date(), "%Y-%m-%d")

oryx <- rvest::read_html(
  "https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html"
) %>% rvest::html_elements("article")

materiel <- oryx %>% rvest::html_elements("li")

# Retreive the start position of each country
country_pos <- materiel %>% rvest::html_text2() %>%
  # T-64BV is the first row in the tank list and marks the beginning of each country
  stringr::str_which("T-64BV")

#' Run Program
data <-
  tibble::tibble(
    country = character(),
    origin = character(),
    system = character(),
    status = character(),
    url = character(),
  )

counter = 0
for (a in seq_along(materiel)) {
  status <- materiel[[a]] %>% rvest::html_elements("a")
  for (b in seq_along(status)) {
    counter = counter + 1
    data[counter, 1] <- ifelse(a < country_pos[2], "Russia", "Ukraine")
    data[counter, 2] <- extract_origin(materiel, a)
    data[counter, 3] <- extract_system(materiel, a)
    data[counter, 4] <- extract_status(status, b)
    data[counter, 5] <- extract_url(status, b)
  }
}

readr::write_csv(data, file=glue::glue("outputfiles/data_{today}.csv"))

data_wide <- data %>%
  dplyr::select(country, system, status) %>%
  dplyr::group_by(country, system, status) %>%
  dplyr::summarise(count = n())

readr::write_csv(data_wide, file=glue::glue("outputfiles/data_wide_{today}.csv"))

logr::log_code()
logr::log_close()

writeLines(readLines(lf))
