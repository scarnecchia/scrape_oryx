#' @title scrape_oryx
#' @description A simple R script for extracting tabular data from Oryx' excellent
#'   post detailing materiel lost by all sides in the [Russian invasion of
#'   Ukraine](https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html).
#'
#'
#' @author Daniel Scarnecchia
#'

#' Setup
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
    data[counter, 1] <- ifelse(a < 138, "Russia", "Ukraine")
    data[counter, 2] <- extract_origin(materiel, b)
    data[counter, 3] <- extract_system(materiel, b)
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

## This code uses a different approach to scrapping the data. It is in some ways less accurate, but hints at a method for data validation.

# data <-
#   tibble::tibble(
#     country = character(),
#     origin = character(),
#     system = character(),
#     captured = numeric(),
#     damaged = numeric(),
#     destroyed = numeric(),
#     total = numeric()
#   )
#
# for (i in seq_along(materiel)) {
#   data[i, 2] <- extract_origin(materiel, i)
#   data[i, 3] <- extract_system(materiel, i)
#   data[i, 4] <- extract_counts(materiel, i, "captured")
#   data[i, 5] <- extract_counts(materiel, i, "damaged")
#   data[i, 6] <- extract_counts(materiel, i, "destroyed")
#   data[i, 7] <- extract_total(materiel, i)
# }

logr::log_code()
logr::log_close()

writeLines(readLines(lf))
