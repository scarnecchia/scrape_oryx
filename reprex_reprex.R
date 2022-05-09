#' ---
#' output: reprex::reprex_document
#' ---

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
library(fs)

get_inputfile <- function(.file) {
  path <- fs::dir_info("inputfiles", type = "file", regexp=".file") %>%
    dplyr::filter(!stringr::str_detect(path, ".bak")) %>%
    dplyr::select(path, change_time, birth_time) %>%
    dplyr::filter(stringr::str_detect(path, .file)) %>%
    dplyr::filter(birth_time == max(birth_time)) %>%
    dplyr::pull(path)

  message(path)

  # logr::put(path)

  readr::read_csv(path)
}

get_inputfile(.file="totals_by_system")
