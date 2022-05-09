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

dplyr::filter(stringr::str_detect(path, .file)) %>%

get_inputfile <- function(.file) {
  path <- fs::dir_info("inputfiles", type = "file", regexp=".file") %>%
    dplyr::filter(!stringr::str_detect(path, ".bak")) %>%
    dplyr::select(path) %>%
    dplyr::filter(stringr::str_detect(path, .file)) %>%
    dplyr::mutate(date_created = stringr::str_remove_all(fs::path_file(path), "[a-zA-Z_.]+"),
                  date_created = as.Date(date_created)) %>%
    dplyr::filter(date_created == max(date_created)) %>%
    dplyr::pull(path)

  message(path)

  # logr::put(path)

  readr::read_csv(path)
}

get_inputfile(.file="totals_by_system")
