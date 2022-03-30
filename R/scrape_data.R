#' scrape_data
#' @description Gets data by system.
#'
#' @return a tibble
#' @export
scrape_data <- function() {
  materiel <-
    get_data(
      "https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
      "article"
    ) %>%
    rvest::html_elements("li")

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
      url = character()
    )

  counter = 0
  for (a in seq_along(materiel)) {
    status <- materiel[[a]] %>% rvest::html_elements("a")
    for (b in seq_along(status)) {
      counter = counter + 1
      data[counter, 1] <-
        ifelse(a < country_pos[2], "Russia", "Ukraine")
      data[counter, 2] <- extract_origin(materiel, a)
      data[counter, 3] <- extract_system(materiel, a)
      data[counter, 4] <- extract_status(status, b)
      data[counter, 5] <- extract_url(status, b)
    }
  }

  data <- data %>%
    dplyr::mutate(status = stringr::str_extract_all(status, "destroyed|captured|abandoned|damaged")) %>%
    tidyr::unnest_longer(status) %>%
    dplyr::mutate(date_recorded = as.Date(lubridate::today())) %>%
    trim_all()

  previous <- readr::read_csv(glue::glue("inputfiles/totals_by_system{lubridate::today()}.csv")) %>%
    trim_all() %>%
    dplyr::mutate(date_recorded = as.Date(date_recorded))

  check <- data %>%
    dplyr::anti_join(previous, by = c("url")) %>%
    dplyr::mutate(date_recorded = as.Date(date_recorded))

  if (nrow(check) > 0) {

  data <- check %>% dplyr::bind_rows(readr::read_csv(glue::glue("inputfiles/totals_by_system{lubridate::today()}.csv"))) %>%
    dplyr::arrange(country, system, date_recorded)

  previous %>%  readr::write_csv("inputfiles/totals_by_system.csv.bak")

  data %>% readr::write_csv(glue::glue("inputfiles/totals_by_system{lubridate::today()+1}.csv"))

  } else {
    logr::put("No new data")
    data <- previous
  }

  data <- create_keys(data) %>%
    dplyr::group_by(matID) %>%
    dplyr::filter(date_recorded == min(date_recorded)) %>%
    dplyr::ungroup()

  return(data)

}

total_by_system_wide <- function(indsn) {
  indsn %>% dplyr::select(country, system, status) %>%
    dplyr::group_by(country, system, status) %>%
    dplyr::summarise(count = n()) %>%
    tidyr::pivot_wider(names_from = status, values_from = count) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)),
                  total = destroyed + captured + damaged + abandoned)
}
