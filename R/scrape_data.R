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
      matID = character(),
      url = character(),
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
      data[counter, 6] <- extract_url(status, b)
    }
  }

  data <- data %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(
      matID = dplyr::case_when(
        country == "Russia" ~ glue::glue("7{dplyr::cur_group_rows()}"),
        country == "Ukraine" ~ glue::glue("380{dplyr::cur_group_rows()}")
      )
    ) %>%
    dplyr::mutate(matID = as.numeric(matID)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(status = stringr::str_extract_all(status, "destroyed|captured|abandoned|damaged")) %>%
    tidyr::unnest_longer(status)

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
