#' totals_by_type
#' @description Gets data by system category.
#'
#' @return a tibble
create_by_type <- function(country) {
  if (country == "Russia") {
    url <-
      russia_url
  } else {
    url <-
      ukraine_url
  }

  heads <-
    get_data(
      url,
      "article div"
    ) %>%
    rvest::html_elements("h3") %>%
    rvest::html_text2()

  # Drop the empty cell padding
  heads <- heads[nchar(heads) > 0]

  # Get the positons of the Russia and Ukraine headers
  pos <- heads %>% stringr::str_which(country) %>% as.double()

  totals <- tibble(
    country = character(),
    equipment = character(),
    destroyed = character(),
    abandoned = character(),
    captured = character(),
    damaged = character()
  )

  for (l in seq_along(heads)) {
    totals[l, "equipment"] <-
      heads[l] %>% stringr::str_remove_all(" \\(.*\\)")
    totals[l, "destroyed"] <-
      heads[l] %>% stringr::str_extract("destroyed: \\d+") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "abandoned"] <-
      heads[l] %>% stringr::str_extract("(abandoned|aboned): \\d+") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "captured"] <-
      heads[l] %>% stringr::str_extract("captured: \\d+") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "damaged"] <-
      heads[l] %>% stringr::str_extract("damaged: \\d+") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
  }


  country_df <- totals %>%
    dplyr::mutate(
      dplyr::across(destroyed:damaged, ~ as.double(tidyr::replace_na(.x, "0"))),
      type_total = destroyed + abandoned + captured + damaged,
      row_id = 1:n()
    ) %>%
    dplyr::mutate(country = tidyr::replace_na(country, !!!country)) %>%
    select(-row_id) %>%
    dplyr::mutate(
      equipment = replace(equipment, pos, "All Types"),
    ) %>%
    dplyr::rename(equipment_type = equipment)

  return(country_df)
}

totals_by_type <- function() {
  russia <- create_by_type("Russia")
  ukraine <- create_by_type("Ukraine")

  totals_df <- russia %>%
    dplyr::bind_rows(ukraine, .id=NULL)

  return(totals_df)
}



