#' totals_by_Type
#' @description Gets data by system category.
#'
#' @return a tibble
totals_by_type <- function() {
  heads <-
    get_data(
      "https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html",
      "article div"
    ) %>%
    rvest::html_elements("h3") %>%
    rvest::html_text2()

  # Drop the empty cell padding
  heads <- heads[nchar(heads) > 0]

  # Get the positons of the Russia and Ukraine headers
  rus_pos <- heads %>% stringr::str_which("Russia") %>% as.double()
  ukr_pos <- heads %>% stringr::str_which("Ukraine") %>% as.double()

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
      heads[l] %>% stringr::str_extract("destroyed: ...") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "abandoned"] <-
      heads[l] %>% stringr::str_extract("(abandoned|aboned): ...") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "captured"] <-
      heads[l] %>% stringr::str_extract("captured: ...") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
    totals[l, "damaged"] <-
      heads[l] %>% stringr::str_extract("damaged: ...") %>%
      stringr::str_remove_all("[:alpha:]|[:punct:]")
  }

  totals_df <- totals %>%
    dplyr::mutate(
      dplyr::across(destroyed:damaged, ~ as.double(tidyr::replace_na(.x, "0"))),
      type_total = destroyed + abandoned + captured + damaged,
      row_id = 1:n(),
      country = dplyr::case_when(row_id < ukr_pos ~ "Russia",
                                 row_id >= ukr_pos ~ "Ukraine")
    ) %>%
    select(-row_id) %>%
    dplyr::mutate(
      equipment = replace(equipment, rus_pos, "All Types"),
      equipment = replace(equipment, ukr_pos, "All Types")
    ) %>%
    dplyr::rename(equipment_type = equipment)

  return(totals_df)
}
