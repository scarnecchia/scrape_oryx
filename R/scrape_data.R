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
      data[counter, 5] <- extract_url(status, b)
    }
  }

  return(data)
}
