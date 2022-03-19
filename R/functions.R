get_data <- function(url, elements) {
  rvest::read_html(url) %>% rvest::html_elements(elements)
}

extract_total <- function(indsn, x) {
  total <- indsn[[x]] %>% rvest::html_text2() %>%
    stringr::str_extract("\\d+(?= \\b)") %>%
    readr::parse_double()
}

extract_origin <- function(indsn, x) {
  indsn[[x]] %>% rvest::html_element("img") %>%
    rvest::html_attr("src") %>%
    stringr::str_extract("(Flag_of_the_|Flag_of_)([a-zA-Z_]+|[a-zA-Z]+)") %>%
    stringr::str_remove("(Flag_of_the_|Flag_of_)") %>%
    stringr::str_replace_all("_", " ")
}

extract_counts <- function(indsn, x, condition) {
  counts <- indsn[[x]] %>% rvest::html_text2() %>%
    stringr::str_remove_all(".*(?=:)") %>%
    stringr::str_remove_all(": ") %>%
    stringr::str_remove_all("\\(") %>%
    stringr::str_remove_all("\\)") %>%
    stringr::str_remove_all("and")

  if (condition == "captured") {
    counts <- counts %>% stringr::str_extract_all("\\d+(?=, captured)")
  } else if (condition == "damaged") {
    counts <- counts %>% stringr::str_extract_all("\\d+(?=, damaged)")
  } else if (condition == "destroyed") {
    counts <- counts %>% stringr::str_extract_all("\\d+(?=, destroyed)")
  } else {
    print("Invalid Condition")
  }

  counts <- unlist(counts)
  return(length(counts))
}

extract_system <- function(indsn, x) {
  indsn[[x]] %>% rvest::html_text2() %>%
    stringr::str_remove_all("^\\d+ ") %>%
    stringr::str_extract(".*(?=:)")
}

extract_status <- function(indsn, x) {
  indsn[[x]] %>%
    rvest::html_text2() %>%
    stringr::str_remove_all("\\(") %>%
    stringr::str_remove_all("\\)") %>%
    stringr::str_remove_all("\\d+") %>%
    stringr::str_extract("destroyed|captured|abandoned|damaged|aboned")
}

extract_url <- function(indsn, x) {
  indsn[[x]] %>%
    rvest::html_attr("href")
}
