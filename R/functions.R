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
    stringr::str_remove_all("[:punct:]") %>%
    stringr::str_replace_all("and", "") %>%
    stringr::str_remove_all("\\d+") %>%
    stringr::str_remove_all("^\\s+") %>%
    stringr::str_replace_all("aboned", "abandoned") %>%
    stringr::str_replace_all("sunk|scuttled", "destroyed")
}

extract_url <- function(indsn, x) {
  indsn[[x]] %>%
    rvest::html_attr("href")
}

#' create_keys
#' @description creates the surrogate keys `sysID`, `imageID`, `matID`, and `eventID`
#'
#' @param indsn a dataframe with columns `country`, `system`, `url`, and `status`.
#'
#' @return
#' @export
#'
#' @examples
create_keys <- function(indsn) {
  indsn <- indsn %>% dplyr::ungroup()

  sysID <- indsn %>%
    dplyr::distinct(system) %>%
    dplyr::mutate(sysID = dplyr::row_number())

  indsn <- indsn %>%
    dplyr::left_join(sysID)

  imageID <- indsn %>%
    dplyr::distinct(url) %>%
    dplyr::mutate(imageID = dplyr::row_number())

  indsn <- indsn %>%
    dplyr::left_join(imageID)

  matID <-
    indsn %>% dplyr::mutate(matID = glue::glue("{sysID}{imageID}")) %>%
    dplyr::mutate(matID = as.character(matID))

  indsn <- indsn %>%
    dplyr::left_join(matID)

  statusID <-
    indsn %>%
    dplyr::distinct(matID, status, .keep_all = TRUE) %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(
      statusID = dplyr::case_when(
        country == "Russia" ~ glue::glue("7{matID}{dplyr::cur_group_rows()}"),
        country == "Ukraine" ~ glue::glue("380{matID}{dplyr::cur_group_rows()}")
      )
    ) %>%
    dplyr::mutate(statusID = as.character(statusID)) %>%
    dplyr::ungroup()

  indsn <- indsn %>%
    dplyr::left_join(statusID, by=c("Country", "MatID")) %>%
    dplyr::arrange(country, sysID)

  return(indsn)
}
