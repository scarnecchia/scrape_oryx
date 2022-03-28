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


trim_all <- function(indsn) {
  indsn %>% dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(tidyr::everything(), ~ stringr::str_trim(.,)))
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

  statusID <- indsn %>%
    dplyr::distinct(status) %>%
    dplyr::mutate(statusID = dplyr::row_number())

  indsn <- indsn %>%
     dplyr::left_join(statusID)

  matID <- indsn %>%
    dplyr::distinct(country, sysID, imageID, statusID) %>%
    dplyr::mutate(matID = dplyr::case_when(
      country == "Russia" ~ glue::glue("7-{sysID}{imageID}{statusID}"),
      country == "Ukraine" ~ glue::glue("380-{sysID}{imageID}{statusID}")
    ))

  indsn <- indsn %>%
    dplyr::left_join(matID)

  return(indsn)
}
