create_event_tables <- function(indsn, ...) {
  x <- indsn %>% dplyr::group_by(...) %>%
    {
      setNames(group_split(.), group_keys(.)[[1]])
    }

  x %>% names(.) %>%
    purrr::map( ~ write_csv(x[[.]], glue::glue("outputfiles/event_{.}_{today}.csv")))
}
