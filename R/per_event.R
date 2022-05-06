#' create_event_tables
#' @description splits a dataset. Column to split dataset by can be passed to function through additional arguments
#'
#' @param indsn A dataset name
#' @param ...
#'
#' @return a csv file.
#' @export
create_event_tables <- function(indsn, ...) {
  idnsn <- indsn %>% dplyr::ungroup()

  x <- indsn %>%
    dplyr::group_by(...) %>%
    {
      setNames(dplyr::group_split(.), dplyr::group_keys(.)[[1]])
    }

  x %>%
    names(.) %>%
    purrr::map(~ write_csv(x[[.]], glue::glue("outputfiles/event_{.}.csv")))
}
