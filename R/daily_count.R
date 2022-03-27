daily_count <- function() {
  baseline <- readr::read_csv("inputfiles/daily_count_baseline.csv")

  today_total <- totals_by_type() %>%
    dplyr::mutate(date_recorded = as.Date(today()))

  baseline %>%
    dplyr::bind_rows(today_total) %>%
    dplyr::group_by(country, equipment_type) %>%
    dplyr::arrange(country, equipment_type, date_recorded) %>%
    readr::write_csv("inputfiles/daily_count_baseline.csv")

  running_count <- baseline %>%
    dplyr::bind_rows(today_total) %>%
    dplyr::group_by(country, equipment_type) %>%
    dplyr::arrange(country, equipment_type, date_recorded) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ .x - dplyr::lag(.x), .names =
                                  "{.col}_diff")) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(.x, 0)))

  return(running_count)
}


graph_counts <- function(indsn, type_id, count_type) {
  data <- indsn %>%
    dplyr::filter(equipment_type == type_id) %>%
    dplyr::rename(count = count_type) %>%
    dplyr::select(country, equipment_type, count, date_recorded) %>%
    dplyr::arrange(date_recorded)

  g <-
    ggplot2::ggplot(data, ggplot2::aes(date_recorded, count, colour = country)) + ggplot2::geom_line() +
    ggplot2::scale_x_date(
      "Date",
      date_breaks = "3 days",
      date_minor_breaks = "1 day",
      date_labels = "%d %b %Y"
    ) + ggplot2::scale_y_continuous("Equipment Losses", breaks = scales::pretty_breaks()) +
    ggthemes::geom_rangeframe() + ggthemes::theme_few() + ggplot2::guides(color =
                                                                            ggplot2::guide_legend(title = "Country")) +
    ggplot2::labs(
      title = glue::glue(
        "Total Equipment Losses Through {format(lubridate::today(), '%B %d, %Y')}"
      ),
      subtitle = glue::glue("Equipement Type: {type_id}"),
      caption = "Data From: https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust =
                                                         1))
  return(g)
}
