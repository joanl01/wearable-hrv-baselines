get_mode <- function(x) {
  df <- tibble(x = x)
  df |> count(x) |> filter(n == max(n)) |>
    pull(x)
}
convert_bbi_sleep_15_min <- function(df, tz = "Australia/Brisbane"){
  df |>
    mutate(
      time_15_min = cut_time_15min_interval(time_min)
    ) |>
    group_by(time_15_min) |>
    filter(!is.na(type)) |>
    reframe(bbi = mean(bbi), type = get_mode(type))  |>
    mutate(
      time_15_min = ymd_hms(time_15_min, truncated = 3, tz = tz),
      date = as_date(time_15_min)
    )
}