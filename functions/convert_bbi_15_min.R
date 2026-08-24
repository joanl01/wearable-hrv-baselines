convert_bbi_15_min <- function(df, tz = "Australia/Brisbane"){
  df |>
    mutate(
      time_15_min = cut_time_15min_interval(date_time)
    ) |>
    group_by(time_15_min) |>
    reframe(bbi = mean(bbi))  |>
    mutate(
      time_15_min = ymd_hms(time_15_min, truncated = 3, tz = tz),
      date = as_date(time_15_min)
    )
}