convert_bbi_min <- function(df){
  df |>
    mutate(
      time_min = cut_time_min_interval(date_time)
    )  |>
    summarise(bbi = mean(bbi), .by = time_min) |>
    mutate(time_min = ymd_hms(time_min, truncated = 3))
}

convert_bbi_secs <- function(df){
  df |>
    mutate(
      time_secs = cut_time_secs_interval(date_time)
    )  |>
    summarise(bbi = mean(bbi), .by = time_secs) |>
    mutate(time_secs = ymd_hms(time_secs, truncated = 3))
}