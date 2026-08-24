convert_sleep_min_row <- function(date_time, duration, type){
  duration_min <- duration / 1000 / 60
  df <- tibble(
    time_min = date_time + dminutes(0:(duration_min-1)),
    type = type
  )
  df
}
convert_sleep_min <- function(sleep){
  sleep |> select(date_time, duration = durationInMs, type) |>
    pmap(convert_sleep_min_row) |>
    list_rbind()
  
}