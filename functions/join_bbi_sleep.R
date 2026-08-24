join_bbi_sleep <- function(bbi, sleep){
  bbi |>
    full_join(sleep, by = "time_min") |>
    mutate(
      date = as_date(time_min)
    )
}