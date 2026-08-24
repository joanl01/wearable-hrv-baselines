read_sleep <- function(csv){
  # Read in CSV
  df <- read_csv(csv, skip = 5)
  # Convert data
  df <- df |>
    mutate(
      date_time = ymd_hms(isoDate)
    )
  # df <- df |>
  #   select(date_time, duration = durationInMs, type)
  df
}