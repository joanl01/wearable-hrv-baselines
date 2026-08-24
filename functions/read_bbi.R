read_bbi <- function(file){
  # Read in
  df <- read_csv(file, skip = 5)
  # Convert date time
  df <- df |>
    mutate(
      date_time = ymd_hms(isoDate)
    )
  # Select columns
  # df <- df |>
  #   select(date_time, bbi,unixTimestampInMs)
  df
}
read_bbis <- function(folder){
  fs::dir_ls(folder) |>
    map(read_bbi) |>
    list_rbind()
}