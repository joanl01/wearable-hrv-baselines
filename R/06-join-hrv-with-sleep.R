# Load packages
pacman::p_load(readr, tidyverse, gt, fs, here, purrr, scales, lubridate, caTools, pROC)

# Read all functions
fs::dir_ls(here::here("functions")) |> walk(source)

# Load HRV data
hrvs <- readRDS(here::here("data", "hrv_garmin-non-linear.rds"))

# Sleep file names and matching participant IDs
participant_ids <- c("FLARE001", "FLARE002", "FLARE003", 
                     "FLARE005", "FLARE006", "FLARE007", 
                     "FLARE008", "FLARE010", "FLARE011")

sleep_paths <- paste0(
  "data/", participant_ids, "_", 
  c("HJ", "DC", "TM", "JT", "JA", "JH", "AP", "KL", "JS"), 
  "_data/garmin-connect-sleep-stage/000000_garmin-connect-sleep-stage_",
  c("FLARE001-HJ_75ff26b5.csv", "FLARE002-DC_286e12ca.csv", "FLARE003-TM_b0ab838e.csv", 
    "FLARE005-JT_c93a96a0.csv", "FLARE006-JA_658375b2.csv", "FLARE007-JH_2528da08.csv", 
    "FLARE008-AP_227592d9.csv", "FLARE010-KL_95e1c8ef.csv", "FLARE011-JS_318181f0.csv")
)

sleep_paths <- here::here(sleep_paths)

# Read sleep data, name by participant
sleep_data <- map(sleep_paths, read_sleep)
sleep_data <- map(sleep_data, na.omit)
names(sleep_data) <- participant_ids


# Join each participant's HRV with sleep
hrv_with_sleep_all <- map2_dfr(participant_ids, sleep_data, function(pid, sleep_df) {
  
  # Subset HRV data for this participant
  hrv_df <- hrvs %>% filter(str_detect(file, pid))
  
  # Expand sleep_df to per-second resolution
  sleep_expanded <- sleep_df %>%
    mutate(
      start_time = date_time,
      end_time = date_time + milliseconds(duration)
    ) %>%
    rowwise() %>%
    do({
      seq(
        from = .$start_time,
        to = .$end_time - seconds(1),
        by = "1 sec"
      ) |> 
        as_tibble() |>
        mutate(type = .$type)
    }) %>%
    rename(timestamp = value)
  
  # Bin to 5-minute intervals
  sleep_binned <- sleep_expanded %>%
    mutate(bin_time = floor_date(timestamp, "5 minutes")) %>%
    group_by(bin_time, type) %>%
    summarise(seconds_in_stage = n(), .groups = "drop") %>%
    group_by(bin_time) %>%
    slice_max(order_by = seconds_in_stage, n = 1, with_ties = FALSE) %>%  # pick dominant stage
    ungroup()
  
  # Join with HRV data
  hrv_labeled <- hrv_df %>%
    left_join(sleep_binned, by = c("min" = "bin_time")) %>%
    mutate(
      type.y = fct_na_value_to_level(type.y, "presumed awake"),
      date = as.Date(min),
      participant = pid
    )
  
  return(hrv_labeled)
})

write_rds(hrv_with_sleep_all, file = here::here("data", "hrv_with_sleep_joined.rds"))

