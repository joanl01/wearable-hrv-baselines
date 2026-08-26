# Load packages
pacman::p_load(
  readr, tidyverse, gt, fs, here, purrr, scales, lubridate, caTools,
  tidymodels, GGally, patchwork, tidytext, harrypotter
)

# Read all functions
fs::dir_ls(here::here("functions")) |> walk(source)

# Load data
hrv_data <- read_rds(
  here::here("data", "hrv_with_sleep_joined.rds")
)

# Get all participants
participants <- sort(unique(hrv_data$participant))

participants


pca_results <- participants |>
  map(
    ~ run_pca_participant(
      participant_id = .x,
      data = hrv_data
    )
  ) |>
  set_names(participants)



