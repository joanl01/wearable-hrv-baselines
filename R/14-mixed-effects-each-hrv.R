# Load packages
pacman::p_load(readr, tidyverse, gt, tidymodels, corrplot, vip, DALEXtra, GGally, plotly,ggdendro, lme4)
# Read all functions
fs::dir_ls(here::here("functions")) |> walk(source)

hrvs <- read_rds(here::here("data", "hrv_with_sleep_joined.rds")) |> select(-seconds_in_stage)
stress_hrv <- read_rds(here::here("data", "hrv_with_sleep_and_stress.rds"))|> select(-seconds_in_stage)

# input combinations
timeline <- c("first moment before wake","first moment after wake", "deep sleep", "awake")
days <- c(5)

# Create combinations and name the columns
all_combinations <- expand.grid(
  timeline = timeline,
  days = days,
  stringsAsFactors = FALSE
)


# Apply the function to each row of combinations
artifacts_combined_results <- purrr::pmap_dfr(
  all_combinations,
  function(timeline, days, metric) {
    result <- summarise_hrv_timeline_by_artifact_no_summarise(stress_hrv, timeline = timeline, days = days)
    result |> 
      mutate(timeline = timeline, days = factor(days))
  }
)

# Select HR, ULF, HF
# Clean modelling dataset
validation_df <- artifacts_combined_results %>%
  select(participant, timeline, HR, HF, HRV) %>%
  drop_na()


# Mixed Effects model
m_hr <- lmer(HR ~ timeline + (1|participant), data = validation_df)
m_hf <- lmer(HF ~ timeline + (1|participant), data = validation_df)
m_hrv <- lmer(HRV ~ timeline + (1|participant), data = validation_df)

# Get summary 
summary(m_hr)
summary(m_hf)
summary(m_hrv)





