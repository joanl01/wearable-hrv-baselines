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
  select(participant, target_time, timeline, HR, HF, ULF) %>%
  drop_na()


validation_df <- validation_df %>%
  mutate(
    rest_state = factor(case_when(
      timeline %in% c("deep sleep","first moment before wake") ~ "rest",
      TRUE ~ "non_rest"
    )))

validation_df |> mutate("scaled HR" = scale(HR)[,1], 
                        "scaled HF" = scale(HF)[,1],
                        "scaled ULF" = scale(ULF)[,1])
glmer(rest_state ~ HR + HF + ULF + (1|participant) ,data = validation_df, family = binomial)


validation_df$HR_z  <- scale(validation_df$HR)[,1]
validation_df$HF_z  <- scale(validation_df$HF)[,1]
validation_df$ULF_z <- scale(validation_df$ULF)[,1]

model <- glmer(
  rest_state ~ HR_z + HF_z + ULF_z + (1 | participant),
  data = validation_df,
  family = binomial
)



# Model summary
summary(model)

# Odds ratios with CI
results <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)

# broom.mixed::tidy(model,conf.int = TRUE)
results$OR  <- exp(results$estimate)
results$CI_low  <- exp(results$conf.low)
results$CI_high <- exp(results$conf.high)
fixed_tab <- results |> select(-"effect") |> gt() |> 
  cols_label(term = "Term", 
             estimate = "Estimated Coefficients",
             std.error = "Standardised Error",
             statistic = "Test Statistic", 
             p.value = "p-value",
             conf.low = "Confidence Interval: Lower Bound",
             conf.high = "Confidence Interval: Higher Bound") |> 
  text_case_match(
    "HR_z" ~ "scaled HR",
    "HF_z" ~ "scaled HF",
    "ULF_z" ~ "scaled ULF"
  ) |>
  fmt_number(
    decimals = 4
  )

fixed_tab  

# compute_icc <- function(data, outcome) {
#   formula <- as.formula(paste0(outcome, " ~ 1 + (1 | participant)"))
#   model <- lmer(formula, data = data)
#   
#   var_comp <- as.data.frame(VarCorr(model))
#   
#   between_var <- var_comp$vcov[var_comp$grp == "participant"]
#   within_var  <- attr(VarCorr(model), "sc")^2
#   
#   icc <- between_var / (between_var + within_var)
#   return(icc)
# }

icc_results <- tibble(
  metric = c("HR", "HF", "ULF"),
  ICC = c(
    compute_icc(validation_df |> filter(rest_state == "non_rest"), "HR"),
    compute_icc(validation_df |> filter(rest_state == "non_rest"), "HF"),
    compute_icc(validation_df |> filter(rest_state == "non_rest"), "ULF")
  )
)

icc_results

icc_by_timeline_rest <- validation_df %>%
  group_by(timeline) %>%
  summarise(
    ICC_HR  = compute_icc(cur_data(), "HR"),
    ICC_HF  = compute_icc(cur_data(), "HF"),
    ICC_ULF = compute_icc(cur_data(), "ULF")
  )

icc_by_timeline_results <- icc_by_timeline_rest |> gt()|>
  fmt_number(
    decimals = 4
  )

icc_by_timeline_results
