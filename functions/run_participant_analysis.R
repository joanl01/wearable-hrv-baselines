run_participant_analysis <- function(participant_id, data, features) {
  
  message("Running participant: ", participant_id)
  

# get participant data  
  participant_data <- data %>%
    filter(participant == participant_id)
  

# find last light/rem
  
  sleep_transition_data <- participant_data %>%
    filter(type.y %in% c("rem", "light")) %>%
    group_by(date) %>%
    filter(min == max(min)) %>%
    ungroup() %>%
    rename(target_time = min)
  
  
  # If no sleep transition data exists, stop this participant
  if (nrow(sleep_transition_data) == 0) {
    message("  No REM/light transition data. Skipping.")
    return(NULL)
  }
  
  
# find sleep windows +15min and -15 min
  
  sleep_windows <- sleep_transition_data %>%
    mutate(
      window_start = target_time - minutes(15),
      window_end   = target_time + minutes(15)
    ) %>%
    select(participant, date, window_start, window_end)
  
  
# find all HRV within window
  
  sleep_transition_data_all <- participant_data %>%
    inner_join(
      sleep_windows,
      by = c("participant", "date")
    ) %>%
    filter(
      min >= window_start,
      min <= window_end
    )
  
  
  # Check that there is enough data
  if (nrow(sleep_transition_data_all) < 10) {
    message("  Too few observations. Skipping.")
    return(NULL)
  }
  
  
# create asleep 
  
  sleep_transition_data_all <- sleep_transition_data_all %>%
    mutate(
      asleep = ifelse(
        type.y %in% c("light", "deep", "rem"),
        1,
        0
      )
    )
  
  
# create split
  
  sample <- caTools::sample.split(
    sleep_transition_data_all$asleep,
    SplitRatio = 0.8
  )
  
  train <- sleep_transition_data_all %>%
    filter(sample == TRUE)
  
  test <- sleep_transition_data_all %>%
    filter(sample == FALSE)
  
  
  # Check both classes exist in training and testing data
  if (length(unique(train$asleep)) < 2 ||
      length(unique(test$asleep)) < 2) {
    message("  Only one outcome class in train/test. Skipping.")
    return(NULL)
  }
  
  
# clean train & test data
  
  train_clean <- train %>%
    select(all_of(features), asleep) %>%
    na.omit()
  
  test_clean <- test %>%
    select(all_of(features), asleep) %>%
    na.omit()
  
  
  # full model
  full_model <- glm(
    asleep ~ .,
    data = train_clean,
    family = binomial
  )
  
  
# null model
  null_model <- glm(
    asleep ~ 1,
    data = train_clean,
    family = binomial
  )
  
#stepwise models
  
  backward_model <- stats::step(
    full_model,
    direction = "backward",
    trace = 0
  )
  
  forward_model <- stats::step(
    null_model,
    scope = list(
      lower = null_model,
      upper = full_model
    ),
    direction = "forward",
    trace = 0
  )
  
  stepwise_model <- stats::step(
    null_model,
    scope = list(
      lower = null_model,
      upper = full_model
    ),
    direction = "both",
    trace = 0
  )
  
  
# predictions
  test_clean <- test_clean %>%
    mutate(
      predicted_prob_backwards =
        predict(
          backward_model,
          newdata = test_clean,
          type = "response"
        ),
      
      predicted_prob_forwards =
        predict(
          forward_model,
          newdata = test_clean,
          type = "response"
        ),
      
      predicted_prob_stepwise =
        predict(
          stepwise_model,
          newdata = test_clean,
          type = "response"
        )
    )
  
  
# get roc and auc
  
  roc_backwards <- pROC::roc(
    test_clean$asleep,
    test_clean$predicted_prob_backwards,
    quiet = TRUE
  )
  
  roc_forwards <- pROC::roc(
    test_clean$asleep,
    test_clean$predicted_prob_forwards,
    quiet = TRUE
  )
  
  roc_stepwise <- pROC::roc(
    test_clean$asleep,
    test_clean$predicted_prob_stepwise,
    quiet = TRUE
  )
  
  # get tables
  
  summary <- tibble(
    Participant = participant_id,
    Method = c(
      "Backwards",
      "Forwards",
      "Both"
    ),
    AUC = c(
      as.numeric(pROC::auc(roc_backwards)),
      as.numeric(pROC::auc(roc_forwards)),
      as.numeric(pROC::auc(roc_stepwise))
    )
  )
  
  
# plot roc 
  
  df_backwards <- data.frame(
    FPR = 1 - roc_backwards$specificities,
    TPR = roc_backwards$sensitivities,
    Model = paste0(
      "Backwards (AUC = ",
      round(pROC::auc(roc_backwards), 3),
      ")"
    )
  )
  
  df_forwards <- data.frame(
    FPR = 1 - roc_forwards$specificities,
    TPR = roc_forwards$sensitivities,
    Model = paste0(
      "Forwards (AUC = ",
      round(pROC::auc(roc_forwards), 3),
      ")"
    )
  )
  
  df_stepwise <- data.frame(
    FPR = 1 - roc_stepwise$specificities,
    TPR = roc_stepwise$sensitivities,
    Model = paste0(
      "Bidirectional (AUC = ",
      round(pROC::auc(roc_stepwise), 3),
      ")"
    )
  )
  
  df_all <- bind_rows(
    df_backwards,
    df_forwards,
    df_stepwise
  )
  
  
# create roc plot
  
  roc_plot <- df_all %>%
    ggplot(aes(x = FPR, y = TPR, color = Model)) +
    geom_line(linewidth = 1, alpha = 0.5) +
    geom_abline(
      slope = 1,
      linetype = "dashed"
    ) +
    labs(
      title = paste(
        "ROC curves:",
        participant_id
      ),
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_minimal() +
    facet_wrap(~Model) +
    theme(
      legend.position = "none",
      text = element_text(size = 15)
    )
  
  
  
  output_dir <- here::here(
    "figures",
    "Chapter 5",
    "logistic-regression"
  )
  
  dir_create(output_dir)
  
  ggsave(
    filename = file.path(
      output_dir,
      paste0(participant_id, "-roc-plot.pdf")
    ),
    plot = roc_plot,
    width = 16,
    height = 9,
    units = "in",
    dpi = 500
  )
  
  
# spit results
  
  list(
    participant = participant_id,
    train = train,
    test = test,
    train_clean = train_clean,
    test_clean = test_clean,
    full_model = full_model,
    backward_model = backward_model,
    forward_model = forward_model,
    stepwise_model = stepwise_model,
    roc_backwards = roc_backwards,
    roc_forwards = roc_forwards,
    roc_stepwise = roc_stepwise,
    summary = summary,
    roc_plot = roc_plot
  )
}

