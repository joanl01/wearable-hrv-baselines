# Load packages
pacman::p_load(readr, tidyverse, gt, fs, here, purrr, caTools, pROC)

# Load Data
hrv_with_sleep_all <- readRDS(here::here("data", "hrv_with_sleep_joined.rds"))
# 
# # Find the *last* occurrence of "rem" or "light" per day
# sleep_transition_data <- hrv_with_sleep_all%>% filter(participant == "FLARE007") %>% 
#   filter(type.y %in% c("rem", "light")) %>%
#   group_by(date) %>%
#   filter(min == max(min)) %>%
#   ungroup() %>%
#   rename(target_time = min)
# 
# # Join back to top_days to get 15-min window around each last sleep stage per day
# sleep_windows <- sleep_transition_data %>%
#   rowwise() %>%
#   mutate(
#     window_start = target_time - minutes(15),
#     window_end = target_time + minutes(15)
#   ) %>%
#   ungroup() %>%
#   select(date, window_start, window_end)
# 
# # For each window, filter top_days
# sleep_transition_data_all <-  hrv_with_sleep_all %>% filter(participant == "FLARE007") %>% 
#   inner_join(sleep_windows, by = "date") %>%
#   filter(min >= window_start & min <= window_end) %>%
#   select(-window_start, -window_end)
# 
# # Result
# sleep_transition_data_all

# Use a column (usually the outcome variable) for stratified splitting
sample <- sample.split(sleep_transition_data_all$type.y, SplitRatio = 0.8)

# Then split the data frame
train <- subset(sample, sample == TRUE)
test  <- subset(sample, sample == FALSE)



# model_data <- sleep_transition_data %>%
#   select(asleep, HR, SDNN, SDANN, rMSSD, pNN50, SDSD, ULF, VLF, LF, HF, LFHF,
#          PoincarePlot.SD1, PoincarePlot.SD2, REC, RATIO, DET) %>%
#   na.omit()
# 
# # Fit a logistic regression model
# sleep_model <- glm(asleep ~ ., data = model_data, family = binomial)
# 
# # Predict sleep state on same data
# model_data$predicted_prob <- predict(sleep_model, type = "response")
# model_data$predicted_state <- ifelse(model_data$predicted_prob > 0.5, 1, 0)
# 
# table(Predicted = model_data$predicted_state, Actual = model_data$asleep)
# 
# 
# 



# Apply to all datasets
for (df_name in c("train", "test", "hrv_with_sleep_all")) {
  assign(df_name, get(df_name) %>%
           mutate(asleep = ifelse(type.y %in% c("light", "deep","rem"), 1, 0)))
}



features <- c("HR", "SDNN", "SDANN", "rMSSD", "pNN50", "SDSD",
              "ULF", "VLF", "LF", "HF", "LFHF",
              "PoincarePlot.SD1", "PoincarePlot.SD2",
              "REC", "RATIO", "DET", "DIV", "Lmax",   "Lmean", "LmeanWithoutMain" ,"ENTR", "TREND","LAM","Vmax")

# Remove rows with NAs
train_clean <- train %>%
  select(all_of(features), asleep) %>%
  na.omit()

# Fit logistic regression model
sleep_model <- glm(asleep ~ ., data = train_clean, family = binomial)



test_clean <- test %>%
  select(all_of(features), asleep) %>%
  na.omit()

# Predict probabilities
test_clean$predicted_prob <- predict(sleep_model, newdata = test_clean, type = "response")
test_clean$predicted_state <- ifelse(test_clean$predicted_prob > 0.5, 1, 0)

# Evaluate performance
table(Predicted = test_clean$predicted_state, Actual = test_clean$asleep)

# AUC
roc_obj <- roc(test_clean$asleep, test_clean$predicted_prob)
auc(roc_obj)


roc_model <- roc(test_clean$asleep, test_clean$predicted_prob)
auc_model <- auc(roc_model)

print(paste("Trained Model AUC:", auc_model))

plot(roc_model, col = "blue", main = "ROC: Trained Model")
# legend("bottomright", legend = c("Trained", "Baseline"),
#        col = c("blue", "red"), lty = 1)


# Null model (intercept only)
null_model <- glm(asleep ~ 1, data = train_clean, family = binomial)

# Full model (all predictors)
full_model <- glm(asleep ~ ., data = train_clean, family = binomial)


backward_model <- stats::step(full_model, direction = "backward")

forward_model <- stats::step(null_model, 
                      scope = list(lower = null_model, upper = full_model), 
                      direction = "forward")

stepwise_model <- stats::step(null_model, 
                       scope = list(lower = null_model, upper = full_model), 
                       direction = "both")



# # Predict on test data (make sure variables match)
# test_clean$predicted_prob_stepwise <- predict(stepwise_model, newdata = test_clean, type = "response")
# test_clean$predicted_state_stepwise <- ifelse(test_clean$predicted_prob_stepwise > 0.5, 1, 0)
# 
# # Confusion matrix
# table(Predicted = test_clean$predicted_state_stepwise, Actual = test_clean$asleep)
# 
# # AUC
# roc_stepwise <- roc(test_clean$asleep, test_clean$predicted_prob_stepwise)
# auc(roc_stepwise)
# 



# Predict on test data (backwards)
test_clean$predicted_prob_backwards_test <- predict(backward_model, newdata = test_clean, type = "response")

test_clean$predicted_state_backwards_test <- ifelse(test_clean$predicted_prob_backwards_test > 0.5, 1, 0)


# Confusion matrix
table(Predicted = test_clean$predicted_state_backwards_test, Actual = test_clean$asleep)


# AUC
roc_backwards_test <- roc(test_clean$asleep, test_clean$predicted_prob_backwards_test)
auc(roc_backwards_test)



# Predict on test data (make sure variables match)
test_clean$predicted_prob_forwards <- predict(forward_model, newdata = test_clean, type = "response")
test_clean$predicted_state_forward <- ifelse(test_clean$predicted_prob_forwards > 0.5, 1, 0)

# Confusion matrix
table(Predicted = test_clean$predicted_state_forward, Actual = test_clean$asleep)

# AUC
roc_forward_test <- roc(test_clean$asleep, test_clean$predicted_prob_forwards)
auc(roc_forward_test)

# Predict on test data (make sure variables match)
test_clean$predicted_prob_stepwise_test <- predict(stepwise_model, newdata = test_clean, type = "response")
test_clean$predicted_state_stepwise_test <- ifelse(test_clean$predicted_prob_stepwise > 0.5, 1, 0)

# Confusion matrix
table(Predicted = test_clean$predicted_state_stepwise_test, Actual = test_clean$asleep)

# AUC
roc_stepwise_test <- roc(test_clean$asleep, test_clean$predicted_prob_stepwise_test)
auc(roc_stepwise_test)



summary <- data.frame(
  Method = c("Backwards", "Forwards", "Both"),
  AUC = c(auc(roc_backwards_test), auc(roc_forward_test), auc(roc_stepwise_test))
)

summary %>% gt()


# Convert each ROC object to a data frame
df_backwards_test <- data.frame(
  FPR = 1 - roc_backwards_test$specificities,
  TPR = roc_backwards_test$sensitivities,
  Model = "Backwards"
)

df_forward_test <- data.frame(
  FPR = 1 - roc_forward_test$specificities,
  TPR = roc_forward_test$sensitivities,
  Model = "Forwards"
)

df_stepwise_test <- data.frame(
  FPR = 1 - roc_stepwise_test$specificities,
  TPR = roc_stepwise_test$sensitivities,
  Model = "Both"
)

# Combine all into one data frame
df_all_test <- bind_rows(df_backwards_test, df_forward_test, df_stepwise_test)

# Plot with ggplot
df_all_test |> ggplot( aes(x = FPR, y = TPR, color = Model)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  labs(
    title = "ROC: Stepwise Models",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) + harrypotter::scale_color_hp("Ravenclaw", discrete = TRUE) + theme_minimal()




# ----Predict on ALL data (backwards)----
hrv_with_sleep_all$predicted_prob_backwards <- predict(backward_model, newdata = hrv_with_sleep_all, type = "response")

hrv_with_sleep_all$predicted_state_backwards <- ifelse(hrv_with_sleep_all$predicted_prob_backwards > 0.5, 1, 0)


# Confusion matrix
table(Predicted = hrv_with_sleep_all$predicted_state_backwards, Actual = hrv_with_sleep_all$asleep)


# AUC
roc_backwards <- roc(hrv_with_sleep_all$asleep, hrv_with_sleep_all$predicted_prob_backwards)
auc(roc_backwards)





# ----Predict on ALL data (forwards)----
hrv_with_sleep_all$predicted_prob_forwards <- predict(forward_model, newdata = hrv_with_sleep_all, type = "response")
hrv_with_sleep_all$predicted_state_forward <- ifelse(hrv_with_sleep_all$predicted_prob_forwards > 0.5, 1, 0)

# Confusion matrix
table(Predicted = hrv_with_sleep_all$predicted_state_forward, Actual = hrv_with_sleep_all$asleep)

# AUC
roc_forward <- roc(hrv_with_sleep_all$asleep, hrv_with_sleep_all$predicted_prob_forwards)
auc(roc_forward)

# ----Predict on ALL data (both)----
hrv_with_sleep_all$predicted_prob_stepwise <- predict(stepwise_model, newdata = hrv_with_sleep_all, type = "response")
hrv_with_sleep_all$predicted_state_stepwise <- ifelse(hrv_with_sleep_all$predicted_prob_stepwise > 0.5, 1, 0)

# Confusion matrix
table(Predicted = hrv_with_sleep_all$predicted_state_stepwise, Actual = hrv_with_sleep_all$asleep)

# AUC
roc_stepwise <- roc(hrv_with_sleep_all$asleep, hrv_with_sleep_all$predicted_prob_stepwise)
auc(roc_stepwise)



summary <- data.frame(
  Method = c("Backwards", "Forwards", "Both"),
  AUC = c(auc(roc_backwards), auc(roc_forward), auc(roc_stepwise))
)


summary %>% gt()



# Convert each ROC object to a data frame
df_backwards <- data.frame(
  FPR = 1 - roc_backwards$specificities,
  TPR = roc_backwards$sensitivities,
  Model = "Backwards"
)

df_forward <- data.frame(
  FPR = 1 - roc_forward$specificities,
  TPR = roc_forward$sensitivities,
  Model = "Forwards"
)

df_stepwise <- data.frame(
  FPR = 1 - roc_stepwise$specificities,
  TPR = roc_stepwise$sensitivities,
  Model = "Both"
)

# Combine all into one data frame
df_all <- bind_rows(df_backwards, df_forward, df_stepwise)

# Plot with ggplot
df_all |> ggplot( aes(x = FPR, y = TPR, color = Model)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  labs(
    title = "ROC: Stepwise Models",
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) + harrypotter::scale_color_hp("Ravenclaw", discrete = TRUE) + theme_minimal()






