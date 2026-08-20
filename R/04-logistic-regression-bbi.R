# Logistic Regression
pacman::p_load(tidyverse, patchwork, tidymodels, readr, lubridate, fs, here, hms, caret, purrr)
options(digits=10)
# Read all functions
fs::dir_ls(here::here("functions")) |> walk(source)

# Define BBI data paths
bbi_paths <- paste0("FLARE", sprintf("%03d", 1:11), "_", c("HJ", "DC", "TM", "BP", "JT", "JA", "JH", "AP", "AJ", "KL", "JS"), "_data/garmin-device-bbi")
bbi_paths <- here::here("data", bbi_paths)

# Read BBI data

bbi_data <- map(bbi_paths, read_bbis)

names(bbi_data) <- paste0("bbi_df", sprintf("%02d", 1:11))

bbi_data <- map(bbi_data, na.omit)


# Define Sleep data paths (skipping 4 and 9 as no sleep data)
sleep_paths <- c(
  "FLARE001_HJ_data", "FLARE002_DC_data", "FLARE003_TM_data", 
  "FLARE005_JT_data", "FLARE006_JA_data", "FLARE007_JH_data", 
  "FLARE008_AP_data", "FLARE010_KL_data", "FLARE011_JS_data"
) %>% 
  paste0("/garmin-connect-sleep-stage/000000_garmin-connect-sleep-stage_", 
         c("FLARE001-HJ_75ff26b5.csv", "FLARE002-DC_286e12ca.csv", "FLARE003-TM_b0ab838e.csv", 
           "FLARE005-JT_c93a96a0.csv", "FLARE006-JA_658375b2.csv", "FLARE007-JH_2528da08.csv", 
           "FLARE008-AP_227592d9.csv", "FLARE010-KL_95e1c8ef.csv", "FLARE011-JS_318181f0.csv"))
sleep_paths <- here::here("data", sleep_paths)

# Read Sleep data
sleep_data <- map(sleep_paths, read_sleep) 
sleep_data <- map(sleep_data, na.omit)
names(sleep_data) <- paste0("sleep_df", c(1:3, 5:8, 10:11))

bbi_data <- map(bbi_data, convert_bbi_min)
sleep_data <- map(sleep_data, convert_sleep_min)


# Define indices of datasets with both BBI and sleep data
indices_with_both <- c(1:3, 5:8, 10:11)

# Create an empty list to store the joined data sets
joined_data <- list()

# Loop over the indices and join the corresponding BBI and sleep data sets
for (i in indices_with_both) {
  bbi_name <- paste0("bbi_df", sprintf("%02d", i))
  sleep_name <- paste0("sleep_df", i)
  
  joined_name <- paste0("joined_df", sprintf("%02d", i))
  joined_data[[joined_name]] <- join_bbi_sleep(bbi = bbi_data[[bbi_name]], sleep = sleep_data[[sleep_name]])
}

# Assign names to the joined data sets for easier access
names(joined_data) <- paste0("joined_df", sprintf("%02d", indices_with_both))

joined_data_without_na <- joined_data

# Change NA to awake 
for (j in 1: length(joined_data_without_na)) {
  joined_data_without_na[[j]]$type <- as.factor(joined_data_without_na[[j]]$type)
  joined_data_without_na[[j]]$type <- joined_data_without_na[[j]]$type |> fct_na_value_to_level("not_sleeping")
}



# Change to sleeping & not sleeping, unmeasureable is removed
joined_data_without_na <- map(joined_data_without_na, function(df){
  df <- df %>% 
    # Convert the 'type' column to character to avoid factor level issues
    mutate(type = as.character(type)) %>%
    mutate(type = ifelse(type %in% c("deep", "light", "rem"), "sleeping", type)) %>% 
    mutate(type = ifelse(type %in% c("awake"), "not_sleeping", type)) %>% 
    mutate(type = as.factor(type)) %>% 
    mutate(date = as.Date(time_min),
           time = as_hms(time_min)) %>% 
    filter(type == "sleeping"| type == "not_sleeping") |> na.omit()
})

joined_data_without_na_lg <- map(joined_data_without_na, function(df2){
  # Set up recipe
  df2_recipe <- 
    recipe(type ~ bbi , data = df2)
  # Set up logistic regression
  df2_log_M1 <- 
    logistic_reg() |> 
    set_engine("glm") |> 
    set_mode("classification")
  
  #Set up workflow
  df2_WF1 <- 
    workflow() |> 
    add_recipe(df2_recipe) |> 
    add_model(df2_log_M1)
  
  
  # Fit model
  df2_fit1 <- df2_WF1 |> fit(df2)
  
  # df2_fit1 |> 
  #   tidy()
  # 
  ddf2 <- df2 %>%
    mutate(fitted = predict(df2_fit1, new_data = df2)$.pred_class)
  
})

reports <- purrr::imap(
  joined_data_without_na_lg,
  ~ make_logistic_reg_plot(
    data = .x,
    participant = .y
  )
)


# ---- thesis results----

fitted03 <- joined_data_without_na_lg$joined_df03 %>%
  na.omit() %>%
  ggplot(aes(x = fitted, y = bbi, fill = fitted)) + 
  geom_boxplot() + 
  harrypotter::scale_fill_hp(discrete = TRUE, house = "ravenclaw") +
  labs(
    title = "BBI by Predicted Values of Logistic Regression for Participant 3", 
    x = "Category",
    y = "BBI", 
    fill = "Group"
  ) +
  theme_bw() + 
  theme(legend.position = "none", 
        text = element_text(size = 16))

# Create actual boxplot
actual_boxplot03 <- joined_data_without_na_lg$joined_df03 %>%
  ggplot(aes(x = type, y = bbi, fill = type)) + 
  geom_boxplot() + 
  harrypotter::scale_fill_hp(discrete = TRUE, house = "ravenclaw") +
  labs(
    title = "BBI by True Values for Particpant 3", 
    x = "Category",
    y = "BBI", 
    fill = "Group"
  ) +
  theme_bw() + 
  theme(legend.position = "none", 
        text = element_text(size = 16))

# Combine both plots and adjust layout
report03 <- fitted03 + actual_boxplot03 + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

report03

ggsave(here::here("figures", "Chapter 3", "bbi-sleep-log_reg_joined03.pdf"), 
       plot = report03,
       width = 16, 
       height = 9,
       units = "in",
       dpi = 500)



fitted07 <- joined_data_without_na_lg$joined_df07 %>%
  na.omit() %>%
  ggplot(aes(x = fitted, y = bbi, fill = fitted)) + 
  geom_boxplot() + 
  harrypotter::scale_fill_hp(discrete = TRUE, house = "ravenclaw") +
  labs(
    title = "BBI by Predicted Values of Logistic Regression for Participant 7", 
    x = "Category",
    y = "BBI", 
    fill = "Group"
  ) +
  theme_bw() + 
  theme(legend.position = "none", 
        text = element_text(size = 16))

# Create actual boxplot
actual_boxplot07 <- joined_data_without_na_lg$joined_df07 %>%
  ggplot(aes(x = type, y = bbi, fill = type)) + 
  geom_boxplot() + 
  harrypotter::scale_fill_hp(discrete = TRUE, house = "ravenclaw") +
  labs(
    title = "BBI by True Values for Particpant 7", 
    x = "Category",
    y = "BBI", 
    fill = "Group"
  ) +
  theme_bw() + 
  theme(legend.position = "none", 
        text = element_text(size = 16))

# Combine both plots and adjust layout
report07 <- fitted07 + actual_boxplot07 + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

report07

ggsave(here::here("figures", "Chapter 3", "log_reg_joined07.pdf"), 
       plot = report07,
       width = 16, 
       height = 9,
       units = "in",
       dpi = 500)


# --- ROC ----

roc_curves <- map(joined_data_without_na, function(df2){
  # Get predicted probabilities
  df2_preds <- df2 |> 
    bind_cols(predict(df2_fit1, new_data = df2, type = "prob"))
  
  # Compute ROC curve data
  roc_curve_data <- roc_curve(df2_preds, truth = type, .pred_not_sleeping) 
  
  # Plot ROC curve
  ggplot(roc_curve_data, aes(x = 1 - specificity, y = sensitivity)) +
    geom_line() +
    geom_abline(linetype = "dashed") +
    labs(title = "ROC Curve for Participant x",
         x = "1 - Specificity",
         y = "Sensitivity") +
    theme_classic()
}
)

auc_values <- map(joined_data_without_na, function(df2){
  
  df2_preds <- df2 |> 
    bind_cols(predict(df2_fit1, new_data = df2, type = "prob"))
  
  # Compute AUC
  auc_value <- roc_auc(df2_preds, truth = type, .pred_not_sleeping)  
})
auc_values


joined_data_without_na_CM <- map(joined_data_without_na_lg, function(ddf2){
  CM <- confusionMatrix(data=ddf2$fitted, reference = ddf2$type)
  return(CM)
})
joined_data_without_na_CM



