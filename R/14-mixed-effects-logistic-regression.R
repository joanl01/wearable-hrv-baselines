# Load packages
pacman::p_load(readr, tidyverse, gt, tidymodels, corrplot, vip, DALEXtra, GGally, plotly,ggdendro, lme4)
# Read all functions
fs::dir_ls(here::here("functions")) |> walk(source)

hrvs <- read_rds(here::here("data", "hrv_with_sleep_joined.rds")) |> select(-seconds_in_stage)


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
    result <- summarise_hrv_timeline_by_artifact_no_summarise(hrvs, timeline = timeline, days = days)
    result |> 
      mutate(timeline = timeline, days = factor(days))
  }
)

# settle for logistic regression for now 
model_df <- artifacts_combined_results %>%
  drop_na() %>%
  mutate(
    rest_state = ifelse(timeline %in%
                          c("deep sleep","first moment before wake"),1,0)
  ) |> select(participant, rest_state, where(is.numeric))


model_df <- model_df |> 
  select(-"Time", -"shift", -"sizesp", -"ULFmin", -"ULFmax",
         -"VLFmin", -"VLFmax", -"LFmin", -"LFmax", -"HFmin",
         -"HFmax") |> na.omit()


full_model <- glmer(rest_state ~ . -participant + (1|participant) ,
                    data = model_df,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))

null_model <- glmer(rest_state ~ 1 + (1|participant) ,
                    data = model_df,
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))


reduced_model <- glmer(rest_state ~ HR + HF + ULF + (1|participant) ,
                       data = model_df, 
                       family = binomial,
                       control = glmerControl(optimizer = "bobyqa"))


library(pROC)

roc_full <- roc(model_df$rest_state,
                predict(full_model, type = "response"))

roc_red  <- roc(model_df$rest_state,
                predict(reduced_model, type = "response"))

roc_null <- roc(model_df$rest_state,
                predict(null_model, type = "response"))
# 
# # Plot first curve
# plot(roc_full, col = "black", lwd = 2)
# 
# # Add others
# plot(roc_red,  add = TRUE, col = "red",  lwd = 2)
# plot(roc_null, add = TRUE, col = "blue", lwd = 2)
# 
# # Compute AUCs
# auc_full <- auc(roc_full)
# auc_red  <- auc(roc_red)
# auc_null <- auc(roc_null)
# 
# # Add legend
# legend("bottomright",
#        legend = c(paste("Full model (AUC =", round(auc_full, 3), ")"),
#                   paste("Reduced model (AUC =", round(auc_red, 3), ")"),
#                   paste("Null model (AUC =", round(auc_null, 3), ")")),
#        col = c("black", "red", "blue"),
#        lwd = 2)
# 



# Convert each ROC object to a data frame
full_roc_df <- data.frame(
  FPR = 1 - roc_full$specificities,
  TPR = roc_full$sensitivities,
  Model = paste0("Full, (AUC = ", round(auc(roc_full),3),")" )
)

null_roc_df <- data.frame(
  FPR = 1 - roc_null$specificities,
  TPR = roc_null$sensitivities,
  Model = paste0("Null, (AUC = ", round(auc(roc_null),3), ")" )
)


reduced_roc_df <- data.frame(
  FPR = 1 - roc_red$specificities,
  TPR = roc_red$sensitivities,
  Model = paste0("Reduced, (AUC = ", round(auc(roc_red),3), ")" )
)



# Combine all into one data frame
roc_data_all <- bind_rows(full_roc_df,null_roc_df, reduced_roc_df)

# Plot with ggplot
roc_plot <- roc_data_all |> ggplot( aes(x = FPR, y = TPR, color = Model)) +
  geom_line() +
  geom_abline(slope = 1, linetype = "dashed") +
  labs(
    title = "ROC curves",
    x = "False Positive Rate",
    y = "True Positive Rate"
  )  + theme_minimal() + harrypotter::scale_colour_hp("ravenclaw", discrete = TRUE)

roc_plot
ggsave(here::here("figures", "Chapter 7", "ROC-mixed-effects-logistic-regression.pdf"),
       height = 9, width = 9, plot = roc_plot, units = "in", dpi = 500)


mod_results <- list(full = full_model, reduced = reduced_model, null = null_model) |> 
  map(broom.mixed::glance) |> list_rbind(names_to = "model") |> select(-AIC, -sigma, -nobs) |> 
  gt() |> fmt_number(decimals = 4,columns = c("BIC")) 



mod_results


