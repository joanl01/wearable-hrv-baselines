# Load packages
pacman::p_load(readr, tidyverse, here, purrr, changepoint.np, forecast, xts, patchwork)
fs::dir_ls(here::here("functions")) |> walk(source)


df <- readRDS(here::here("data", "hrv_garmin-non-linear.rds"))
# extract for participant 7 because they have the highest quality data
df <- df %>%
  mutate(
    participant = stringr::str_extract(
      file,
      "FLARE\\d+"
    )
  ) |> filter(participant == "FLARE007")


# Add date column for nice splitting
df <- df %>% mutate(date = as.Date(min))

####### Loop over HR #########

# Define window size
window_size <- 3

unique_dates <- unique(df$date)

# Initialize empty dataframes and list for plots
HR_results <- data.frame()
all_changepoints <- data.frame()
changepoint_plots <- list()
pen.plot <- list()

# Loop through each 3-day window
# i = 2
for (i in 1:(length(unique_dates) - window_size + 1)) {
  window_dates <- unique_dates[i:(i + window_size - 1)]
  df_window <- df %>% filter(date %in% window_dates)
  
  # Fit ARIMA model on the current window
  HR_arima <- auto.arima(df_window$HR)
  
  # Store results
  temp_df <- data.frame(
    time = df_window$min,
    date = df_window$date,
    data = df_window$HR,
    fitted = fitted(HR_arima),
    res = residuals(HR_arima)
  )
  
  HR_results <- bind_rows(HR_results, temp_df)
  
  # Find penalty value within the window
  pen.vals <- seq(0,1000,1)
  elbowplotData <- sapply(pen.vals, function(p) cptfn(data = temp_df$res, pen = p))
  
  # Put them into a dataframe
  penalty_df <- data.frame(pen.val = pen.vals, data = elbowplotData)
  
  pen.plot[[i]] <- penalty_df %>% ggplot(aes(x = pen.val, y = data)) + geom_point() + 
    scale_x_continuous(breaks = seq(0,1000,50)) + scale_y_continuous(breaks = seq(0,1000,10)) + 
    theme_minimal()
  # Pick penalty value and detect change points
  penalty.val <- 400
  HR_cp <- cpt.mean(temp_df$res, penalty='Manual', pen.value=penalty.val, method='PELT') 
  HR_cp_tp <- cpts(HR_cp) 
  
  # Extract change point data
  HR_cp_df <- data.frame(
    time = temp_df$time[HR_cp_tp],
    HR = temp_df$data[HR_cp_tp],
    date = as.Date(temp_df$time[HR_cp_tp])
  )
  
  all_changepoints <- bind_rows(all_changepoints, HR_cp_df)
  
  # Generate and store change point plot
  plot <- ggplot(temp_df, aes(x = time, y = data, col = "data")) +
    geom_line() + geom_line(data = temp_df,aes(x = time, y = fitted, col = "fitted")) + 
    geom_point(data = HR_cp_df, aes(x = time, y = HR), color = "red", size = 2) +
    facet_wrap(~date, scales = "free_x") +
    theme_minimal() + harrypotter::scale_colour_hp_d("Ravenclaw") + 
    scale_x_datetime(
      breaks = scales::date_breaks("6 hour"), 
      labels = scales::date_format("%H:%M"),
      expand = expansion(mult = c(0, 0))
    ) +
    theme(
      legend.position = "bottom",
      text = element_text(size = 20)
    ) + 
    labs(
      x = "Time", 
      y = "HR"
      # title = "HR against Time"
    )
  
  changepoint_plots[[paste0("Window_", i)]] <- plot
}

# Save all changepoints to inspect later
print(all_changepoints)

# Plot results for a specific date range
HR_results %>% filter(date %in% c("2023-07-17", "2023-07-18", "2023-07-19")) %>% 
  ggplot(aes(x = time, y = data, col= "data")) +
  geom_line() + 
  geom_line(aes(x = time, y = fitted, col = "fitted")) + 
  facet_wrap(~date, scales = "free_x") + theme_bw() +
  harrypotter::scale_colour_hp_d("Ravenclaw") + 
  scale_x_datetime(
    breaks = scales::date_breaks("6 hour"), 
    labels = scales::date_format("%H:%M"),
    expand = expansion(mult = c(0, 0))
  ) +
  theme(
    legend.position = "bottom",
    text = element_text(size = 20)
  ) + 
  labs(
    x = "Time", 
    y = "HR"
    # title = "HR against Time"
  )

cp_plot <- changepoint_plots[[60]]/changepoint_plots[[62]] + 
  plot_layout(guides = 'collect') + 
  plot_annotation(title = "HR against Time for Participant 7 with changepoints detected", 
                  subtitle = "Colour indicates data from Garmin and fitted values from ARIMA model") & 
  theme(legend.position = "bottom", text = element_text(size = 20))

cp_plot

ggsave(here::here("figures", "Chapter 5", "changpoints-thesis.pdf"), 
       plot = cp_plot, 
       height = 9, 
       width = 16,
       units = "in",
       dpi = 500)


