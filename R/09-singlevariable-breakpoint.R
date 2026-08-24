pacman::p_load(tidyverse, zoo, here, patchwork, scales)
hrv_garmin <- read_rds(here::here("data", "hrv_garmin-non-linear.rds"))

# Filter for relevant file
data <- hrv_garmin %>% filter(grepl("FLARE007", file))

# Ensure 'date' column is correctly formatted
data <- data %>% mutate(date = as.Date(min))

# Get unique days
unique_days <- unique(data$date)

# Break-point sum of squares function
fit_break_pt_model <- function(break_pt, x) {
  n <- length(x)
  stopifnot(break_pt < n)
  stopifnot(break_pt > 0)
  left <- x[1:break_pt]
  right <- x[(break_pt + 1):n]
  ss <- sum((left - mean(left))^2) +
    sum((right - mean(right))^2)
  ss
}

# Store plots
all_plots <- list()

# Loop through each unique date
for (d in unique_days) {
  df <- data %>% filter(date == d)
  
  # Skip if too few points
  if (nrow(df) < 5) next
  
  # Plot 1: Time series
  plot1 <- df %>%
    ggplot(aes(min, HR)) +
    geom_line() +
    labs(
      title = paste("HR on", as.Date(d)),
      x = "Time",
      y = "HR"
    ) + theme_minimal()
  
  # Generate parameters and calculate SS
  params <- tibble(break_pt = 1:(nrow(df) - 1))
  params$ss <- map_dbl(params$break_pt, \(i) fit_break_pt_model(i, df$HR))
  
  # # Plot 2: SS vs break point
  # plot2 <- params %>%
  #   ggplot(aes(break_pt, ss)) +
  #   geom_line() + 
  #   labs(title = paste("SS by Break Point on", as.Date(d))) + theme_bw()
  
  
  
  
  params <- params %>%
    mutate(ss_smooth = rollmean(ss, k = 5, fill = NA))
  
  # Function to find local minima using a moving window
  find_true_local_minima <- function(x, window = 10) {
    minima <- c()
    for (i in (window + 1):(length(x) - window)) {
      local_window <- x[(i - window):(i + window)]
      if (which.min(local_window) == (window + 1)) {
        minima <- c(minima, i)
      }
    }
    return(minima)
  }
  
  # Find candidate local minima
  candidate_indices <- find_true_local_minima(params$ss_smooth, window = 20)
  
  # Extract data for those indices
  candidate_minima <- params[candidate_indices, ]
  
  # Optional: Filter out minima that are too close to each other (e.g. within 100 points)
  minima_final <- list()
  used <- c()
  
  for (i in candidate_indices) {
    if (all(abs(i - used) > 100)) {
      minima_final[[length(minima_final) + 1]] <- params[i, ]
      used <- c(used, i)
    }
  }
  
  minima_final_df <- bind_rows(minima_final)
  
  # 🎯 Plot: SS with circled local minima
  plot2 <- params %>%
    ggplot(aes(break_pt, ss_smooth)) +
    geom_line() + 
    geom_point(data = minima_final_df, aes(break_pt, ss_smooth),
               color = "red") + theme_minimal()+ scale_y_continuous(labels = comma) +
  labs(title = "Refined Local Minima in SS Curve",
       x = "Breakpoint",
       y = "Smoothed Sum of ")
  # Store combined plot
  all_plots[[as.character(d)]] <- plot1 / plot2
}

# View one example
ggsave(here::here("figures", "breakpoints-good.pdf"),
       plot = all_plots[["19557"]],
       height = 9,
       width = 16,
       dpi = 500,
       units = "in")


ggsave(here::here("figures", "breakpoints-bad.pdf"),
       plot = all_plots[["19581"]],
       height = 9,
       width = 16,
       dpi = 500,
       units = "in")


(all_plots[["19557"]])
(all_plots[["19581"]])
