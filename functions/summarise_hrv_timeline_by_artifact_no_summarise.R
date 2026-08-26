summarise_hrv_timeline_by_artifact_no_summarise <- function(hrvs, 
                                               timeline = "first moment after wake", 
                                               days = 5, 
                                               window_mins = 5) {
  require(dplyr)
  require(lubridate)
  require(purrr)
  
  # Define a safe way to find the timeline moment
  get_timeline_time <- function(df, timeline) {
    df <- df %>% arrange(min)
    
    if (timeline == "first moment before wake") {
      last_asleep <- df %>% filter(type.y %in% c("rem", "deep", "light")) %>% pull(min)
      if (length(last_asleep) == 0) return(NA)
      return(max(last_asleep))
      
    } else if (timeline == "first moment after wake") {
      last_asleep <- df %>% filter(type.y %in% c("rem", "deep", "light")) %>% pull(min)
      if (length(last_asleep) == 0) return(NA)
      return(max(last_asleep) + minutes(window_mins))
      
    } else if (timeline == "deep sleep") {
      deep_times <- df %>% filter(type.y == "deep") %>% pull(min)
      if (length(deep_times) == 0) return(NA)
      # Return up to `window_mins` separate 5-min timestamps
      return(max(deep_times, window_mins))
    } else if (timeline == "awake") {
      random_awake <- df %>% filter(type.y %in% c("presumed awake")) %>% pull(min)
      if (length(random_awake) == 0) return(NA)
      return(sample(random_awake, 1))
    }
    else {
      return(NA)
    }
  }
  
  hrvs <- hrvs %>% 
    filter(complete.cases(.))
  
  # Step 1: Extract timeline moments
  timeline_targets <- hrvs %>%
    group_by(participant, date) %>%
    group_modify(~ tibble(target_time = get_timeline_time(.x, timeline))) %>%
    ungroup() %>%
    filter(!is.na(target_time))
  
  # Step 2: Join target times back to data
  target_data <- timeline_targets %>%
    left_join(hrvs, by = c("participant", "date")) %>%
    filter(
      min >= target_time & min < target_time + minutes(window_mins)
    )
  
  # ---- Step 3: Select best 5 windows per participant ----
  # Compute artifact level *per window*
  window_scores <- target_data %>%
    group_by(participant, date, target_time) %>%
    summarise(window_artifact = mean(artifacts), .groups = "drop")
  
  # Pick the 5 windows with lowest average artifacts
  best_windows <- window_scores %>%
    group_by(participant) %>%
    slice_min(window_artifact, n = days) %>%
    ungroup()
  
  # Keep all HRV rows belonging to these windows
  result <- best_windows %>%
    left_join(target_data, by = c("participant", "date", "target_time")) |> 
    select(-window_artifact)
  
  return(result)
}
