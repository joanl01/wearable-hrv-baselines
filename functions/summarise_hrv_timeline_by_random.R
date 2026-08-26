summarise_hrv_timeline_by_random <- function(hrvs, 
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
  
  # Step 3: Select `days` with lowest artifacts per participant
  # Step 3: Sample days (conditionally)
  result <- target_data %>%
    group_by(participant) %>%
    group_modify(~ {
      if (nrow(.x) <= days) {
        .x  # keep all rows if less than or equal to 'days'
      } else {
        dplyr::sample_n(.x, days)
      }
    }) %>%
    ungroup()
  
  return(result)
}
