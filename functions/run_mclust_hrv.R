run_mclust_hrv <- function(data, 
                           metrics = hrv_metrics,
                           participant_col = "participant",
                           G = NULL,
                           suffix = "") {
  
  # Check that requested metrics exist
  metrics <- metrics[metrics %in% names(data)]
  
  if (length(metrics) == 0) {
    stop("None of the requested HRV metrics were found in the data.")
  }
  
  # Run Mclust separately for every participant × metric
  results <- data %>%
    group_by(.data[[participant_col]]) %>%
    group_modify(~ {
      
      participant_data <- .x
      
      for (metric in metrics) {
        
        # Extract values
        x <- participant_data[[metric]]
        
        # Only use finite, non-missing values
        valid <- is.finite(x) & !is.na(x)
        
        # Need enough observations and variation for Mclust
        if (sum(valid) < 2 || length(unique(x[valid])) < 2) {
          participant_data[[paste0("cluster_", metric, suffix)]] <- NA_integer_
          next
        }
        
        # Run Mclust
        fit <- tryCatch(
          {
            if (is.null(G)) {
              Mclust(x[valid])
            } else {
              Mclust(x[valid], G = G)
            }
          },
          error = function(e) NULL
        )
        
        # Create cluster vector
        clusters <- rep(NA_integer_, length(x))
        
        if (!is.null(fit)) {
          clusters[valid] <- fit$classification
        }
        
        participant_data[[paste0("cluster_", metric, suffix)]] <- clusters
      }
      
      participant_data
    }) %>%
    ungroup()
  
  return(results)
}
