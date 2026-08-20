# Load necessary packages
pacman::p_load(tidyverse, RHRV)

# Look into the data folder, where all bbi data is stored
# Labfront files have 'garmin-device-bbi' included in the filename
files_bbi = list.files('data/', 'garmin-device-bbi', recursive = TRUE, full.names = TRUE)

# Loop through all files
hrv_garmin = map_dfr(files_bbi, function(x) { 
  # Function for computing HRV for each file x
  
  # Read the file, round date_time to the closest minute, and compare raw bbi with clean bbi
  bbi_garmin = readr::read_csv(x, skip = 6) |> 
    mutate(
      isoDate_min = round_date(isoDate, "5 minute"),
      raw_bbi = c(diff(unixTimestampInMs), 0),
      artif = (bbi != raw_bbi)
    )
  
  # Loop through each unique minute
  hrv_i = map_dfr(unique(bbi_garmin$isoDate_min), function(y) {
    # Function for computing HRV for each minute y
    
    # Filter data for that minute
    bbi_min = bbi_garmin %>% 
      dplyr::filter(isoDate_min == y)
    
    # Create RHRV object
    hrv.data = CreateHRVData()
    
    # Check if sample size is sufficient
    if (nrow(bbi_min) > 10 && all(bbi_min$bbi > 0, na.rm = TRUE)) {
      tryCatch({
        # Format bbi for RHRV and include it in the object
        hrv.data$Beat = data.frame(
          Time = c(0, cumsum(bbi_min$bbi[-length(bbi_min$bbi)])/1000),
          RR = bbi_min$bbi
        )
        
        # Compute HRV metrics
        HRV = hrv.data %>% 
          BuildNIHR() %>% 
          InterpolateNIHR(freqhr = 4) %>% 
          CreateNonLinearAnalysis() %>%   
          CreateTimeAnalysis(size = 5, interval = 7.8125) %>% 
          CreateFreqAnalysis() %>% 
          CalculatePowerBand(indexFreqAnalysis = 1, size = 300, shift = 30) %>% 
          PoincarePlot(indexNonLinearAnalysis = 1, timeLag = 1, doPlot = FALSE)
        
        # Non-linear analysis steps
        selectedTL <- CalculateTimeLag(HRV, doPlot = FALSE, technique = "ami", method = "first.minimum")
        selectedED <- CalculateEmbeddingDim(
          HRVData = HRV,
          maxEmbeddingDim = 20,
          threshold = 0.98,
          timeLag = selectedTL,
          numberPoints = 5000, doPlot = F)
        HRV <- HRV |> CreateNonLinearAnalysis() |> RQA(
          indexNonLinearAnalysis = 2,
          embeddingDim = selectedED,
          timeLag = selectedTL,
          lmin = 5,
          vmin = 5,
          radius = 73,
          doPlot = FALSE
        )
        
        # Extract relevant metrics from RQA
        rqa_metrics <- HRV$NonLinearAnalysis[[2]]$rqa[2:12]
        if (is.null(rqa_metrics)) {
          message("Skipping RQA analysis due to null result")
          rqa_metrics <- data.frame()
        } else {
          # Convert to data.frame, extract key values if needed
          if (is.list(rqa_metrics)) {
            rqa_metrics <- as.data.frame(rqa_metrics)
          }
        }
        
        # Return data frame with all HRV metrics
        return(cbind(
          data.frame(
            file = x,
            min = y,
            HR = mean(HRV$HR),
            artifacts = mean(bbi_min$artif)
          ),
          HRV$TimeAnalysis %>% as.data.frame(),
          HRV$FreqAnalysis %>% as.data.frame(),
          HRV$NonLinearAnalysis[[1]] %>% as.data.frame(),
          rqa_metrics
        ) %>% select(-size))
        
      }, error = function(e) {
        # Handle errors for this specific minute
        message("Error processing minute:", y, "-", e$message)
        return(NULL)
      })
    } else {
      message("Skipping minute:", y, "due to insufficient or invalid data.")
      return(NULL)
    }
  })
})

write_rds(hrv_garmin, 'data/hrv_garmin-non-linear.rds')
