# Load packages
pacman::p_load(readr, tidyverse, gt, fs, here, purrr, caTools, pROC)

# Load Data
hrv_with_sleep_all <- readRDS(here::here("data", "hrv_with_sleep_joined.rds"))


features <- c(
  "HR", "SDNN", "SDANN", "SDNNIDX", "pNN50", "SDSD", "rMSSD",
  "IRRR", "MADRR", "TINN", "HRVi", "HRV",
  "ULF", "VLF", "LF", "HF", "LFHF",
  "PoincarePlot.SD1", "PoincarePlot.SD2",
  "REC", "RATIO", "DET", "DIV", "Lmax", "Lmean",
  "LmeanWithoutMain", "ENTR", "TREND", "LAM", "Vmax"
)
participants <- unique(hrv_with_sleep_all$participant)

results <- purrr::map(
  participants,
  ~ run_participant_analysis(
    participant_id = .x,
    data = hrv_with_sleep_all,
    features = features
  )
)

# Remove participants that were skipped
results <- purrr::compact(results)

names(results) <- purrr::map_chr(
  results,
  "participant"
)


# ---------------------------------------------------------
# Combine AUC results for all participants
# ---------------------------------------------------------

auc_results <- purrr::map_dfr(
  results,
  "summary"
)

auc_results

