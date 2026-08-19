# Load packages
pacman::p_load(readr, tidyverse, gt, fs, here, purrr,scales)

# Read all functions
fs::dir_ls(here::here("functions")) |> walk(source)

# Define BBI data paths
bbi_paths <- paste0("FLARE", sprintf("%03d", 1:11), "_", c("HJ", "DC", "TM", "BP", "JT", "JA", "JH", "AP", "AJ", "KL", "JS"), "_data/garmin-device-bbi")
bbi_paths <- here::here("data", bbi_paths)

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

# Read BBI data
bbi_data <- map(bbi_paths, read_bbis)

# Read Sleep data
sleep_data <- map(sleep_paths, read_sleep) 

# Convert BBI and Sleep data to minute intervals
bbi_data <- map(bbi_data, na.omit)
bbi_data <- map(bbi_data, convert_bbi_min)

# BBI number 6 NA (removed for now)
sleep_data <- map(sleep_data, na.omit)
sleep_data <- map(sleep_data, convert_sleep_min)

# Sleep number 11 NA(also removed for now)


# naming the list for each participant
names(bbi_data) <- paste0("bbi_df", sprintf("%02d", 1:11))
names(sleep_data) <- paste0("sleep_df", c(1:3, 5:8, 10:11))

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

# Change NA to not sleeping (due to lack of sleep data)
for (j in 1: length(joined_data_without_na)) {
  joined_data_without_na[[j]]$type <- as.factor(joined_data_without_na[[j]]$type)
  joined_data_without_na[[j]]$type <- joined_data_without_na[[j]]$type |> fct_na_value_to_level("not_sleeping")
}



# ---- make plots -----


# Get 15 minute intervals
joined_15_without_na <- map(joined_data_without_na, convert_bbi_sleep_15_min)

# plot and save all
plots_bbi_and_sleep_without_na <- imap(joined_15_without_na, function(df,i){
  plot_bbi_and_sleep(df, paste("BBI against Sleep Quality for ", i))
})

# save plots
for (i in names(plots_bbi_and_sleep_without_na)) {
  ggsave(
    filename = here::here("figures", "Chapter 3", "sleep-vs-bbi-all", paste0(i, "_BBI_sleep.pdf")),
    plot = plots_bbi_and_sleep_without_na[[i]],
    width = 16,
    height = 9,
    dpi = 500
  )
}

# good example of data
plot_data_thesis_p7 <- joined_15_without_na$joined_df07 |> filter(!is.na(bbi))|> 
  filter(date == "2023-06-19"|date == "2023-06-20"|date == "2023-06-21"|date == "2023-06-22"| date == "2023-06-23"|date == "2023-06-24")


plot_data_thesis_p7 |> plot_bbi_and_sleep(title = "BBI against Time coloured by Sleep Quality for Participant 7")

ggsave(here::here("figures", "Chapter 3", "bbi_vs_sleep_plot_without_na07.pdf"), 
       width = 16,
       height = 9,
       units = "in",
       dpi = 500)


# bad example of data
plot_data_thesis_p6 <- joined_15_without_na$joined_df06 |> filter(!is.na(bbi))|> 
  filter(date == "2023-05-27"|date == "2023-05-28"|date == "2023-05-29"|date == "2023-05-31"| date == "2023-06-13"|date == "2023-06-01")
plot_data_thesis_p6 |> plot_bbi_and_sleep(title = "BBI against Time coloured by Sleep Quality for Participant 6")



ggsave(here::here("figures", "Chapter 3", "bbi_vs_sleep_plot_without_na06.pdf"), 
       width = 16,
       height = 9,
       units = "in",
       dpi = 500)
