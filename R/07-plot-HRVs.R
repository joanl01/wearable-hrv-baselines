pacman::p_load(patchwork, tidyverse, harrypotter, lubridate, hms)

# Read in data
# remove seconds in stage
hrvs <- read_rds(here::here("data", "hrv_with_sleep_joined.rds")) |> select(-seconds_in_stage)

# Remove other measures that are not HRV related
hrvs <- hrvs |> select(-"Time", -"shift", -"sizesp", -"ULFmin", -"ULFmax",
                       -"VLFmin", -"VLFmax", -"LFmin", -"LFmax", -"HFmin",
                       -"HFmax", -"type.x", -"file")


# HRV metrics you want to plot
hrv_metrics <- c(
  "SDNN", "SDANN", "SDNNIDX", "pNN50", "SDSD", "rMSSD",
  "IRRR", "MADRR", "TINN", "HRVi", "HRV",
  "ULF", "VLF", "LF", "HF", "LFHF",
  "PoincarePlot.SD1", "PoincarePlot.SD2",
  "REC", "RATIO", "DET", "DIV", "Lmax", "Lmean",
  "LmeanWithoutMain", "ENTR", "TREND", "LAM", "Vmax"
)

# Prepare data
hrvs_long <- hrvs %>%
  mutate(
    min = as.POSIXct(min),
    date = as.Date(date)
  ) %>%
  pivot_longer(
    cols = all_of(hrv_metrics),
    names_to = "metric",
    values_to = "value"
  )

# Loop through participants
for (p in unique(hrvs_long$participant)) {
  
  # Create participant output directory if it doesn't exist
  output_dir <- here::here("figures", "Chapter 5", p)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Loop through each HRV metric
  for (metric_name in hrv_metrics) {
    
    # Filter for participant + metric
    plot_data <- hrvs_long %>%
      filter(
        participant == p,
        metric == metric_name
      )
    
    # Make plot
    p_plot <- ggplot(
      plot_data,
      aes(
        x = hms::as_hms(min),
        y = value,
        colour = type.y
      )
    ) +
      geom_step(
        aes(group = 1),
        linewidth = 0.7,
        na.rm = TRUE
      ) +
      facet_wrap(
        ~ date,
        scales = "free_x"
      ) + 
      scale_x_time( 
        limits = c( hms::as_hms("00:00:00"), 
                    hms::as_hms("23:59:59") ), 
        breaks = hms::as_hms(
          c( "00:00:00", "06:00:00", "12:00:00", "18:00:00" )), 
        labels = scales::label_time("%H:%M"), 
        expand = c(0, 0) 
        ) +
      labs(
        title = paste(metric_name, "-", p),
        x = "Time",
        y = metric_name,
        colour = "Sleep pattern"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 14
        ),
        strip.text = element_text(
          face = "bold"
        ),
        legend.position = "bottom"
      ) +
      scale_colour_hp("ravenclaw", discrete = TRUE)
    
    # Save plot
    ggsave(
      filename = file.path(
        output_dir,
        paste0(metric_name, ".pdf")
      ),
      plot = p_plot,
      width = 32,
      height = 18,
      dpi = 300
    )
  }
}


# thesis plots

p7 <- hrvs |> filter(participant == "FLARE007") |> filter(date == "2023-07-19")


HR_plot <- p7 |> ggplot(aes(x = as_hms(min), y = HR, col = type.y)) +
  geom_step(
    aes(group = 1),
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ date,
    scales = "free_x"
  ) + 
  scale_x_time( 
    limits = c( hms::as_hms("00:00:00"), 
                hms::as_hms("23:59:59") ), 
    breaks = hms::as_hms(
      c( "00:00:00", "06:00:00", "12:00:00", "18:00:00" )), 
    labels = scales::label_time("%H:%M"), 
    expand = c(0, 0) 
  ) +
  labs(
    title = "HR",
    x = "Time",
    y = "HR",
    colour = "Sleep pattern"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  scale_colour_hp("ravenclaw", discrete = TRUE)


rMSSD_plot <- p7 |> ggplot(aes(x = as_hms(min), y = rMSSD, col = type.y)) +
  geom_step(
    aes(group = 1),
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ date,
    scales = "free_x"
  ) + 
  scale_x_time( 
    limits = c( hms::as_hms("00:00:00"), 
                hms::as_hms("23:59:59") ), 
    breaks = hms::as_hms(
      c( "00:00:00", "06:00:00", "12:00:00", "18:00:00" )), 
    labels = scales::label_time("%H:%M"), 
    expand = c(0, 0) 
  ) +
  labs(
    title = "rMSSD",
    x = "Time",
    y = "rMSSD",
    colour = "Sleep pattern"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  scale_colour_hp("ravenclaw", discrete = TRUE)



LFHF_plot <- p7 |> ggplot(aes(x = as_hms(min), y = LFHF, col = type.y)) +
  geom_step(
    aes(group = 1),
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ date,
    scales = "free_x"
  ) + 
  scale_x_time( 
    limits = c( hms::as_hms("00:00:00"), 
                hms::as_hms("23:59:59") ), 
    breaks = hms::as_hms(
      c( "00:00:00", "06:00:00", "12:00:00", "18:00:00" )), 
    labels = scales::label_time("%H:%M"), 
    expand = c(0, 0) 
  ) +
  labs(
    title = "LFHF",
    x = "Time",
    y = "LFHF",
    colour = "Sleep pattern"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  scale_colour_hp("ravenclaw", discrete = TRUE)



SD1_plot <- p7 |> ggplot(aes(x = as_hms(min), y = PoincarePlot.SD1, col = type.y)) +
  geom_step(
    aes(group = 1),
    linewidth = 0.7,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ date,
    scales = "free_x"
  ) + 
  scale_x_time( 
    limits = c( hms::as_hms("00:00:00"), 
                hms::as_hms("23:59:59") ), 
    breaks = hms::as_hms(
      c( "00:00:00", "06:00:00", "12:00:00", "18:00:00" )), 
    labels = scales::label_time("%H:%M"), 
    expand = c(0, 0) 
  ) +
  labs(
    title = "PoincarePlot.SD1",
    x = "Time",
    y = "PoincarePlot.SD1",
    colour = "Sleep pattern"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  ) +
  scale_colour_hp("ravenclaw", discrete = TRUE)


final_plot <- (HR_plot + rMSSD_plot) / (LFHF_plot + SD1_plot) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom", text = element_text(size = 20)) & 
  plot_annotation(title = "HRV metrics from each domain against Time for Participant 7 within one day",
       subtitle = " Colour indicates different sleep patterns")


ggsave(here::here("figures", "Chapter 5", "4hrv-time-lineplot.pdf"),
       width = 16,
       height = 9,
       units = "in",
       dpi = 500,
       plot = final_plot)
