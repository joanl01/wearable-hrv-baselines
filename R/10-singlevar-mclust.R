pacman::p_load(mclust, tidyverse, readr)
hrv_labeled <- read_rds(here::here("data", "hrv_with_sleep_joined.rds"))

hrv_labeled <- hrv_labeled |> 
  mutate(state = ifelse(
    type.y %in% c("deep", "light", "rem"),
    "sleeping",
    "awake"
  ))
hrv_metrics <- c(
  "HR", "SDNN", "SDANN", "SDNNIDX", "pNN50", "SDSD", "rMSSD",
  "IRRR", "MADRR", "TINN", "HRVi", "HRV",
  "ULF", "VLF", "LF", "HF", "LFHF",
  "PoincarePlot.SD1", "PoincarePlot.SD2",
  "REC", "RATIO", "DET", "DIV", "Lmax", "Lmean",
  "LmeanWithoutMain", "ENTR", "TREND", "LAM", "Vmax"
)

participants <- unique(hrv_labeled$participant)

for (person in participants) {
  for (hrvar in hrv_metrics) {
    
    sleep.hist <- hrv_labeled |>
      filter(participant == person) |>
      ggplot(aes(x = .data[[hrvar]], fill = type.y)) +
      geom_histogram(alpha = 0.7, position = "identity") +
      theme_minimal() +
      facet_wrap(~state) +
      theme(
        legend.position = "bottom",
        text = element_text(size = 15)
      ) +
      labs(
        title = paste0(
          "Histogram of ", hrvar ," for ", person,
          " split by waking and sleeping stages"
        ),
        subtitle = "Colour indicates sleep pattern"
      ) +
      harrypotter::scale_fill_hp("ravenclaw", discrete = TRUE)
    
    ggsave(
      here::here(
        "figures", "Chapter 5", person,
        paste0(hrvar, "-vs-sleep.pdf")
      ),
      height = 9,
      width = 16,
      units = "in",
      dpi = 500,
      plot = sleep.hist
    )
  }
}



# plot histogram for state
hrv_labeled |> filter(participant == "FLARE007") |>  ggplot(aes(x = HR, fill = type.y)) + 
  geom_histogram(alpha = 0.7, position = "identity") + theme_minimal() + 
  facet_wrap(~state) + theme(legend.position = "bottom", text = element_text(size = 15)) + 
  labs(title = "Histogram of HR for Participant 7 split by waking and sleeping stages",
       subtitle = "Colour indicates sleep pattern") + harrypotter::scale_fill_hp("ravenclaw", discrete = TRUE)

ggsave(here::here("figures", "Chapter 5", "HR-sleep-histogram-thesis.pdf"),
       width = 16, height = 9, units= "in", dpi = 500)






# Using one example to see if it works
df3_HR_clust <- Mclust(hrv_labeled$HR)
summary(df3_HR_clust)
plot(df3_HR_clust)  # Shows the fitted density
hrv_labeled$cluster_HR<- df3_HR_clust$classification

hrv_labeled %>% mutate(date = as.Date(min)) %>% 
  ggplot(aes(x = min, col = factor(cluster_HR), y = HR )) + 
  geom_point() +
  labs(title = "HR clustered into Automatic Groups",
       x = "HR", col = "cluster group") +
  theme_minimal() + facet_wrap(~type.y) + harrypotter::scale_color_hp("Ravenclaw", discrete = TRUE)


hrv_labeled %>% ggplot(aes(HR, fill = factor(cluster_HR))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(title = "HR clustered into Automatic Groups",
       x = "HR", fill = "cluster_HR") +
  theme_minimal() + harrypotter::scale_fill_hp("ravenclaw", discrete = TRUE)





# Mclust with 2 

df3_HR_clust_2 <- Mclust(hrv_labeled$HR, G =2)
summary(df3_HR_clust_2)
plot(df3_HR_clust_2)  # Shows the fitted density
hrv_labeled$cluster_HR_2 <- df3_HR_clust_2$classification

hrv_labeled %>% mutate(date = as.Date(min)) %>% 
  ggplot(aes(x = min, col = factor(cluster_HR_2), y = HR )) + 
  geom_point() +
  labs(title = "HR cluster_HRed into Automatic Groups",
       x = "HR", fill = "cluster_HR") +
  theme_minimal() + facet_wrap(~type.y) + harrypotter::scale_color_hp("Ravenclaw", discrete = TRUE)


hrv_labeled %>% ggplot(aes(HR, fill = factor(cluster_HR_2))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(title = "HR cluster_HRed into 2 Groups",
       x = "HR", fill = "cluster_HR") +
  theme_minimal() + harrypotter::scale_fill_hp("Ravenclaw", discrete = TRUE)




hrv_labeled %>%
  group_by(cluster_HR) %>%
  summarise(
    mean_HR = mean(HR),
    sd_HR = sd(HR),
    n = n()
  ) %>% gt()



hrv_labeled %>%
  group_by(cluster_HR_2) %>%
  summarise(
    mean_HR = mean(HR),
    sd_HR = sd(HR),
    n = n()
  ) %>% gt()
