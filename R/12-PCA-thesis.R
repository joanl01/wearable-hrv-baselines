# Load packages
pacman::p_load(readr, tidyverse, gt, fs, here, purrr,scales, lubridate, caTools,
               tidymodels, GGally, patchwork)

# Read all functions
fs::dir_ls(here::here("functions")) |> walk(source)

hrv_labeled <- read_rds(here::here("data", "hrv_with_sleep_joined.rds")) |> filter(participant == "FLARE007") |> 
  select(-"Time", -"shift", -"sizesp", -"ULFmin", -"ULFmax",
         -"VLFmin", -"VLFmax", -"LFmin", -"LFmax", -"HFmin",
         -"HFmax", -"file",-type.x, -seconds_in_stage,-DIV, -participant)|> 
  na.omit()

# PCA
df3_PCA <-
  recipe(~., hrv_labeled |> select(where(is.numeric))) %>%
  step_zv(all_numeric()) |> 
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric()) %>%
  prep()
df3_PCA$steps[[3]]$res$sdev^2/ sum(df3_PCA$steps[[3]]$res$sdev^2)
tidied <- tidy(df3_PCA, n = 3) |> as.data.frame()
tidied <- tidied |> mutate(component = factor(component, levels = paste0("PC", 1:43)))

# Loadings plot
loadings <- tidied|> 
  mutate(terms = tidytext::reorder_within(terms, 
                                          abs(value), 
                                          component)) %>% filter(component %in% paste0("PC", 1:6)) |> 
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  harrypotter::scale_fill_hp("ravenclaw", discrete = TRUE) + 
  labs(title = "Loadings plot of the first 6 principal components of PCA for Participant 7",
       x = "Absolute value of contribution",
       y = NULL, fill = "Positive?"
  ) + theme_bw() + theme(legend.position = "bottom") + scale_x_continuous(breaks = c(0, 0.1,0.2, 0.3,0.4,0.5, 0.6))

ggsave(here::here("figures", "Chapter 6", "PCA", "loadings-plot.pdf"),
       width = 12, height = 8, 
       units = "in", dpi = 500)

# Scores
df3_PCA |> juice()


numeric_hrv <- hrv_labeled %>% dplyr::select(where(is.numeric))
numeric_hrv <- numeric_hrv %>% tidyr::drop_na()
# eig_result <- eigen(cor(numeric_hrv))
summary(numeric_hrv)

sapply(numeric_hrv, function(x) sum(!is.finite(x)))  # Count of Inf/NaN per column

clean_hrv <- hrv_labeled %>%
  select(where(is.numeric)) %>%
  filter(if_all(everything(), is.finite)) %>%
  drop_na() %>%
  select(where(~ sd(.) > 0))  # Remove constant columns

eig_result <- eigen(cor(clean_hrv))
eig_result

sd <- df3_PCA$steps[[3]]$res$sdev
sd^2


# Create a data frame of eigenvalues and % variance explained
scree_df <- data.frame(
  PC = paste0("PC", 1:length(eig_result$values)),
  Eigenvalue = eig_result$values,
  Variance_Explained = eig_result$values / sum(eig_result$values)
)

# Plot
scree_plot <- ggplot(scree_df, aes(x = reorder(PC, -Eigenvalue), y = Variance_Explained)) +
  geom_col(fill = "steelblue") +
  geom_line(aes(group = 1), color = "black") +
  geom_point(color = "black") +
  labs(
    title = "Scree Plot of PCA for Participant 7",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal()

ggsave(here::here("figures", "Chapter 6", "PCA", "scree-plot.pdf"),
       height = 5, width = 10, units = "in",
       dpi = 500, plot = scree_plot)
