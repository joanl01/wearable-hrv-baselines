# Load packages
pacman::p_load(readr, tidyverse, gt, tidymodels, corrplot, vip, DALEXtra, GGally, plotly,ggdendro)
# Read all functions
fs::dir_ls(here::here("functions")) |> walk(source)

hrvs <- read_rds(here::here("data", "hrv_with_sleep_joined.rds")) |> select(-seconds_in_stage)


# input combinations
timeline <- c("first moment before wake","first moment after wake", "deep sleep", "awake")
days <- c(5)

# Create combinations and name the columns
all_combinations <- expand.grid(
  timeline = timeline,
  days = days,
  stringsAsFactors = FALSE
)


# Apply the function to each row of combinations
combined_results_stress <- purrr::pmap_dfr(
  all_combinations,
  function(timeline, days, metric) {
    result <- summarise_hrv_timeline_by_random(hrvs, timeline = timeline, days = days)
    result |> 
      mutate(timeline = timeline, days = factor(days))
  }
)


plot <- combined_results_stress %>% na.omit() %>% filter(DIV< Inf) %>% select(where(is.numeric)) %>% 
  select(-"Time", -"shift", -"sizesp", -"ULFmin", -"ULFmax",
         -"VLFmin", -"VLFmax", -"LFmin", -"LFmax", -"HFmin",
         -"HFmax") 

pairs_random <- plot |> select(SDANN, rMSSD, TINN, HRVi, DET, REC) |> ggpairs() + theme_classic() + 
  theme(text = element_text(size = 15)) + labs(title = "Pairwise plot for five randomly selected days")


ggsave(here::here("figures", "Chapter 6", "Correlation", "random-days","random-days-pairs-plot.pdf"),
       width = 16, 
       height = 9, 
       units = "in",
       dpi = 500,
       plot = pairs_random)

hclust_rand <- hclust(dist(t(as.matrix(plot)))) %>%
  ggdendrogram(size = 5, rotate = FALSE) + labs(
    title = "Cluster Dendrogram of all baseline measures using randomly selected days"
  ) + theme(text = element_text(size = 20))

ggsave(here::here("figures", "Chapter 6", "Correlation", "random-days", "random-days-hclust.pdf"),
       plot = hclust_rand, 
       width = 16, 
       height = 9,
       dpi = 500,
       units = "in")


# Splitting by each timeline and days, get the correlations
df <- combined_results_stress %>%
  group_by(days, timeline, participant) %>%
  filter(n() >= 3) %>%          # ✅ keep only groups with ≥ 3 rows
  nest() %>%
  mutate(corrs = map(data, get_corr)) %>%
  unnest(corrs)

# Filter unnecessary variables
# add comparing variable column
df <- df |>
  mutate(comp = str_glue("{var1}={var2}")) |> 
  filter(!var1 %in% c("Time", "shift", "sizesp", "ULFmin", 
                      "ULFmax", "VLFmin", "VLFmax", "LFmin","LFmax", "HFmin","HFmax", "DIV")) |> 
  filter(!var2 %in% c("Time", "shift", "sizesp", "ULFmin", 
                      "ULFmax", "VLFmin", "VLFmax", "LFmin","LFmax", "HFmin","HFmax","DIV"))



# Select unique combinations of comparisons
df_unique <- df |>
  mutate(
    var_min = pmin(var1, var2),
    var_max = pmax(var1, var2)
  ) |>
  distinct(timeline, days, var_min, var_max, .keep_all = TRUE)|>
  mutate(comp = str_glue("{var1}={var2}")) |> filter(var1 != var2)

unique_measures <- df$var1 |> unique()


corr_all <- combined_results_stress |> get_corr()

corr_all <- corr_all |>
  mutate(comp = str_glue("{var1}={var2}")) |> 
  filter(!var1 %in% c("Time", "shift", "sizesp", "ULFmin", 
                      "ULFmax", "VLFmin", "VLFmax", "LFmin","LFmax", "HFmin","HFmax", "DIV")) |> 
  filter(!var2 %in% c("Time", "shift", "sizesp", "ULFmin", 
                      "ULFmax", "VLFmin", "VLFmax", "LFmin","LFmax", "HFmin","HFmax","DIV"))

vars <- names(hrvs)

heat_df_random <- corr_all |>
  mutate(
    var1 = factor(var1, levels = vars),
    var2 = factor(var2, levels = vars)
  ) |>
  filter(as.numeric(var1) < as.numeric(var2))


corr_all_unique <- corr_all |>
  mutate(
    var_min = pmin(var1, var2),
    var_max = pmax(var1, var2)
  ) |>
  mutate(comp = str_glue("{var1}={var2}")) |> filter(var1 != var2)

heatmap_random_all <- heat_df_random|> 
  ggplot(aes(x = var1, y = var2, fill = cor)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(cor, 2)), size = 2) + 
  scale_fill_gradient2(
    low = "#006699",      # negative correlations
    mid = "white",        # 0 correlation
    high = "#B35900",     # positive correlations
    midpoint = 0
  )+
  coord_fixed() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6),
    panel.grid = element_blank(),
    # strip.text = element_text(size = 6)
  ) +
  labs(
    x = "Variable 1",
    y = "Variable 2",
    fill = "Correlation"
  )

ggsave(here::here("figures", "Chapter 6", "Correlation", "random-days", "random-days-heatmap-all.pdf"),
       height = 10, 
       width = 10,
       units = "in",
       plot = heatmap_random_all,
       dpi = 500)



# Get all unique combinations
combinations <- df_unique %>%
  distinct(participant, timeline, days)



# Loop over each combination and save plot
combinations %>%
  pwalk(function(participant, timeline, days) {
    
    # Filter the data for this combo
    df_sub <- df_unique %>%
      filter(participant == !!participant,
             timeline == !!timeline,
             days == !!days)
    
    # Skip empty data
    if (nrow(df_sub) == 0) return(NULL)
    
    # Build the plot
    p <- ggplot(df_sub, aes(x = var_min, y = var_max, fill = cor)) +
      geom_tile(color = "black") +
      geom_text(aes(label = round(cor, 2)), color = "black", size = 2) +
      scale_fill_gradient2(
        low = "darkgreen", mid = "white", high = "purple",
        midpoint = 0, limits = c(-1, 1)
      ) +
      coord_fixed() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        panel.grid = element_blank(),
        strip.text = element_text(size = 8)
      ) +
      labs(
        title = paste0(participant, " – ", timeline),
        x = "Variable 1",
        y = "Variable 2",
        fill = "Correlation"
      )
    
    # Construct safe filename
    filename <- here::here(
      "figures",
      "Chapter 6", "Correlation",
      "random-days",
      paste0("random-days-", participant, "-", gsub(" ", "-", timeline), ".pdf")
    )
    
    # Save the plot
    ggsave(filename, plot = p, width = 10, height = 10, dpi = 500)
  })

# par(mfrow = c(2, 2))  # 2x2 layout

clean_df <- combined_results_stress |> select(-DIV) |> na.omit()|> 
  select(-"Time", -"shift", -"sizesp", -"ULFmin", 
         -"ULFmax", -"VLFmin", -"VLFmax", -"LFmin",
         -"LFmax", -"HFmin",-"HFmax")


for (participant in unique(combined_results_stress$participant)) {
  
  # Subset to current participant
  df_participant <- clean_df %>% filter(participant == !!participant)
  
  # ---- Deep Sleep ----
  deep_sleep <- df_participant %>% 
    filter(timeline == "deep sleep") %>% 
    select(where(is.numeric))
  
  if (nrow(deep_sleep) > 1) {
    deep_sleep <- hclust(dist(t(as.matrix(deep_sleep)))) %>%
      ggdendrogram(size = 5, rotate = FALSE) + labs(
        title = paste("Cluster Dendrogram of Deep Sleep baselines\nParticipant:", participant)
      )
    ggsave(here::here("figures", "Chapter 6", "Correlation", "random-days", paste0("random-days-deep-sleep-hclust-",participant,".pdf")),
           plot = deep_sleep,
           height = 10, 
           width = 10, 
           units = "in",
           dpi = 500)
  } else {
    plot.new()
    title(main = paste("No data for Deep Sleep\nParticipant:", participant))
  }
  
  # ---- First Moment Before Wake ----
  fmb4w <- df_participant %>%
    filter(timeline == "first moment before wake") %>%
    select(where(is.numeric))
  
  if (nrow(fmb4w) > 1) {
    first_before <- hclust(dist(t(as.matrix(fmb4w)))) %>%
      ggdendrogram(size = 5, rotate = FALSE) + labs(
        title = paste("Cluster Dendrogram of First Moment Before Awake\nParticipant:", participant)
      )
    ggsave(here::here("figures", "Chapter 6", "Correlation", "random-days",paste0("random-days-first-before-hclust-",participant,".pdf")),
           plot = first_before,
           height = 10, 
           width = 10, 
           units = "in",
           dpi = 500)
    
  } else {
    plot.new()
    title(main = paste("No data for First Moment Before Awake\nParticipant:", participant))
  }
  
  
  # ---- First Moment After Wake ----
  fmafw <- df_participant %>%
    filter(timeline == "first moment after wake") %>%
    select(where(is.numeric))
  
  if (nrow(fmafw) > 1) {
    first_after <- hclust(dist(t(as.matrix(fmafw)))) %>%
      ggdendrogram(size = 5, rotate = FALSE) + labs(
        title = paste("Cluster Dendrogram of First Moment After Awake\nParticipant:", participant))
    ggsave(here::here("figures", "Chapter 6", "Correlation", "random-days", paste0("random-days-first-after-hclust-",participant,".pdf")),
           plot = first_after,
           height = 10, 
           width = 10, 
           units = "in",
           dpi = 500)
  } else {
    plot.new()
    title(main = paste("No data for First Moment After Awake\nParticipant:", participant))
  }
  
  
  # ---- Awake ----
  awake <- df_participant %>%
    filter(timeline == "awake") %>%
    select(where(is.numeric))
  
  if (nrow(awake) > 1) {
    rand_awake <- hclust(dist(t(as.matrix(awake)))) %>% ggdendrogram(size = 5, rotate = FALSE) + labs(
      title = paste("Cluster Dendrogram of Random Moment of Presumed Awake\nParticipant:", participant))
    ggsave(here::here("figures", "Chapter 6", "Correlation", "random-days", paste0("random-days-rand-awake-hclust-",participant,".pdf")),
           plot = rand_awake,
           height = 10, 
           width = 10, 
           units = "in",
           dpi = 500)
    
  } else {
    plot.new()
    title(main = paste("No data for Awake\nParticipant:", participant))
  }
  
  # Optionally pause between participants
  # readline(prompt = paste0("Press [Enter] to view next participant (", participant, ")..."))
}

