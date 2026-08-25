plot_mclust_metric_histogram <- function(data, person, metric, cluster_suffix = "") {
  
  cluster_col <- paste0("cluster_", metric, cluster_suffix)
  temp_data <- data |> filter(participant == person)
  
  figure <- ggplot(
    temp_data,
    aes(
      x = .data[[metric]],
      fill = factor(.data[[cluster_col]])
    )
  ) +
    geom_histogram(alpha = 0.7, position = "identity") +
    labs(
      title = paste(metric, "clustered into automatic groups"),
      x = metric,
      fill = "Cluster"
    ) +
    theme_minimal() +
    harrypotter::scale_fill_hp(
      "Ravenclaw",
      discrete = TRUE
    )
  ggsave(here::here("figures", "Chapter 5", "mclust", person, paste0(metric, cluster_suffix, "-histogram.pdf")),
         plot = figure,
         width = 16, 
         height = 9, 
         units = "in",
         dpi = 500)
}
