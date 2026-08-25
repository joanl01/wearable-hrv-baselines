plot_mclust_metric <- function(data, person, metric, cluster_suffix = "") {
  
  cluster_col <- paste0("cluster_", metric, cluster_suffix)
  temp_data <- data |> filter(participant == person)
  
  figure <- ggplot(
    temp_data,
    aes(
      x = min,
      y = .data[[metric]],
      colour = factor(.data[[cluster_col]])
    )
  ) +
    geom_point() +
    labs(
      title = paste(metric, "clustered into automatic groups"),
      x = "Time",
      y = metric,
      colour = "Cluster"
    ) +
    theme_minimal() +
    facet_wrap(~type.y) +
    harrypotter::scale_color_hp(
      "Ravenclaw",
      discrete = TRUE
    )
  ggsave(here::here("figures", "Chapter 5", "mclust", person, paste0(metric, cluster_suffix, ".pdf")),
         plot = figure,
         width = 16, 
         height = 9, 
         units = "in",
         dpi = 500)
}
