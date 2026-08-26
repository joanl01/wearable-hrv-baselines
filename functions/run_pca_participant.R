
run_pca_participant <- function(participant_id, data) {
  
  message("Running PCA for: ", participant_id)
  
# Remove unwanted columns
  
  hrv_labeled <- data |>
    filter(participant == participant_id) |>
    select(
      -Time,
      -shift,
      -sizesp,
      -ULFmin,
      -ULFmax,
      -VLFmin,
      -VLFmax,
      -LFmin,
      -LFmax,
      -HFmin,
      -HFmax,
      -file,
      -type.x,
      -seconds_in_stage,
      -DIV,
      -participant
    ) |>
    na.omit()
  
# Conduct PCA
  
  df3_PCA <- recipe(
    ~ .,
    hrv_labeled |> select(where(is.numeric))
  ) |>
    step_zv(all_numeric()) |>
    step_normalize(all_numeric()) |>
    step_pca(all_numeric()) |>
    prep()
# variance explained
  
  variance_explained <- df3_PCA$steps[[3]]$res$sdev^2 /
    sum(df3_PCA$steps[[3]]$res$sdev^2)
  
  # Print variance explained
  print(variance_explained)
  
# Find loadings
  
  tidied <- tidy(df3_PCA, n = 3) |>
    as.data.frame()
  
  # Only use components that actually exist
  n_components <- min(6, length(unique(tidied$component)))
  
  tidied <- tidied |>
    mutate(
      component = factor(
        component,
        levels = paste0(
          "PC",
          seq_len(length(unique(tidied$component)))
        )
      )
    )
  
  loadings <- tidied |>
    mutate(
      terms = tidytext::reorder_within(
        terms,
        abs(value),
        component
      )
    ) |>
    filter(
      component %in% paste0("PC", 1:n_components)
    ) |>
    ggplot(
      aes(
        abs(value),
        terms,
        fill = value > 0
      )
    ) +
    geom_col() +
    facet_wrap(
      ~component,
      scales = "free_y"
    ) +
    tidytext::scale_y_reordered() +
    harrypotter::scale_fill_hp(
      "ravenclaw",
      discrete = TRUE
    ) +
    labs(
      title = paste0(
        "Loadings plot of the first ",
        n_components,
        " principal components of PCA for ",
        participant_id
      ),
      x = "Absolute value of contribution",
      y = NULL,
      fill = "Positive?"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom"
    ) +
    scale_x_continuous(
      breaks = c(
        0, 0.1, 0.2, 0.3,
        0.4, 0.5, 0.6
      )
    )
  
# save loadings plot
  ggsave(
    here::here(
      "figures",
      "Chapter 6",
      "PCA",
      paste0(
        participant_id,
        "-loadings-plot.pdf"
      )
    ),
    plot = loadings,
    width = 12,
    height = 8,
    units = "in",
    dpi = 500
  )
# Get scores 
  
  scores <- df3_PCA |>
    juice()
  
# Calculate eigenvalues
  
  numeric_hrv <- hrv_labeled |>
    select(where(is.numeric)) |>
    tidyr::drop_na()
  
  clean_hrv <- hrv_labeled |>
    select(where(is.numeric)) |>
    filter(
      if_all(
        everything(),
        is.finite
      )
    ) |>
    drop_na() |>
    select(
      where(
        ~ sd(.) > 0
      )
    )
  
# Find eigenvalues
  
  eig_result <- eigen(
    cor(clean_hrv)
  )
  
# Get scree plot data
  scree_df <- data.frame(
    PC = paste0(
      "PC",
      seq_along(eig_result$values)
    ),
    Eigenvalue = eig_result$values,
    Variance_Explained =
      eig_result$values /
      sum(eig_result$values)
  )
  
# Make scree pots
  
  scree_plot <- ggplot(
    scree_df,
    aes(
      x = reorder(
        PC,
        -Eigenvalue
      ),
      y = Variance_Explained
    )
  ) +
    geom_col(
      fill = "steelblue"
    ) +
    geom_line(
      aes(group = 1),
      color = "black"
    ) +
    geom_point(
      color = "black"
    ) +
    labs(
      title = paste0(
        "Scree Plot of PCA for ",
        participant_id
      ),
      x = "Principal Component",
      y = "Proportion of Variance Explained"
    ) +
    theme_minimal()
  
# save scree plot
  ggsave(
    here::here(
      "figures",
      "Chapter 6",
      "PCA",
      paste0(
        participant_id,
        "-scree-plot.pdf"
      )
    ),
    plot = scree_plot,
    height = 5,
    width = 10,
    units = "in",
    dpi = 500
  )
  
# return results as list
  
  list(
    participant = participant_id,
    pca = df3_PCA,
    scores = scores,
    loadings = tidied,
    variance_explained = variance_explained,
    eigenvalues = eig_result,
    scree_data = scree_df,
    loading_plot = loadings,
    scree_plot = scree_plot
  )
}
