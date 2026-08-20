
make_logistic_reg_plot <- function(data, participant, output_dir = "figures/Chapter 3/logistic_reg") {
  
  # Predicted values boxplot
  fitted_plot <- data %>%
    na.omit() %>%
    ggplot(aes(x = fitted, y = bbi, fill = fitted)) + 
    geom_boxplot() + 
    harrypotter::scale_fill_hp(
      discrete = TRUE, 
      house = "ravenclaw"
    ) +
    labs(
      title = paste(
        "BBI by Predicted Values of Logistic Regression for Participant",
        participant
      ), 
      x = "Category",
      y = "BBI", 
      fill = "Group"
    ) +
    theme_bw() + 
    theme(
      legend.position = "none", 
      text = element_text(size = 16)
    )
  
  # Actual values boxplot
  actual_plot <- data %>%
    ggplot(aes(x = type, y = bbi, fill = type)) + 
    geom_boxplot() + 
    harrypotter::scale_fill_hp(
      discrete = TRUE, 
      house = "ravenclaw"
    ) +
    labs(
      title = paste(
        "BBI by True Values for Participant",
        participant
      ), 
      x = "Category",
      y = "BBI", 
      fill = "Group"
    ) +
    theme_bw() + 
    theme(
      legend.position = "none", 
      text = element_text(size = 16)
    )
  
  # Combine plots
  report <- fitted_plot + actual_plot + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(here::here(output_dir))) {
    dir.create(here::here(output_dir), recursive = TRUE)
  }
  
  # Save plot
  ggsave(
    here::here(
      output_dir,
      paste0("log_reg_", participant, ".pdf")
    ), 
    plot = report,
    width = 16, 
    height = 9,
    units = "in",
    dpi = 500
  )
  
  return(report)
}