plot_bbi_and_sleep <- function(df, title){
  
  df_plot <- df |> 
    filter(!is.na(bbi)) |> 
    mutate(
      time_of_day = hms::as_hms(time_15_min)
    )
  
  df_plot |> 
    ggplot(aes(x = time_of_day, y = bbi, col = type)) + 
    theme_bw() +
    geom_step(aes(group = 1), linewidth = 1) + 
    harrypotter::scale_colour_hp_d("Ravenclaw") + 
    facet_wrap(~ date, scales = "fixed") +
    scale_x_time(
      limits = c(
        hms::as_hms("00:00:00"),
        hms::as_hms("23:59:59")
      ),
      breaks = hms::as_hms(c(
        "00:00:00",
        "06:00:00",
        "12:00:00",
        "18:00:00"
      )),
      labels = scales::label_time("%H:%M"),
      expand = c(0, 0)
    ) +
    theme(
      legend.position = "bottom",
      text = element_text(size = 20)
    ) + 
    labs(
      x = "Time", 
      y = "BBI", 
      col = "Sleep pattern"
    ) + 
    ggtitle(title)
}
