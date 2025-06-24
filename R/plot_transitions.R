plot_transitions <- function(data, size) {
  # Visualizes cognitive status transitions over time using an alluvial plot.
  # Represents the flow of individuals between cognitive states across waves.
  # Arguments:
  #   - data: The dataset containing transition data.
  #   - size: The font size for labels in the plot.
  # Returns:
  #   - A ggplot object showing transitions between cognitive states over time.
  
  require(ggalluvial)
  
  data |>
    ggplot(aes(x = Wave, y = n, stratum = Classification, 
               fill = Classification, alluvium = ID
    )) +
    geom_stratum(alpha = 0.5, width = 0.5) +
    geom_flow(width = 0.5) +
    geom_text(
      stat = "stratum",
      aes(label = stringr::str_wrap(Classification, width = 10)),
      hjust = 0.5,
      vjust = 0.5,
      size = size
    ) +
    labs(title = "Dementia transitions across time", 
         x = "", y = "Frequency") +
    scale_fill_viridis_d(direction = -1) +
    theme(axis.text = element_text(size = 10), 
          text = element_text(size = 10)) +
    ggeasy::easy_remove_legend()
}