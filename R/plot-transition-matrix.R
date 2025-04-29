plot_transition_matrix <- function(matrix, observed = TRUE) {
  # Creates a heatmap visualization of cognitive state transition probabilities.
  # Generates either observed or estimated transition plots with consistent formatting,
  # including labeled probability values and a diverging color scale for emphasis.
  # Arguments:
  #   - matrix: Tidy transition matrix from reshape_matrix()
  #   - observed: Logical flag indicating whether data represents observed (TRUE) 
  #               or estimated (FALSE) transitions
  # Returns:
  #   - ggplot heatmap object with:
  #     * State transitions as cells
  #     * Probability values displayed numerically
  #     * Custom color scale and axis formatting
  
  if(observed == TRUE) {
    subtitle <- "Observed state transitions between assessment waves"
  } else {
    subtitle <- "Estimated state transitions between assessment waves"
  }
  
  matrix |>
    ggplot(aes(x = from_state, y = to_state, fill = probability)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(
      aes(label = format(round(probability, 3), nsmall = 3)),
      size = 4.5, 
      color = "#212427",
      fontface = "bold") +
    colorspace::scale_fill_continuous_diverging(
      palette = "Blue-Red 3", mid = 0.50, alpha = 0.5, 
      limits = c(0, 1), name = "Transition \nProbability") +
    labs(title = "Transition Probabilities Across Cognitive States",
         subtitle = subtitle,
         x = "Previous State (t - 1)", 
         y = "Current State (t)") +
    theme(
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "right",
      legend.text = element_text(size = 9),
      panel.grid = element_blank()
    )
}
