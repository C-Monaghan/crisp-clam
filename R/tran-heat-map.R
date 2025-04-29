tran_heat_map <- function(data) {
  # Generates heatmap visualization of transition frequencies between cognitive states.
  # Uses color intensity and labeled values to show transition patterns across time periods.
  # Arguments:
  #   - data: Transition dataset from create_transition_dataset()
  # Returns:
  #   - ggplot heatmap with state transitions as cells
  # 
  data |>
    ggplot(aes(x = t_minus_1, y = t, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 3.5) +  # Add counts
    scale_fill_gradient(low = "white", high = "steelblue") +
    facet_wrap(~ Period, ncol = 3) +  # Split by time period
    labs(
      x = "Previous State (t-1)", 
      y = "Current State (t)",
      title = "Cognitive State Transitions Between Time Periods",
      fill = "Frequency"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggeasy::easy_move_legend("bottom")
}
