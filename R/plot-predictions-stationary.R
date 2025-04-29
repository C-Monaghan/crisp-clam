plot_predictions_stationary <- function(predictions, variable, x_axis) {
  # Visualizes predicted transition probabilities from stationary multi-state models.
  # Shows probability curves by procrastination level, stratified by baseline state.
  # Arguments:
  #   - predictions: Tidy predictions from tidy_predictions()
  # Returns:
  #   - Faceted ggplot showing predicted probability curves
  # 
  predictions |>
    ggplot(aes(x = {{variable}}, y = prob, colour = status_prev)) +
    geom_line(linewidth = 1) +
    ggokabeito::scale_colour_okabe_ito() +
    labs(
      title = "Predicted transition probabilities (stationary model)",
      x = x_axis, y = "Probability", colour = "Previous State") +
    facet_wrap(~ status, labeller = labeller(
      status = function(x){paste0("Transition to: ", x)}
    ))
}
