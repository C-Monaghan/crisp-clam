plot_additive_predictions <- function(data, var, x_label, subtitle) {
  
  # Generating predictions
  data |>
    ggplot(aes(x = as.numeric(.data[[var]]), y = prob, color = status_prev)) +
    geom_line(linewidth = 1) +
    ggokabeito::scale_color_okabe_ito() +
    labs(
      title = "Transition Probabilities (Additive Model)",
      subtitle = subtitle,
      x = x_label, 
      y = "Predicted Probability", 
      color = "Previous State"
    ) +
    facet_grid(wave ~ status, labeller = labeller(
      wave = function(x) case_when(
        x == 2 ~ "Year = 2018",
        x == 3 ~ "Year = 2020",
        x == 4 ~ "Year = 2022",
      ),
      status = function(x) paste("Transition to:", x)
    )) +
    theme(
      plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
      panel.spacing = unit(1, "lines")) 
}