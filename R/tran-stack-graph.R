tran_stack_graph <- function(data) {
  # Creates stacked bar charts visualizing cognitive state transitions across time periods.
  # Shows composition of current states by previous state, faceted by observation period.
  # Arguments:
  #   - data: Transition dataset from create_transition_dataset()
  # Returns:
  #   - ggplot object showing stacked transition proportions
  # 
  data |>
    ggplot(aes(x = t_minus_1, y = Freq, fill = t)) +
    geom_col(position = "stack", colour = "black") +
    facet_wrap(~ Period, ncol = 3) +
    labs(
      x = "Previous State (t-1)", 
      y = "Count",
      title = "Outcomes by Prior Cognitive State",
      fill = "Current State (t)"
    ) +
    ggokabeito::scale_fill_okabe_ito() +
    ggeasy::easy_move_legend("bottom")
}
