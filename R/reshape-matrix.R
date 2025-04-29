reshape_matrix <- function(matrix) {
  # Converts a transition probability matrix into a tidy format suitable for visualization.
  # Transforms the matrix into long format with explicit factor levels for cognitive states,
  # preserving the original ordering while preparing for ggplot compatibility.
  # Arguments:
  #   - matrix: A square transition probability matrix with states as row/column names
  # Returns:
  #   - Tidy data frame with columns:
  #     * from_state: Factor indicating origin cognitive state
  #     * to_state: Factor indicating destination cognitive state (reversed for plotting)
  #     * probability: Numeric transition probability values
  
  matrix |>
    as.data.frame() |>
    mutate(
      from_state = factor(c(
        "Normal cognition", "MCI", "Dementia"),
        levels = c("Normal cognition", "MCI", "Dementia"))
    ) |>
    reshape2::melt(
      id.vars = "from_state", 
      variable.name = "to_state", 
      value.name = "probability") |>
    mutate(to_state = factor(to_state, levels = rev(levels(to_state))))
}
