observed_transition_matrix <- function(data) {
  # Converts transition data into a properly formatted probability transition matrix.
  # Ensures consistent state ordering and converts proportions to matrix format suitable
  # for multi-state modeling and visualization.
  # Arguments:
  #   - data: Transition dataset from create_transitions()
  # Returns:
  #   - Square transition probability matrix with states as row/column names
  # 
  # The order I want my matrix in
  state_order <- c("Normal Cognition", "MCI", "Dementia")
  
  data |>
    group_by(Status, next_wave_status) |>
    summarise(freq = n(),  .groups = "drop") |>
    group_by(Status) |>
    mutate(freq = round(proportions(freq), 3)) |>
    ungroup() |>
    complete(Status, next_wave_status, fill = list(freq = 0)) |>
    tidyr::pivot_wider(names_from = next_wave_status, values_from = freq, names_sort = TRUE) |>
    tibble::column_to_rownames("Status") |>
    select(all_of(state_order)) |>
    _[state_order, ] |>
    as.matrix()
}
