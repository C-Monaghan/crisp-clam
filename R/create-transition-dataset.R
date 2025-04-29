create_transition_dataset <- function(data, transition_results) {
  # Combines multiple transition tables into a single analysis-ready dataset.
  # Formats period labels, ensures consistent factor levels, and structures data for visualization.
  # Arguments:
  #   - data: List of year pairs to process (e.g., list(c(2016,2018)))
  #   - transition_results: List containing transition tables from create_transition_table()
  # Returns:
  #   - Tidy dataset with transition frequencies between all specified periods
  # 
  data |>
    purrr::map_dfr(~ {
      period_name <- paste(.x[2], .x[1], sep = " - ")
      tbl <- transition_results[[paste(.x[2], .x[1], sep = "-")]]$table
      
      as.data.frame(tbl) |>
        rename(t_minus_1 = 1, t = 2) %>%  # Positional renaming
        mutate(Period = period_name, .before = t_minus_1)
    }) |>
    mutate(
      Period = stringr::str_replace(Period, "(\\d+) - (\\d+)", "\\2 - \\1"),
      Period = factor(Period, levels = c("2016 - 2018", "2018 - 2020", "2020 - 2022")),
      t_minus_1 = factor(t_minus_1, levels = c("Normal Cognition", "MCI", "Dementia")),
      t = factor(t, levels = c("Normal Cognition", "MCI", "Dementia"))
    )
}