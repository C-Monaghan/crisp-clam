count_transitions <- function(data, years, absorbing = FALSE) {
  # Counts the occurrences of cognitive status transitions across specified 
  # years.
  # Converts numeric cognitive status codes (1, 2, 3) into descriptive labels
  # ("Normal Cognition", "MCI", "Dementia") and handles missing or unexpected values.
  # Aggregates the data to count transitions and reshapes it into a long format for analysis.
  # Arguments:
  #    - data: The input dataset containing cognitive function data.
  #    - years: A vector of years for which transitions should be counted.
  # Returns:
  #   - A dataset with counts of cognitive status transitions, including
  #   "Missing" and "Other" categories.
  
  # Create dynamic column names based on the years provided
  cogfunction_cols <- paste0("cogfunction", years)
  
  data <- data |>
    select(ID, all_of(cogfunction_cols)) |>
    na.omit() |>
    tidyr::pivot_longer(
      cols = !ID,
      names_to = "Wave",
      values_to = "Classification"
    ) |>
    mutate(
      Wave = stringr::str_replace(Wave, "cogfunction", "HRS "),
      Wave = factor(Wave, levels = c(paste0("HRS ", years))),
      
      Classification = case_when(
        Classification == 1 ~ "Normal Cognition",
        Classification == 2 ~ "MCI",
        Classification == 3 ~ "Dementia")
    )
  
  if(absorbing == TRUE){
    data <- data |>
      group_by(ID) |>
      mutate(
        Classification = ifelse(
          cumany(Classification == "Dementia"), "Dementia", Classification)) |>
      ungroup()
  }
  
  data <- data |>
    mutate(Classification = factor(
      Classification,
      levels = c("Normal Cognition", "MCI", "Dementia")))
  
  return(data)
}
