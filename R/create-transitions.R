create_transitions <- function(data, absorbing = FALSE){
  # Reshapes data from wide to long format to track cognitive status transitions over time.
  # Calculates the next wave's cognitive status for each individual and creates a transition column.
  # Optionally treats "Dementia" as an absorbing state, meaning once an individual is classified
  # with dementia, their status cannot change in subsequent waves.
  # Arguments:
  #   - data: The dataset containing cognitive status data.
  #   - absorbing: A logical flag indicating whether "Dementia" should be treated 
  #   as an absorbing state.
  # Returns:
  #   - A dataset with transition information, including current and next wave statuses.
  
  # Reshape the data from wide to long format to track cognitive status over waves
  data <- data |>
    select(ID, starts_with("cogfunction")) |>
    tidyr::pivot_longer(cols = !ID,
                        names_to = "Wave",
                        values_to = "Status") |>
    mutate(Wave = as.factor(stringr::str_replace(Wave, "cogfunction", ""))) |>
    # Arrange by ID and Wave to prepare for transition calculation
    arrange(ID, Wave) |>
    group_by(ID) |>
    # Get the next wave's cognitive status for each person
    mutate(next_wave_status = lead(Status)) |>
    ungroup()
  
  # We can optionally specify dementia as an absorbing state
  # Once an individual is classified with dementia they cannot be classified
  # with anything else 
  if(absorbing == TRUE) {
    data <- data |>
      group_by(ID) |>
      mutate(
        Status = ifelse(cumany(Status == "Dementia"), "Dementia", Status),
        next_wave_status = ifelse(cumany(Status == "Dementia"), "Dementia", next_wave_status),
        transition = paste(Status, next_wave_status, sep = " to ")
      ) |>
      filter(Wave %in% c(2016, 2018, 2020))
  }
  
  # Filter out rows where either the current or next status is missing
  data <- data |>
    group_by(ID) |>
    filter(!is.na(Status), !is.na(next_wave_status)) |>
    ungroup() |>
    # Create a new column representing the transition from one status to the next
    mutate(
      transition = paste(Status, next_wave_status, sep = " to "),
      transition = factor(
        transition, 
        levels = c("Normal Cognition to Normal Cognition", "Normal Cognition to MCI",
                   "Normal Cognition to Dementia", "MCI to Normal Cognition", "MCI to MCI",
                   "MCI to Dementia", "Dementia to Dementia") 
      ))
  
  return(data)
}
