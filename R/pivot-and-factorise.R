pivot_and_factorise <- function(data, time_vary = FALSE) {
  # Converts cognitive function data from wide to long format and factorizes key variables.
  # Pivots multiple cogfunction columns into wave/status pairs and converts categorical
  # variables (Gender, Education, status) to factors with meaningful labels.
  # Arguments:
  #   - data: Dataset containing cognitive function variables in wide format
  # Returns:
  #   - Long-format dataset with factorized variables, ordered by ID and wave
  # 
  
  if(time_vary == TRUE) {
    # Pivoting depression variable
    dep_scores <- data |>
      select(ID, starts_with("apathy")) |>
      pivot_longer(
        cols = !ID,
        names_to = "remove",
        values_to = "Depression") |>
      select(Depression)
    
    # Pivoting whole dataset
    data <- data |>
      select(!starts_with("apathy")) |>
      pivot_longer(
        cols = starts_with("cogfunction"),
        names_to = "wave",
        names_prefix = "cogfunction",
        values_to = "status")
    
    # Adding in depression scores
    data <- data |> cbind(dep_scores)
  } else{
    # No pivoting of depression
    data <- data |>
      tidyr::pivot_longer(
        cols = starts_with("cogfunction"), 
        names_to = "wave",
        names_prefix = "cogfunction", 
        values_to = "status") |>
      rename(Depression = Total_dep_2016) |>
      select(!starts_with("Total_dep_"))
  }
  
  # Factorizing
  data <- data |>
    mutate(
      Gender = factor(Gender, levels = c(0, 1)),
      Education_tri = factor(Education_tri, levels = c(0, 1, 2)),
      wave = factor(wave),
      status = factor(
        status,
        levels = c("Normal Cognition", "MCI", "Dementia"),
        labels = c(1, 2, 3))) |>
    relocate(wave, .after = ID) |>
    relocate(status, .after = wave)
    
  
  return(data)
}
