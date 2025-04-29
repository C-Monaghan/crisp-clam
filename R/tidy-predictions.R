tidy_predictions <- function(predictions) {
  # Restructures model prediction matrices into tidy format for visualization.
  # Converts numeric codes to factor labels and reshapes multiple prediction columns
  # into key-value pairs suitable for ggplot.
  # Arguments:
  #   - predictions: Raw prediction matrix from model output
  # Returns:
  #   - Long-format dataset with probabilities for each cognitive state
  #   
  predictions |>
    as.matrix() |>
    as_tibble() |>
    mutate(
      Gender = factor(ifelse(Gender == 0, "Male", "Female"), levels = c("Male", "Female")),
      status_prev = case_when(
        status_prev == 1 ~ "Normal Cognition",
        status_prev == 2 ~ "MCI",
        status_prev == 3 ~ "Dementia",
      ),
      status_prev = factor(status_prev, levels = c("Normal Cognition", "MCI", "Dementia")),
      Total_p = as.numeric(Total_p),
      across(c(pred.1:pred.3), as.numeric)) |>
    tidyr::pivot_longer(cols = c(pred.1:pred.3), names_to = "status", values_to = "prob") |>
    mutate(status = case_when(
      status == "pred.1" ~ "Normal Cognition",
      status == "pred.2" ~ "MCI",
      status == "pred.3" ~ "Dementia"
    ),
    status = factor(status, levels = c("Normal Cognition", "MCI", "Dementia")),
    status_prev = factor(status_prev, levels = c("Normal Cognition", "MCI", "Dementia")))
}