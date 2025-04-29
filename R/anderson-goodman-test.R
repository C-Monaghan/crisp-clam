anderson_goodman_test <- function(data, stationary_probs, non_stationary_models) {
  
  # Get observed vs. expected counts
  obs_counts <- data |>
    group_by(wave, status_prev, status) |>  
    summarise(n = n(), .groups = "drop")|>
    complete(wave, status_prev, status, fill = list(n = 0))
  
  exp_counts <- purrr::map_dfr(
    names(non_stationary_models), function(w) {
      data_w <- data_list[[w]]
      pred_w <- predict(non_stationary_models[[w]], type = "probs", newdata = pred_grid)
      
      pred_w |>
        as.data.frame() |>
        mutate(wave = as.numeric(w), status_prev = pred_grid$status_prev) |>
        pivot_longer(cols = !c(wave, status_prev), 
                     names_to = "status", 
                     values_to = "prob_nonstat")
  })
  
  # Calculate test statistic components
  ag_test <- obs_counts |>
    left_join(stationary_probs, by = c("status", "status_prev")) |>
    left_join(exp_counts, by = c("wave", "status_prev", "status")) |>
    mutate(
      component = ifelse(prob_stat > 0, n * (prob_nonstat - prob_stat)^2 / prob_stat, 0)
    ) |>
    summarise(ag_stat = sum(component)) |>
    pull(ag_stat)
  
  ag_df <- (nrow(exp_counts_nonstat) - nrow(stationary_probs))
  ag_p_val <- pchisq(ag_test, df = ag_df, lower.tail = FALSE)
  
  return(list(
    ag_test = ag_test,
    ag_df = ag_df,
    ag_p_val = ag_p_val
  ))
}

