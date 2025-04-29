get_time_varying_matrix <- function(model, time_point) {
  expand.grid(
    Gender = factor(0),
    Age = mean(data_stack$Age),
    Education_tri = factor(0),
    Depression = mean(data_stack$Depression),
    Total_p = mean(data_stack$Total_p),
    status_prev = factor(1:3),
    wave = time_point) |> 
    modelr::add_predictions(model, var = "prob", type = "probs") |> 
    select(status_prev, starts_with("prob")) |> 
    mutate(
      status_prev = factor(status_prev, labels = state_names),
      across(starts_with("prob"), ~round(., 3))) |>
    tibble::column_to_rownames("status_prev") |>
    as.matrix() |> `colnames<-`(state_names)
}