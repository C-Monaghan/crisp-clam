create_additive_predictions <- function(
    data, 
    model, 
    var, 
    var_seq, 
    hold_constant = list(Gender = factor(0), Education_tri = factor(0))) {
  
  # Set default prediction sequence based on variable type
  if (is.null(var_seq)) {
    if (is.numeric(data[[var]])) {
      predict_seq <- seq(min(data[[var]], max(data[[var]]), length = 200))
    } else if (is.factor(data[[var]])) {
      predict_seq <- levels(data[[var]])
    } else {
      predict_seq <- unique(data[[var]])
    }
  }
  
  # Create list of all variables needed for prediction
  all_vars <- all.vars(formula(model)[-2])  # Get RHS variables from model formula
  
  # Create base grid with all variables EXCEPT the prediction variable
  base_grid <- expand.grid(
    lapply(all_vars, function(v) {
      if (v == var) return(NULL)  # Skip prediction variable
      
      # Special cases - keep all levels
      if (v %in% c("status_prev", "wave")) {
        return(unique(data[[v]]))
      }
      
      if (v %in% names(hold_constant)) {
        # Use user-specified constant value
        hold_constant[[v]]
      } else if (is.numeric(data[[v]])) {
        # Use mean for numeric variables
        mean(data[[v]], na.rm = TRUE)
      } else if (is.factor(data[[v]])) {
        # Use first level for factors
        factor(levels(data[[v]])[1], levels = levels(data[[v]]))
      } else {
        # Default to most common value
        names(sort(table(data[[v]]), decreasing = TRUE))[1]
      }
    }) |> purrr::compact() |> setNames(all_vars[all_vars != var]),
    KEEP.OUT.ATTRS = FALSE
  )
  
  # Create full prediction grid
  pred_grid <- base_grid |> 
    tidyr::crossing(!!var := var_seq) |>
    modelr::add_predictions(model = model, var = "pred", type = "probs") |>
    tidy_predictions() |>
    mutate(wave = factor(wave))
  
  return(pred_grid)
}