tidy_output <- function(fit, multiplicative = FALSE) {
  output <- broom::tidy(fit, conf.int = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(across(c(estimate:p.value), ~ round(x = ., digits = 3)))
  
  if(any(output$y.level == "1")) {
    output <- output |>
      mutate(y.level = case_when(
        y.level == "1" ~ "MCI - NC",
        y.level == "3" ~ "MCI - Dementia"))
  } else {
    output <- output |>
      mutate(y.level = case_when(
        y.level == "2" ~ "NC - MCI",
        y.level == "3" ~ "NC - Dementia"))
  }
  
  if(multiplicative == FALSE) {
    output <- output |>
      mutate(
        term = case_when(
          term == "Gender1" ~ "Being female",
          term == "Education_tri1" ~ "High school degree vs. No education",
          term == "Education_tri2" ~ "Further education vs. No education",
          term == "Total_dep_2016" ~ "Depression Scores (2016)",
          term == "Total_p" ~ "Procrastination (2020)",
          term == "status_prev2" ~ "Previous state: MCI",
          term == "status_prev3" ~ "Previous state: Dementia",
          term == "wave" ~ "Time",
          TRUE ~ term))
  } else {
    output <- output |>
      mutate(
        term = case_when(
          term == "Gender1" ~ "Being female",
          term == "Education_tri1" ~ "High school degree vs. No education",
          term == "Education_tri2" ~ "Further education vs. No education",
          term == "Depression" ~ "Depression Scores (2016)",
          term == "Total_p" ~ "Procrastination (2020)",
          term == "status_prev2" ~ "Previous state: MCI",
          term == "status_prev3" ~ "Previous state: Dementia",
          term == "wave" ~ "Time",
          term == "Gender1:wave" ~ "Gender & Time",
          term == "Age:wave" ~ "Age & Time",
          term == "Education_tri1:wave" ~ "Education (0 - 1) & Time",
          term == "Education_tri2:wave" ~ "Education (0 - 2) & Time",
          term == "Depression:wave" ~ "Depression & Time",
          term == "Total_p:wave" ~ "Procrastination & Time",
          term == "status_prev2:wave" ~ "Previous state: MCI & Time",
          term == "status_prev3:wave" ~ "Previous state: Dementia & Time",
          TRUE ~ term))
  }
  
  return(output)
}
