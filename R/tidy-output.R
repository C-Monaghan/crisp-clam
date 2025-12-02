tidy_output <- function(fit, multiplicative = FALSE) {
  output <- broom::tidy(fit, conf.int = TRUE) |>
    rename(transition = y.level) |>
    filter(term != "(Intercept)") |>
    mutate(
      estimate = exp(estimate),
      conf.low = exp(conf.low),
      conf.high = exp(conf.high),
      colour = case_when(
        estimate > 1 & p.value < 0.05 ~ "Positive",
        estimate < 1 & p.value < 0.05 ~ "Negative",
        TRUE ~ "NS"
      ),
      colour = factor(colour, levels = c("Positive", "Negative", "NS"))
    ) |>
    mutate(across(c(estimate:p.value), ~ round(x = ., digits = 3)))

  if (any(output$transition == "1")) {
    output <- output |>
      mutate(
        transition = case_when(
          transition == "1" ~ "MCI - NC",
          transition == "3" ~ "MCI - Dementia"
        )
      )
  } else {
    output <- output |>
      mutate(
        transition = case_when(
          transition == "2" ~ "NC - MCI",
          transition == "3" ~ "NC - Dementia"
        )
      )
  }

  if (multiplicative == FALSE) {
    output <- output |>
      mutate(
        term = case_when(
          term == "Gender1" ~ "Being female",
          term == "Education_tri1" ~ "High school degree vs. No education",
          term == "Education_tri2" ~ "Further education vs. No education",
          term == "Apathy" ~ "Apathy",
          term == "Anxiety" ~ "Anxiety",
          term == "Depression" ~ "Depression",
          term == "Total_p" ~ "Procrastination (2020)",
          term == "status_prev2" ~ "Previous state: MCI",
          term == "status_prev3" ~ "Previous state: Dementia",
          term == "Total_p:Age" ~ "Age x Procrastination",
          term == "wave" ~ "Time",
          TRUE ~ term
        )
      )
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
          term == "Age:Total_p" ~ "Age x Procrastination",
          term == "wave" ~ "Time",
          term == "Gender1:wave" ~ "Gender & Time",
          term == "Age:wave" ~ "Age & Time",
          term == "Education_tri1:wave" ~ "Education (0 - 1) & Time",
          term == "Education_tri2:wave" ~ "Education (0 - 2) & Time",
          term == "Depression:wave" ~ "Depression & Time",
          term == "Total_p:wave" ~ "Procrastination & Time",
          term == "status_prev2:wave" ~ "Previous state: MCI & Time",
          term == "status_prev3:wave" ~ "Previous state: Dementia & Time",
          term == "Age:Total_p:wave" ~ "Age x Procrastination x Time",
          TRUE ~ term
        )
      )
  }

  return(output)
}
