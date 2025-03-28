rm(list = ls())

# Packages ---------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Functions --------------------------------------------------------------------
extract_years <- function(data, years, impute = TRUE, cog_total = FALSE, absorbing = TRUE) {
  # Extracts cognitive function data for specified years from a dataset.
  # Converts numeric cognitive status codes (1, 2, 3) into descriptive labels
  # ("Normal Cognition", "MCI", "Dementia") for easier interpretation.
  # Arguments:
  #   - data: The input dataset containing cognitive function data.
  #   - years: A vector of years for which data should be extracted.
  # Returns:
  #   - A dataset with ID and cognitive status columns for the specified years.
  
  # Create dynamic column names based on the years provided
  cogfunction_cols <- paste0("cogfunction", years)
  cogtotal_cols    <- paste0("cogtot27_imp", years)
  
  if(cog_total == FALSE) {
    data <- data |>
      # Select only the ID column and cognitive function columns for the specified years
      select(ID, any_of(cogfunction_cols)) |>
      mutate(across(!c(ID), ~ case_when(
        .x == 1 ~ "Normal Cognition",
        .x == 2 ~ "MCI",
        .x == 3 ~ "Dementia",
        TRUE ~ NA_character_  # To handle missing/other cases
      )))
  } else {
    data <- data |>
      # Select only the ID column and cognitive function columns for the specified years
      select(ID, any_of(cogtotal_cols)) |>
      rename_with( .cols = !ID, .fn = ~ stringr::str_replace(
        string = .x,
        pattern = "cogtot27_imp", 
        replacement = "cog_score_"))
  }
  
  if(impute == TRUE){
    data <- data |>
      tidyr::pivot_longer(
        cols = !ID,
        names_to = "Wave",
        values_to = "Status") |>
      group_by(ID) |>
      tidyr::fill(Status, .direction = "down") |>
      ungroup() |>
      tidyr::pivot_wider(names_from = "Wave", values_from = "Status")
  }
  
  if(absorbing == TRUE) {
    data <- data |>
      tidyr::pivot_longer(
        cols = !ID,
        names_to = "Wave",
        values_to = "Status") |>
      group_by(ID) |>
      mutate(Status = ifelse(cumany(Status == "Dementia"), "Dementia", Status)) |>
      ungroup() |>
      tidyr::pivot_wider(names_from = "Wave", values_from = "Status")
  }
  
  return(data)
}

pivot_and_factorise <- function(data) {
  # Converts cognitive function data from wide to long format and factorizes key variables.
  # Pivots multiple cogfunction columns into wave/status pairs and converts categorical
  # variables (Gender, Education, status) to factors with meaningful labels.
  # Arguments:
  #   - data: Dataset containing cognitive function variables in wide format
  # Returns:
  #   - Long-format dataset with factorized variables, ordered by ID and wave
  # 
  data |>
    tidyr::pivot_longer(
      cols = starts_with("cogfunction"), names_to = "wave",
      names_prefix = "cogfunction", values_to = "status") |>
    mutate(
      Gender = factor(Gender, levels = c(0, 1)),
      Education_tri = factor(Education_tri, levels = c(0, 1, 2)),
      wave = factor(wave),
      status = factor(status, 
                      levels = c("Normal Cognition", "MCI", "Dementia"),
                      labels = c(1, 2, 3))) |>
    relocate(wave, .after = ID)
}

create_transition_table <- function(year_to, year_from) {
  # Creates a transition frequency table between two specified years of cognitive status data.
  # Calculates row sums and total observations for transition analysis.
  # Arguments:
  #   - year_to: Target year for transitions (character or numeric)
  #   - year_from: Origin year for transitions (character or numeric)
  # Returns:
  #   - List containing: 
  #     - transition frequency table
  #     - row sums
  #     - total observations
  # 
  tbl <- table(
    table_data[[paste0("HRS_", year_to)]],
    table_data[[paste0("HRS_", year_from)]],
    dnn = c(year_to, year_from)
  )
  
  # Calculate row sums and total
  row_sums <- rowSums(tbl)
  total <- sum(row_sums)
  
  # Return as a list with both the table and summary stats
  list(
    table = tbl,
    row_sums = row_sums,
    total = total
  )
}

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

observed_transition_matrix <- function(data) {
  # Converts transition data into a properly formatted probability transition matrix.
  # Ensures consistent state ordering and converts proportions to matrix format suitable
  # for multi-state modeling and visualization.
  # Arguments:
  #   - data: Transition dataset from create_transitions()
  # Returns:
  #   - Square transition probability matrix with states as row/column names
  # 
  # The order I want my matrix in
  state_order <- c("Normal Cognition", "MCI", "Dementia")
  
  data |>
    group_by(Status, next_wave_status) |>
    summarise(freq = n(),  .groups = "drop") |>
    group_by(Status) |>
    mutate(freq = round(proportions(freq), 3)) |>
    ungroup() |>
    complete(Status, next_wave_status, fill = list(freq = 0)) |>
    tidyr::pivot_wider(names_from = next_wave_status, values_from = freq, names_sort = TRUE) |>
    tibble::column_to_rownames("Status") |>
    select(all_of(state_order)) |>
    _[state_order, ] |>
    as.matrix()
}

normalise <- function(x) {
  # Normalizes a matrix so that each row sums to 1.
  # Arguments:
  #   - x: The input matrix to normalize.
  # Returns:
  #   - A normalized matrix where each row sums to 1.
  x / rowSums(x)
}

tran_stack_graph <- function(data) {
  # Creates stacked bar charts visualizing cognitive state transitions across time periods.
  # Shows composition of current states by previous state, faceted by observation period.
  # Arguments:
  #   - data: Transition dataset from create_transition_dataset()
  # Returns:
  #   - ggplot object showing stacked transition proportions
  # 
  data |>
    ggplot(aes(x = t_minus_1, y = Freq, fill = t)) +
    geom_col(position = "stack", colour = "black") +
    facet_wrap(~ Period, ncol = 3) +
    labs(
      x = "Previous State (t-1)", 
      y = "Count",
      title = "Outcomes by Prior Cognitive State",
      fill = "Current State (t)"
    ) +
    ggokabeito::scale_fill_okabe_ito() +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      legend.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    ) +
    ggeasy::easy_move_legend("bottom")
}

tran_heat_map <- function(data) {
  # Generates heatmap visualization of transition frequencies between cognitive states.
  # Uses color intensity and labeled values to show transition patterns across time periods.
  # Arguments:
  #   - data: Transition dataset from create_transition_dataset()
  # Returns:
  #   - ggplot heatmap with state transitions as cells
  # 
  data |>
    ggplot(aes(x = t_minus_1, y = t, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 3.5) +  # Add counts
    scale_fill_gradient(low = "white", high = "steelblue") +
    facet_wrap(~ Period, ncol = 3) +  # Split by time period
    labs(
      x = "Previous State (t-1)", 
      y = "Current State (t)",
      title = "Cognitive State Transitions Between Time Periods",
      fill = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      legend.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    ggeasy::easy_move_legend("bottom")
}

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

plot_predictions_stationary <- function(predictions) {
  # Visualizes predicted transition probabilities from stationary multi-state models.
  # Shows probability curves by procrastination level, stratified by gender and baseline state.
  # Arguments:
  #   - predictions: Tidy predictions from tidy_predictions()
  # Returns:
  #   - Faceted ggplot showing predicted probability curves
  # 
  predictions |>
    ggplot(aes(x = Total_p, y = prob, colour = status)) +
    geom_line(linewidth = 1) +
    ggokabeito::scale_colour_okabe_ito() +
    labs(
      title = "**Predicted transition probabilities (stationary model)**",
      subtitle = "*According to gender and previous category*",
      x = "Total Procrastination", y = "Probability", colour = "**Cognition**") +
    facet_grid(status_prev ~ Gender) +
    theme_minimal() +
    theme(
      plot.title = ggtext::element_markdown(hjust = 0.5, size = 14),
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, size = 12),
      legend.title = ggtext::element_markdown(hjust = 0.5),
      strip.text = element_text(size = 10, face = "bold")
    )
}

# Loading data -----------------------------------------------------------------
data <- readxl::read_xlsx(here::here("01__Data/02__Processed/data.xlsx"))

# Predictors -------------------------------------------------------------------
cols <- c(
  "ID", "Gender", "Age", "Education_tri", 
  "Cardio_risk_16", "Total_dep_2016", "Total_p")

# Creating two datasets --------------------------------------------------------
## Creating a stacked dataset (making a previous response category)
data_stack <- data |>
  extract_years(seq(2016, 2022, by = 2)) |>
  na.omit() |> 
  inner_join(data[, cols], by = "ID") |>
  pivot_and_factorise() |>
  group_by(ID) |>
  mutate(Age = Age - (2022 - as.numeric(as.character(wave)))) |>
  # Creating previous cognition column
  mutate(status_prev = lag(status)) |>
  ungroup() |>
  # Removing wave 1
  filter(wave != 2016, !is.na(Total_p))

## Creating a table dataset
table_data <- data |>
  extract_years(seq(2016, 2022, by = 2)) |>
  rename_with(~ gsub("cogfunction", "HRS_", .)) |>
  mutate(
    across(c(HRS_2016:HRS_2022), ~ factor(.x, levels = c("Normal Cognition", "MCI", "Dementia")))
  )

## Creating transition frequencies ---------------------------------------------
### These are the time periods we are interested in
time_periods <- list(
  c("2016", "2018"),
  c("2018", "2020"), 
  c("2020", "2022")
)

### Apply the function to all time periods
transition_results <- purrr::map(time_periods, ~ create_transition_table(.x[1], .x[2]))
names(transition_results) <- purrr::map_chr(time_periods, ~ paste(.x[2], .x[1], sep = "-"))

### Creating one dataset
transition_frequencies <- time_periods |>
  create_transition_dataset(transition_results = transition_results)

## Creating observed transition matrix -----------------------------------------
transition_matrix_observed <- data |>
  extract_years(seq(2016, 2022, by = 2)) |>
  create_transitions() |>
  observed_transition_matrix()

# Visualisation ----------------------------------------------------------------
transition_frequencies |> tran_stack_graph()
transition_frequencies |> tran_heat_map()

# Modelling --------------------------------------------------------------------
## Stationary model ------------------------------------------------------------
fit_1 <- nnet::multinom(
  status ~ Gender, family = multinomial, data = data_stack)
fit_2 <- nnet::multinom(
  status ~ Gender + Total_p, family = multinomial, data = data_stack)
fit_3 <- nnet::multinom(
  status ~ Gender + Total_p + status_prev, family = multinomial, data = data_stack)

anova(fit_1, fit_2, fit_3)

# Outputting model summary 
# summary(fit_3); step(fit_3)

broom::tidy(fit_3)

# Checking p-values
coefs     <- summary(fit_3)$coefficients
std_error <- summary(fit_3)$standard.errors
z_score   <- coefs / std_error

2 * (1 - pnorm(abs(z_score))) |> round(digits = 3)

## Making a prediction dataset -------------------------------------------------
pred_data <- expand.grid(
  Gender = levels(data_stack$Gender),
  status_prev = levels(data_stack$status_prev),
  Total_p = seq(0, 60, length = 200))

## Plotting predictions
pred_data |>
  modelr::add_predictions(
    model = fit_3, var = "pred", type = "probs") |>
  tidy_predictions() |>
  plot_predictions_stationary()

# Getting an estimated transition matrix ---------------------------------------
# Get all unique states
names <- c("Normal Cognition", "MCI", "Dementia")
states <- sort(unique(data_stack$status))
n_states <- length(states)

# Create empty transition matrix
transition_matrix_estimated <- matrix(
  0, nrow = n_states, ncol = n_states,
  dimnames = list(paste("From", names), paste("To", names))
  )

# Getting estimated probabilities
estimated_probs <- expand.grid(Gender = factor(0),
            Total_p = mean(data_stack$Total_p),
            status_prev = states) |>
  modelr::add_predictions(model = fit_3, var = "pred", type = "probs")

estimated_probs$pred[1, ]

# Filling in matrix
for(i in 1:n_states) {
  transition_matrix_estimated[i, ] <- estimated_probs$pred[i, ]
}

transition_matrix_estimated <- transition_matrix_estimated |> round(digits = 3)

# Comparing both observed and predicted
transition_matrix_observed
transition_matrix_estimated

## Non Stationary model --------------------------------------------------------
## Small processing to fix wave column
data_stack <- data_stack |>
  mutate(wave = case_when(
    wave == "2016" ~ 1,
    wave == "2018" ~ 2,
    wave == "2020" ~ 3,
    wave == "2022" ~ 4
  ))

# Fitting non-stationary models
fit_4 <- nnet::multinom(
  status ~ Gender + Total_p + status_prev + wave, family = multinomial, data = data_stack)
fit_5 <- nnet::multinom(
  status ~ Gender + Total_p + (status_prev * wave), family = multinomial, data = data_stack)
fit_6 <- nnet::multinom(
  status ~ (Gender + Total_p + status_prev) * wave, family = multinomial, data = data_stack)

# Comparing stationary and non stationary (model 4 seems to be best)
anova(fit_3, fit_4, fit_5, fit_6)

broom::tidy(fit_4)

# Visualising
expand.grid(
  Gender = factor(0),
  status_prev = levels(data_stack$status_prev),
  Total_p = seq(0, 60, length = 200),
  wave = unique(data_stack$wave)) |>
  modelr::add_predictions(model = fit_4, var = "pred", type = "probs") |>
  tidy_predictions() |>
  mutate(wave = factor(wave)) |>
  ggplot(aes(x = Total_p, y = prob, color = status_prev)) +
  geom_line(linewidth = 1) +
  ggokabeito::scale_color_okabe_ito() +
  labs(
    title = "Time-Varying Transition Probabilities",
    x = "Total Procrastination", 
    y = "Predicted Probability", color = "Previous State (t-1)") +
  facet_grid(wave ~ status, labeller = labeller(
    wave = function(x) paste("Years since baseline:", x),
    status = function(x) paste("Transition to:", x)
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 10, face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
    )

state_names <- c("Normal", "MCI", "Dementia")

expand.grid(
  Gender = factor(0),
  Total_p = mean(data_stack$Total_p),
  status_prev = factor(1:3),
  wave = c(2, 3, 4)) |> 
  modelr::add_predictions(fit_4, var = "prob", type = "probs") |>
  select(status_prev, wave, starts_with("prob")) |>
  mutate(
    status_prev = factor(status_prev, labels = state_names),
    across(starts_with("prob"), ~ round(., 3)))

# Function to extract matrix for given time point
get_time_varying_matrix <- function(model, time_point) {
  expand.grid(
    Gender = factor(0),  # Male (reference)
    Total_p = mean(data_stack$Total_p),  # Mean procrastination
    status_prev = factor(1:3),  # All previous states
    wave = time_point
  ) |> 
    modelr::add_predictions(model, var = "prob", type = "probs") |> 
    select(status_prev, starts_with("prob")) |> 
    mutate(
      status_prev = factor(status_prev, labels = state_names),
      across(starts_with("prob"), ~round(., 3))
    ) |>
    tibble::column_to_rownames("status_prev") |>
    as.matrix() |>
    `colnames<-`(state_names)
}

time_points <- unique(data_stack$wave)

# Get matrices for all time points
time_varying_matrices <- purrr::map(
  setNames(time_points, paste(time_points, "Years")),
  ~ get_time_varying_matrix(fit_4, .x)
)


plot_data <- purrr::imap_dfr(
  time_varying_matrices,
  ~ as.data.frame(.x) |> 
    tibble::rownames_to_column("From") |> 
    pivot_longer(-From, names_to = "To", values_to = "Probability") |> 
    mutate(Time = .y),
  .id = "TimePoint"
) |> 
  mutate(
    From = factor(From, levels = state_names),
    To = factor(To, levels = rev(state_names)),
    TimePoint = forcats::fct_inorder(TimePoint)
  )

ggplot(plot_data, aes(x = From, y = To, fill = Probability)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(
    aes(label = sprintf("%.2f", Probability)),
    color = ifelse(plot_data$Probability > 0.5, "white", "black"),
    size = 4) +
  scale_fill_viridis_c(option = "plasma", limits = c(0, 1)) +
  facet_wrap(~ TimePoint, nrow = 1) +
  labs(
    title = "Estimated Time-Varying Transition Matrices",
    subtitle = "Showing changes in transition probabilities over time",
    x = "Previous State (t-1)",
    y = "Next State (t)",
    fill = "Probability"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing = unit(1, "lines"),
    strip.text = element_text(face = "bold")
  )
