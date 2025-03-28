rm(list = ls())

set.seed(34521)

library(dplyr)
library(ggplot2)
library(patchwork)

source(here::here("./02__Models/00__Functions.R"))

# For creating transition tables
create_transition_table <- function(year_to, year_from) {
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

# Reading in data --------------------------------------------------------------
path_data <- "./01__Data/02__Processed/"

data <- readxl::read_xlsx(here::here(path_data, "data.xlsx"))

cols <- c(
  "ID", "Gender", "Age", "Education_tri", 
  "Cardio_risk_16", "Total_dep_2016", "Total_p")

# Creating a stacked dataset (making a previous response category)
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

# Creating transition tables ---------------------------------------------------
table_data <- data |>
  extract_years(seq(2016, 2022, by = 2)) |>
  rename_with(~ gsub("cogfunction", "HRS_", .)) |>
  mutate(
    across(c(HRS_2016:HRS_2022), ~ factor(.x, levels = c("Normal Cognition", "MCI", "Dementia")))
    )

# Define the time periods to analyze
time_periods <- list(
  c("2016", "2018"),
  c("2018", "2020"), 
  c("2020", "2022")
)

# Apply the function to all time periods
transition_results <- purrr::map(time_periods, ~ create_transition_table(.x[1], .x[2]))
names(transition_results) <- purrr::map_chr(time_periods, ~ paste(.x[2], .x[1], sep = "-"))

# Creating one dataset
transition_frequencies <- time_periods %>%
  purrr::map_dfr(~ {
    period_name <- paste(.x[2], .x[1], sep = " - ")
    tbl <- transition_results[[paste(.x[2], .x[1], sep = "-")]]$table
    
    as.data.frame(tbl) %>%
      rename(t_minus_1 = 2, t = 1) %>%  # Positional renaming
      mutate(Period = period_name, .before = t_minus_1)
  }) %>%
  mutate(
    Period = stringr::str_replace(Period, "(\\d+) - (\\d+)", "\\2 - \\1"),
    Period = factor(Period, levels = c("2016 - 2018", "2018 - 2020", "2020 - 2022")),
    t_minus_1 = factor(t_minus_1, levels = c("Normal Cognition", "MCI", "Dementia")),
    t = factor(t, levels = c("Normal Cognition", "MCI", "Dementia"))
  )

# Plotting results -------------------------------------------------------------
tran_plot_1 <- transition_frequencies |>
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

tran_plot_2 <- transition_frequencies |>
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

tran_plot_1 + tran_plot_2

# Transitions from 2016 - 2018
table_1 <- table(table_data$HRS_2018, table_data$HRS_2016, dnn = c("2018", "2016"))
sum(table_1[1,]); sum(table_1[2,]); sum(table_1[3,])
sum(rowSums(table_1))  
rowSums(table_1)

# Transitions from 2018 - 2020
table_2 <- table(table_data$HRS_2020, table_data$HRS_2018, dnn = c("2020", "2018"))
sum(table_2[1,]); sum(table_2[2,]); sum(table_2[3,])
sum(rowSums(table_2))  
rowSums(table_2)

# Transitions from 2020 - 2022
table_3 <- table(table_data$HRS_2022, table_data$HRS_2020, dnn = c("2022", "2020"))
sum(table_3[1,]); sum(table_3[2,]); sum(table_3[3,])
sum(rowSums(table_3))  
rowSums(table_3)

table_1; table_2; table_3

# Overall transitions
table_4 <- table(data_stack$status, data_stack$status_prev)
sum(rowSums(table_4))

table_5 <- table(data_stack$status, data_stack$Gender)
chisq.test(table_5)

# Outputting probabilities to a file -------------------------------------------
# sink(here::here("02__Models/Results/Probabilities.txt"))

tran_probs <- data |>
  extract_years(years = seq(2016, 2022, by = 2), impute = TRUE) |>
  create_transitions(absorbing = TRUE) |> 
  select(Wave, transition) |>
  mutate(Wave = as.numeric(Wave)) |>
  filter(Wave %in%  c(1, 2, 3)) |>
  table() |>
  prop.table(margin = 1) |>
  round(digits = 3)

# sink()
  
# Creating a transition matrix per wave ----------------------------------------
state_names <- c("Normal Cognition", "MCI", "Dementia")

# Initial list to store the probability matrices
prob_wave_list <- vector("list", 3)

# Loop through each wave and create the probability matrix
for (i in 1:3) {
  prob_wave_list[[i]] <- matrix(
    data = c(
      tran_probs[i, 1], tran_probs[i, 2], tran_probs[i, 3], # Normal cognition
      tran_probs[i, 4], tran_probs[i, 5], tran_probs[i, 6], # MCI
      0, 0, tran_probs[i, 7]                                # Dementia (absorbing)
    ),
    nrow = 3,
    ncol = 3,
    byrow = TRUE,
    dimnames = list(state_names, state_names)
  )
}

prob_wave_list[[1]] |> normalise() |> round(digits = 3)
prob_wave_list[[2]] |> normalise() |> round(digits = 3)
prob_wave_list[[3]] |> normalise() |> round(digits = 3)


# Fitting multinomial regression taking into account previous 
# two classification states 
cols <- c(
  "ID", "Gender", "Age", "Education_tri", 
  "Cardio_risk_16", "Total_dep_2016", "Total_p")

my_data <- data |>
  extract_years(years = seq(2016, 2022, by = 2), impute = TRUE) |>
  # create_transitions(absorbing = TRUE) |>
  tidyr::pivot_longer(cols = !ID,
                      names_to = "Wave",
                      values_to = "Status") |>
  group_by(ID) |>
  mutate(Status = ifelse(cumany(Status == "Dementia"), "Dementia", Status)) |>
  ungroup() |>
  tidyr::pivot_wider(names_from = "Wave", 
                     values_from = "Status") |>
  inner_join(data[, cols], by = "ID") |>
  mutate(across(c(cogfunction2016:cogfunction2022), ~ case_when(
    .x == "Normal Cognition" ~ 1,
    .x == "MCI" ~ 2,
    .x == "Dementia" ~ 3,
    TRUE ~ NA)),
    across(c(cogfunction2016:cogfunction2022), ~ factor(.x, levels = c(1, 2, 3))),
    Gender = factor(Gender, levels = c(0, 1)),
    Education_tri = factor(Education_tri, levels = c(0, 1, 2)),
    Age = Age - 6
    ) |>
  rename(Cardio = Cardio_risk_16, 
         Depression = Total_dep_2016,
         Procrastination = Total_p)

fit <- nnet::multinom(cogfunction2022 ~ Gender 
               + Age 
               + Cardio
               + Depression
               + cogfunction2018
               + cogfunction2020
               + Procrastination,
               data = my_data)

multi_data <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE) |>
  mutate(across(c(estimate:conf.high), ~ round(.x, digits = 3))) |>
  filter(term != "(Intercept)") |>
  mutate(term = case_when(
    term == "Gender1" ~ "Being female",
    term == "cogfunction20182" ~ "Having MCI (2018)",
    term == "cogfunction20183" ~ "Having Demetia (2018)",
    term == "cogfunction20202" ~ "Having MCI (2020)",
    term == "cogfunction20203" ~ "Having Demetia (2020)",
    TRUE ~ term),
    
    term = factor(
      term, 
      levels = c("Being female", "Age", "Cardio", "Depression",
                 "Having MCI (2018)", "Having Demetia (2018)",
                 "Having MCI (2020)", "Having Demetia (2020)",
                 "Procrastination"))
    ) |>
  filter(!term %in% c("Having Demetia (2018)", "Having Demetia (2020)",
                      "Having MCI (2018)", "Having MCI (2020)"))

multi_data |>
  ggplot(aes(x = estimate, y = term, colour = y.level)) +
  ggstance::geom_pointrangeh(
    aes(xmin = conf.low, xmax = conf.high),
    position= ggstance::position_dodgev(height=0.75),
    size = .75) +
  geom_vline(xintercept = 1) +
  labs(title = "Results from Multinomial Logistic Regression",
       subtitle = "Reference Level: Normal Cognition",
       x = "Estimate", y = "") +
  scale_colour_discrete(name = "Cognitive Function", labels = c("MCI", "Dementia")) +
  theme_bw() +
  ggeasy::easy_move_legend("bottom")



markov_data |>
  select(Gender, Age) |>
  mutate(Age = Age - (2022 - 2016)) |>
  filter(Age > 50) |>
  count()


# Fitting a stationary model ---------------------------------------------------
fit_1 <- nnet::multinom(status ~ Gender, family = multinomial, data = data_stack)
fit_2 <- nnet::multinom(status ~ Gender + Total_p, family = multinomial, data = data_stack)
fit_3 <- nnet::multinom(status ~ Gender + Total_p + status_prev, family = multinomial, data = data_stack)

anova(fit_1, fit_2, fit_3)

summary(fit_3)
step(fit_3)

# Estimated coefficients
fit_3_coefs <-  as.numeric(c(
  coef(fit_3)[1, ], 
  coef(fit_3)[2, ]))

# Variance-Covariance matrix
fit_3_vcov <- VGAM::vcov(fit_3)

# Log likelihood
logLik(fit_3)

# Making a prediction dataset --------------------------------------------------
pred_data <- expand.grid(
  Gender = levels(data_stack$Gender),
  status_prev = levels(data_stack$status_prev),
  Total_p = seq(0, 60, length = 200))

predictions <- pred_data |>
  modelr::add_predictions(
    model = fit_3, var = "pred", type = "probs"
  ) |>
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
    across(c(pred.1:pred.3), as.numeric))

predictions_plot <- predictions |>
  tidyr::pivot_longer(cols = c(pred.1:pred.3), names_to = "status", values_to = "prob") |>
  mutate(status = case_when(
    status == "pred.1" ~ "Normal Cognition",
    status == "pred.2" ~ "MCI",
    status == "pred.3" ~ "Dementia"
  ),
  status = factor(status, levels = c("Normal Cognition", "MCI", "Dementia")),
  status_prev = factor(status_prev, levels = c("Normal Cognition", "MCI", "Dementia")))
  
stationary_plot <- predictions_plot |>  
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

stationary_plot
