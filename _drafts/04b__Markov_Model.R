rm(list = ls())

set.seed(34521)

library(dplyr)
library(ggplot2)
library(patchwork)
library(msm)

source(here::here("./02__Models/00__Functions.R"))

# Reading in data --------------------------------------------------------------
path_data <- "./01__Data/02__Processed/"

data <- readxl::read_xlsx(here::here(path_data, "data.xlsx"))

# Applying Markov Model (MSM Package) ------------------------------------------
# Creating suitable data set
cols <- c(
  "ID", "Gender", "Age", "Education_tri", 
  paste0("Cardio_risk_", seq(16, 22, by = 2)), 
  paste0("Total_dep_20", seq(16, 22, by = 2)),
  "Total_p")

markov_data <- data |>
    extract_years(seq(2016, 2022, by = 2), impute = TRUE) |>
    na.omit() |>
    inner_join(data[, cols], by = "ID") |>
    pivot_data() |>
  backtrack_age(base_year = 2016, current_year = 2022) |>
  group_by(ID) |>
  mutate(
    # Baseline covariates (measured in 2016)
    Age = min(Age),
    Cardio_risk = Cardio_risk[Wave == 1],
    Depression = Depression[Wave == 1]
    ) |>
  ungroup() |>
  # Only focusing on older adults
  filter(Age > 50)

# Let's look at a state table for the number of different transitions
markov_data |>
  select(ID, Wave, Status) |>
  mutate(Status = case_when(
    Status == 1 ~ "Normal Cognition", 
    Status == 2 ~ "MCI", 
    Status == 3 ~ "Dementia")) |>
  msm::statetable.msm(state = Status, subject = ID)

# Creating q matrix to specify allowed transitions
# Define possible transitions
states <- c("Normal Cognition", "MCI", "Dementia")

# Theoretical transition probabilities
q_matrix <- rbind(
  c(0.00, 0.10, 0.05),  # From Normal, can transition to MCI or Dementia
  c(0.15, 0.00, 0.15),  # From MCI, can transition to Normal or Dementia
  c(0.00, 0.00, 0.00)         # From Dementia, no transitions (absorbing state)
)

rownames(q_matrix) <- states
colnames(q_matrix) <- states

q_matrix

# Creating e matrix to specify misclassifications
# e_matrix <- rbind(
#   c(0.00, 0.10, 0.00),
#   c(0.10, 0.05, 0.00),
#   c(0.00, 0.00, 0.00)
# )
# 
# 
# rownames(e_matrix) <- states
# colnames(e_matrix) <- states
# 
# e_matrix

# Fitting model with no covariates
model_1 <- msm(
  formula = Status ~ Wave,
  subject = ID,
  data = markov_data,
  qmatrix = q_matrix,
  obstype = 1,
  gen.inits = TRUE,
  method = "BFGS",
  control = list(
    fnscale = 5000, 
    maxit = 1000, # To reach convergence
    trace = 2,
    REPORT = 1 # Reporting model iterations
  ))

# Multivariate model: covariates acting on all transitions
model_2 <- msm(
  formula = Status ~ Wave,
  subject = ID,
  data = markov_data,
  qmatrix = q_matrix,
  covariates = ~ Gender + Age + Education_tri + Cardio_risk + Depression + Total_p,
  obstype = 1,
  center = TRUE,
  gen.inits = TRUE,
  method = "BFGS",
  control = list(
    fnscale = 4000, 
    maxit = 1000, # To reach convergence
    trace = 2, 
    REPORT = 1 # Reporting model iterations
  ))

# Multivariate model: covariates acting on all transitions (with interaction)
model_3 <- msm(
  formula = Status ~ Wave,
  subject = ID,
  data = markov_data,
  qmatrix = q_matrix,
  covariates = ~ Gender + Age + Education_tri + Cardio_risk + (Depression * Total_p),
  obstype = 1,
  center = TRUE,
  gen.inits = TRUE,
  method = "BFGS",
  control = list(
    fnscale = 4500, 
    maxit = 1000, # To reach convergence
    trace = 2, 
    REPORT = 1 # Reporting model iterations
  ))

# Multivariate model: covariates acting on NC <-> MCI and MCI -> D
model_4 <- msm(
  formula = Status ~ Wave,
  subject = ID,
  data = markov_data,
  qmatrix = q_matrix,
  covariates = list(
    "1-2" = ~ Gender + Age + Education_tri + Cardio_risk + Depression + Total_p,
    "2-1" = ~ Gender + Age + Education_tri + Cardio_risk + Depression + Total_p,
    "2-3" = ~ Gender + Age + Education_tri + Cardio_risk + Depression + Total_p
  ),
  obstype = 1,
  center = TRUE,
  gen.inits = TRUE,
  method = "BFGS",
  control = list(
    fnscale = 4000, 
    maxit = 1000, # To reach convergence
    trace = 2, 
    REPORT = 1 # Reporting model iterations
  ))


# Multivariate model: covariates acting on NC <-> MCI and MCI -> D (with interaction)
model_5 <- msm(
  formula = Status ~ Wave,
  subject = ID,
  data = markov_data,
  qmatrix = q_matrix,
  covariates = list(
    "1-2" = ~ Gender + Age + Education_tri + Cardio_risk + (Depression * Total_p),
    "2-1" = ~ Gender + Age + Education_tri + Cardio_risk + (Depression * Total_p),
    "2-3" = ~ Gender + Age + Education_tri + Cardio_risk + (Depression * Total_p)
  ),
  obstype = 1,
  center = TRUE,
  gen.inits = TRUE,
  method = "BFGS",
  control = list(
    fnscale = 4000, 
    maxit = 1000, # To reach convergence
    trace = 2, 
    REPORT = 1 # Reporting model iterations
  ))

# Model comparison -------------------------------------------------------------
log_l <- c(logLik(model_1), logLik(model_2), logLik(model_3), logLik(model_4), logLik(model_5))
aic <- AIC(model_1, model_2, model_3, model_4, model_5)

# Very small difference between model 2 and 3 (makes sense)
cbind(log_l, aic) |>
  tibble::rownames_to_column("Model") |>
  mutate(Model = stringr::str_remove(Model, "model_"))

# No significant difference
lmtest::lrtest(model_1, model_2, model_3, model_4, model_5)

# Plotting fitted vs. observed values
# g1 <- gentleman_test(data = msm::prevalence.msm(model_1), model = "Model 1")
# g2 <- gentleman_test(data = msm::prevalence.msm(model_2), model = "Model 2")
# g3 <- gentleman_test(data = msm::prevalence.msm(model_3), model = "Model 3")
# g4 <- gentleman_test(data = msm::prevalence.msm(model_4), model = "Model 4")
# g5 <- gentleman_test(data = msm::prevalence.msm(model_5), model = "Model 5")

# ggpubr::ggarrange(g1, g2, g3, g4, g5, common.legend = TRUE, legend = "bottom")

# Probability Matrix
pmatrix.msm(model_1) |> round(digits = 3)
pmatrix.msm(model_2) |> round(digits = 3) 
pmatrix.msm(model_3) |> round(digits = 3)
pmatrix.msm(model_4) |> round(digits = 3) 
pmatrix.msm(model_5) |> round(digits = 3)

# Hazard Ratios ----------------------------------------------------------------
# All transitions
haz_model_2 <- process_hr(hazard.msm(model_2))
haz_model_3 <- process_hr(hazard.msm(model_3))

# Some transitions
haz_model_4 <- process_hr(hazard.msm(model_4))
haz_model_5 <- process_hr(hazard.msm(model_5))

# Plotting models
haz_model_2 |> calculate_p_value() |> plot_hr()
haz_model_3 |> calculate_p_value() |> plot_hr()
haz_model_4 |> calculate_p_value() |> plot_hr()
haz_ratios <- haz_model_5 |> calculate_p_value() |> plot_hr()


cowplot::save_plot(
  filename = here::here("02__Models/Results/Figures/Hazard_ratios.png"), 
  plot = haz_ratios, base_height = 8)

################################################################################
################################################################################
################################################################################