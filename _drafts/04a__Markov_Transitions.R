rm(list = ls())
  
set.seed(34521)
  
library(dplyr)
library(ggplot2)
library(patchwork)

source(here::here("./02__Models/00__Markov_Functions.R"))

# Reading in data --------------------------------------------------------------
path_data <- "./01__Data/02__Processed/"
  
data <- readxl::read_xlsx(here::here(path_data, "data.xlsx"))

# Overall transition matrix ----------------------------------------------------
# For full dataset (1996 - 2022)
tran_matrix_full <- data |>
  extract_years(years = seq(1996, 2022, by = 2)) |>
  # na.omit() |>
  create_transitions(absorbing = TRUE) |>
  calculate_probabilties() |>
  transition_matrix(longitudinal = FALSE) |>
  normalise() |>
  round(digits = 2)

# For a reduced dataset (2016 - 2022)
tran_matrix_reduced <- data |>
  extract_years(years = seq(2016, 2022, by = 2)) |>
  na.omit() |> # Removing some people who hadn't been recruited in 2016 / 2018
  create_transitions(absorbing = TRUE) |>
  calculate_probabilties() |>
  transition_matrix(longitudinal = FALSE) |>
  normalise() |>
  round(digits = 2)

# Visualizing ------------------------------------------------------------------
plot_matrix(data = t(tran_matrix_full), year = 1996, ts = FALSE)

plot_matrix(data = t(tran_matrix_reduced), year = 2016, ts = FALSE)

# Creating transition matrices for each year ----------------------------------- 
tran_matrices_reduced <- data |>
  extract_years(years = seq(1998, 2022, by = 2)) |>
  create_transitions() |>
  group_by(Wave) |>
  calculate_probabilties() |>
  group_map(~ transition_matrix(.x, longitudinal = TRUE), .keep = TRUE)

# I want my matrix in a certain order
tran_matrices_reduced <- lapply(tran_matrices_reduced, function(mat) {
  mat[c("Normal Cognition", "MCI", "Dementia"), c("Normal Cognition", "MCI", "Dementia")] |>
    normalise()
})

# Creating a singular dataframe of transition probabilities --------------------
wave_names <- c(seq(1998, 2020, by = 2))

# Apply the reshaping function to each matrix and combine the results
transition_probabilties_long <- purrr::map2_dfr(tran_matrices_reduced, wave_names, reshape_matrix) |>
  mutate(
    transition_prob = round(transition_prob, digits = 3),
    from = factor(from, levels = c("Normal Cognition", "MCI", "Dementia")))

# OLD CODE ---------------------------------------------------------------------
# Discrete time markov chain ---------------------------------------------------
# library(markovchain)
# 
# mcCognition <- new("markovchain",
#                    states = c("Normal Cognition", "MCI", "Dementia"),
#                    transitionMatrix = matrix(data = c(
#                      0.5, 0.4, 0.1,
#                      0.3, 0.4, 0.3,
#                      0, 0, 1), 
#                      byrow = TRUE, nrow = 3),
#                    name = "Cognition"
#                    )
# conditionalDistribution(mcCognition, "MCI")
# 
# rmarkovchain(n = 5, object = mcCognition, t0 = "Normal Cognition")
# 
# markovchainFit(data = x, method = "mle", name = "Cognition")$estimate

# Creating an animated transition plot -----------------------------------------
# Directory for output
# path <- here::here("./02__Models/Results/Figures/02__Camcorder/")

# Where camcorder will save plots
# camcorder::gg_record(
#   dir = path,
#   device = "png",
#   width = 10,
#   height = 10,
#   units = "in",
#   dpi = 800
# )

# # Creating plots
# for(wave in wave_names) {
#   print(plot_transition_wave(data = transition_probabilties_long,
#                              wave = wave))
# }
# 
# # Stopping saving
# camcorder::gg_stop_recording()
# 
# # Creating gif
# camcorder::gg_playback(
#   name = here::here(path, "transitions.gif"),
#   first_image_duration = 3,
#   last_image_duration = 3,
#   frame_duration = 1.5
# )
