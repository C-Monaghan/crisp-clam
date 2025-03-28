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
  paste0("Total_dep_20", seq(16, 22, by = 2)), "Total_p")

data |>
  extract_years(seq(2016, 2022, by = 2), cog_total = TRUE) |>
  inner_join(data[, cols], by = "ID") |>
  pivot_data(cog_total = TRUE) |>
  backtrack_age(2016, 2022) |>
  mutate(Total_p = ifelse(Wave == 4, Total_p, NA))

