# Data exploration -------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(ggplot2)
library(ggalluvial)

# source(here::here("02__Models/00__Functions.R"))

count_transitions <- function(data, years) {
  
  # Create dynamic column names based on the years provided
  cogfunction_cols <- paste0("cogfunction", years)
  
  data |>
    select(ID, Total_p, all_of(cogfunction_cols)) |>
    mutate(across(all_of(cogfunction_cols), ~ case_when(
      .x == 1 ~ "Normal Cognition",
      .x == 2 ~ "MCI",
      .x == 3 ~ "Dementia",
      is.na(.x) ~ "Missing",  # Handle missing values
      TRUE ~ "Other"          # Handle other unexpected cases
    ))) |>
    count(across(all_of(cogfunction_cols))) |>
    mutate(ID = row_number()) |>
    tidyr::pivot_longer(cols = all_of(cogfunction_cols),
                        names_to = "Wave",
                        values_to = "Status") |>
    mutate(Status = factor(Status,
                           levels = c("Normal Cognition", "MCI", "Dementia", "Missing"))) |>
    mutate(Wave = stringr::str_replace(Wave, "cogfunction", "HRS "))
}

# Plotting dementia transitions ------------------------------------------------
plot_transitions <- function(data, size) {
  
  data |>
    ggplot(aes(x = Wave, y = n, stratum = Status, fill = Status, alluvium = ID)) +
    geom_stratum(alpha = 0.5, width = 0.5) +
    geom_flow(width = 0.5) +
    geom_text(stat = "stratum", aes(label = stringr::str_wrap(Status, width = 10)), 
              hjust = 0.5, vjust = 0.5, size = size) +
    labs(title = "Dementia transitions across time", x = "", y = "Frequency") +
    scale_fill_viridis_d(direction = -1) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          text = element_text(size = 10)) +
    ggeasy::easy_remove_legend()
}

path_data <- "data/"

# Reading in data --------------------------------------------------------------
data <- readxl::read_xlsx(here::here(path_data, "data.xlsx"))

# Making transition plots 
# 1996 - 2022
river_plot_small <- data |>
  count_transitions(years = seq(1996, 2022, by = 2)) |>
  plot_transitions(size = 2.5)

# 2016 - 2022
river_plot_full <- data |>
  count_transitions(years = seq(2016, 2022, by = 2)) |>
  plot_transitions(size = 3)

# Exporting --------------------------------------------------------------------
export_path <- "./R/Results/Figures/EDA"

cowplot::save_plot(
  filename = here::here(export_path, "01__River_Plot_Small.png"),
  plot = river_plot_small, base_height = 8)

cowplot::save_plot(
  filename = here::here(export_path, "02__River_Plot_Full.png"),
  plot = river_plot_full, base_height = 8)

# OLD CODE ---------------------------------------------------------------------
# Making plots 
# class_plot <- prepare_classification_data(data) |>
#   plot_classification()
# 
# transition_plot <- prepare_transition_data(data) |>
#   plot_transition()
# 
# river_plot <- prepare_river_data(data) |>
#   plot_river()
#   
#   

test <- data |>
  select(any_of(paste0("cogfunction", 2016:2022))) |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Wave",
    values_to = "Classification"
  ) |>
  mutate(
    Wave = stringr::str_replace(Wave, "cogfunction", "wave_"),
    Wave = factor(Wave, levels = c("wave_2016", "wave_2018", 
                                   "wave_2020","wave_2022")),
    
    Classification = factor(case_when(
      Classification == 1 ~ "Normal Cognition",
      Classification == 2 ~ "MCI",
      Classification == 3 ~ "Dementia"), 
      levels = c("Normal Cognition", "MCI", "Dementia"))
    ) |>
  na.omit()

data |>
  count_transitions(years = seq(2016, 2022, by = 2), absorbing = TRUE) |>
  ggplot(aes(x = Wave, fill = Classification)) +
  geom_bar(position = "fill", alpha = 0.5, colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Cognitive Status Classifications (2016 - 2022)", 
       x = "", y = "Percentage") +
  scale_fill_viridis_d(direction = -1) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        text = element_text(size = 10)) +
  ggeasy::easy_move_legend("bottom") +
  ggeasy::easy_remove_legend_title()

test |>
  group_by(Wave) |>
  summarise(count = count(Classification))

data |>
  count_transitions(years = seq(2016, 2022, by = 2), absorbing = TRUE)



data |>
  count_transitions(years = seq(2016, 2022, by = 2), absorbing = TRUE) |>
  group_by(ID, Wave) |>
  reframe(plyr::count(Classification)) |>
  rename(Classification = x, n = freq) |>
  plot_transitions(size = 2.5)
