rm(list = ls())

library(dplyr)
library(easystats)
library(ggplot2)

extract_years <- function(data, years) {
  cogtotal_cols    <- paste0("cogtot27_imp", years)
  cogfunction_cols <- paste0("cogfunction", years)
  
  data |>
    select(ID, any_of(cogtotal_cols), any_of(cogfunction_cols))
}

process_data <- function(data, absorbing = FALSE){
  data <- data |>
    tidyr::pivot_longer(
      cols = matches("cogtot27_imp|cogfunction"),
      names_to = c("Measure", "Year"),
      names_pattern = "(cogtot27_imp|cogfunction)(\\d+)",
      values_to = "Value"
    ) |>
    mutate(
      Measure = ifelse(Measure == "cogtot27_imp", "Score", "Class"),
      Year = factor(Year, levels = c("2016", "2018", "2020", "2022"))
    ) |>
    tidyr::pivot_wider(names_from = "Measure", values_from = "Value") |>
    mutate(
      Class = case_when(
        Class == 1 ~ "Normal Cognition",
        Class == 2 ~ "MCI",
        Class == 3 ~ "Dementia",
        TRUE ~ NA_character_
      )
    )
  
  # We can optionally specify dementia as an absorbing state
  # Once an individual is classified with dementia they cannot be classified
  # with anything else 
  # Additionally, we change their score to be 6 to reflect the highest possible
  # value you can get while still being classified with dementia
  if(absorbing == TRUE){
    data <- data |>
      group_by(ID) |>
      mutate(
        Class = ifelse(cumany(Class == "Dementia"), "Dementia", Class),
        Score = ifelse(Class == "Dementia" & Score > 6, 6, Score)) |>
      ungroup()
  }
  
  data <- data |>
    mutate(Class = factor(Class, levels = c("Normal Cognition", "MCI", "Dementia")))
  
  return(data)
}

backtrack_age <- function(data) {
  data |>
    rename(Age = Age_2022) |>
    mutate(
      Age = Age - case_when(
        Year == "2016" ~ 6,
        Year == "2018" ~ 4,
        Year == "2020" ~ 2,
        TRUE ~ 0
      )
    )
}

# Reading in data --------------------------------------------------------------
path_data <- "./01__Data/02__Processed/"

data <- readxl::read_xlsx(here::here(path_data, "data.xlsx"))

# Data processing --------------------------------------------------------------
ed_level <- c(
  "No_degree", "GED", "High_school", "College_2_years", 
  "College_4_years", "Masters", "PhD", "Other")

ed_label <- c(
  "No degree", "GED", "High school", "College (2 years)", 
  "College (4 years)", "Masters", "PhD", "Other")

# Variables to add to datafile
adding <- data |>
  select(ID, Age_2022, Education, Total_p) |>
  mutate(
    Education = case_when(
      Education == 0 ~ "No_degree",
      Education == 1 ~ "GED",
      Education == 2 ~ "High_school",
      Education == 3 ~ "College_2_years",
      Education == 4 ~ "College_4_years",
      Education == 5 ~ "Masters",
      Education == 6 ~ "PhD",
      Education == 9 ~ "Other"
    ),
    Education = factor(Education, levels = ed_level, labels = ed_label)
  )
 
cog_data <- data |>
  extract_years(years = seq(2016, 2022, by = 2)) |>
  process_data(absorbing = TRUE) |>
  inner_join(adding, by = "ID") |> # Adding additional information
  backtrack_age() |> # Back calculating age
  group_by(ID) |>
  filter(all(!is.na(Score))) |> # Removing individuals who have no classification scores
  ungroup() |>
  filter(!is.na(Total_p)) |> # Removing individuals with no procrastination scores
  mutate(Total_p = ifelse(Year == 2022, Total_p, NA)) # We only have data on 2022 procrastination scores

# Plotting ---------------------------------------------------------------------
cog_data |>
  ggplot(aes(x = Age, y = Score, colour = Class)) +
  geom_jitter(size = 2, width = 0.2, height = 0.2) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 27, by = 5)) +
  facet_wrap(~ Year) +
  theme_bw() +
  ggeasy::easy_remove_legend_title() +
  ggeasy::easy_move_legend(to = "bottom")

cog_data |>
  group_by(Education, Year) |>
  summarise(Score = mean(Score), .groups = "keep") |>
  ggplot(aes(x = Education, y = Score, fill = forcats::fct_rev(Education))) +
  geom_bar(stat = "identity", width = 0.75, alpha = 0.6) +
  scale_y_continuous(breaks = seq(0, 27, by = 3)) +
  labs(title = "Mean Cognitive Score by Education Level",
       subtitle = "Across Time", x = "", y = "Total Cognitive Score") +
  facet_wrap( ~ Year) +
  theme_minimal() +
  coord_flip() +
  ggeasy::easy_remove_legend() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(size = 10, face = "bold"))


cog_data |>
  filter(Year == 2022) |>
  ggplot(aes(x = Total_p, y = Score)) +
  geom_jitter(width = 0.25, alpha = 0.5) +
  labs(
    title = "Procrastination vs. Cognitive Classification Score",
    x = "Total Procrastination", 
    y = "Cognitive Classification Score") +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  scale_y_continuous(breaks = seq(0, 27, by = 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8))


# Modelling --------------------------------------------------------------------
test <- cog_data |>
  filter(Year == 2022) |>
  mutate(Class = ordered(Class, levels = c("Normal Cognition", "MCI", "Dementia")))

model <- MASS::polr(Class ~ poly(Total_p, 2), data = test, Hess = TRUE)

summary(model)

broom::tidy(model)

brant::brant(model = model)

model <- lm(Score ~ poly(Total_p, 2), data = test)

broom::tidy(model) |>
  mutate(across(c(estimate:statistic), \(x) round(x, digits = 2)))

expand.grid(
  Score = rnorm,
  Total_p = seq(0, 60, length = 50)
) |>
  ggplot(aes(x = Score, y = Total_p)) +
  geom_point()

fortify(model) |>
  ggplot() +
  geom_point(aes(x = Total_p, y = Score)) +
  geom_line(aes(x = Total_p, y = .fitted))

test |>
  ggplot(aes(model$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4", alpha = 0.5) +
  labs(title = "Histogram of Model Residuals",
       x = "Residuals", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

plot(parameters(model = model))

plot(check_normality(model, type = "qq"))

predictions <- estimate_expectation(model, data = "grid", ci = 0.95)

test |>
  ggplot(aes(x = Total_p, y = Score)) +
  geom_jitter(width = 0.25, size = 1.25, alpha = 0.5) +
  geom_line(data = predictions, aes(x = Total_p, y = Predicted), 
            colour = "skyblue", linetype = "dashed", linewidth = 1.5) +
  geom_ribbon(data = predictions, aes(x = Total_p, y = Predicted, ymin = CI_low, ymax = CI_high), 
              alpha = 0.25) +
  labs(title = "Predicted values from model", 
       x = "Total Procrastination", y = "Cognitive Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

plot(estimate_expectation(model, data = "grid", ci = 0.95)) + 
  labs(
    title = "Predicted values from model", 
    x = "Total Procrastination", y = "Cognitive Score") +
  theme_minimal() 

x <- cog_data |>
  mutate(ID = as.factor(ID))

model <- lme4::lmer(Score ~ 1 + Total_p + (1|ID), data = cog_data)

minque::lmm(Score ~ Total_p + 1|ID, data = x, method = "reml")

nlme::lme(Score ~ Total_p, random = ~ 1 | ID, data = cog_data, )
