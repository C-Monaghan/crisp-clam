# Aggregating procrastination data with LWC data

rm(list = ls())

library(dplyr)
library(tidyr)

source(here::here("R/00__Functions.R"))

path_data <- "data-raw"

# Reading in data --------------------------------------------------------------
# General data
tracker    <- haven::read_sav(here::here(path_data, "02__Tracker.sav"))
lw_data    <- readxl::read_xlsx(here::here(path_data, "../02__Processed/lw_data_full.xlsx"))

# 2016 data
health_16  <- haven::read_sav(here::here(path_data, "2016/Health_2016.sav"))
cog_16     <- haven::read_sav(here::here(path_data, "2016/Cognition_2016.sav"))

# 2018 data
health_18  <- haven::read_sav(here::here(path_data, "2018/Health_2018.sav"))
cog_18     <- haven::read_sav(here::here(path_data, "2018/Cognition_2018.sav"))


# 2020 data
health_20  <- haven::read_sav(here::here(path_data, "2020/Health_2020.sav"))
proc       <- haven::read_sav(here::here(path_data, "2020/Procrastination.sav"))
cog_20     <- haven::read_sav(here::here(path_data, "2020/Cognition_2020.sav"))

# 2022 data
health_22  <- haven::read_sav(here::here(path_data, "2022/Health_2022.sav"))
cog_22     <- haven::read_sav(here::here(path_data, "2022/Cognition_2022.sav"))


# Filter to procrastination participants ---------------------------------------
proc_red <- proc |>
  filter(!is.na(RV155)) |>
  select(HHID, PN, RV156:RV167) |>
  rename(
    P_1 = "RV156",
    P_2 = "RV157",
    P_3 = "RV158",
    P_4 = "RV159",
    P_5 = "RV160",
    P_6 = "RV161",
    P_7 = "RV162",
    P_8 = "RV163",
    P_9 = "RV164",
    P_10 = "RV165",
    P_11 = "RV166",
    P_12 = "RV167"
  ) |>
  # Recoding missing data values
  mutate(
    across(starts_with("P_"), ~ ifelse(. %in% c(-8, 8, 9), NA, .))
    ) |>
  # Removing participants not in the LW data file
  semi_join(lw_data, by = c("HHID", "PN")) |>
  mutate(Total_p = rowSums(across(P_1:P_12), na.rm = TRUE)) |>
  mutate(across("Total_p", ~ ifelse(. %in% 0, NA, .)))

# Processing each wave of health data ------------------------------------------
health_16 <- health_16 |>
  semi_join(proc_red, by = c("HHID", "PN")) |>
  process_health_data(prefix = "PC") |>
  height_weight()

health_18 <- health_18 |>
  semi_join(proc_red, by = c("HHID", "PN")) |>
  process_health_data(prefix = "QC") |>
  height_weight()

health_20 <- health_20 |>
  semi_join(proc_red, by = c("HHID", "PN")) |>
  process_health_data(prefix = "RC") |>
  height_weight()

health_22 <- health_22 |>
  semi_join(proc_red, by = c("HHID", "PN")) |>
  process_health_data(prefix = "SC") |>
  height_weight()

# Merging into one dataframe
health_info <- health_16 |>
  left_join(health_18, by = c("HHID", "PN"), suffix = c("_2016", "_2018")) |>
  left_join(health_20, by = c("HHID", "PN"), suffix = c("_2018", "_2020")) |>
  left_join(health_22, by = c("HHID", "PN"), suffix = c("_2020", "_2022"))

# Correcting missing height values
heights <- health_info |>
  impute_height()

# Adding heights back into dataframe
health_info <- health_info |>
  mutate(across(starts_with("Height"), ~ heights[[cur_column()]]))

# Doing a BMI calculation
health_info <- health_info |>
  mutate(
    BMI_16 = round(Weight_2016 / (Height_2016^2), digits = 1),
    BMI_18 = round(Weight_2018 / (Height_2018^2), digits = 1),
    BMI_20 = round(Weight_2020 / (Height_2020^2), digits = 1),
    BMI_22 = round(Weight_2022 / (Height_2022^2), digits = 1)
    )

# Processing each wave of cognition data ---------------------------------------
cog_16 <- cog_16 |>
  semi_join(proc_red, by = c("HHID", "PN")) |>
  process_cognition_data(prefix = "PD", focus_total = TRUE)

cog_18 <- cog_18 |>
  semi_join(proc_red, by = c("HHID", "PN")) |>
  process_cognition_data(prefix = "QD", focus_total = TRUE)

cog_20 <- cog_20 |>
  semi_join(proc_red, by = c("HHID", "PN")) |>
  process_cognition_data(prefix = "RD", focus_total = TRUE)

cog_22 <- cog_22 |>
  semi_join(proc_red, by = c("HHID", "PN")) |>
  process_cognition_data(prefix = "SD", focus_total = TRUE)

# Aggregating together
cog_info <- cog_16 |>
  left_join(cog_18, by = c("HHID", "PN"), suffix = c("_2016", "_2018")) |>
  left_join(cog_20, by = c("HHID", "PN"), suffix = c("_2018", "_2020")) |>
  left_join(cog_22, by = c("HHID", "PN"), suffix = c("_2020", "_2022"))
  
# Filtering tracker file -------------------------------------------------------
tracker_info <- tracker |>
  rename(HHID = hhid, PN = pn, Gender = gender, Age = sage, Education = degree) |>
  semi_join(proc_red, by = c("HHID", "PN")) |>
  select(HHID, PN, Gender, Age, Education) |>
  mutate(
    Gender      = ifelse(Gender == 1, 0, 1),
    Education   = ifelse(Education == 9, NA, Education),
    No_degree   = ifelse(Education == 0, 1, 0),
    High_school = ifelse(Education %in% c(1, 2), 1, 0),
    College     = ifelse(Education %in% c(3, 4), 1, 0),
    Postgrad    = ifelse(Education %in% c(5, 6), 1, 0),
    
    Education_tri = case_when(
      No_degree == 1 ~ 0,
      High_school == 1 ~ 1,
      College == 1 ~ 2,
      Postgrad == 1 ~ 2,
      TRUE ~ NA,
    ))

# Full joining both datasets
data <- list(tracker_info, health_info, cog_info, lw_data, proc_red) |>
  purrr::reduce(full_join, by = c("HHID", "PN")) |>
  mutate(ID = row_number(), .before = HHID) |>
  mutate(
    Cardio_risk_16 = rowSums(across(Hypertension_2016:H_problem_2016)),
    Cardio_risk_16 = ifelse(!is.na(BMI_16) & BMI_16 >= 30, Cardio_risk_16 + 1, Cardio_risk_16),
    
    Cardio_risk_18 = rowSums(across(Hypertension_2018:H_problem_2018)),
    Cardio_risk_18 = ifelse(!is.na(BMI_18) & BMI_18 >= 30, Cardio_risk_18 + 1, Cardio_risk_18),
    
    Cardio_risk_20 = rowSums(across(Hypertension_2020:H_problem_2020)),
    Cardio_risk_20 = ifelse(!is.na(BMI_20) & BMI_20 >= 30, Cardio_risk_20 + 1, Cardio_risk_20),
  
    Cardio_risk_22 = rowSums(across(Hypertension_2022:H_problem_2022)),
    Cardio_risk_22 = ifelse(!is.na(BMI_22) & BMI_22 >= 30, Cardio_risk_22 + 1, Cardio_risk_22)
  )|>
  select(!c(HHID, PN)) |>
  select(ID:Education_tri, Cardio_risk_16:Cardio_risk_22, Total_dep_2016:Total_dep_2022, imrc_imp2018:Total_p)


# Exporting --------------------------------------------------------------------
export_path <- "data/"

writexl::write_xlsx(
  path = here::here(export_path, "data.xlsx"),
  x = data)

