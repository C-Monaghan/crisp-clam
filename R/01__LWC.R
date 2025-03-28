# Creating the LW classifications for the 2022 HRS dataset

rm(list = ls())

# Custom made package
# devtools::install_github("C-Monaghan/lwc2022")

path_data <- "data-raw"

# Reading in data --------------------------------------------------------------
data <- haven::read_sav(here::here(path_data, "2022/Cognition_2022.sav"))
proc <- haven::read_sav(here::here(path_data, "2020/Procrastination.sav"))

# Filter only to procrastination participants 
proc <- proc |>
  dplyr::filter(!is.na(RV155))

lw_2022_data <- data |>
  # Getting procrastination participants
  dplyr::semi_join(proc, by = c("HHID", "PN")) |>
  # Removing (potential) web respondents
  dplyr::filter(!is.na(SD182M1), is.na(SD182WM1)) |>
  lwc2022::extract() |>
  lwc2022::score() |>
  lwc2022::classify()

# Joining classifications to original LW datafile -------------------------------
lw_data <-  haven::read_dta(here::here(path_data, "03__LW_Dementia.dta"))

# Joining 2022 classifications
lw_data_full <- lw_data |>
  dplyr::rename(HHID = hhid, PN = pn) |>
  dplyr::semi_join(lw_2022_data, by = c("HHID", "PN")) |>
  cbind(lw_2022_data) |>
  dplyr::distinct() |> # HHID & PN duplicate so this removes the duplication
  dplyr::select(!c(HHID.1, PN.1))

# Exporting --------------------------------------------------------------------
export_path <- "data"

writexl::write_xlsx(
  path = here::here(export_path, "lw_data_full.xlsx"),
  x = lw_data_full)
