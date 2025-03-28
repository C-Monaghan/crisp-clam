
data <- readxl::read_xlsx(here::here("data/lw_data_full.xlsx"))



write.csv(x = data, file = here::here("data/data.csv"))
saveRDS(object = data, file = here::here("data/data.rds"))
haven::write_dta(data = data, path = here::here("data/data.dta"))
