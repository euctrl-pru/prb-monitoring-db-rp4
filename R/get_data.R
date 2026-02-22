library(janitor)
library(tidyr)
library(dplyr)
library(openxlsx)
library(readxl)
library(stringr)
library(here)
library(lubridate)

# main state params ----
params_table <- read_mytable("lists.xlsx", "lists", "Table_States") %>% clean_names()

state_list <- params_table %>% select(state) %>% unlist()

aua_entities_table <- read_mytable("lists.xlsx", "lists", "Table_AUA") %>% clean_names()

acc_list_table <- read_mytable("lists.xlsx", "lists", "Table_ACCs") %>% clean_names()

ecz_list_table <- read_mytable("lists.xlsx", "lists", "Table_ECZ") %>%
  left_join(
    read_mytable("lists.xlsx", "lists", "Table_forecast"),
    by = "forecast_id"
    ) %>% clean_names()

tcz_list_table <- read_mytable("lists.xlsx", "lists", "Table_TCZ") %>% clean_names()

context_data_table <- read_mytable("context_data.xlsx", "context", "Table_context") %>%  clean_names()

other_orgs_table <- read_mytable("lists.xlsx", "lists", "Table_PP_other_ANSPs") %>%  clean_names()

saf_ansp_table <- read_mytable("lists.xlsx", "lists", "Table_SAF_ANSP") %>% clean_names()

# traffic ----
## forecast & actuals ----
statfor_mvt  <-  read_xlsx(
  here(data_folder, statfor_mvt_file),
  sheet = "data",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names() %>% 
  filter (daio == "T" & year >= rp_min_year-1 & year <= rp_max_year)

statfor_tsu  <-  read_xlsx(
  here(data_folder, statfor_tsu_file),
  sheet = "data",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>%
  clean_names() %>% 
  filter (year >= rp_min_year-1 & year <= rp_max_year)

## targets ----
traffic_target  <-  read_xlsx(
  here(data_folder, "targets.xlsx"),
  sheet = "IFR_MVTS",
  range = cell_limits(c(3, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  select(state, year, x121_ecz_name,x121_ecz_ifr_mvt, x121_ecz_su)
  
# ENV ----
## targets ----
### KEA ----
kea_target  <-  read_xlsx(
  here(data_folder, "targets.xlsx"),
  sheet = "KEA",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  rbind (
    read_xlsx(
      here(data_folder, "targets.xlsx"),
      sheet = "KEA_FABEC",
      range = cell_limits(c(1, 1), c(NA, NA))) %>%
      as_tibble() %>% 
      clean_names()
  ) %>% 
  select(year, state, 
         kea_target = x321_kea_target)

## actuals ----
### KEA ----
kea_actual  <-  read_xlsx(
  here(data_folder, "env_actuals.xlsx"),
  sheet = "kea",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

kea_actual_mm  <-  read_xlsx(
  here(data_folder, "env_actuals.xlsx"),
  sheet = "kea_mm",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  mutate(month = as.Date(month))

### KEP ----
kep_actual <-  read_xlsx(
  here(data_folder, "env_actuals.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "kep",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

kep_actual_mm  <-  read_xlsx(
  here(data_folder, "env_actuals.xlsx"),
  sheet = "kep_mm",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  mutate(month = as.Date(month))

### SCR ----
scr_actual <-  read_xlsx(
  here(data_folder, "env_actuals.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "scr",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 


scr_actual_mm  <-  read_xlsx(
  here(data_folder, "env_actuals.xlsx"),
  sheet = "scr_mm",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() %>% 
  mutate(month = as.Date(month))

### AXOT ----
axot_actual_ms  <-  read_xlsx(
  here(data_folder, "env_actuals.xlsx"),
  sheet = "axot_ms",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

asma_actual_ms  <-  read_xlsx(
  here(data_folder, "env_actuals.xlsx"),
  sheet = "asma_ms",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# ceff raw data ----
# data_raw_t1_ert  <-  read_xlsx(
#   paste0(data_folder, "CEFF dataset master.xlsx"),
#   sheet = "Enroute_T1",
#   range = cell_limits(c(1, 1), c(NA, NA))) %>%
#   as_tibble() %>% 
#   clean_names() 

# data_raw_t1_trm  <-  read_xlsx(
#   paste0(data_folder, "CEFF dataset master.xlsx"),
#   # here("data","hlsr2021_data.xlsx"),
#   sheet = "Terminal_T1",
#   range = cell_limits(c(1, 1), c(NA, NA))) %>%
#   as_tibble() %>% 
#   clean_names() 
