## import data  ----
if (!data_loaded) {
  source("R/get_data.R")
}

data_raw  <-  cap_apt_pis_actual

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country) %>%
  mutate_at(vars(-one_of(c('year', 'airport_icao'))), ~ ifelse(year > year_report, NA, .)) %>% 
  filter(airport_icao %in% airports_table$apt_code) %>% 
  left_join(airports_table, by = c("airport_icao" = "apt_code")) %>% 
  arrange(apt_name) %>% 
  rename("Airport name" = apt_name) %>% 
  mutate("Avg arrival\nATFM delay (KPI#2)" = format(round(terminal_delay, 2), decimals = 2),
         "Slot adherence (PI#1)" = paste0(format(round(slot_adherence*100, 1), decimals = 1), "%"),
         "ATC pre departure\ndelay (PI#2)" = format(round(atc_predep_dly, 2), decimals = 2),
         "All causes pre departure\ndelay (PI#3)" = format(round(all_cause_predep_dly, 1), decimals = 1),
  ) %>% 
  select(
    year,
    "Airport name"
    # , "Avg arrival\nATFM delay (KPI#2)"
    # , "Slot adherence (PI#1)"
    , "ATC pre departure\ndelay (PI#2)"
    , "All causes pre departure\ndelay (PI#3)"
  ) %>% 
  pivot_wider(names_from = "year", values_from = c(    
    # "Avg arrival\nATFM delay (KPI#2)"
    # , "Slot adherence (PI#1)"
    "ATC pre departure\ndelay (PI#2)"
    , "All causes pre departure\ndelay (PI#3)"
    )
              # , names_glue = "{year}_{.value}" #suffix to prefix
              ) %>% 
  mutate(across(everything(), ~ str_replace_all(.x, fixed("NA%"), "NA"))) 




