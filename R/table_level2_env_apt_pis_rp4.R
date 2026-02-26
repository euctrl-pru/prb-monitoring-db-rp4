
## import data  ----
if (!data_loaded) {
  source("R/get_data.R")
}

data_raw_axot  <-  axot_actual_apt
data_raw_asma  <-  asma_actual_apt
data_raw_cdo  <-  cdo_actual_apt

## prepare data ----
data_prep_axot <- data_raw_axot  %>%
  filter(
    state == .env$country,
    airport_code %in% airports_table$apt_code) %>%
  mutate_at(vars(-one_of(c('year', 'airport_code'))), ~ ifelse(year > year_report, NA, .)) %>%
  left_join(airports_table, by = c("airport_code" = "apt_code")) %>% 
  mutate(type = "Additional taxi-out time (PI#3)",
         mymetric = format(round(value,2), decimals =2)
  ) %>%
  select(apt_name, year, type, mymetric)
# %>% 
# filter(airport_icao %in% airports_table$apt_code)

data_prep_asma <- data_raw_asma  %>%
  filter(
    state == .env$country,
    airport_code %in% airports_table$apt_code) %>%
  mutate_at(vars(-one_of(c('year', 'indicator_type', 'airport_code'))), ~ ifelse(year > year_report, NA, .)) %>%
  left_join(airports_table, by = c("airport_code" = "apt_code")) %>% 
  mutate(type = "Additional ASMA time (PI#4)",
         mymetric = format(round(value,2), decimals =2 )
  ) %>%
  select(apt_name, year, type, mymetric)

data_prep_cdo <- data_raw_cdo  %>%
  filter(
    state == .env$country,
    airport_code %in% airports_table$apt_code) %>%
  mutate_at(vars(-one_of(c('year', 'airport_code'))), ~ ifelse(year > year_report, NA, .)) %>%
  left_join(airports_table, by = c("airport_code" = "apt_code")) %>% 
  mutate(type = "Share of arrivals applying CDO (PI#5)",
         mymetric = if_else(is.na(value) == TRUE, NA_character_, paste0(round(value*100,0), '%'))
  ) %>%
  select(apt_name, year, type, mymetric) 

data_prep <- data_prep_axot %>% 
  rbind(data_prep_asma) %>% 
  rbind(data_prep_cdo) %>%
  pivot_wider(names_from = "type", values_from = "mymetric"
              # , names_glue = "{year}_{.value}" #suffix to prefix
  ) %>%
  pivot_wider(names_from = "year", values_from = c("Additional taxi-out time (PI#3)",
                                                   "Additional ASMA time (PI#4)",
                                                   "Share of arrivals applying CDO (PI#5)")
              # , names_glue = "{year}_{.value}" #suffix to prefix
  ) %>%
  rename("Airport" = apt_name)

data_prep_pdf <- data_prep %>%
  mutate(
    across(c(2:11), ~if_else(is.na(.x), 'NA', .x))
  ) 


# plot table ----

table1 <- mygtable(data_prep, myfont*0.9) %>% 
  tab_spanner_delim(
    delim = "_"
  ) |> 
  tab_header(
    title = md("**Airport level**")
  )

table1
