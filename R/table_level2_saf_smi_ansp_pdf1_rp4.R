if (!exists("country") | is.na(country)) {country <- "Poland"
source("R/parameters.R")
}


# import data  ----
if (!data_loaded) {
  source("R/get_data.R")
}

data_raw  <- saf_smi_actual_apt

# process data ----
if (country != rp_full) {
  myentity_order <- data_raw %>% 
    filter(state == .env$country) %>% 
    group_by(entity) %>% 
    summarise(total_fh = sum(flight_hours, na.rm = TRUE)) %>% 
    arrange(desc(total_fh)) %>% 
    mutate(myentity = factor(entity, levels = entity)) %>% 
    select(myentity)
  
  data_calc <- data_raw %>% 
    filter(state == .env$country) %>% 
    mutate(myentity = entity)
} else {
  myentity_order <- data_raw %>% 
    group_by(state) %>% 
    summarise(total_fh = sum(flight_hours, na.rm = TRUE)) %>% 
    arrange(desc(total_fh)) %>% 
    mutate(myentity = factor(state, levels = state)) %>% 
    select(myentity)
  
  data_calc <- data_raw %>% 
    group_by(year, state) %>% 
    summarise(
      smi = sum(smi, na.rm = TRUE),
      flight_hours = sum(flight_hours, na.rm = TRUE),
      rate_per_100_000 = round(smi / flight_hours * 100000,2),
      .groups = "drop"
    ) %>% 
    mutate(myentity = state)
  
}

data_prep <- data_calc %>% 
  group_by(myentity) %>% 
  arrange(year) %>% 
  mutate(
    variation = if_else(lag(rate_per_100_000, 1) == 0, 0, rate_per_100_000/lag(rate_per_100_000, 1) -1),
    variation = if_else(is.nan(variation), NA, variation),
    variation = if_else(year>year_report, NA, variation),
    smi = if_else(year>year_report, NA, smi),
    flight_hours = if_else(year>year_report, NA, flight_hours),
    rate_per_100_000 = if_else(year>year_report, NA, rate_per_100_000)
  ) %>% 
  ungroup() %>% 
  select(
    myentity,
    year,
    "Flight hours" = flight_hours,
    "Number of SMIs" = smi,
    NULL
  ) %>% 
  pivot_wider(names_from = "year", values_from = c(3:4)) %>% 
  mutate(
    myentity = factor(myentity, levels = myentity_order$myentity),
  ) %>% 
  arrange(myentity) %>% 
  mutate(
    "#" = row_number()
  ) %>% 
  relocate(
    "#", .before = 1
  ) %>% 
  mutate(
    across(
      -(1:2),
      ~ format(round(.x, 0), big.mark = ",", nsmall = 0)
    )
  ) 

if (country == rp_full) {
  data_prep <- data_prep %>% 
  rename(
    State = myentity
  )
  } else {
    data_prep <- data_prep %>% 
      rename(
        ANSP = myentity
      )
  }

    

# plot table ----
table1 <-mygtable(data_prep, myfont*0.9) %>% 
  tab_spanner_delim(
    delim = "_"
  ) |> 
  tab_header(
    title = md("**Rate of SMI with ANS contribution per 100,000 flight hours**")
  )

if (!knitr::is_latex_output()) {
  table1
}
