if (!exists("country") | is.na(country)) {country <- "Poland"
source("R/parameters.R")
}

# import data  ----
if (!data_loaded) {
  source("R/get_data.R")
}

data_raw  <- saf_ri_actual_apt

# process data ----
data_prep <- data_raw %>% 
  filter(state == .env$country & year == year_report) %>% 
  arrange(desc(ifr_movements)) %>% 
  mutate(
    rank = row_number(),
    ifr_movements = format(round(ifr_movements,0), big.mark   = ",", nsmall = 0),
    ri = format(round(ri,0), big.mark   = ",", nsmall = 0),
    rate_per_100_000 = format(round(rate_per_100_000,2), big.mark   = ",", nsmall = 2)
    
  ) %>% 
  select(
    "#" = rank,
    "Airport name" = name_apt,
    "APT movements" = ifr_movements,
    "Number of RI" = ri,
    "Rate RI  per 100,000" = rate_per_100_000
  )

# plot table ----
table1 <- mygtable(data_prep, myfont) %>% 
  tab_options(column_labels.background.color = "white",
              # column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 2, align = "left") %>%
  tab_header(
    title = md(paste0("**Rate of RIs per 100,000 airport movements - ", country, "**"))
  )

if (!knitr::is_latex_output()) {
  table1
}

