if (exists("country") == FALSE) {country <- "Belgium"}
if (!exists("cost_type")) {cost_type <- "en route"}

# source("R/parameters.R")

# import data  ----
if (!exists("data_new_major")) {
  source("R/get_investment_data.R")
}

# process data  ----
if (cost_type == "en route") {
  data_filtered <- data_cost_inv_rt %>% 
    select(member_state, cost_type,
           d_2020 = d_enr_2020,
           d_2021 = d_enr_2021,
           d_2022 = d_enr_2022,
           d_2023 = d_enr_2023,
           d_2024 = d_enr_2024,
           
           a_2020 = a_enr_2020,
           a_2021 = a_enr_2021,
           a_2022 = a_enr_2022,
           a_2023 = a_enr_2023,
           a_2024 = a_enr_2024
    )
} else {
  data_filtered <- data_cost_inv_rt %>% 
    select(member_state, cost_type,
           d_2020 = d_ter_2020,
           d_2021 = d_ter_2021,
           d_2022 = d_ter_2022,
           d_2023 = d_ter_2023,
           d_2024 = d_ter_2024,
           
           a_2020 = a_ter_2020,
           a_2021 = a_ter_2021,
           a_2022 = a_ter_2022,
           a_2023 = a_ter_2023,
           a_2024 = a_ter_2024
    )
}

data_filtered <- data_filtered %>% 
    filter(member_state == .env$country)

data_calc <- data_filtered %>% 
  select(-member_state) %>% 
  pivot_longer(
    cols = -cost_type,  
    names_to = c("type", "year"),  # Create "type" and "year" columns
    names_pattern = "(.+?)_(\\d{4})",  # Regex: Extract "type" + 4-digit year
    values_to = "value"  # Store values in "value" column
  ) %>% 
  mutate(
    value = value/10^3,
    type = if_else(type == "d", "Determined", "Actual")
  ) %>% 
  group_by(cost_type, type, year) %>% 
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
  mutate(value = if_else(type == "Actual" & as.numeric(year) > year_report, NA, value)) %>% 
  rename(category = cost_type) %>% 
  pivot_wider(id_cols = c(category, year), names_from ="type", values_from = "value") %>% 
  mutate (
    Difference = Actual - Determined
    ) %>% 
  pivot_longer(-c(category, year), names_to = "type", values_to = "value") 

data_calc_summary <- data_calc %>%
  filter(as.numeric(year) <= year_report) %>%
  group_by(category, type) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(year = "RP3") %>% 
  select(category, year, type, value) 

data_prep <- rbind(data_calc, data_calc_summary) %>% 
  arrange(year) %>% 
  pivot_wider(id_cols = c(category, type), names_from = "year", values_from = "value") %>% 
  mutate(category = factor(category, levels = c("Depreciation",
                                                "Cost of capital",
                                                "Cost of leasing"))) %>% 
  arrange(category)

data_prep1 <- data_prep %>% filter(type == "Determined") %>% 
      summarise(across(-c(category, type), ~sum(.x, na.rm = FALSE))) %>%
      mutate(category = "Total costs of new and existing investments (M€<sub>2017</sub>)") %>%
      select(colnames(select(data_prep, -type))) %>%  
  bind_rows(
    data_prep %>% filter(type == "Determined") 
  ) %>% 
  select(-type) %>% 
  mutate(category = purrr::map(category, gt::html))

data_prep2 <- data_prep %>% filter(type == "Actual") %>% 
  summarise(across(-c(category, type), ~sum(.x, na.rm = FALSE))) %>%
  mutate(category = "Total costs of new and existing investments (M€<sub>2017</sub>)") %>%
  select(colnames(data_prep1)) %>%  
  bind_rows(
    data_prep %>% filter(type == "Actual") 
  ) %>% 
  select(-type) %>% 
  mutate(category = purrr::map(category, gt::html))

data_prep3 <- data_prep %>% filter(type == "Difference") %>% 
  summarise(across(-c(category, type), ~sum(.x, na.rm = FALSE))) %>%
  mutate(category = "Total difference (M€<sub>2017</sub>)") %>%
  select(colnames(data_prep1)) %>%  
  bind_rows(
    data_prep %>% filter(type == "Difference") 
  ) %>% 
  mutate(across(2:7, ~ if_else(
    type == "Difference_perc" & !is.na(type),
    paste0(if_else(.x >0, "+", ""), round(.x, 0), "%"),
    format_parens(.x)
  ))) %>%
  ungroup() %>%   
  select(-type) %>% 
  mutate(across(2:7, ~ if_else(str_detect(.x, "NA%"), NA, .x))) %>% 
  mutate(category = purrr::map(category, gt::html))


# render tables ----
first_column_width <- 40
  
## table1 -----
table1 <- mygtable(data_prep1, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    category = "Determined costs",
    "2020" = "2020D",
    "2021" = "2021D",
    "2022" = "2022D",
    "2023" = "2023D",
    "2024" = "2024D",
  ) %>%
  fmt_number(
    columns = 2:7,   
    decimals = if_else(country == rp_full, 1, 2),
    use_seps = TRUE,
    sep_mark = ",",
    dec_mark = "."
  ) %>% 
  # fmt_percent(
  #   columns = 3,   # replace with your actual column name
  #   decimals = 0
  # ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = 1)
    ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = 7)
  ) %>% 
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = 1,
      rows = 2:nrow(data_prep1)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_body(
      columns = "RP3"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = "RP3"
    )
  )%>% 
  cols_width(
    category ~ pct(first_column_width),
    c(2:7) ~ pct((100-first_column_width) / 6)
  )

  
## table2 -----
table2 <- mygtable(data_prep2, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    category = "Actual costs",
    "2020" = "2020A",
    "2021" = "2021A",
    "2022" = "2022A",
    "2023" = "2023A",
    "2024" = "2024A",
  ) %>%
  fmt_number(
    columns = 2:7,   
    decimals = if_else(country == rp_full, 1, 2),
    use_seps = TRUE,
    sep_mark = ",",
    dec_mark = "."
  ) %>% 
  # fmt_percent(
  #   columns = 3,   # replace with your actual column name
  #   decimals = 0
  # ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = 1)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = 7)
  ) %>% 
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = 1,
      rows = 2:nrow(data_prep2)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_body(
      columns = "RP3"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = "RP3"
    )
  )%>% 
  cols_width(
    category ~ pct(first_column_width),
    c(2:7) ~ pct((100-first_column_width) / 6)
  )


## table3 -----
table3 <- mygtable(data_prep3, myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    category = "Difference (A-D)"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(rows = 1)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = 7)
  ) %>% 
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = 1,
      rows = 2:nrow(data_prep3)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_body(
      columns = "RP3"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = "RP3"
    )
  )%>% 
  cols_width(
    category ~ pct(first_column_width),
    c(2:7) ~ pct((100-first_column_width) / 6)
  )



# create latex table
if (knitr::is_latex_output()) {
  table_level2_cef_cost_infl <- mylatex(table1)
  
} else {
  # table1
  # table2
  # table3
}

