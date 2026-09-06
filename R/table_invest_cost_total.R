if (exists("country") == FALSE) {
  country <- "Denmark"
  source("R/params_country.R")
}

# import data  ----
if (!exists("data_costs")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_calc_all <- data_costs |>
  filter(ansp_type == "Main") |>
  select(
    member_state,
    category = type_of_investment,
    contains('20'),
    -contains("wacc")
  )

if (country != rp_full) {
  data_calc_filtered <- data_calc_all |>
    filter(
      member_state == .env$country
    )
} else {
  data_calc_filtered <- data_calc_all
}

data_calc <- data_calc_filtered |>
  group_by(category) |>
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE) / 10^6),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -category,
    names_to = c("year", "type"),
    names_pattern = "^x(\\d{4})([da])$",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = year,
    values_from = value
  ) |>
  mutate(
    type = if_else(type == 'd', 'Determined', 'Actual'),
    RP4 = rowSums(across(matches("^\\d{4}$")), na.rm = TRUE)
  ) |>
  group_by(category) |>
  mutate(
    summarise(
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  ) |>
  mutate(across(
    matches("^\\d{4}$"),
    ~ if (as.integer(cur_column()) > as.integer(.env$year_report)) {
      if_else(type == "Actual", NA_real_, .x)
    } else {
      .x
    }
  ))

difference_rows <- data_calc |>
  group_by(category) |>
  summarise(
    across(
      where(is.numeric),
      ~ .x[type == "Actual"] - .x[type == "Determined"]
    ),
    .groups = "drop"
  ) |>
  mutate(type = "Difference", .after = category)

difference_perc_rows <- data_calc |>
  group_by(category) |>
  summarise(
    across(
      where(is.numeric),
      ~ if_else(
        .x[type == "Determined"] == 0,
        NA_real_,
        .x[type == "Actual"] / .x[type == "Determined"] - 1
      )
    ),
    .groups = "drop"
  ) |>
  mutate(type = "Difference_perc", .after = category)

data_prep <- bind_rows(
  data_calc,
  difference_rows,
  difference_perc_rows
) |>
  mutate(
    category = case_when(
      category == 'New major investment' ~ 'New major investments from RP4',
      category == 'New major investments' ~ 'New major investments from RP4',
      category == 'Other new investments' ~ 'Other new investments from RP4',
      category == 'Other new investment' ~ 'Other new investments from RP4',
      category == 'Major investment from RP3' ~ 'Major investments from RP3',
      category ==
        'Existing investment from previous RPs' ~ 'Existing investments from previous RPs',
      .default = category
    ),
    category = factor(
      category,
      levels = c(
        'New major investments from RP4',
        'Other new investments from RP4',
        'Major investments from RP3',
        'Existing investments from previous RPs'
      )
    )
  ) |>
  arrange(category, type) |>
  ungroup()

data_prep1 <- data_prep %>%
  filter(type == "Determined") %>%
  summarise(across(-c(category, type), ~ sum(.x, na.rm = FALSE))) %>%
  mutate(
    category = paste0(
      "Total costs of new and existing investments (M€<sub>",
      cef_ref_year,
      "</sub>)"
    )
  ) %>%
  select(colnames(select(data_prep, -type))) %>%
  bind_rows(
    data_prep %>% filter(type == "Determined")
  ) %>%
  select(-type) %>%
  rename_with(
    ~ paste0(.x, "D"),
    .cols = all_of(as.character(rp_years))
  ) %>%
  mutate(category = purrr::map(category, gt::html)) %>%
  relocate(category, .before = everything())


data_prep2 <- data_prep %>%
  filter(type == "Actual") %>%
  summarise(across(-c(category, type), ~ sum(.x, na.rm = FALSE))) %>%
  mutate(
    category = paste0(
      "Total costs of new and existing investments (M€<sub>",
      cef_ref_year,
      "</sub>)"
    )
  ) %>%
  bind_rows(
    data_prep %>% filter(type == "Actual")
  ) %>%
  select(-type) %>%
  rename_with(
    ~ paste0(.x, "A"),
    .cols = all_of(as.character(rp_years))
  ) %>%
  mutate(category = purrr::map(category, gt::html)) %>%
  relocate(category, .before = everything())

data_prep3 <- data_prep %>%
  filter(type == "Difference") %>%
  summarise(across(-c(category, type), ~ sum(.x, na.rm = FALSE))) %>%
  mutate(
    category = paste0("Total difference (M€<sub>", cef_ref_year, "</sub>)")
  ) %>%
  bind_rows(
    data_prep %>% filter(str_detect(type, "Difference"))
  ) %>%
  mutate(
    category = if_else(
      type == "Difference_perc" & !is.na(type),
      "% change of actual with respect to determined",
      category
    )
  ) %>%
  rowwise() %>%
  mutate(across(
    1:6,
    ~ if_else(
      type == "Difference_perc" & !is.na(type),
      paste0(if_else(.x > 0, "+", ""), janitor::round_half_up(.x, 0), "%"),
      format_parens(.x)
    )
  )) %>%
  ungroup() %>%
  select(-type) %>%
  mutate(category = purrr::map(category, gt::html)) %>%
  relocate(category, .before = everything()) %>%
  mutate(across(2:7, ~ if_else(str_detect(.x, "NA%"), NA, .x)))


# render tables ----
first_column_width <- 40

## table1 -----
table1 <- mygtable(data_prep1, myfont) %>%
  tab_options(
    column_labels.background.color = "#F2F2F2",
    column_labels.font.weight = 'bold',
    container.padding.y = 0
  ) %>%
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    category = "Determined costs"
  ) %>%
  fmt_number(
    columns = 2:7,
    decimals = 2,
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
      columns = rp_short
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = rp_short
    )
  ) %>%
  cols_width(
    category ~ pct(first_column_width),
    c(2:7) ~ pct((100 - first_column_width) / 6)
  )


## table2 -----
table2 <- mygtable(data_prep2, myfont) %>%
  tab_options(
    column_labels.background.color = "#F2F2F2",
    column_labels.font.weight = 'bold',
    container.padding.y = 0
  ) %>%
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    category = "Actual costs"
  ) %>%
  fmt_number(
    columns = 2:7,
    decimals = 2,
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
      columns = rp_short
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = rp_short
    )
  ) %>%
  cols_width(
    category ~ pct(first_column_width),
    c(2:7) ~ pct((100 - first_column_width) / 6)
  )


## table3 -----
table3 <- mygtable(data_prep3, myfont) %>%
  tab_options(
    column_labels.background.color = "#F2F2F2",
    column_labels.font.weight = 'bold',
    container.padding.y = 0
  ) %>%
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
    style = cell_text(style = "italic"),
    locations = cells_body(
      rows = grepl("% change", category, fixed = TRUE)
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_body(
      columns = rp_short
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "#E5E5E5",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = rp_short
    )
  ) %>%
  cols_width(
    category ~ pct(first_column_width),
    c(2:7) ~ pct((100 - first_column_width) / 6)
  )


# create latex table
if (knitr::is_latex_output()) {
  table_level2_cef_cost_infl <- mylatex(table1)
} else {
  # table1
  # table2
  # table3
}
