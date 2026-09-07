if (exists("country") == FALSE) {
  country <- "Estonia"
  source("R/params_country.R")
}

# import data  ----
if (!exists("data_costs_cp1")) {
  source("R/get_investment_data.R")
}

# process data  ----

if (country != rp_full) {
  data_calc_filtered <- data_costs_cp1 |>
    filter(
      member_state == .env$country
    )
} else {
  data_calc_filtered <- data_costs_cp1
}


data_pre_calc <- data_calc_filtered |>
  select(
    category = cp1_atm_functionality_cp1_af_sub_functionality_cp1_s_af,
    contains("20"),
    -contains("wacc")
  ) |>
  mutate(
    category = case_when(
      str_detect(category, "AF[1-9](?!\\d)") ~
        paste0("CP1-", str_extract(category, "AF[1-9](?!\\d)")),
      .default = category
    )
  ) |>
  group_by(category) |>
  summarise(
    across(
      where(is.numeric),
      ~ sum(.x, na.rm = TRUE) / 10^3
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -category,
    names_to = c("year", "type"),
    names_pattern = "^x(\\d{4})([da])$",
    values_to = "value"
  ) |>
  mutate(
    type = if_else(type == "d", "Determined", "Actual"),
    value = if_else(
      type == "Actual" & as.numeric(year) > year_report,
      NA,
      value
    )
  ) |>
  pivot_wider(
    id_cols = c(category, year),
    names_from = "type",
    values_from = "value"
  )

if (nrow(data_pre_calc) == 0) {
  table1 <- ''
  table2 <- ''
  table3 <- ''
} else {
  data_calc <- data_pre_calc |>
    mutate(
      Difference = Actual - Determined,
    ) |>
    pivot_longer(-c(category, year), names_to = "type", values_to = "value")

  data_calc_summary <- data_calc %>%
    filter(as.numeric(year) <= year_report) %>%
    group_by(category, type) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = rp_short) %>%
    select(category, year, type, value)

  # end_categories <- c(
  #   "Other new investments from RP4",
  #   "Major investments from RP3",
  #   "Existing investments from previous RPs"
  # )
  #
  # other_categories <- sort(setdiff(unique(data_calc$category), end_categories))

  data_prep <- rbind(data_calc, data_calc_summary) %>%
    arrange(year) %>%
    pivot_wider(
      id_cols = c(category, type),
      names_from = "year",
      values_from = "value"
    ) |>
    # mutate(
    #   category = factor(
    #     category,
    #     levels = c(other_categories, end_categories)
    #   )
    # ) |>
    arrange(category, type)

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
    mutate(category = purrr::map(category, gt::html))

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
    select(colnames(data_prep1)) %>%
    bind_rows(
      data_prep %>% filter(type == "Actual")
    ) %>%
    select(-type) %>%
    mutate(category = purrr::map(category, gt::html))

  data_prep3 <- data_prep %>%
    filter(type == "Difference") %>%
    summarise(across(-c(category, type), ~ sum(.x, na.rm = FALSE))) %>%
    mutate(
      category = paste0("Total difference (M€<sub>", cef_ref_year, "</sub>)")
    ) %>%
    select(colnames(data_prep1)) %>%
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
      2:7,
      ~ if_else(
        type == "Difference_perc" & !is.na(type),
        paste0(if_else(.x > 0, "+", ""), janitor::round_half_up(.x, 0), "%"),
        format_parens(.x)
      )
    )) %>%
    ungroup() %>%
    select(-type) %>%
    mutate(across(2:7, ~ if_else(str_detect(.x, "NA%"), NA, .x))) %>%
    mutate(category = purrr::map(category, gt::html))

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
}

# create latex table
if (knitr::is_latex_output()) {
  table_level2_cef_cost_infl <- mylatex(table1)
} else {
  # table1
  # table2
  # table3
}
