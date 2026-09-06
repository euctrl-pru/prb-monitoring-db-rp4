if (exists("country") == FALSE) {
  country <- "Belgium"
  source("R/params_country.R")
}

# import data  ----
if (!exists("data_assets")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_calc <- data_assets |>
  filter(
    type_of_investment %in%
      c(
        "New major investment",
        "New major investments",
        "Other new investments",
        "Other new investment",
        "Additional new major investment",
        "Additional new major investments",
        "Additional other new investment",
        "Additional other new investments"
      ) &
      ansp_type == "Main"
  ) |>
  mutate(
    type_of_investment = case_when(
      type_of_investment ==
        "New major investments" ~ "Included in the performance plan",
      type_of_investment ==
        "New major investment" ~ "Included in the performance plan",
      type_of_investment == "Additional new major investments" ~ "Additional",
      type_of_investment == "Additional new major investment" ~ "Additional",
      type_of_investment ==
        "Other new investments" ~ "Other new investments (below 5M€ each)",
      type_of_investment ==
        "Other new investment" ~ "Other new investments (below 5M€ each)",
      type_of_investment ==
        "Additional other new investment" ~ "Other new investments (below 5M€ each)",
      type_of_investment ==
        "Additional other new investments" ~ "Other new investments (below 5M€ each)",
      .default = type_of_investment
    )
  ) |>
  group_by(member_state, type_of_investment) |>
  summarise(
    value_of_the_assets = sum(value_of_the_assets, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(member_state) |>
  mutate(
    value = value_of_the_assets / 10^6
  ) |>
  ungroup()

if (country != rp_full) {
  data_pre_prep <- data_calc |>
    filter(member_state == .env$country) |>
    select(
      type = type_of_investment,
      value
    )
} else {
  data_pre_prep <- data_calc |>
    group_by(type_of_investment) |>
    summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    rename(
      type = type_of_investment
    )
}

total_asset_value <- data_pre_prep |>
  summarise(value = sum(value, na.rm = TRUE)) |>
  pull()

asset_types <- tibble(
  type = c(
    "New major investments (above 5M€ each)",
    "Included in the performance plan",
    "Additional",
    "Other new investments (below 5M€ each)"
  )
)


data_prep <- data_pre_prep |>
  filter(type %in% c("Included in the performance plan", "Additional")) |>
  summarise(value = sum(value, na.rm = TRUE)) |>
  mutate(
    type = "New major investments (above 5M€ each)"
  ) |>
  relocate(type, .before = everything()) |>
  rbind(data_pre_prep) |>
  right_join(asset_types, by = "type") |>
  mutate(
    type = factor(type, levels = asset_types$type),
    value = replace_na(value, 0),
    share = value / total_asset_value
  ) |>
  arrange(type)

total_value <- format(
  janitor::round_half_up(total_asset_value, 2),
  nsmall = 2,
  big.mark = ","
)

table1 <- mygtable(data_prep, myfont) %>%
  tab_options(
    column_labels.background.color = "#F2F2F2",
    column_labels.font.weight = 'bold',
    container.padding.y = 0
  ) %>%
  cols_align(columns = 1, align = "left") %>%
  cols_label(
    type = html(paste0(
      "Total value of the asset for new investments (M€<sub>",
      cef_ref_year,
      "</sub>)"
    )),
    value = total_value,
    share = "% of total"
  ) %>%
  fmt_number(
    columns = 2, # replace with your actual column name
    decimals = 2,
    use_seps = TRUE,
    sep_mark = ",",
    dec_mark = "."
  ) %>%
  fmt_percent(
    columns = 3, # replace with your actual column name
    decimals = 0
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = c(1, 4)
    )
  ) %>%
  tab_style(
    style = cell_text(indent = px(20)),
    locations = cells_body(
      columns = c(type),
      rows = type == "Additional" | type == "Included in the performance plan"
    )
  )


# create latex table
if (knitr::is_latex_output()) {
  table_level2_cef_cost_infl <- mylatex(table1)
} else {
  table1
}
