if (exists("country") == FALSE) {
  country <- "Belgium"
  source("R/params_country.R")
}

# import data  ----
if (!exists("data_assets")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_calc <- data_assets %>%
  filter(
    type_of_investment %in%
      c(
        "New major investment",
        "New major investments",
        "Additional new major investment",
        "Additional new major investments"
      ) &
      ansp_type == "Main"
  ) |>
  group_by(member_state) |>
  summarise(
    en_route_asset_value = sum(en_route_asset_value, na.rm = TRUE),
    terminal_asset_value = sum(terminal_asset_value, na.rm = TRUE),
    .groups = "drop"
  )

if (country != rp_full) {
  data_pre_prep <- data_calc |>
    filter(member_state == .env$country)
} else {
  data_pre_prep <- data_calc |>
    summarise(
      en_route_asset_value = sum(en_route_asset_value, na.rm = TRUE),
      terminal_asset_value = sum(terminal_asset_value, na.rm = TRUE)
    )
}

if (nrow(data_pre_prep) != 0) {
  data_prep <- data_pre_prep |>
    mutate(
      total_value_of_the_asset_nominal_euros = en_route_asset_value +
        terminal_asset_value,
      enroute_value = en_route_asset_value / 10^6,
      terminal_value = terminal_asset_value / 10^6,
      enroute_share = en_route_asset_value /
        total_value_of_the_asset_nominal_euros,
      terminal_share = terminal_asset_value /
        total_value_of_the_asset_nominal_euros,
      NULL
    ) %>%
    select(
      enroute_value,
      terminal_value,
      enroute_share,
      terminal_share,
      NULL
    ) |>
    gather() %>%
    mutate(
      type = case_when(
        str_detect(key, "enroute") ~ "En route",
        str_detect(key, "terminal") ~ "Terminal"
      ),
      key = str_remove_all(key, "enroute_"),
      key = str_remove_all(key, "terminal_")
    ) %>%
    pivot_wider(id_cols = type, names_from = "key", values_from = "value")

  total_value <- format(
    janitor::round_half_up(sum(data_prep$value, na.rm = TRUE), 2),
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
        "Total value of the asset for new major investments (M€<sub>",
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
        # cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = 1
      )
    )

  table1
} else {
  cat("No new major investments.")
}

# # create latex table
# if (knitr::is_latex_output()) {
#   table_level2_cef_cost_infl <- mylatex(table1)
# } else {
#   table1
# }
