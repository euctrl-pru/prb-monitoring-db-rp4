if (exists("country") == FALSE) {
  country <- "Bulgaria"
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
        "Additional new major investment",
        "Additional new major investments"
      ) &
      ansp_type == "Main"
  ) |>
  select(
    member_state,
    value_of_the_assets,
    new_atm_system,
    overhaul_of_existing_atm_system,
    other_atm,
    cns,
    infrastructure,
    ancillary,
    other,
    unknown
  ) |>
  mutate(
    across(-c(member_state, unknown), ~ replace_na(.x, "0")),
    across(-c(member_state, unknown), ~ as.numeric(.x)),
  ) |>
  pivot_longer(
    -c(member_state, value_of_the_assets),
    values_to = "value",
    names_to = "type"
  ) |>
  group_by(member_state, type) |>
  summarise(
    value = sum(value * value_of_the_assets, na.rm = TRUE) / 10^6,
    .groups = "drop"
  ) |>
  mutate(
    type = case_when(
      type == "new_atm_system" ~ "New ATM system",
      type ==
        "overhaul_of_existing_atm_system" ~ "Overhaul of existing ATM system",
      type == "other_atm" ~ "Other ATM",
      type == "cns" ~ "CNS",
      type == "infrastructure" ~ "Infrastructure",
      type == "ancillary" ~ "Ancillary",
      type == "other" ~ "Other",
      type == "unknown" ~ "Unknown",
    )
  )

if (country != rp_full) {
  data_pre_prep <- data_calc |>
    filter(member_state == .env$country)
} else {
  data_pre_prep <- data_calc |>
    group_by(type) |>
    summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      member_state = rp_full
    )
}

if (nrow(data_pre_prep) != 0) {
  data_prep <- data_pre_prep |>
    mutate(
      share = value / sum(value, na.rm = TRUE),
      type = factor(
        type,
        levels = c(
          "New ATM system",
          "Overhaul of existing ATM system",
          "Other ATM",
          "CNS",
          "Infrastructure",
          "Ancillary",
          "Other",
          "Unknown"
        )
      )
    ) |>
    arrange(type) |>
    select(type, value, share)

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

# create latex table
# if (knitr::is_latex_output()) {
#   table_level2_cef_cost_infl <- mylatex(table1)
# } else {}
