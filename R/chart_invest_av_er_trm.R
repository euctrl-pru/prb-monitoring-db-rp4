if (exists("country") == FALSE) {
  country <- "Belgium"
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

data_prep <- data_pre_prep |>
  mutate(
    total_value_of_the_asset_nominal_euros = en_route_asset_value +
      terminal_asset_value,
    enroute_share = en_route_asset_value /
      total_value_of_the_asset_nominal_euros *
      100,
    terminal_share = terminal_asset_value /
      total_value_of_the_asset_nominal_euros *
      100,
    NULL
  ) %>%
  select(
    enroute_share,
    terminal_share,
    NULL
  ) %>%
  gather() %>%
  mutate(
    type = case_when(
      key == "enroute_share" ~ "En route",
      key == "terminal_share" ~ "Terminal"
    ),
    xlabel = "Asset value",
    mymetric = value,
    textposition = if_else(mymetric == 0 | mymetric > 2, "inside", "outside"),
    textlabel = if_else(
      mymetric == 0,
      " ",
      paste0(format(janitor::round_half_up(mymetric, 0), nsmall = 0), "%")
    )
  ) %>%
  select(xlabel, type, mymetric, textlabel, textposition)

# chart ----
## legend
if (knitr::is_latex_output()) {
  local_legend_x <- 1
  local_legend_y <- 0.5
} else {
  local_legend_x <- 0.5
  local_legend_y <- -0.05
  local_legend_xanchor <- 'center'
}


# plot chart ----
mydonutchart(
  data_prep,
  height = myheight - 95,
  colors = c('#22A0DD', '#044598'),
  hovertemplate = "%{label}: %{value}%",
  title_text = "Asset value: en route and terminal",
  minsize = 14,
  legend_x = local_legend_x,
  legend_y = local_legend_y,
  legend_xanchor = local_legend_xanchor,
  legend_orientation = "h",
  margin = list(t = 40)
)
