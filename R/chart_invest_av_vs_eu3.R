if (exists("country") == FALSE) {
  country <- "Bulgaria"
}

# import data  ----
if (!exists("data_assets")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_pre_prep <- data_assets |>
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
        "overhaul_of_existing_atm_system" ~ "Overhaul of existing\nATM system",
      type == "other_atm" ~ "Other ATM",
      type == "cns" ~ "CNS",
      type == "infrastructure" ~ "Infrastructure",
      type == "ancillary" ~ "Ancillary",
      type == "other" ~ "Other",
      type == "unknown" ~ "Unknown",
    )
  )

data_prep_uw <- data_pre_prep |>
  group_by(type) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  mutate(
    mymetric = 100 * value / sum(value, na.rm = TRUE),
    xlabel = "Union-wide"
  ) |>
  select(xlabel, type, mymetric)

data_prep_ansp <- data_pre_prep |>
  filter(member_state == .env$country) |>
  mutate(
    mymetric = 100 * value / sum(value, na.rm = TRUE),
    xlabel = "ANSP"
  ) |>
  select(xlabel, type, mymetric)

if (nrow(data_prep_ansp) != 0) {
  data_prep <- rbind(data_prep_ansp, data_prep_uw) %>%
    mutate(xlabel = factor(xlabel, levels = c("ANSP", "Union-wide")))

  # chart ----
  ## chart parameters ----
  local_suffix <- "%"
  local_decimals <- 0

  ###set up order of traces
  local_hovertemplate <- paste0('%{y:,.', local_decimals, 'f}', local_suffix)

  #### legend
  if (knitr::is_latex_output()) {
    local_legend_y <- mylegend_y
    local_legend_x <- -0.18
    local_legend_xanchor <- 'left'
    local_legend_fontsize <- myfont - 1
  } else {
    local_legend_y <- -0.12
    local_legend_x <- -0.1
    local_legend_xanchor <- 'left'
    local_legend_fontsize <- myfont - 1
  }

  # plot chart ----
  myplot <- mybarchart2(
    data_prep,
    height = myheight + 20,
    colors = c(
      '#044598',
      '#22A0DD',
      '#58595B',
      '#FFF000',
      '#7030A0',
      '#2E8B57',
      '#F28E2B',
      '#D62728'
    ),
    # colors = c('#044598', '#22A0DD', '#58595B', '#FFF000', '#7030A0'),
    local_factor = c(
      "New ATM system",
      "Overhaul of existing\nATM system",
      "Other ATM",
      "CNS",
      "Infrastructure",
      "Ancillary",
      "Other",
      "Unknown"
    ),
    shape = c(
      "",
      "/",
      "",
      "/",
      "",
      "/",
      "",
      "/",
      "",
      "/",
      "",
      "/",
      "",
      "/",
      "",
      "/"
    ),

    suffix = local_suffix,
    decimals = local_decimals,

    hovertemplate = local_hovertemplate,
    hovermode = "x unified",

    textangle = 0,
    textposition = "inside",
    textfont_color = 'black',
    insidetextanchor = 'middle',

    bargap = 0.25,
    barmode = 'stack',

    title_text = "",
    title_y = 0.99,

    yaxis_title = paste0("Asset value for new investments\nfor RP", rp, " (%)"),
    yaxis_ticksuffix = "%",
    yaxis_tickformat = ".0f",
    yaxis_titlefont_size = myfont - 1,

    legend_y = local_legend_y,
    legend_x = local_legend_x,
    legend_xanchor = local_legend_xanchor,
    legend_fontsize = local_legend_fontsize
  )

  myplot
}
