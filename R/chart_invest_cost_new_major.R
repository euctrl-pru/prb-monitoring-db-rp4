if (exists("country") == FALSE) {
  country <- "Bulgaria"
}

# import data  ----
if (!exists("data_costs")) {
  source("R/get_investment_data.R")
}


# process data  ----
data_prep <- data_costs |>
  filter(
    member_state == .env$country,
    ansp_type == "Main",
    !is.na(name_of_investment),
    !name_of_investment %in% c('n/a', '0')
  ) |>
  select(category = name_of_investment, contains("20"), -contains("wacc")) |>
  group_by(category) |>
  summarise(
    across(
      where(is.numeric),
      ~ sum(.x, na.rm = TRUE) / 10^6
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -category,
    names_to = c("year", "type"),
    names_pattern = "^x(\\d{4})([da])$",
    values_to = "value"
  ) |>
  group_by(category, type) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  mutate(
    type = if_else(type == "d", "Determined", "Actual")
  ) |>
  select(
    xlabel = category,
    type,
    mymetric = value
  ) |>
  mutate(xlabel = sapply(xlabel, wrap_label))

## find number of investments
no_investments <- data_prep %>% nrow() / 2

# chart ----
## chart parameters ----
local_suffix <- ""
local_decimals <- 1

###set up order of traces
local_hovertemplate <- paste0('%{y:,.', local_decimals, 'f}', local_suffix)

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont - 1
} else {
  local_legend_y <- 1.3
  local_legend_x <- 0
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont - 1
}

# plot chart ----
myplot <- mybarchart2(
  data_prep,
  height = myheight + 100,
  colors = c(PRBPlannedColor, PRBActualColor),
  local_factor = c("Determined", "Actual", NULL),
  # shape = c("/", "", "/", "", "/", "", "/", "", "/", ""),

  suffix = local_suffix,
  decimals = local_decimals,

  hovertemplate = local_hovertemplate,
  hovermode = "x unified",

  textangle = if_else(country == "Spain", -90, 0),
  textposition = "outside",
  textfont_color = 'black',
  insidetextanchor = 'middle',

  bargap = 0.25,
  barmode = 'group',

  title_text = paste0("Total costs of major investments - RP", rp),
  title_y = 0.99,

  textfont_size = myfont - 2,
  xaxis_tickfont_size = myfont - 2,
  xaxis_tickangle = -90,

  yaxis_title = paste0(
    "Total costs of investments\nin RP",
    rp,
    " (M€<sub>",
    cef_ref_year,
    "</sub>)"
  ),
  yaxis_ticksuffix = local_suffix,
  yaxis_tickformat = ".0f",
  yaxis_titlefont_size = myyaxis_titlefont_size - 1,
  yaxis_standoff = 5,

  legend_y = local_legend_y,
  legend_x = local_legend_x,
  legend_xanchor = local_legend_xanchor,
  legend_fontsize = local_legend_fontsize,
  legend_orientation = "h",

  margin = list(t = 60)
)

if (country == "Spain") {
  myplot |>
    layout(
      yaxis = list(
        range = c(0, 400)
      )
    )
} else {
  myplot
}
