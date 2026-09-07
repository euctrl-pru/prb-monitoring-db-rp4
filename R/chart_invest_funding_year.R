if (exists("country") == FALSE) {
  country <- "Bulgaria"
}

# import data  ----
if (!exists("data_funding_rt")) {
  source("R/get_investment_data.R")
}


# process data  ----
if (country == rp_full) {
  data_filtered <- data_funding_rt
} else {
  data_filtered <- data_funding_rt |>
    filter(member_state == .env$country)
}

data_prep1 <- data_filtered |>
  summarise(
    across(
      where(is.numeric),
      ~ sum(.x, na.rm = TRUE) / 10^3
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = c("year"),
    names_pattern = "^x(\\d{4})",
    values_to = "value"
  )

data_prep_total <- data_prep1 |>
  summarise(value = sum(value, na.rm = TRUE)) |>
  mutate(year = rp_short) |>
  select(year, value)

data_prep <- rbind(data_prep1, data_prep_total) |>
  mutate(
    type = "Total self-declared funding"
  ) |>
  select(
    xlabel = year,
    type,
    mymetric = value
  )

# chart ----
## chart parameters ----
local_suffix <- ""
local_decimals <- 2

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
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
}

# plot chart ----
myplot <- mybarchart2(
  data_prep,
  height = myheight,
  colors = c(PRBPlannedColor),
  local_factor = c("Total self-declared funding", NULL),
  # shape = c("/", "", "/", "", "/", "", "/", "", "/", ""),

  suffix = local_suffix,
  decimals = local_decimals,

  hovertemplate = local_hovertemplate,
  hovermode = "x unified",

  textangle = 0,
  textposition = "outside",
  textfont_color = 'black',
  insidetextanchor = 'middle',

  bargap = 0.25,
  barmode = 'group',

  title_text = "Total self-declared funding (reporting tables)",
  title_y = 0.99,

  yaxis_title = paste0(
    "Total self-declared funding (M€<sub>",
    cef_ref_year,
    "</sub>)"
  ),
  yaxis_ticksuffix = local_suffix,
  yaxis_tickformat = ".0f",

  legend_y = local_legend_y,
  legend_x = local_legend_x,
  legend_xanchor = local_legend_xanchor,
  legend_fontsize = local_legend_fontsize
)

myplot
