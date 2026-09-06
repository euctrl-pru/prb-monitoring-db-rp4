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
    type = case_when(
      type_of_investment ==
        "New major investments" ~ "New and additional major investments",
      type_of_investment ==
        "New major investment" ~ "New and additional major investments",
      type_of_investment ==
        "Additional new major investments" ~ "New and additional major investments",
      type_of_investment ==
        "Additional new major investment" ~ "New and additional major investments",
      type_of_investment == "Other new investments" ~ "Other new investments",
      type_of_investment == "Other new investment" ~ "Other new investments",
      type_of_investment ==
        "Additional other new investment" ~ "Other new investments",
      type_of_investment ==
        "Additional other new investments" ~ "Other new investments",
      .default = type_of_investment
    )
  ) |>
  group_by(member_state, type) |>
  summarise(
    mymetric = sum(value_of_the_assets, na.rm = TRUE) / 10^6,
    .groups = "drop"
  ) |>
  select(
    xlabel = member_state,
    type,
    mymetric
  )

data_prep_sort <- data_pre_prep |>
  group_by(xlabel) |>
  summarise(
    mymetric = sum(mymetric, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(
    desc(mymetric)
  ) |>
  select(
    xlabel
  ) |>
  pull()

data_prep <- data_pre_prep |>
  mutate(
    xlabel = factor(xlabel, levels = data_prep_sort)
  )

# chart ----
## chart parameters ----
local_suffix <- ""
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
  local_legend_y <- 1.1
  local_legend_x <- 1
  local_legend_xanchor <- 'right'
  local_legend_fontsize <- myfont
}

# plot chart ----
myplot <- mybarchart2(
  data_prep,
  height = myheight + 40,
  colors = c(PRBPlannedColor, PRBActualColor),
  local_factor = c(
    "New and additional major investments",
    "Other new investments",
    NULL
  ),
  # shape = c("/", "", "/", "", "/", "", "/", "", "/", ""),

  suffix = local_suffix,
  decimals = local_decimals,

  hovertemplate = local_hovertemplate,
  hovermode = "x unified",

  textangle = 0,
  textposition = "none",
  textfont_color = 'black',
  insidetextanchor = 'middle',

  bargap = 0.25,
  barmode = 'stack',

  title_text = "",
  title_y = 0.99,

  xaxis_tickangle = -90,

  yaxis_title = paste0(
    "CAPEX per Member State (M€<sub>",
    cef_ref_year,
    "</sub>)"
  ),
  yaxis_ticksuffix = local_suffix,
  yaxis_tickformat = ",.0f",

  legend_y = local_legend_y,
  legend_x = local_legend_x,
  legend_xanchor = local_legend_xanchor,
  legend_fontsize = local_legend_fontsize
)

myplot
