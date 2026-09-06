if (exists("country") == FALSE) {
  country <- "Bulgaria"
}

# import data  ----
if (!exists("data_costs")) {
  source("R/get_investment_data.R")
}


# process data  ----
data_prep <- data_cost_ses %>%
  mutate(
    Determined = determined_costs_of_investments / total_determined_costs * 100,
    Actual = actual_costs_of_investments / total_actual_costs * 100,
  ) %>%
  select(
    state,
    Determined,
    Actual
  ) %>%
  pivot_longer(
    -state,
    names_to = "type",
    values_to = "mymetric"
  ) %>%
  select(
    xlabel = state,
    type,
    mymetric
  ) %>%
  arrange(desc(mymetric))

states_factor <- unique(data_prep$xlabel)

data_prep <- data_prep %>%
  mutate(xlabel = factor(xlabel, levels = states_factor))

# chart ----
## chart parameters ----
local_suffix <- "%"
local_decimals <- 1

###set up order of traces
local_hovertemplate <- paste0('%{y:.', local_decimals, 'f}', local_suffix)

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont - 1
} else {
  local_legend_y <- 1
  local_legend_x <- 1
  local_legend_xanchor <- 'right'
  local_legend_fontsize <- myfont - 2
}

# plot chart ----
myplot <- mybarchart2(
  data_prep,
  height = myheight + 40,
  colors = c(PRBPlannedColor, PRBActualColor),
  local_factor = c("Determined", "Actual", NULL),
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
  barmode = 'group',

  title_text = "",
  title_y = 0.99,

  xaxis_tickangle = -90,

  yaxis_title = "Share of costs of investments\nin total costs (%)",
  yaxis_standoff = 5,
  yaxis_ticksuffix = local_suffix,
  yaxis_tickformat = ",.0f",
  yaxis_tickfont_size = myfont,

  legend_orientation = "v",
  legend_y = local_legend_y,
  legend_x = local_legend_x,
  legend_xanchor = local_legend_xanchor,
  legend_fontsize = local_legend_fontsize
)

myplot
