if (exists("country") == FALSE) {
  country <- "Bulgaria"
}

# import data  ----
if (!exists("data_costs")) {
  source("R/get_investment_data.R")
}


# process data  ----
data_calc_all <- data_costs |>
  filter(ansp_type == "Main") |>
  select(
    member_state,
    type_of_investment,
    contains('20'),
    -contains("wacc")
  )

if (country != rp_full) {
  data_calc_filtered <- data_calc_all |>
    filter(
      member_state == .env$country
    )
} else {
  data_calc_filtered <- data_calc_all
}

data_prep <- data_calc_filtered |>
  select(xlabel = type_of_investment, contains("20"), -contains("wacc")) |>
  group_by(xlabel) |>
  summarise(
    across(
      where(is.numeric),
      ~ sum(.x, na.rm = TRUE) / 10^6
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -xlabel,
    names_to = c("year", "type"),
    names_pattern = "^x(\\d{4})([da])$",
    values_to = "mymetric"
  ) |>
  group_by(xlabel, type) |>
  summarise(mymetric = sum(mymetric, na.rm = TRUE), .groups = "drop") |>
  mutate(
    type = recode(
      type,
      d = "Determined",
      a = "Actual"
    )
  ) |>
  mutate(
    xlabel = case_when(
      xlabel == 'New major investment' ~ 'New major inv.\nfrom RP4',
      xlabel == 'Other new investment' ~ 'Other new inv.\nfrom RP4',
      xlabel == 'Major investment from RP3' ~ 'Major inv.\nfrom RP3',
      xlabel ==
        'Existing investment from previous RPs' ~ 'Existing inv.\nfrom prev. RPs',
      .default = xlabel
    ),
    xlabel = factor(
      xlabel,
      levels = c(
        'New major inv.\nfrom RP4',
        'Other new inv.\nfrom RP4',
        'Major inv.\nfrom RP3',
        'Existing inv.\nfrom prev. RPs'
      )
    )
  ) |>
  arrange(xlabel, type)


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
  local_legend_y <- -0.2
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
}

# plot chart ----
myplot <- mybarchart2(
  data_prep,
  height = myheight,
  colors = c(PRBPlannedColor, PRBActualColor),
  local_factor = c("Determined", "Actual", NULL),
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

  title_text = paste0("Total costs of investments by category - RP", rp),
  title_y = 0.99,

  yaxis_title = paste0(
    "Total costs of investments in RP",
    rp,
    " (M€<sub>",
    cef_ref_year,
    "</sub>)"
  ),
  yaxis_ticksuffix = local_suffix,
  yaxis_tickformat = ".0f",
  yaxis_titlefont_size = myyaxis_titlefont_size - 1,

  legend_y = local_legend_y,
  legend_x = local_legend_x,
  legend_xanchor = local_legend_xanchor,
  legend_fontsize = local_legend_fontsize
)

myplot
