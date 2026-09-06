if (exists("country") == FALSE) {
  country <- "France"
}

# import data  ----
if (!exists("data_capex")) {
  source("R/get_investment_data.R")
}

# process data  ----
data_prep <- data_capex %>%
  filter(member_state == .env$country) %>%
  mutate(
    pp_new_major_share = new_major_investments_as_per_pp / total * 100,
    pp_other_new_share = other_new_investments_as_per_pp / total * 100,
    add_new_major_share = additional_new_major_investments / total * 100,
    add_other_new_share = 0
  ) %>%
  select(
    pp_new_major_share,
    pp_other_new_share,
    add_new_major_share,
    add_other_new_share
  ) %>%
  gather() %>%
  mutate(
    category = ifelse(grepl("^pp", key), "pp", "add"), # Extract category
    share_type = sub("^(pp_|add_)", "", key)
  ) %>% # Remove category prefix
  mutate(
    type = case_when(
      share_type == "new_major_share" ~ "New major investments",
      share_type == "other_new_share" ~ "Other new investments"
    ),
    xlabel = case_when(
      category == "pp" ~ "Included in\nthe performance plan",
      category == "add" ~ "Additional"
    ),
    xlabel = factor(
      xlabel,
      levels = c("Included in\nthe performance plan", "Additional")
    ),
    mymetric = value
  ) %>%
  arrange(desc(xlabel)) %>%
  select(xlabel, type, mymetric)

# chart ----
## chart parameters ----
local_suffix <- "%"
local_mydecimals <- 0

###set up order of traces
local_hovertemplate <- paste0('%{y:,.', local_mydecimals, 'f}', local_suffix)

#### legend
if (knitr::is_latex_output()) {
  local_legend_y <- mylegend_y
  local_legend_x <- -0.18
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont - 1
} else {
  local_legend_y <- -0.27
  local_legend_x <- -0.2
  local_legend_xanchor <- 'left'
  local_legend_fontsize <- myfont
}

# plot chart ----
myplot <- mybarchart2(
  data_prep,
  height = myheight - 50,
  colors = c('#22A0DD', '#044598'),
  local_factor = c("New major investments", "Other new investments"),

  suffix = local_suffix,
  decimals = local_mydecimals,

  hovertemplate = local_hovertemplate,
  hovermode = "x unified",

  textangle = 0,
  textposition = "inside",
  insidetextanchor = 'middle',
  textfont_color = 'white',

  bargap = 0.25,
  barmode = 'stack',

  title_text = "Asset value by investment category",
  title_y = 0.99,

  yaxis_title = "Total new asset value (%)",
  yaxis_ticksuffix = "%",
  yaxis_tickformat = ".0f",

  legend_y = local_legend_y,
  legend_x = local_legend_x,
  legend_xanchor = local_legend_xanchor,
  legend_fontsize = local_legend_fontsize - 1,
  legend_orientation = "h",
  margin = list(t = 40)
)

myplot
