if (exists("country") == FALSE) {country <- rp_full}
if (!data_loaded) {
  source("R/get_data.R")
}

# import data  ----
data_raw  <-  saf_eosm_ansp_ses

# process data  ----
mylabels <- c(
  "Safety\nculture",
  "Safety policy\n& objectives",
  "Safety risk\nmanagement",
  "Safety\nassurance",
  "Safety\npromotion"
  )

data_prep <- data_raw %>% 
  filter(year == year_report) %>% 
  select(
    - c(state)
  ) %>% 
  mutate(
    xlabel = case_when (
      management_objectives == "Safety culture" ~ mylabels[[1]],
      management_objectives == "Safety policy & objectives" ~ mylabels[[2]],
      management_objectives == "Safety risk management" ~ mylabels[[3]],
      management_objectives == "Safety assurance" ~ mylabels[[4]],
      management_objectives == "Safety promotion" ~ mylabels[[5]]
    ),
    xlabel = factor (xlabel, levels = mylabels
    ),
  ) %>% 
  select(
    xlabel,
    type = level,
    mymetric = number_of_ans_ps
  ) 

# chart ----
## chart parameters ----
local_suffix <- ""
local_decimals <- 0

###set up order of traces
local_hovertemplate <- paste0('%{y:,.', local_decimals, 'f}', local_suffix)

#### legend
if (knitr::is_latex_output()) {
  local_height <- myheight-40
  local_xaxis_tickfont_size <- myfont -4
  local_yaxis_tickfont_size <- myfont -2
  local_textfont_size <- myfont -2
  
  local_legend_y <- 1.35
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont-2
  
} else {
  local_height <- myheight
  local_xaxis_tickfont_size <- myfont
  local_yaxis_tickfont_size <- myfont
  local_textfont_size <- myfont-1
  local_legend_y <- 1.35
  local_legend_x <- 0.5
  local_legend_xanchor <- 'center'
  local_legend_fontsize <- myfont
  
}

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = local_height,
                      colors = c('#585858',  PRBActualColor, '#00B0F0', '#196AB4'),
                      local_factor = c("A",
                                       "B",
                                       'C', 
                                       'D',
                                        NULL),
                      # shape = c("/", "", "/", "", "/", "", "/", "", "/", ""),
                      
                      suffix = local_suffix,
                      decimals = local_decimals,
                      
                      hovertemplate = local_hovertemplate,
                      hovermode = "x unified",
                      
                      textangle = 0,
                      textposition = "outside",
                      textfont_color = 'black',
                      insidetextanchor = 'middle',
                      textfont_size = local_textfont_size,
                      
                      bargap = 0.25,
                      barmode = 'group',
                      
                      title_text = "",
                      title_y = 0.99,
                      
                      xaxis_tickfont_size = local_xaxis_tickfont_size,
                      yaxis_title = "Number of ANSPs",
                      yaxis_ticksuffix = local_suffix,
                      yaxis_tickformat = "",
                      yaxis_tickfont_size = local_yaxis_tickfont_size,
                      
                      trace_showlegend = FALSE,
                      
                      legend_y = local_legend_y, 
                      legend_x = local_legend_x,
                      legend_xanchor = local_legend_xanchor,
                      legend_fontsize = local_legend_fontsize) %>% 
  layout(
    showlegend = TRUE,
    yaxis = list(
      showticklabels = FALSE,
      range = c (0,30)
    )
  ) %>% 
  # to force full legend
  add_trace(
    data = data.frame(
      xlabel = mylabels[[1]],
      type = "A",
      mymetric = 100
    ),
    x = ~xlabel, y = ~mymetric,
    type = "scatter",
    mode = "markers",
    name = "A",
    marker = list(color = '#585858',
                  symbol = "square",
                  size = 11),
    # showlegend = TRUE,
    hoverinfo = "skip",
    inherit = FALSE
  ) %>% 
  add_trace(
    data = data.frame(
      xlabel = mylabels[[1]],
      type = "B",
      mymetric = 100
    ),
    x = ~xlabel, y = ~mymetric,
    type = "scatter",
    mode = "markers",
    name = "B",
    marker = list(color = PRBActualColor,
                  symbol = "square",
                  size = 11),
    # showlegend = TRUE,
    hoverinfo = "skip",
    inherit = FALSE
  ) %>% 
  add_trace(
    data = data.frame(
      xlabel = mylabels[[1]],
      type = "C",
      mymetric = 100
    ),
    x = ~xlabel, y = ~mymetric,
    type = "scatter",
    mode = "markers",
    name = "C",
    marker = list(color = '#00B0F0',
                  symbol = "square",
                  size = 11),
    # showlegend = TRUE,
    hoverinfo = "skip",
    inherit = FALSE
  ) %>% 
  add_trace(
    data = data.frame(
      xlabel = mylabels[[1]],
      type = "D",
      mymetric = 100
    ),
    x = ~xlabel, y = ~mymetric,
    type = "scatter",
    mode = "markers",
    name = "D",
    marker = list(color = '#196AB4',
                  symbol = "square",
                  size = 11),
    # showlegend = TRUE,
    hoverinfo = "skip",
    inherit = FALSE
  )

# colors = c('#585858',  '#FFC000', '#00B0F0', '#196AB4'),



myplot




