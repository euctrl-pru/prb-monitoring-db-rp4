if (!exists("country") | is.na(country)) {country = rp_full}
if (!data_loaded) {
  source("R/get_data.R")
} 

## import data  ----
if (country == rp_full) {
  data_raw  <-  cap_delay_bin_actual_ses |> 
    #so it has the same columns as the state case
    mutate(ansp = "ansp",
           state = rp_full,
           total = total_dlyflt)
  
  } else {  
  data_raw  <-  cap_delay_bin_actual
}

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country,
    year <= .env$year_report) %>%
  mutate(
    x0_5mins_perc = if_else(total ==0, 0, x0_5mins /total *100),
    x5_15_mins_perc = if_else(total ==0, 0, x5_15_mins /total *100),
    x15_30_mins_perc = if_else(total ==0, 0, x15_30_mins /total *100),
    x30_60_mins_perc = if_else(total ==0, 0, x30_60_mins /total *100),
    x60_mins_perc = if_else(total ==0, 0, x60_mins /total  *100)
    ) %>% 
  select(
    year, state, ansp,
    x0_5mins_perc,
    x5_15_mins_perc,
    x15_30_mins_perc,
    x30_60_mins_perc,
    x60_mins_perc
  ) %>% 
    pivot_longer(-c(year, state, ansp), names_to = 'type', values_to = 'mymetric') %>% 
  mutate(
    xlabel = year,
    mymetric = round(mymetric,0),
    type = case_when(
      type == 'x0_5mins_perc'  ~ '<5 min',
      type == 'x5_15_mins_perc' ~ '5 - 15 min',
      type == 'x15_30_mins_perc' ~ '15 - 30 min',
      type == 'x30_60_mins_perc' ~ '30 - 60 min',
      type == 'x60_mins_perc' ~ '>60 min'
    )
  ) %>%  
  select(
    xlabel,
    type,
    mymetric)

## chart parameters ----
c_suffix <- "%"
c_decimals <- 0

### trace parameters
c_colors = c('#4472C4', '#EF7D22', '#A0A0A0', '#FDB014', '#5B9BD5')
###set up order of traces
c_factor <- c('<5 min',
              '5 - 15 min',
              '15 - 30 min',
              '30 - 60 min',
              '>60 min') 
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textposition <- "inside"
c_insidetextanchor <- 'middle'

### layout parameters
c_barmode <- 'stack'

#### title
if (knitr::is_latex_output()) {
  c_title_text <- "Distribution of IFR flights per\nthe duration of en route ATFM delay"
  c_title_y <- 0.95
  c_legend_y <- -0.2
  c_legend_x <- -0.1
  c_legend_xanchor <- 'left'
  c_margin <- list(t = 50)
  
} else {
  c_title_text <- "Distribution of IFR flights per the duration of en route ATFM delay"
  c_title_y <- 0.99
  c_legend_y <- mylegend_y
  c_legend_x <- 0.5
  c_legend_xanchor <- 'center'
  c_margin <- mymargin
  
}

#### yaxis
c_yaxis_title <- "Share of IFR flights (%)"
c_yaxis_tickformat <- ".0f"


## plot chart  ----
myplot <- mybarchart2(data_prep, 
                      height = myheight,
                      colors = c_colors,
                      local_factor = c_factor,
                      suffix = c_suffix,
                      decimals = c_decimals,
                      barmode = c_barmode,
                      
                      hovertemplate = c_hovertemplate,
                
                      textangle = c_textangle,
                      textposition = c_textposition,
                      insidetextanchor = c_insidetextanchor,
                      
                      title_text = c_title_text,
                      title_y = c_title_y,
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat,
                      
                      legend_y <- c_legend_y,
                      legend_x <- c_legend_x,
                      legend_xanchor <- c_legend_xanchor,
                      
                      margin = c_margin
) %>% 
    layout(xaxis = list(range= c(rp_min_year-0.5 ,rp_max_year+0.5),
                        tickformat = '.0f'),
           yaxis = list(rangemode = "tozero"))


myplot
