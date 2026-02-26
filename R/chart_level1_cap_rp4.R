if (exists("cztype") == FALSE) {cztype = "enroute"}
if (!data_loaded) {
  source("R/get_data.R")
} 

if (country == rp_full) {
  # SES case ----
  ## import data  ----
  if(cztype == "enroute") {data_raw_actual <- cap_ert_atfm_actual_ses} else {data_raw_actual <- cap_trm_atfm_actual_ses}
  data_raw_target  <-  cap_ert_target_ses
  
  ## prepare data ----
  data_prep_target <- data_raw_target %>% 
    mutate(
      xlabel = year,
      myothermetric = round(delay_target, 2),
      type = "Target"
    ) %>% 
    select(
      xlabel,
      type,
      myothermetric
    ) %>% arrange(xlabel)
  
  if(cztype == 'terminal') {data_prep_target$myothermetric = NA}

  data_prep <- data_raw_actual %>% 
    select(c(year, atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc)) %>% 
    pivot_longer(
      cols = c(atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "mymetric"
    ) %>% 
    mutate(
      movements = NA,
      xlabel = year,
      type = case_when(
        type == "atc_capacity" ~ "Capacity",
        type == "atc_staffing" ~ "Staffing",
        type == "atc_disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      ),
      mymetric = case_when(
        xlabel > year_report ~ NA,
        .default = mymetric
      )
    ) 
  
  data_prep_total <- data_raw_actual %>% 
    mutate(xlabel = year,
           myothermetric = case_when(
             xlabel > year_report ~ NA,
             .default = format(round(average_delay,2), digits = 2)
           ),
           type = "Total") %>% 
    select(xlabel, myothermetric, type)
    
} else {
# state case ----
  ## import data  ----
  if (cztype == "enroute") {
    data_raw_target <- cap_ert_target
    data_raw_actual <- cap_ert_atfm_actual
    
  } else {
    data_raw_actual <- cap_trm_atfm_actual
    data_raw_target <- cap_trm_target 
  }
  
  ## prepare data ----
  data_prep_target <- data_raw_target %>% 
    filter(
      state == .env$country) %>% 
    mutate(
      myothermetric = round(delay_target, 2),
      type = 'Target',
      xlabel = year
    ) %>% 
    select(
      xlabel,
      type,
      myothermetric
    ) %>% arrange(xlabel)
  
  data_prep <- data_raw_actual %>% 
    filter(
      state == .env$country,
      year <= .env$year_report) %>% 
    rename(xlabel = year) %>% 
    pivot_longer(
      cols = c(atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "mymetric"
    ) %>% 
    mutate(
      mymetric = case_when(
        is.na(mymetric) == TRUE & xlabel == year_report ~ 0,
        .default = mymetric
      ),
      type = case_when(
        type == "atc_capacity" ~ "Capacity",
        type == "atc_staffing" ~ "Staffing",
        type == "atc_disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      )
    )
  
  data_prep_total <- data_prep %>% 
    select(xlabel, mymetric) %>% 
    group_by(xlabel) %>% 
    summarise(myothermetric = sum(mymetric)) %>%
    mutate(myothermetric = case_when(
      is.na(myothermetric) == TRUE ~ "",
      .default = format(round(myothermetric,2), digits = 2))
           ) %>% 
    mutate(type = "Total")
}

# chart parameters ----
c_suffix <- ""
c_decimals <- 2

### trace parameters
c_colors = c('#EF7D22', '#F9CCAB', '#FDB014', '#70AD47', '#A0A0A0')

###set up order of traces
c_factor <- c("Capacity", "Staffing", 
              "Disruptions", "Weather",
              "Other non-ATC")

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- 0
c_textposition <- "inside"
c_insidetextanchor <- 'middle'
c_textfont_color <- 'transparent'
c_textfont_size <- 1

### layout parameters
c_barmode <- 'stack'

#### title
c_title_text <- paste0("Average ", 
                       if_else(cztype == "enroute", "en route", "arrival"),
                       " ATFM delay per flight by delay groups")

#### yaxis
c_yaxis_title <- "ATFM delay (min/flight)"
c_yaxis_tickformat <- ".2f"

#### legend
if (knitr::is_latex_output()) {
  c_legend_y <- mylegend_y
  c_legend_x <- -0.18
  c_legend_xanchor <- 'left'
  c_legend_fontsize <- myfont-1

} else {
  c_legend_y <- mylegend_y
  c_legend_x <- -0.1
  c_legend_xanchor <- 'left'
  c_legend_fontsize <- myfont

}

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+20,
                      colors = c_colors,
                      local_factor = c_factor,
                      suffix = c_suffix,
                      decimals = c_decimals,
                      barmode = c_barmode,

                      hovertemplate = c_hovertemplate,
                      
                      textangle = c_textangle,
                      textposition = c_textposition,
                      textfont_color = c_textfont_color,
                      textfont_size = c_textfont_size,
                      insidetextanchor = c_insidetextanchor,
                      
                      title_text = c_title_text,
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat,
                      
                      legend_y = c_legend_y,
                      legend_fontsize = c_legend_fontsize
) %>% 
  add_line_trace2(., data_prep_total,
                  name = "Total delay",
                  textfontcolor = 'black',
                  linecolor = "rgba(0,0,0,0)",
                  markercolor = "rgba(0,0,0,0)",
                  showlegend = FALSE,
                  textweight = TRUE
  ) %>% 
  add_line_trace2(., data_prep_target,
                  name = "Target",
                  # textfontcolor = 'black',
                  # linecolor = "rgba(0,0,0,0)",
                  showlegend = TRUE,
                  textweight = FALSE
  ) %>%  
  layout(xaxis = list(
    range = c(rp_min_year-0.5, rp_max_year+0.5)
  ))

myplot
