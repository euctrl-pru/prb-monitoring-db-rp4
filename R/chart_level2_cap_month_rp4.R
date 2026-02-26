if (!exists("country") | is.na(country)) {country = "Spain"}
if (exists("cztype") == FALSE) {cztype = "terminal"}
if (!data_loaded) {
  source("R/get_data.R")
}

if (country == rp_full) {
  # SES case ----
  ## import data  ----
  if (cztype == "enroute") {data_raw_actual <- cap_ert_atfm_actual_mm_ses} else {data_raw_actual <- cap_trm_atfm_actual_mm_ses}

  data_raw_target  <-  cap_ert_target_ses
  
  ## prepare data ----
  data_prep_target <- data_raw_target %>% 
    filter(
      year == .env$year_report) %>% 
    right_join(filter(data_raw_actual, year == .env$year_report),
               by = "year") %>% 
    mutate(
      xlabel = month,
      myothermetric = round(delay_target, 2),
      type = "Target"
    ) %>% 
    select(
      xlabel,
      myothermetric,
      type
    )
  
  if(cztype == 'terminal') {data_prep_target$myothermetric = NA}

  data_prep_actual <- data_raw_actual %>% 
    filter(
      year == .env$year_report) %>% 
    select(c(month, atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc)) %>% 
    pivot_longer(
      cols = c(atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "mymetric"
    ) %>% 
    mutate(
      xlabel = month,
      type = case_when(
        type == "atc_capacity" ~ "Capacity",
        type == "atc_staffing" ~ "Staffing",
        type == "atc_disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      ) 
    ) 

  data_prep_total <- data_raw_actual %>% 
    filter(year == .env$year_report) %>% 
    mutate(xlabel = month,
           myothermetric = format(round(average_delay,2), digits = 2),
           type = "Total") %>% 
    select(xlabel, myothermetric, type)
  
  
} else {
  # state case ----
  ## import data  ----
  if (cztype == "enroute") {
    data_raw_target <- cap_ert_target
    data_raw_actual <- cap_ert_atfm_actual_mm
    
  } else {
    data_raw_target <- cap_trm_target 
    data_raw_actual <- cap_trm_atfm_actual_mm
  }
  
  ## prepare data ----
  data_prep_target <- data_raw_target %>% 
    filter(
      state == .env$country,
      year == .env$year_report
      ) %>% 
    mutate(
      myothermetric = round(delay_target, 2),
      xlabel = year,
      type = "Target"
    ) %>% 
    select(
      xlabel,
      type,
      myothermetric)
  
  data_prep_actual <- data_raw_actual %>% 
    filter(
      state == .env$country,
      year == .env$year_report
      ) %>% 
    pivot_longer(
      cols = c(atc_capacity, atc_staffing, atc_disruptions, weather, other_non_atc),
      names_to = "type",
      values_to = "mymetric"
    ) %>% 
    mutate(
      mymetric = round(mymetric,2),
      xlabel = month,
      type = case_when(
        type == "atc_capacity" ~ "Capacity",
        type == "atc_staffing" ~ "Staffing",
        type == "atc_disruptions" ~ "Disruptions",
        type == "weather" ~ "Weather",
        type == "other_non_atc" ~ "Other non-ATC"
      )
    ) %>% 
    select(xlabel, type, mymetric)
  
  data_prep_total <- data_prep_actual %>% 
    select(xlabel, mymetric) %>% 
    group_by(xlabel) %>% 
    summarise(myothermetric = sum(mymetric)) %>%
    mutate(myothermetric = case_when(
      is.na(myothermetric) == TRUE ~ "",
      .default = format(round(myothermetric,2), digits = 2)
      )
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
if (knitr::is_latex_output()) {
  c_level1_title <- " ATFM delay\nby delay groups  - "
  c_title_y <- 0.95
  c_legend_y <- -0.2
  c_legend_x <- -0.18
  c_legend_xanchor <- 'left'
  c_legend_fontsize <- myfont-1
  c_margin <- list(t = 50)
  c_xaxis_tickangle <- -90 
  
} else {
  c_level1_title <- " ATFM delay by delay groups  - "
  c_title_y <- 0.99
  c_legend_y <- mylegend_y
  c_legend_x <- -0.1
  c_legend_xanchor <- 'left'
  c_legend_fontsize <- myfont
  c_margin <- mymargin
  c_xaxis_tickangle <- 0 
}

c_title_text <- paste0("Monthly distribution of ", if_else(cztype == "enroute", "en route", "arrival"),
                       c_level1_title, year_report)

#### xaxis
c_xaxis_dtick <- 'M1'
c_xaxis_tickformat <- "%b"

#### yaxis
c_yaxis_title <- "ATFM delay (min/flight)"
c_yaxis_tickformat <- ".2f"

# plot chart ----
p1 <- mybarchart2(data_prep_actual, 
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
                      title_y = c_title_y,
                      
                      xaxis_dtick = c_xaxis_dtick,
                      xaxis_tickformat = c_xaxis_tickformat,
                      xaxis_tickangle = c_xaxis_tickangle,
                  
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat,
                      
                      legend_x = c_legend_x,
                      legend_y = c_legend_y,
                      legend_xanchor = c_legend_xanchor,
                      legend_fontsize = c_legend_fontsize,
                      
                      margin = c_margin
) %>% 
  add_line_trace2(., data_prep_total,
                  name = "Total delay",
                  textfontcolor = 'black',
                  linecolor = "rgba(0,0,0,0)",
                  markercolor = "rgba(0,0,0,0)",
                  showlegend = FALSE,
                  textweight = TRUE
  ) %>% 
  layout(
    yaxis = list(rangemode = "tozero")
    )

p1

