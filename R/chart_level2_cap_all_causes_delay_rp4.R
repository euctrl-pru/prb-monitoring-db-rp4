if (!data_loaded) {
  source("R/get_data.R")
} 

## import data  ----
if (country == rp_full) {
  data_raw  <-  cap_all_c_predep_delay_actual_ses
  
} else {
  data_raw  <-  cap_all_c_predep_delay_actual
}

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country,
    year <= .env$year_report) %>%
  mutate(
    xlabel = year,
    mymetric = round(all_cause_predep_dly,1),
    type = "Pre-departure delay per flight"
    ) %>% 
 select(
    xlabel,
    type,
    mymetric)

## chart parameters ----
c_suffix <- ""
c_decimals <- 1

### trace parameters
c_colors <- PRBPlannedColor

###set up order of traces
c_factor <- data_prep$type %>% unique() 

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textposition <- "outside"
c_insidetextanchor <- NULL

### layout parameters
c_barmode <- 'stack'

#### title
c_title_text <- paste0("All causes pre-departure delay")


#### yaxis
c_yaxis_title <- "Delay (min/flight)"
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
                      
                      textposition = c_textposition,
                      insidetextanchor = c_insidetextanchor,
                      
                      title_text = c_title_text,

                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat
) %>% 
  layout(xaxis = list(range= c(rp_min_year-0.5 ,rp_max_year+0.5),
                      tickformat = '.0f'))


myplot


