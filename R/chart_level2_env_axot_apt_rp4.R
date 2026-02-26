## import data  ----
if (!data_loaded) {
  source("R/get_data.R")
}

data_raw <- axot_actual_apt

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country,
    year == .env$year_report,
    airport_code %in% airports_table$apt_code) %>%
  left_join(airports_table, by = c("airport_code" = "apt_code")) %>% 
  mutate(
    xlabel = apt_name,
    type = indicator_type,
    mymetric = value
  ) %>%  
  select(
    xlabel,
    type,
    mymetric)

## chart parameters ----
c_suffix <- ""
c_decimals <- 2

### trace parameters
c_colors = PRBSecondBlue

###set up order of traces
c_factor <- data_prep %>% select(type) %>% unique() 

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textposition <- "outside"
c_insidetextanchor <- NA
c_textfont_color <- 'black'

#### title
c_title_text <- paste0("AXOT, main airport(s) - ", year_report)

#### yaxis
c_yaxis_title <- "AXOT (min/flight)"
c_yaxis_tickformat <- paste0(".",c_decimals, "f")

## plot chart  ----
myplot <- mybarchart2(data_prep, 
                      colors = c_colors,
                      local_factor = c_factor,
                      suffix = c_suffix,
                      decimals = c_decimals,
                      
                      hovertemplate = c_hovertemplate,
                      
                      textangle = c_textangle,
                      textposition = c_textposition,
                      insidetextanchor = c_insidetextanchor,
                      
                      title_text = c_title_text,
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat
)

myplot 

