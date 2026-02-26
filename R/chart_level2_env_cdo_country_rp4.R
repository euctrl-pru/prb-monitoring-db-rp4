## import data  ----
if (!data_loaded) {
  source("R/get_data.R")
}

data_raw <- cdo_actual_ms

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country
    ) %>% 
  mutate(
    xlabel = year,
    type = indicator_type,
    mymetric = case_when(
      year <= year_report ~ round(value * 100, 0),
      .default = NA),
    textposition = 'top',
    linedash = 'solid'
  ) %>%  
  select(
    xlabel,
    type,
    mymetric,
    textposition,
    linedash
    )

## chart parameters ----
c_suffix <- "%"
c_decimals <- 0


### trace parameters
c_colors = PRBSecondBlue

###set up order of traces
c_factor <- data_prep %>% select(type) %>% unique() 

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

#### title
c_title_text <- paste0("CDOs")

#### yaxis
c_yaxis_title <- "CDOs (%)"
c_yaxis_tickformat <- paste0(".",c_decimals, "f")
c_yaxis_rangemode <- NA
c_yaxis_range <- c((floor(min(data_prep$mymetric, na.rm = TRUE)/5)*5)-5, (ceiling(max(data_prep$mymetric, na.rm = TRUE)/5)*5)+5)

## define chart function ----
myplot <- mylinechart2(data_prep, 
                      colors = c_colors,
                      local_factor = c_factor,
                      suffix = c_suffix,
                      decimals = c_decimals,
                      
                      hovertemplate = c_hovertemplate,
                      
                      textangle = c_textangle,

                      title_text = c_title_text,
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat
)

myplot %>% add_empty_trace(data_prep)

