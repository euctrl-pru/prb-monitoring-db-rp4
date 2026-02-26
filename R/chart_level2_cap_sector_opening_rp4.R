if (!data_loaded) {
  source("R/get_data.R")
}

## import data  ----
if (country == rp_full) {
  data_raw  <-  cap_sector_hours_actual_ses |> 
    #so it has the same columns as the state case
    mutate(ansp = "ansp",
           state = rp_full)
  
} else {
  data_raw  <- cap_sector_hours_actual

}

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country,
    year <= .env$year_report) %>%
  mutate(
    xlabel = year,
    mymetric = round(total_soh,0),
    type = "Sector opening hours"
    ) %>% 
 select(
    xlabel,
    type,
    mymetric)

## chart parameters ----
c_suffix <- ""
c_decimals <- 0

### trace parameters
c_colors = c('#8497B0')

###set up order of traces
c_factor <- data_prep$type %>% unique() 
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- 'middle'
c_textfont_color <- 'white'

### layout parameters
c_barmode <- 'stack'

#### title
c_title_text <- paste0("Sector opening hours",
                       if_else(country != rp_full,
                               paste0(" - ", main_ansp), "")
                       )
#### yaxis
c_yaxis_title <- "Sector opening hours"
c_yaxis_tickformat <- ",.0f"


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
                      textangle = c_textangle,
                      textfont_color = c_textfont_color,
                      insidetextanchor = c_insidetextanchor,
                      
                      title_text = c_title_text,
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat
)

myplot %>% 
  layout(xaxis = list(range= c(rp_min_year-0.5,rp_max_year+0.5), tickformat = '.0f'))
