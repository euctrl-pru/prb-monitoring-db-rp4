if (!data_loaded) {
  source("R/get_data.R")
}

# import data  ----
if (country == rp_full){
  ## SES case ----
  data_raw_kep  <-  kep_actual_ses %>% 
    mutate(value = kep_value_percent/100,
           state = rp_full)

  data_raw_scr  <-  scr_actual_ses %>% 
    mutate(value = scr_value,
           state = rp_full)
    
} else  {
  ## State case ----
  data_raw_kep <- kep_actual
  data_raw_scr <- scr_actual
}

# prepare data ----
## create sequence of years to ensure full series
rp_years_df <- data.frame(rp_years) %>% rename(xlabel = rp_years)

data_prep_kep <- data_raw_kep %>% 
  mutate(type = indicator_type,
         mymetric = round(value * 100,2),
         xlabel = year) %>% 
  filter(
    state == country,
    year <= year_report
  ) %>% 
  select(xlabel, type, mymetric)

data_prep_kep_full <- data_prep_kep %>% 
  right_join(rp_years_df, by = 'xlabel') 

data_prep_scr <- data_raw_scr %>% 
  mutate(type = indicator_type,
         mymetric = round(value*100,2),
         xlabel = year) %>% 
  filter(
    state == country,
    year <= year_report
  ) %>% 
  select(xlabel, type, mymetric)

data_prep_scr_full <- data_prep_scr %>% 
  right_join(rp_years_df, by = 'xlabel') 


data_prep <- data_prep_kep_full %>% 
  rbind(data_prep_scr_full)


# chart parameters ----
c_suffix <- "%"
c_decimals <- 2

### trace parameters
c_colors = c(PRBActualColor, PRBPlannedColor )

###set up order of traces
c_factor <- c("KEP", "SCR")

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- 'middle'

#### title
c_title_text <-  paste0("KEP & SCR")

#### yaxis
c_yaxis_title <- "KEP & SCR (%)"
c_yaxis_tickformat <- paste0(".",c_decimals, "f")

# plot chart ----
myplot <- mybarchart2(data_prep, 
                      height = myheight+30,
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

myplot %>% add_empty_trace(data_prep)
