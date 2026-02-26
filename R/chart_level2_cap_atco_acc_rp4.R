## import data  ----
if (!data_loaded) {
  source("R/get_data.R")
}

data_raw_planned  <- cap_atco_acc_planned
data_raw_actual  <- cap_atco_acc_actual

data_raw <- data_raw_planned %>% 
  left_join (data_raw_actual, by = c("state", "year", "acc"))

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country,
    year == .env$year_report) %>% 
  mutate(
    planned_atco_number = round(planned_atco_number,0),
    actual_atco_number = round(actual_atco_number,0)
    ) |> 
  select(acc, planned_atco_number, actual_atco_number) %>% 
  rename(xlabel = acc,
         Planned = planned_atco_number,
         Actual = actual_atco_number) %>% 
  pivot_longer(-xlabel, names_to = 'type', values_to = 'mymetric')

## chart parameters ----
c_suffix <- ""
c_decimals <- 0

### trace parameters
c_colors = c(PRBPlannedColor, PRBActualColor)

###set up order of traces
c_factor <- c('Planned', 'Actual') 
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textposition <- "outside"
c_insidetextanchor <- NULL

### layout parameters
c_barmode <- 'group'

#### title
c_title_text <- paste0("ATCOs in operation per ACC - ", year_report)

#### yaxis
c_yaxis_title <- "ATCOs in OPS (FTEs)"
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
)

myplot %>% 
  layout(bargroupgap = 0.15)
