## import data  ----
if (!data_loaded) {
  source("R/get_data.R")
}

data_raw <- env_mil_actual

## prepare data ----
data_prep <- data_raw %>% 
  filter(
    state == .env$country,
    year <= .env$year_report) %>% 
  select(
    xlabel = year,
    "RAI" = rai_cdr,
    "RAU" = rau_cdr
  ) |> 
  pivot_longer(-xlabel, names_to = 'type', values_to = 'mymetric') |> 
  mutate(mymetric = round(mymetric, 0))

## chart parameters ----
c_suffix <- "%"
c_decimals <- 0

### trace parameters
c_colors = c(PRBActualColor, PRBPlannedColor)

###set up order of traces
c_factor <- c("RAI", "RAU") 

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- 0
c_textposition <- "outside"
c_insidetextanchor <- NA

#### title
c_title_text <- paste0("RAI & RAU via available conditional routes (PIs#7 & 8)")

#### yaxis
c_yaxis_title <- "RAI & RAU (%)"
c_yaxis_tickformat <- paste0(".",0, "f")

#### margin
C_margin <- list(t = 60)

## plot chart  ----
myplot <- mybarchart2(data_prep, 
                      height = myheight + 30,
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
                      yaxis_tickformat = c_yaxis_tickformat,
                      
                      margin = C_margin
) %>%  
    layout(yaxis = list(range= c(0,100)),
           bargroupgap = 0.15)


if (all(is.na(data_prep$mymetric)) == TRUE) {
  myplot <- myplot |> 
    layout(xaxis = list(range= c(rp_min_year-0.5, rp_max_year + 0.5)))
  }

myplot
