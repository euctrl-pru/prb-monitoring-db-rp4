############### state level adapted to RP4, not NM

if (!exists("country") | is.na(country)) {country = rp_full}
if (!data_loaded) {
  source("R/get_data.R")
}

if (country == "Network Manager") {
  # NM case ----
  ## import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "NM_data.xlsx"),
    sheet = "Environment",
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## prepare data ----
  data_for_chart <- data_raw %>% 
    filter(year_report == .env$year_report) %>% 
    mutate(
      target = round(nm_target * 100, 2),
      actual = round(actual * 100, 2)
    ) %>% 
    select(year, target, actual)
  
} else if (country == rp_full) {
  # SES case ----
  ## import data  ----
  data_raw_target  <-  kea_target_ses %>% 
    mutate(kea_target = kea_reference_value_percent/100,
           state = rp_full)
  
  data_raw_actual  <-  kea_actual_mm_ses %>% 
    mutate(value = hfe_kpi_percent/100,
           state = rp_full)

} else  {
  # State case ----
  ## import data  ----
  data_raw_target <- kea_target
  data_raw_actual <- kea_actual_mm

}

## prepare data ----
target_value <- data_raw_target %>%
  filter(
    state == .env$country,
    year == .env$year_report
  ) %>%
  mutate(
    # type = 'Target',
    target = round(kea_target *100, 2)
  ) %>%
  select(
    target
  ) %>% pull()

data_prep_actual <- data_raw_actual %>% 
  filter(
    state == country,
    lubridate::year(month) == year_report) %>% 
  mutate (mymetric = round(value *100, 2),
          xlabel = lubridate::floor_date(month, unit = 'month' ),
          type = "Actual") %>% 
  select(
    xlabel,
    mymetric,
    type
  ) 

## prepare datasetfor chart
data_prep_target <- data_prep_actual %>% 
  mutate(myothermetric = target_value,
         type = "Target") %>% 
  select(
    xlabel,
    myothermetric,
    type
  )


## chart parameters ----
c_suffix <- "%"
c_decimals <- 2

c_colors <- PRBActualColor
c_factor <- c("Actual")

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

#### title
c_title_text <- paste0(if_else(country == "Network Manager", "KEP (monthly)", "KEA (monthly)"))

#### yaxis
c_yaxis_title <- paste0(if_else(country == "Network Manager", "KEP", "KEA"), " (%)")
c_yaxis_tickformat <- paste0(".",c_decimals, "f")

#### xaxis
c_xaxis_dtick <- 'M1'
c_xaxis_tickformat<- "%b"

if (knitr::is_latex_output()) {
  c_legend_y <- -0.18
  c_xaxis_tickangle <- -90
  
} else {
  c_legend_y <- mylegend_y
  c_xaxis_tickangle <- 0
  
}

#### text
c_textangle <- -90
c_textposition <- "inside"
c_insidetextanchor <- 'middle'

# plot chart ----
myplot <- mybarchart2(data_prep_actual, 
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

                      xaxis_dtick = c_xaxis_dtick,
                      xaxis_tickformat = c_xaxis_tickformat,
                      xaxis_tickangle = c_xaxis_tickangle,
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat
)

myplot %>% 
  add_line_trace2(., data_prep_target,
                  textfontcolor = "rgba(0,0,0,0)",
                  markercolor = "rgba(0,0,0,0)",
  ) 

