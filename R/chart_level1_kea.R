############### state level adapted to RP4, also SES , not NM

if (!exists("doclevel")) {doclevel = "level1"}
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
  data_prep <- data_raw %>% 
    filter(year_report == .env$year_report) %>% 
    mutate(
      xlabel = year
      # target = round(nm_target * 100, 2),
      # actual = round(actual * 100, 2)
    ) 
  
  data_prep_actual <- data_prep %>% 
    mutate(mymetric = case_when(
           year > year_report ~ NA,
           .default = round(actual * 100, 2)),
           type = "Actual"
           ) %>% 
    select(xlabel, mymetric, type)
  
  data_prep_target <- data_prep %>% 
    mutate(myothermetric = round(nm_target * 100, 2),
           type = "Target")
  
} else if (country == rp_full) {
  # SES case ----
  data_raw_target  <-  kea_target_ses %>% 
    mutate(kea_target = kea_reference_value_percent/100,
           state = rp_full)
  
  data_raw_actual  <-  kea_actual_ses %>% 
    mutate(value = hfe_kpi_percent/100,
           state = rp_full)
  
} else  {
  # State case ----
  data_raw_target <- kea_target
  data_raw_actual <- kea_actual
}
  
## prepare data ----
data_prep_target <- data_raw_target %>% 
  filter(
    state == .env$country
  ) %>% 
  mutate(
    xlabel = year,
    myothermetric = round(kea_target *100, 2),
    type = "Target"
  ) %>% 
  select(
    xlabel,
    myothermetric,
    type
  )

data_prep_actual <- data_raw_actual %>% 
  filter(
    state == country,
    year <= year_report) %>% 
  mutate (actual = value) %>% 
  select(
    year,
    actual
  ) %>% 
  mutate(
    xlabel = year,
    mymetric = round(actual * 100,2),
    type = "Actual"
  ) %>% 
  select (
    xlabel,
    mymetric,
    type
  )

## chart parameters ----
c_suffix <- "%"
c_decimals <- 2

c_colors <- PRBActualColor
c_factor <- c("Actual")

### hover
c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

#### title
if (knitr::is_latex_output()) {
  c_level1_title <- "Average horizontal flight efficiency\nof the actual trajectory (KEA)"
  c_title_y <- 0.95
  c_margin <- list(t = 50)
  
} else {
  c_level1_title <- "Average horizontal flight efficiency of the actual trajectory (KEA)"
  c_title_y <- 0.99
  c_margin <- mymargin
  
}

c_title_text <- paste0(if_else(country == "Network Manager", 
                               "KEP", 
                               if_else(doclevel == "level1",
                                       c_level1_title,
                                       "KEA")))

#### yaxis
c_yaxis_title <- paste0(if_else(country == "Network Manager", "KEP", "KEA"), " (%)")
c_yaxis_tickformat <- paste0(".",c_decimals, "f")

#### text
c_textangle <- mytextangle
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
                      
                      yaxis_title = c_yaxis_title,
                      yaxis_ticksuffix = c_suffix,
                      yaxis_tickformat = c_yaxis_tickformat,
                      
                      legend_x = 0.5,
                      legend_xanchor = 'center'
)


myplot %>% 
  add_line_trace2(., data_prep_target,
                  textdecimals = c_decimals,
                  textsuffix = c_suffix
  )
