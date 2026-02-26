# import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "SES CEFF.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "SES_DUC",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

# prepare data ----
data_prep_all <- data_raw %>% 
  filter(
    year_report == .env$year_report) %>% 
  mutate(
    unit_cost_er = round(duc_value, 3)
  ) %>% 
  select(
    year,
    status,
    unit_cost_er
  ) %>% 
  mutate(xlabel = as.character(year),
         type = str_replace(status, "Actual", "Actual unit cost"),
         type = str_replace(type, "Determined", "Determined unit cost")
  ) %>% 
  arrange(xlabel)

data_prep <- data_prep_all %>% 
  filter(status == "Actual" | status == "Determined") %>% 
  mutate(mymetric = case_when(
    as.numeric(str_replace(year,"-", "")) > year_report & year != "2020-2021" & status == "Actual" ~ NA ,
    .default = round(unit_cost_er,2))
    )

data_actual_trend <- data_prep_all %>% 
  select(xlabel, type, unit_cost_er) %>% 
  filter(type %like% "Actual unit cost") %>% 
  mutate(unit_cost_er = case_when(
    as.numeric(str_replace(xlabel,"-", "")) > year_report & xlabel != "2020-2021" ~ NA,
    .default = unit_cost_er
  )) %>% 
  pivot_wider(names_from = 'type', values_from = 'unit_cost_er' ) %>% 
  clean_names()%>% 
  mutate(type = 'Actual')

data_target_trend <- data_prep_all %>%
  select(xlabel, type, unit_cost_er) %>% 
  filter(type %like% "Target")  %>% 
  pivot_wider(names_from = 'type', values_from = 'unit_cost_er' ) %>% 
  clean_names() %>% 
  mutate(type = 'Target')

# chart parameters ----
mysuffix <- ""
mydecimals <- 2

### trace parameters
mycolors = c('#5B9BD5', '#FFC000')
###set up order of traces
myfactor <- data_prep %>% select(type) %>% unique() 
invisible(as.list(myfactor$type))
myfactor <- sort(myfactor$type, decreasing = TRUE)
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- "middle"
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("En route unit costs - ", rp_full)
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "En route unit costs (€2017)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".0f"

#### legend
if (knitr::is_latex_output()) {
  mylegend_x <- -0.1
  mylegend_xanchor <- 'left'
  
} else {
  mylegend_x <- 0.5
  mylegend_xanchor <- 'center'
  
}


#### margin
mylocalmargin = mymargin

# plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals) %>% 
add_trace(
  data = data_target_trend,
  x = ~ xlabel,
  y = ~ target,
  yaxis = 'y1',
  mode = 'line+markers', 
  type = 'scatter',
  name = "Target trend",
  text = ~ paste0("<b>", format(target_trend*100,  big.mark  = ",", nsmall = 1), 
                  "%",
                  "</b>"),
  textangle = 0,
  textposition = 'top',
  textfont = list(color = '#FF0000', size = myfont),
  line = list(color = '#FF0000', width = mylinewidth),
  marker = list(size = mylinewidth * 3, 
                color = '#FF0000',
                symbol = NA),
  hovertemplate =  paste0('%{y:,.1f}%'),
  showlegend = T
) %>% 
  add_trace(
    data = data_actual_trend,
    x = ~ xlabel,
    y = ~ actual_unit_cost,
    yaxis = 'y1',
    mode = 'line+markers', 
    type = 'scatter',
    name = "Actual trend",
    text = ~ paste0("<b>", format(actual_unit_cost_trend*100,  big.mark  = ",", nsmall = 1), 
                    "%",
                    "</b>"),
    textangle = 0,
    textposition = 'bottom',
    textfont = list(color = 'black', size = myfont),
    line = list(color = '#ED7D31', width = mylinewidth),
    marker = list(size = mylinewidth * 3, 
                  color = '#ED7D31',
                  symbol = NA),
    hovertemplate =  paste0('%{y:,.1f}%'),
    showlegend = T
  ) 
