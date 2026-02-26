# import data  ----
if (!data_loaded) {
  source("R/get_data.R")
} 

data_raw <- statfor_tsu
data_raw_planned <- traffic_target
data_raw_rts <- ceff_t1_ert_ecz

# prepare data ----

## rts data
data_prep_rts_ses <- data_raw_rts |> 
  select(year, status, x5_4_total_su) %>% 
  group_by (year, status) %>% 
  summarise (x5_4_total_su = sum(x5_4_total_su, na.rm = TRUE), .groups = "drop" )
  
data_prep_rts <- data_raw_rts |> 
  filter(entity_code == ecz_list$ecz_id[1]) |> 
  select(year, status, x5_4_total_su)

if (country == "Spain") {
  data_canarias_rts <- data_raw_rts |> 
    filter(entity_code == ecz_list$ecz_id[2]) |> 
    select(year, status, x5_4_total_su) 
    
  data_prep_rts <- data_prep_rts |> 
    rbind(data_canarias_rts) |> 
    group_by(year, status) |> 
    summarise(x5_4_total_su = sum(x5_4_total_su, na.rm = TRUE)) 
} else if (country == rp_full) {
  
  data_prep_rts <- data_prep_rts_ses
} 
  
## statfor data
max_actual_year <- as.numeric(substrRight(forecast, 4))-1

data_spain <- data_raw %>% 
  filter(str_detect(tz_id, "Spain") & tz_id != "Spain") %>% 
  group_by(forecast_id, forecast_name, rank, year) %>% 
  summarise(tsu = sum(tsu), .groups = "drop") %>% 
  mutate(tz_id = "Spain") %>% 
  relocate(tz_id, .before = year)

data_prep <- rbind(data_raw, data_spain) %>% 
  filter(
    tz_id == statfor_zone, 
    year <= rp_max_year,
    year >= rp_min_year-1
  ) %>% 
  mutate(rank = paste0(rank, ' forecast'))


data_prep_forecast <-  data_prep %>%
  filter(
    forecast_id == .env$forecast_id
  ) %>%
  mutate(tsu = case_when (
    year > max_actual_year ~ tsu,
    TRUE ~ NA
    )
  )

## we take the actuals from the rts
data_prep_actual_rts <- data_prep_rts |>
  filter(status == "A") |>
  mutate(tsu = case_when(
    year > year_report ~ NA,
    .default = x5_4_total_su
    ),
    rank = 'Actual'
    ) |> 
  select(year, rank, tsu)

## but we need the base year from statfor
data_prep_actual_statfor <-  data_prep %>%
  filter(
    forecast_id == max(forecast_id),
    rank == 'Base forecast'
  ) %>%
  mutate(
    forecast_id = .env$forecast_id,
    rank = 'Actual'
    # , tsu = case_when (
    # year <= year_report ~ tsu,
    # TRUE ~ NA
    # )
  ) |> 
  filter(year == rp_min_year-1) |> 
  select(year, rank, tsu)

## merge actual tables
data_prep_actual <- data_prep_actual_rts |> rbind(data_prep_actual_statfor) |> 
  arrange(year)

data_prep_planned <- data_raw_planned %>% 
  filter(x121_ecz_name == country,
         # status == 'D',
         year >= rp_min_year) %>% 
  select(state, year, x121_ecz_su)  %>% 
  group_by(year) %>% summarise (tsu = sum(x121_ecz_su, na.rm=TRUE)) %>% 
  mutate(rank = 'Determined')

if (country == rp_full) {
  data_prep_planned <- data_raw_planned %>% 
    filter(status == 'D',
           year >= rp_min_year) %>% 
    select(state, year, x121_ecz_su)  %>% 
    group_by(year) %>% summarise (tsu = sum(x121_ecz_su, na.rm=TRUE)) %>% 
    mutate(rank = 'Determined')
}

# chart ----
## set parameters for chart ----
  c_colors <-  c('#1969B4','#044598', '#229FDD')
  
  if (knitr::is_latex_output()) {
    c_title <- paste0("En route service units - ", forecast, " -\n",
                      if_else(country == "Spain",
                              country, ecz_list$ecz_name[1]))
    c_title_y <- 0.95
    c_margin = list (t = 40, l = 0)
    c_legend_x <- -0.1
  } else {
    c_title <- paste0("En route service units - ", forecast, " - ", 
                      if_else(country == "Spain", 
                              country, ecz_list$ecz_name[1]))
    c_legend_x <- 0
  }

## define chart function ----
  myc <- function (width = mywidth, height = myheight, font = myfont, linewidth = mylinewidth, margin = mymargin) {
    plot_ly(
      width = width,
      height = height,
      data = data_prep_forecast,
    x = ~ year,
    y = ~ round(tsu/1000,0),
    yaxis = "y1",
    cliponaxis = FALSE,
    yaxis = "y1",
    type = 'scatter',  mode = 'lines+markers',
    line = list(width = linewidth, dash = 'dash'),
    marker = list(size = linewidth * 3),
    color = ~ rank,
    colors = c_colors,
    opacity = 1,
    showlegend = T
  ) %>% 
    add_trace(
      data = data_prep_planned,
      inherit = FALSE,
      x = ~ year,
      y = ~ round(tsu,0),
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',  mode = 'lines+markers',
      line = list(width = linewidth, dash = 'solid', color = PRBPlannedColor),
      marker = list(size = linewidth * 3, color = PRBPlannedColor),
      color = ~ rank,
      opacity = 1,
      showlegend = T
    ) %>%
    add_trace(
      data = data_prep_actual,
     inherit = FALSE,
     x = ~ year,
     y = ~ round(tsu/1000,0),
     yaxis = "y1",
     cliponaxis = FALSE,
     yaxis = "y1",
     type = 'scatter',  mode = 'markers',
     line = list(width = linewidth, dash = 'solid', color = PRBActualColor),
     marker = list(size = linewidth * 3, color = PRBActualColor),
     color = ~ rank,
     opacity = 1,
     showlegend = T
    ) %>%
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text = c_title,
                   y = c_title_y, 
                   x = mytitle_x, 
                   xanchor = mytitle_xanchor, 
                   yanchor = mytitle_yanchor,
                   font = list(size = mytitle_font_size)
                   ),
      dragmode = FALSE,
      hovermode = "x unified",
      hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
      xaxis = list(title = "",
                   gridcolor = 'rgb(255,255,255)',
                   showgrid = FALSE,
                   showline = FALSE,
                   showticklabels = TRUE,
                   dtick = 1,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   tickfont = list(size = myfont)
                   ),
      yaxis = list(title = "En route service units ('000)",
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   tickprefix = " ",
                   tickformat = ",",
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   zerolinecolor = 'rgb(240,240,240)',
                   titlefont = list(size = myfont), tickfont = list(size = myfont)
                   ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h', 
        xanchor = "left",
        x = mylegend_x, 
        y = -0.1,
        font = list(size = myfont)
        ),
      margin = margin
      
      
    )
  }
  
## plot chart ----
  myc(mywidth, myheight+20, myfont, mylinewidth, c_margin)
