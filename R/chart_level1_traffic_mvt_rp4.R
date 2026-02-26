# import data  ----
if (!data_loaded) {
  source("R/get_data.R")
} 

data_raw <- statfor_mvt
data_raw_planned <- traffic_target

# prepare data ----
max_actual_year <- as.numeric(substrRight(forecast, 4))-1

data_prep <- data_raw %>% 
  filter(
    tz == statfor_zone
  ) %>% 
  mutate(rank = paste0(rank, ' forecast'))

data_prep_forecast <-  data_prep %>%
  filter(
    forecast_id == .env$forecast_id
  ) %>%
  mutate(mvts = case_when (
    year > max_actual_year ~ mvts,
    TRUE ~ NA
  )
  )

data_prep_actual <-  data_prep %>%
  filter(
    forecast_id == max(forecast_id),
    rank == 'Base forecast'
  ) %>%
  mutate(
    forecast_id = .env$forecast_id,
    rank = 'Actual',
    mvts = case_when (
      year <= year_report ~ mvts,
      TRUE ~ NA
    )
  )

data_prep_planned <- data_raw_planned %>% 
  filter(state == .env$country,
         year >= rp_min_year) %>% 
  select(state, year, x121_ecz_ifr_mvt, x121_ecz_name) %>%
  rename(mvts = x121_ecz_ifr_mvt) %>% 
  mutate(rank = 'Planned')

## Spain is always different
if (country == "Spain") {
  data_prep_planned <- data_prep_planned %>% filter (x121_ecz_name == "Spain")    
}

# chart ----
## set parameters for chart ----
c_colors <-  c('#1969B4','#044598', '#229FDD')

if (knitr::is_latex_output()) {
  c_title <- paste0("IFR movements - ", forecast, " -\n",
                    if_else(country == "Spain",
                            country, ecz_list$ecz_name[1]))
  c_title_y <- 0.95
  c_legend_x <- -0.1
  
} else {
  c_title <- paste0("IFR movements - ", forecast, " - ", 
                    if_else(country == "Spain", 
                            country, ecz_list$ecz_name[1]))
  c_title_y <- 0.99
  c_legend_x <- 0
}

## define chart function ----
myc <- function (width = mywidth, 
                 height = myheight,
                 fontsize = myfont,
                 linewidth = mylinewidth, 
                 margin = mymargin
                 )
{
  plot_ly(
    width = width,
    height = height,
    data = data_prep_forecast,
    x = ~ year,
    y = ~ round(mvts/1000,0),
    yaxis = "y1",
    cliponaxis = FALSE,
    yaxis = "y1",
    type = 'scatter',  mode = 'lines+markers',
    line = list(width = linewidth, dash = 'dash'),
    marker = list(size = linewidth * 3),
    color = ~ rank,
    colors = c_colors,
    opacity = 1,
    # hovertemplate = paste('Target: %{y:.2f}%<extra></extra>'),
    # hoverinfo = "none",
    showlegend = T
  ) %>% 
    add_trace(
      data = data_prep_planned,
      inherit = FALSE,
      x = ~ year,
      y = ~ round(mvts,0),
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
      y = ~ round(mvts/1000,0),
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',  mode = 'lines+markers',
      line = list(width = linewidth, dash = 'solid', color = PRBActualColor),
      marker = list(size = linewidth * 3, color = PRBActualColor),
      color = ~ rank,
      opacity = 1,
      # hovertemplate = paste('Target: %{y:.2f}%<extra></extra>'),
      # hoverinfo = "none",
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
                   tickfont = list(size = fontsize)
      ),
      yaxis = list(title = "IFR movements ('000)",
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   tickformat = ",",
                   tickprefix = " ",
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   zerolinecolor = 'rgb(240,240,240)',
                   titlefont = list(size = fontsize), 
                   tickfont = list(size = fontsize)
      ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h', 
        xanchor = "left",
        x = c_legend_x, 
        y =-0.1,
        font = list(size = fontsize)
      ),
      margin = mymargin
      
    )
}

## plot chart ----
p <- myc()

p