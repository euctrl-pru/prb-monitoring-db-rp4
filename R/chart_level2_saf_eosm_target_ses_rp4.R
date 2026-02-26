if (!data_loaded) {
  source("R/get_data.R")
}

# import data  ----
data_raw  <-  saf_eosm_ses

data_prep <- data_raw %>% 
  filter(year_report == .env$year_report) %>% 
  select(-year_report, -state) %>% 
  group_by(year, status) %>% 
  summarise(number_of_ansps = min(number_of_ans_ps, na.rm = TRUE)) %>%
  mutate(
    number_of_ansps = if_else(status == "Actual" & year > .env$year_report,
                              NA,
                              number_of_ansps),
    mytextpos = case_when(
      # status == 'Planned' ~ "bottom center",
      TRUE ~ 'right'
    ),
    dash = if_else(status == "Planned", "dot", "solid"),
    status = paste0(
      "No of ANSPs on or above targets (", tolower(status), ")"
    )
  ) %>% 
  arrange(status, year)


# chart ----
## set parameters for chart ----
mycolors <-  c(PRBPlannedColor, PRBActualColor)

## define chart function ----
myc <- function (mywidth, myheight, myfont, mylinewidth, mymargin) {
  plot_ly(
    width = mywidth,
    height = myheight,
    data = filter(data_prep, status == "No of ANSPs on or above targets (planned)"),
    x = ~ year,
    y = ~ number_of_ansps,
    yaxis = "y1",
    cliponaxis = FALSE,
    yaxis = "y1",
    type = 'scatter',  mode = 'line',
    line = list(width = mylinewidth, dash = ~ dash, color = mycolors[[2]]),
    marker = list(size = mylinewidth * 3, color = mycolors[[2]]),
    # color = ~ status,
    # colors = mycolors[[1]],
    # opacity = 1,
    name = ~ status,
    text = ~ number_of_ansps,
    textposition = ~ mytextpos,
    textfont = list(color = 'black', size = myfont),
    hovertemplate = paste0('%{xother} %{y:.0f}'),
    showlegend = T
  ) %>%
    add_trace(
      data = filter(data_prep, status == "No of ANSPs on or above targets (actual)"),
      x = ~ year,
      y = ~ number_of_ansps,
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',  mode = 'line',
      name = ~ status,
      line = list(width = mylinewidth, dash = ~ dash, color = mycolors[[1]]),
      marker = list(size = mylinewidth * 3, color = mycolors[[1]]),
      # color = ~ status,
      # colors = mycolors[[1]],
      # opacity = 1,
      text = ~ number_of_ansps,
      textposition = ~ mytextpos,
      textfont = list(color = 'black', size = myfont),
      hovertemplate = paste0('%{xother} %{y:.0f}'),
      showlegend = T
      
    ) %>% 
    config( responsive = TRUE,
                    displaylogo = FALSE,
                    displayModeBar = F
                    # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text = paste0(""),
                   y = mytitle_y, 
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
      yaxis = list(title = "Number of ANSPs achieving\nEoSM targets",
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   tickformat = ",",
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   zerolinecolor = myyaxis_zerolinecolor,
                   rangemode = "tozero",
                   titlefont = list(size = myfont), tickfont = list(size = myfont)
      ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h', 
        xanchor = "center",
        x = 0.5, 
        y =-0.1,
        traceorder = "reversed",
        font = list(size = myfont-1)
      ),
      margin = mymargin
      
      
    )
}

## plot chart ----
myc(mywidth, myheight+30, myfont, 3, mymargin)

