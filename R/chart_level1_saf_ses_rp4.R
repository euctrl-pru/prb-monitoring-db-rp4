if (!data_loaded) {
  source("R/get_data.R")
} 
# import data  ----
data_raw  <-  saf_eosm_ses %>% 
  mutate(management_objectives = str_replace_all(management_objectives, 'Other Mos' , 'Other MOs'))


data_prep <- data_raw %>% 
  filter(year_report == .env$year_report) %>% 
  select(-year_report) %>% 
  mutate(
    management_objectives = paste(management_objectives, str_to_lower(status)),
    mytextpos = case_when(
      management_objectives == 'Other MOs planned' ~ "top center",
      TRUE ~ 'bottom center'
    ),
  ) 

data_prep_actual <- data_prep %>% 
  filter(status == 'Actual') %>% 
  mutate(number_of_ans_ps = case_when(
    year > year_report ~ NA,
    .default = number_of_ans_ps
  ))

data_prep_planned <- data_prep %>% 
  filter(status == 'Planned')

# chart ----
## set parameters for chart ----
mycolors <-  c(PRBActualColor, PRBActualColor,PRBPlannedColor, PRBPlannedColor)

## define chart function ----
myc <- function (mywidth, myheight, myfont, mylinewidth, mymargin) {
  plot_ly(
    width = mywidth,
    height = myheight,
    data = data_prep_planned,
    x = ~ year,
    y = ~ number_of_ans_ps,
    yaxis = "y1",
    cliponaxis = FALSE,
    yaxis = "y1",
    type = 'scatter',  mode = 'lines+markers',
    line = list(width = mylinewidth, dash = 'dot'),
    marker = list(size = mylinewidth * 3),
    color = ~ management_objectives,
    colors = mycolors,
    opacity = 1,
    text = ~ number_of_ans_ps,
    textposition = ~ mytextpos,
    textfont = list(color = 'black', size = myfont),
    hovertemplate = paste0('%{xother} %{y:.0f}'),
    showlegend = T
  ) %>% 
    add_trace(
      data = data_prep_actual,
      x = ~ year,
      y = ~ number_of_ans_ps,
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = mylinewidth, dash = 'solid'),
      color = ~ management_objectives,
      colors = mycolors,
      opacity = 1,
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
      yaxis = list(title = "No of ANSPs achieving\nEoSM targets",
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
        xanchor = "left",
        x = if_else(knitr::is_latex_output(),-0.15,-0.05), 
        y =-0.1,
        font = list(size = if_else(knitr::is_latex_output(),myfont-4,myfont-1.5))
      ),
      margin = mymargin
      
      
    )
}

## plot chart ----
myc(mywidth, 
    if_else(knitr::is_latex_output(),myheight+10,myheight+30), 
    myfont, 
    mylinewidth, 
    mymargin)

