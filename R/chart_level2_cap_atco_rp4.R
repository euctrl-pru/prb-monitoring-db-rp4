if (!data_loaded) {
  source("R/get_data.R")
}

## import data  ----
if (country == rp_full) {
  data_raw  <-  cap_atco_acc_ses |> 
    #so it has the same columns as the State case
    mutate(ansp = "ansp",
           state = rp_full,
           acc = "ZZZZ")
    
} else {
  data_raw_planned  <- cap_atco_acc_planned
  data_raw_actual  <- cap_atco_acc_actual
  
  data_raw <- data_raw_planned %>% 
    left_join (data_raw_actual, by = c("state", "year", "acc"))
  
}

## prepare data ----

data_prep_acc <- data_raw |> 
  filter(state == .env$country) |> 
  # left_join(acc_list_table, by = c("acc" = "acc_id")) |> 
  select(year,
         Planned = planned_atco_number,
         Actual = actual_atco_number,
         acc) |> 
  pivot_longer(c(-year, -acc), values_to = "value", names_to = "type") |> 
  arrange(acc, type, year)


data_prep_ansp <- data_prep_acc %>% group_by(type, year) %>% 
  summarise(value = sum(value)) %>% 
  mutate(acc = if_else(country == rp_full, rp_full, main_ansp)) %>% ungroup() 


data_for_chart <- rbind(data_prep_ansp, data_prep_acc) %>% 
  group_by(acc) %>% 
  mutate(min_y_axis = min(value, na.rm=T)/1.5,
         value = case_when (
           year > year_report & type == "Actual" ~ NA,
           .default = round(value, 0)
           )
         ) 

acc_list_for_chart <- unique(data_for_chart$acc)


# chart ----
## set parameters for chart ----
c_colors = c(PRBActualColor, PRBPlannedColor)

c_title <- paste0("ATCOs in operation",
                  if_else(country != rp_full,
                          paste0(" - ", main_ansp), ""))


## define chart function ----
myc <- function (width = mywidth, height = myheight, font = myfont, linewidth = mylinewidth, margin = mymargin) {
  myplot <- plot_ly( 
    width = width,
    height = height,
  )
  for (i in 1:length(acc_list_for_chart)) {
    df <- data_for_chart %>% filter(acc == acc_list_for_chart[i])
    myplot <- myplot %>% 
      add_trace(
        data = df,
        x = ~ year,
        y = ~ value,
        yaxis = "y1",
        cliponaxis = FALSE,
        yaxis = "y1",
        type = 'scatter',  mode = 'lines+markers',
        line = list(width = linewidth, dash = 'solid'),
        marker = list(size = linewidth * 3),
        color = ~ type,
        colors = c_colors,
        opacity = 1,
        visible = ifelse(i == 1, TRUE, FALSE),  # Set the initial visibility
        # visible = c(rep(TRUE, 2), rep(FALSE, length(acc_list_for_chart)*2 - 2)),  # Set the initial visibility
        # hovertemplate = paste('Target: %{y:.2f}%<extra></extra>'),
        # hoverinfo = "none",
        showlegend = T
      ) %>% 
      add_trace(
        x = ~ year,
        y = ~ min_y_axis,
        yaxis = "y1",
        cliponaxis = FALSE,
        yaxis = "y1",
        type = 'scatter',  mode = 'lines',
        line = list(width = 0, dash = 'solid', color = 'transparent'),
        opacity = 1,
        visible = ifelse(i == 1, TRUE, FALSE),  # Set the initial visibility
        showlegend = F,
        hoverinfo = 'none'
      )
  }
  
  # Create updatemenu buttons for selecting accs
  updatemenus = list(list(
    xref = 'paper',
    xanchor = 'left',
    yanchor = "top",
    x = -0.2,
    y = 1.2,
    font = list(family = "Arial", color="black", size=font),
    pad = 0,
    bgcolor = 'white', 
    bordercolor = '#e0e0e0', 
    active = 0,
    buttons = lapply(1:length(acc_list_for_chart), function(i) {
      list(
        label = acc_list_for_chart[i],
        method = "update",
        args = list(
          list(visible = replace(
            rep(FALSE, length(acc_list_for_chart) * 3), seq((i - 1) * 3 + 1, i * 3, by = 1), TRUE
          ))
          # list(title = list(
          #   text = paste0("<b>EUROCONTROL 7-year forecast for ", tz_values[i], " 2024-2030 </b> <br><span style='font-size:0.8em;color:grey'>Actual and future IFR movements</span>")
          # ))
        )
      )
    })
  ))
  
  
  myplot <- myplot %>% 
    config( responsive = TRUE,
                    displaylogo = FALSE,
                    displayModeBar = F
    ) %>% 
    layout(
      # updatemenus = updatemenus,  # in the end we don't use the menu
      font = list(family = "Roboto"),
      title = list(text = c_title,
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
                   tickfont = list(size = font)
      ),
      yaxis = list(title = "ATCOs in OPS (FTEs)",
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   tickformat = ",",
                   tickprefix = " ",
                   # rangemode = "tozero",
                   # autorange = 'max',
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   zerolinecolor = 'rgb(240,240,240)',
                   titlefont = list(size = font), tickfont = list(size = font)
      ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h', 
        xanchor = "center",
        x = 0.5, 
        y =-0.1,
        font = list(size = font)
      ),
      margin = margin
    )
  myplot
}

## plot chart ----
myc()


