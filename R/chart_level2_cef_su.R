
# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "enroute")}
# ez=1

# define cz ----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]
# cztype <- "terminal"
mycz <- if_else(cztype == "terminal",
                tcz_list$tcz_id[ez],
                ecz_list$ecz_id[ez])
mycz_name <- if_else(cztype == "terminal",
                     tcz_list$tcz_name[ez],
                     ecz_list$ecz_name[ez])

# import data  ----
if (country == rp_full) {
  ## SES  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES CEFF.xlsx"),
    sheet = if_else(cztype == "terminal", "SES_TRM_all", "SES_ERT_all"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() |> 
    # so the field name is the same as for state
    mutate(x5_4_total_su = su_cz)


} else {
  ## State  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    sheet = if_else(cztype == "terminal", "Terminal_T1", "Enroute_T1"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
}

# prepare data ----
data_prep_split <- data_raw %>% 
  filter(
    year != 20202021,
    entity_code == mycz) %>% 
  mutate(
    mymetric = case_when (
      status == 'A' & year > max(.env$year_report, 2021) ~ NA,
      .default = x5_4_total_su
    ),
    year_text = as.character(year)
  ) %>%  
  select(
    year,
    status,
    mymetric,
    year_text
  ) 

data_prep2020_2021 <- data_prep_split %>% 
  filter(
    year < 2022) %>% 
  group_by(status) |> 
  summarise(mymetric = sum(mymetric, na.rm = TRUE)) |> 
  mutate(year_text = "2020-2021")

data_prep <- data_prep_split |> 
  filter(year > 2021) |> 
  select(-year) |>
  rbind(data_prep2020_2021) |> 
  mutate(mymetric = round(mymetric/1000, 0),
         status = str_replace(status, "A", "Actual SUs"),
         status = str_replace(status, "D", "Planned SUs")
  ) |>  
  arrange(year_text)


## replace 0 by NAs so they are not plotted
data_prep[data_prep == 0] <- NA

## add a column to determine the minimum y axis value
data_prep <- data_prep %>% mutate(min_y_axis = min(mymetric, na.rm=T)/1.5)

## create dfs for series 
data_prep_planned <- data_prep %>% 
  filter(status == "Planned SUs") 

data_prep_actual <- data_prep %>% 
  filter(status == "Actual SUs")

# define chart function ----
myc <- function (mywidth, myheight, myfont, mylinewidth, mymargin) {
  plot_ly( 
    width = mywidth,
    height = myheight
  ) %>% 
    add_trace(
      data = data_prep_planned,
      x = ~ year_text,
      y = ~ mymetric,
      text = ~ format(round(mymetric*.02, 0), nsmall = 0,  big.mark = ','),
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      error_y = list(type = "data", array = ~mymetric*0.02, color= 'grey'),
      opacity = 1,
      hoverinfo = 'none',
      # hovertemplate = paste('±2%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      data = data_prep_planned,
      x = ~ year_text,
      y = ~ mymetric,
      text = ~ format(round(mymetric*.1, 0), nsmall = 0,  big.mark = ','),
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      error_y = list(type = "data", array = ~mymetric*0.1, color= 'black'),
      opacity = 1,
      hoverinfo = 'none',
      # hovertemplate = paste('±10%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      data = data_prep_planned,
      x = ~ year_text,
      y = ~ mymetric,
      name = ~status,
      text = ~status, 
      yaxis = "y1",
      type = 'scatter',  mode = 'lines+markers',
      line = list(width = mylinewidth, dash = 'solid', color = '#5B9BD5'),
      marker = list(size = mylinewidth * 3, color = '#5B9BD5'),
      # error_y = list(type = "data", array = ~mymetric*0.1, color= 'black'),
      opacity = 1,
      hovertemplate = paste('%{text}: %{y:,.0f}<extra></extra>'),
      showlegend = T
      # hoverinfo = 'none'
    ) %>%
    add_trace(                                  #we add them again for the tooltip order
      data = data_prep_planned,
      x = ~ year_text,
      y = ~ mymetric,
      text = ~ format(round(mymetric*.02, 0), nsmall = 0,  big.mark = ','),
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      opacity = 1,
      # hoverinfo = 'none',
      hovertemplate = paste('±2%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      data = data_prep_planned,
      x = ~ year_text,
      y = ~ mymetric,
      text = ~ format(round(mymetric*.1, 0), nsmall = 0,  big.mark = ','),
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      opacity = 1,
      # hoverinfo = 'none',
      hovertemplate = paste('±10%: ±%{text}<extra></extra>'),
      showlegend = F
    ) %>%
    add_trace(
      inherit = FALSE,
      data = data_prep_actual,
      x = ~ year_text,
      y = ~ mymetric,
      name = ~status,
      text = ~status, 
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',  mode = 'lines+markers',
      line = list(width = mylinewidth, dash = 'solid', color = '#FFC000'),
      marker = list(size = mylinewidth * 3, color = '#FFC000'),
      opacity = 1,
      hovertemplate = paste('%{text}: %{y:,.0f}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>% 
    add_trace(      ## to push the y axis down
      x = ~ year_text,
      y = ~ min_y_axis,
      yaxis = "y1",
      cliponaxis = FALSE,
      yaxis = "y1",
      type = 'scatter',  mode = 'lines',
      line = list(width = 0, dash = 'solid', color = 'transparent'),
      opacity = 1,
      hoverinfo = 'none',
      showlegend = F
    ) %>% 
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text = mychart_title,
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
      yaxis = list(title = myaxis_title,
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
                   titlefont = list(size = myfont), tickfont = list(size = myfont)
      ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h', 
        xanchor = "center",
        x = 0.5, 
        y =-0.1,
        font = list(size = myfont)
      ),
      margin = mymargin
    )
}

# set parameters for chart ----
myaxis_title <- paste0(if_else(cztype == "terminal", "Terminal", "En route"),
                       " service units ('000)")
mychart_title <- paste0(if_else(cztype == "terminal", "Terminal", "En route"),
                        " service units")

mylocalmargin = list (t = 40, b = 80)

mylegend_y_position <- -0.37

# plot chart ----
myc(mywidth, myheight+20, myfont, mylinewidth, mylocalmargin) %>% 
  layout(
    annotations = list( 
    list (
      xanchor = "right",
      x = 0.49,
      y = mylegend_y_position,
      text = '<span style="color:grey;font-family:Arial"><b>Ɪ</b></span>  ±2% dead-band',
      font = list(size = myfont),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      # arrowhead = 7,
      ax = 0,
      ay = 0
    ),
    list (
      xanchor = "left",
      x = 0.51,
      y = mylegend_y_position,
      text = '<span style="color:black;font-family:Arial"><b>Ɪ</b></span>  ±10% threshold',
      font = list(size = myfont),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      # arrowhead = 7,
      ax = 0,
      ay = 0
    )
    )
  )
    
