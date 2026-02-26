if (!exists("saf_ansp_index")) {saf_ansp_index = 1}

# import data  ----
if (!data_loaded) {
  source("R/get_data.R")
}


# prepare data ----
data_prep_eosm <- saf_eosm %>% 
  filter(
    ms == country,
  ) %>% 
  select(
    ms, entity_name, year, eo_sm_score
  ) %>% 
  arrange(
    entity_name, year
  )

data_prep_maturity <- saf_maturity %>% 
  filter(
    ms == country
    ) %>% 
  select(
    - c(entity, reference_period, ansp_meeting_targets_yearly)
  ) %>% 
  pivot_longer(-c(ms, entity_name, year), names_to = "type", values_to = "score") %>%  
  separate_wider_delim(type, delim = "_", names = c("status", "type"),
                       too_many = "merge") %>% 
  mutate(type = str_replace_all(type, "_", " "),
         type = str_to_sentence(type)
  ) %>% 
  filter(status == "actual",
         year <= year_report) %>%
  mutate(
    score_text = case_when (
      score == 20 ~ 'A',
      score == 40 ~ 'B',
      score == 60 ~ 'C',
      score == 80 ~ 'D',
      .default = as.character(score)
     )
  )

# plot chart ----
myc <-  function(width = NA, height, fontsize = myfont, margin = 70, ansp_name = main_safety_ansp) {
    plot_ly(
      data = filter(data_prep_maturity, tolower(entity_name) == tolower(ansp_name)),
      width = width,
      height = height,
      x = ~ year,
      y = ~ score,
      yaxis = "y1",
      cliponaxis = FALSE,
      type = "bar",
      color = ~ factor(type, levels = c("Policy and objectives",
                                        "Risk management",
                                        "Assurance",
                                        "Promotion",
                                        "Culture")
                       ),
      colors = c('#0070C0', '#44546A', '#DAE3F3', '#00B0F0', '#002060'),
      text = ~ paste0(type, ': ', score_text),
      textfont = list(color = 'transparent'),
      hovertemplate = paste0('%{text}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      data = filter(data_prep_eosm, year<= year_report & tolower(entity_name) == tolower(ansp_name)),
      x = ~ year,
      y = ~ eo_sm_score,
      yaxis = "y2",
      type = 'scatter',
      mode = "markers",
      name = "EoSM score",
      marker = list (color = PRBActualColor,
                     symbol = "diamond",
                     size = 11),
      hovertemplate = paste0('EoSM score %{y}<extra></extra>'),
      # hovertemplate = paste('%{text}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      data = filter(data_prep_eosm, tolower(entity_name) == tolower(ansp_name)),
      x = ~ year,
      y = 60,
      yaxis = "y1",
      type = 'scatter',
      mode = "line",
      name = "Target other MOs",
      line = list (color = PRBTargetColor, width = 3, dash = 'solid'),
      hoverinfo = 'none',
      showlegend = F
    ) %>%
    add_trace(
      inherit = FALSE,
      data = filter(data_prep_eosm, tolower(entity_name) == tolower(ansp_name)),
      x = ~ year,
      y = 80,
      yaxis = "y1",
      type = 'scatter',
      mode = "line",
      name = "Target risk mgt",
      line = list (color = PRBTargetColor, width = 3, dash = 'solid'),
      # hovertemplate = paste0('%{x}'),
      hoverinfo = 'none',
      showlegend = F
    ) %>%
    add_trace (
               inherit = FALSE,
               # data = data_prep_eosm,
               x = rp_max_year+0.3,
               y = 60,
               yaxis = "y1",
               type = 'scatter',
               mode = "marker",
               name = "fake series",
               marker = list (color = 'transparent'),
               # hovertemplate = paste0('%{x}'),
               hoverinfo = 'none',
               showlegend = F
    ) %>%
    add_trace (
      inherit = FALSE,
      data = filter(data_prep_eosm, tolower(entity_name) == tolower(ansp_name)),
      x =  ~ year,
      y = 60,
      yaxis = "y1",
      type = 'scatter',
      mode = "marker",
      name = "",
      marker = list (color = 'transparent'),
      hovertemplate = paste0('-'),
      # hoverinfo = 'none',
      showlegend = F
    ) %>%
    add_annotations (text = c('Risk management target',
                              'Other MO targets'),
                     x = 0.97,
                     y = c(86, 66),      
                     showarrow = F,
                     xref = "paper",
                     yref = "y",
                     yanchor = "center",
                     xanchor = "right",
                     align = "right",
                     # textangle = -90,
                     font = list(color = PRBTargetColor, size = fontsize)
    ) %>% 
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text=paste0("EoSM - ", ansp_name ),
                   y = mytitle_y, 
                   x = mytitle_x, 
                   xanchor = mytitle_xanchor, 
                   yanchor =  mytitle_yanchor,
                   font = list(size = mytitle_font_size)
      ),
      dragmode = FALSE,
      bargap = 0.25,
      hovermode = "x unified",
      hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
      xaxis = list(title = "",
                   gridcolor = 'rgb(255,255,255)',
                   showgrid = FALSE,
                   showline = FALSE,
                   showticklabels = TRUE,
                   dtick = 1,
                   # range = list(2020, 2024),
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   tickfont = list(size = fontsize)
      ),
      yaxis = list(title = "Minimum maturity level",
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = TRUE,
                   showline = FALSE,
                   # dtick = 20,
                   # showticklabels = TRUE,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = TRUE,
                   tickvals = c(20, 40, 60, 80),
                   ticktext = c("A  ", "B  ", "C  ", "D  "),
                   zerolinecolor = 'rgb(240,240,240)',
                   titlefont = list(size = fontsize), 
                   # showticklabels = FALSE
                   tickfont = list(size = fontsize, color = 'black')
      ),
      yaxis2 = list(title = "EoSM score",
                   overlaying = "y",
                   side = "right",
                   showgrid = FALSE,
                   showline = FALSE,
                   ticksuffix = "",
                   tickformat = ",.0f",
                   dtick = 25,
                   range = list(0,113),
                   zeroline = TRUE,
                   zerolinecolor = 'rgb(255,255,255)',
                   titlefont = list(size = fontsize), tickfont = list(size = fontsize)
      ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h', 
        xanchor = "left",
        x = -0.05, 
        y =-0.1,
        font = list(size = myfont*0.95)
      ),
      margin = list(t = margin/2, r = margin)
  )
  
}

if (knitr::is_latex_output()) {
  myc(ansp_name = saf_ansps[[saf_ansp_index]], 
      height = 290,
      fontsize = 12.5)

  } else {
    chart_params <- as_tibble(saf_ansps) %>% 
      rename(ansp_name = value) %>% 
      mutate(height = 320)

    mycharts <- pmap(chart_params, myc)
}

