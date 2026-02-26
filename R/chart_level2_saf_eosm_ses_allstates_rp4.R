if (!data_loaded) {
  source("R/get_data.R")
}

# import data  ----
data_raw_maturity  <-  saf_maturity
data_raw_eosm  <-  saf_eosm

# prepare data ----

data_prep_eosm <- data_raw_eosm %>% 
  select(
    ms, entity_name, year, eo_sm_score
  ) 

data_prep_maturity <- data_raw_maturity %>% 
  select(
    - c(ms, entity, reference_period, ansp_meeting_targets_yearly)
  ) %>% 
  pivot_longer(-c(entity_name, year), names_to = "type", values_to = "score") %>%  
  separate_wider_delim(type, delim = "_", names = c("status", "type"),
                       too_many = "merge") %>% 
  mutate(type = str_replace_all(type, "_", " "),
         type = str_to_sentence(type)
  ) %>% 
  filter(status == "actual",
         year == year_report) %>% 
  mutate(group = if_else(type == "Risk management", "Safety risk management", "All other components")) %>% 
  group_by(entity_name, group) %>% 
  summarise(score = min(score, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    score_text = case_when (
      score == 80 ~ 'D',
      score == 60 ~ 'C',
      score == 40 ~ 'B',
      score == 20 ~ 'A',
      .default = as.character(score)
     )
  )
  

# plot chart ----
myc <-  function(local_width, local_height, local_font, local_margin) {
    plot_ly(
      data = data_prep_maturity,
      width = local_width,
      height = local_height,
      x = ~ entity_name,
      y = ~ score,
      yaxis = "y1",
      cliponaxis = FALSE,
      type = "bar",
      color = ~ factor(group, levels = c("Safety risk management",
                                         "All other components")
      ),
      colors = c('#FFC000', '#196AB4'),
      text = ~ paste0(group, ': ', score_text),
      textfont = list(color = 'transparent'),
      hovertemplate = paste0('%{text}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      data = filter(data_prep_eosm, year== year_report),
      x = ~ entity_name,
      y = ~ eo_sm_score,
      yaxis = "y2",
      type = 'scatter',
      mode = "markers",
      name = "EoSM score",
      marker = list (color = '#A5A5A5',
                     symbol = "circle",
                     size = if_else(knitr::is_latex_output(),5,7)),
      hovertemplate = paste0('EoSM score %{y}<extra></extra>'),
      # hovertemplate = paste('%{text}<extra></extra>'),
      # hoverinfo = "none",
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      data = filter(data_prep_eosm, year== year_report),
      x = ~ entity_name,
      y = 60,
      yaxis = "y1",
      type = 'scatter',
      mode = "line",
      name = "2024 Target all other components",
      line = list (color = '#FF0000', 
                   width = if_else(knitr::is_latex_output(), 1, 2), 
                   dash = 'dash'
      ),
      hoverinfo = 'none',
      showlegend = T
    ) %>%
    add_trace(
      inherit = FALSE,
      data = filter(data_prep_eosm, year== year_report),
      x = ~ entity_name,
      y = 80,
      yaxis = "y1",
      type = 'scatter',
      mode = "line",
      name = "2024 Target safety risk management",
      line = list (color = '#FF0000', 
                   width = if_else(knitr::is_latex_output(), 1, 2), 
                   dash = 'solid'
                   ),
      # hovertemplate = paste0('%{x}'),
      hoverinfo = 'none',
      showlegend = T
    ) %>%
    # add_trace (
    #            inherit = FALSE,
    #            # data = data_prep_eosm,
    #            x = 2024.3,
    #            y = 60,
    #            yaxis = "y1",
    #            type = 'scatter',
    #            mode = "marker",
    #            name = "fake series",
    #            marker = list (color = 'transparent'),
    #            # hovertemplate = paste0('%{x}'),
    #            hoverinfo = 'none',
    #            showlegend = F
    # ) %>%
    # add_trace (
    #   inherit = FALSE,
    #   data = data_prep_eosm,
    #   x =  ~ year,
    #   y = 60,
    #   yaxis = "y1",
    #   type = 'scatter',
    #   mode = "marker",
    #   name = "",
    #   marker = list (color = 'transparent'),
    #   hovertemplate = paste0('-'),
    #   # hoverinfo = 'none',
    #   showlegend = F
    # ) %>%
    # add_annotations (text = c('2024 Risk\nmanagement target'),
    #                  x = c(0.02),
    #                  y = c(80),
    #                  showarrow = T,
    #                  ax = c(-100),
    #                  ay = c(-45),
    #                  xref = "paper",
    #                  yref = "y",
    #                  yanchor = "center",
    #                  xanchor = "left",
    #                  align = "left",
    #                  # textangle = -90,
    #                  font = list(color = '#FF0000', size = local_font-1),
    #                  arrowhead = 2,
    #                  arrowcolor = '#FF0000',
    #                  arrowsize = 3,
    #                  arrowwidth = 0.5 
    # ) %>%
    # add_annotations (text = c('2024 Other\ncomponents target'),
    #                  x = c(0.02),
    #                  y = c(60),
    #                  showarrow = T,
    #                  ax = c(-100),
    #                  ay = c(45),
    #                  xref = "paper",
    #                  yref = "y",
    #                  yanchor = "center",
    #                  xanchor = "left",
    #                  align = "left",
    #                  # textangle = -90,
    #                  font = list(color = '#FF0000', size = local_font-1),
    #                  arrowhead = 2,
    #                  arrowcolor = '#FF0000',
    #                  arrowsize = 3,
    #                  arrowwidth = 0.5 
    # ) %>%
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>% 
    layout(
      font = list(family = "Roboto"),
      title = list(text="",
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
                   tickfont = list(size = local_font-1),
                   tickangle = -90
      ),
      yaxis = list(
        # title = "",
        title = "Minimum maturity level",
        # gridcolor = 'rgb(255,255,255)',
        showgrid = TRUE,
        showline = FALSE,
        # dtick = 20,
        # showticklabels = TRUE,
        # tickcolor = 'rgb(127,127,127)',
        # ticks = 'outside',
        zeroline = TRUE,
        range= c(0,110),
        tickvals = c(20, 40, 60, 80, 100),
        ticktext = c("A  ", "B  ", "C  ", "D  ", "  "),
        zerolinecolor = 'rgb(240,240,240)',
        titlefont = list(size = local_font), 
        # showticklabels = FALSE
        tickfont = list(size = local_font, color = 'black')
      ),
      yaxis2 = list(title = "EoSM score",
                    overlaying = "y",
                    side = "right",
                    showgrid = FALSE,
                    showline = FALSE,
                    ticksuffix = "",
                    tickformat = ",.0f",
                    dtick = 20,
                    range = list(0,110),
                    zeroline = TRUE,
                    zerolinecolor = 'rgb(255,255,255)',
                    titlefont = list(size = local_font), tickfont = list(size = local_font)
      ),
      # showlegend = FALSE
      legend = list(
        orientation = 'h', 
        xanchor = if_else(knitr::is_latex_output(), "left", "center"),
        x = if_else(knitr::is_latex_output(), -0.15, 0.5), 
        y = if_else(knitr::is_latex_output(), 1.5, 1.3),
        font = list(size = if_else(knitr::is_latex_output(), local_font-3.6, local_font-1) )
      ),
      margin = list(t = local_margin, 
                    # l=100,
                    r = 40)
    )
  
}

if (knitr::is_latex_output()) {
  myc(NA, 250, 8, 0)
  
} else {
  myc(NA, 420, 14, 60)
}

