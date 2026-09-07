## import data  ----
if (!exists("data_loaded")) {
  source("R/get_data.R")
}

data_raw_axot <- axot_actual_ms %>% rename(mymetric = value)
data_raw_axit <- axit_actual_ms %>% rename(mymetric = value)
data_raw_asma <- asma_actual_ms %>% rename(mymetric = value)

rp_years_df <- data.frame(rp_years) %>% rename(xlabel = rp_years)

## prepare data ----

data_prep <- data_raw_asma %>%
  rbind(data_raw_axot) %>%
  rbind(data_raw_axit) %>%
  filter(
    state == .env$country
  ) %>%
  mutate(
    type = case_when(
      indicator_type == "STATE_ASMA" ~ "ASMA",
      indicator_type == "STATE_TAXI" ~ "AXOT",
      indicator_type == "STATE_TAXI_IN" ~ "AXIT"
    ),
    xlabel = year
  ) %>%
  mutate(
    mymetric = case_when(
      xlabel <= year_report ~ mymetric,
      .default = NA
    )
  ) %>%
  right_join(rp_years_df, by = 'xlabel') %>%
  mutate(type = if_else(is.na(type) == TRUE, "ASMA", type))

## chart parameters ----
c_suffix <- ""
c_decimals <- 2

### trace parameters
c_colors = c('#0070C050', '#FFC00050', '#ababab50')

###set up order of traces
c_factor <- c("AXOT", "ASMA", "AXIT")

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

c_textangle <- 0
c_textposition <- "bottom"
c_insidetextanchor <- NA

#### title
c_title_text <- paste0("AXOT, ASMA & AXIT - State level")

#### yaxis
c_yaxis_title <- "AXOT, ASMA & AXIT (min/flight)"
c_yaxis_ticksuffix <- ""
c_yaxis_tickformat <- paste0(".", c_decimals, "f")
c_yaxis_rangemode <- NA
c_yaxis_range <- c(
  janitor::round_half_up(min(data_prep$mymetric, na.rm = TRUE) / 5) * 5,
  janitor::round_half_up(max(data_prep$mymetric, na.rm = TRUE) / 5) * 5
)

#### margin
c_margin <- list(t = 60)

## define chart function ----
myareachart <- function(
  width = mywidth,
  height = myheight,
  font = myfont,
  margin = c_margin
) {
  plot_ly(
    data = filter(data_prep, type == c_factor[[1]]),
    width = width,
    height = height,
    x = ~xlabel,
    y = ~mymetric,
    yaxis = "y1",
    line = list(width = mylinewidth),
    fillcolor = c_colors[[1]],
    name = c_factor[[1]],
    text = ~ paste0(
      '\n',
      format(mymetric, big.mark = ",", nsmall = c_decimals),
      c_suffix
    ),
    textangle = c_textangle,
    textposition = c_textposition,
    insidetextanchor = c_insidetextanchor,
    textfont = list(color = mytextfont_color, size = mytextfont_size),
    cliponaxis = FALSE,
    type = 'scatter',
    stackgroup = 'one',
    mode = "none",
    hovertemplate = c_hovertemplate,
    # hoverinfo = "none",
    showlegend = mytrace_showlegend
  ) %>%
    add_trace(
      data = filter(data_prep, type == c_factor[[2]]),
      fillcolor = c_colors[[2]],
      name = c_factor[[2]]
    ) %>%
    config(
      responsive = TRUE,
      displaylogo = FALSE,
      displayModeBar = F
      # modeBarButtons = list(list("toImage")),
    ) %>%
    layout(
      uniformtext = list(minsize = myminsize, mode = 'show'),
      font = list(family = myfont_family),
      title = list(
        text = c_title_text,
        x = mytitle_x,
        y = mytitle_y,
        xanchor = mytitle_xanchor,
        yanchor = mytitle_yanchor,
        font = list(size = mytitle_font_size)
      ),
      dragmode = FALSE,
      bargap = mybargap,
      barmode = mybarmode,
      hovermode = myhovermode,
      hoverlabel = list(bgcolor = myhoverlabel_bgcolor),
      xaxis = list(
        title = myxaxis_title,
        gridcolor = myxaxis_gridcolor,
        showgrid = myxaxis_showgrid,
        showline = myxaxis_showline,
        showticklabels = myxaxis_showticklabels,
        dtick = myxaxis_dtick,
        range = c(rp_min_year - 0.3, rp_max_year),
        tickformat = '.0f',
        # tickcolor = 'rgb(127,127,127)',
        # ticks = 'outside',
        zeroline = myxaxis_zeroline,
        tickfont = list(size = myxaxis_tickfont_size)
      ),
      yaxis = list(
        title = c_yaxis_title,
        gridcolor = myyaxis_gridcolor,
        showgrid = myyaxis_showgrid,
        showline = myyaxis_showline,
        tickprefix = myyaxis_tickprefix,
        ticksuffix = c_yaxis_ticksuffix,
        tickformat = c_yaxis_tickformat,
        range = c_yaxis_range,
        rangemode = c_yaxis_rangemode,
        zeroline = myyaxis_zeroline,
        zerolinecolor = myyaxis_zerolinecolor,
        titlefont = list(size = myyaxis_titlefont_size),
        tickfont = list(size = myyaxis_tickfont_size)
      ),
      legend = list(
        traceorder = mylegend_traceorder,
        orientation = mylegend_orientation,
        xanchor = mylegend_xanchor,
        yanchor = mylegend_yanchor,
        x = mylegend_x,
        y = mylegend_y,
        font = list(size = mylegend_font_size)
      ),
      margin = margin
    )
}


## plot chart  ----
if (year_report == rp_min_year) {
  myplot <- mybarchart2(
    data_prep,
    height = myheight,
    colors = c("#AFD2EB", "#FFEBAB", '#cdcdcd'),
    local_factor = c_factor,
    decimals = c_decimals,
    suffix = c_suffix,
    barmode = "stack",

    hovertemplate = c_hovertemplate,

    textangle = c_textangle,
    textposition = c_textposition,
    insidetextanchor = c_insidetextanchor,

    title_text = c_title_text,

    yaxis_title = c_yaxis_title,
    yaxis_ticksuffix = c_suffix,
    yaxis_tickformat = c_yaxis_tickformat
  )

  myplot %>% add_empty_trace(data_prep)
} else {
  myareachart()
}
