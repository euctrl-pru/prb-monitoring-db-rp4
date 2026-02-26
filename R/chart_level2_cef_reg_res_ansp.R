
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

if (country == rp_full) {
  # SES  ----
  ## import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES CEFF.xlsx"),
    sheet = if_else(cztype == "terminal", "SES_TRM_all", "SES_ERT_all"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  ## pre-prep data ----
  data_pre_prep <- data_raw |> 
    mutate(
      regulatory_result = ro_e_ansp1 + net_result_ansp1,
      revenues_ansp = revenues_ansp1 + net_result_ansp1
      ) |> 
    select(
      year,
      status,
      regulatory_result,
      revenues_ansp
    ) |> 
    pivot_wider( names_sep = "_", names_from = "status", values_from = c(3:4))  
    # select(-revenues_ansp_D) 
  
  data_for_chart_wide <- data_pre_prep |> 
    mutate(
      regulatory_result = case_when(
        year == 2021 ~ regulatory_result_A + pull(select(filter(data_pre_prep, year == 2020), regulatory_result_A)),
        .default = regulatory_result_A),
      
      ex_ante_roe = case_when(
        year == 2021 ~ regulatory_result_D + pull(select(filter(data_pre_prep, year == 2020), regulatory_result_D)),
        .default = regulatory_result_D),
      
      actual_revenues = case_when(
        year == 2021 ~ revenues_ansp_A + pull(select(filter(data_pre_prep, year == 2020), revenues_ansp_A)),
        .default = revenues_ansp_A),
      
      determined_revenues = case_when(
        year == 2021 ~ revenues_ansp_D + pull(select(filter(data_pre_prep, year == 2020), revenues_ansp_D)),
        .default = revenues_ansp_D),

      share_rr_act_rev_expost = regulatory_result/actual_revenues * 100,
      share_rr_act_rev_exante = ex_ante_roe/determined_revenues * 100,
      
      year_text = if_else(year == 2021, "2020-2021", as.character(year))
    ) |>  
    filter(year>2020) |> 
    select(year_text, regulatory_result, ex_ante_roe, share_rr_act_rev_expost, share_rr_act_rev_exante) |> 
    mutate(
      regulatory_result = case_when(
        as.numeric(str_replace(year_text, "-", "")) > year_report & year_text != '2020-2021' ~ NA,
        .default = regulatory_result/1000
      ),
      
      share_rr_act_rev_expost = case_when(
        as.numeric(str_replace(year_text, "-", "")) > year_report & year_text != '2020-2021' ~ NA,
        .default = share_rr_act_rev_expost
      ),
      
      share_rr_act_rev_exante = case_when(
        as.numeric(str_replace(year_text, "-", "")) > year_report & year_text != '2020-2021' ~ NA,
        .default = share_rr_act_rev_exante
      ),
      
      ex_ante_roe = case_when(
        as.numeric(str_replace(year_text, "-", "")) > year_report & year_text != '2020-2021' ~ NA,
        .default = ex_ante_roe/1000
      )
      
    )
  
} else {
  # State  ----
  ## import data ----
  data_reg_res <- regulatory_result(cztype, mycz)
    
  ## pre-prep data ----
  data_for_chart_wide <- data_reg_res %>% 
    filter(type == 'Main ANSP') %>% 
    mutate(
      share_rr_act_rev_expost = regulatory_result/actual_revenues * 100,
      share_rr_act_rev_exante = ex_ante_roe/x4_2_cost_excl_vfr_d * 100
      ) %>% 
    select(year_text,regulatory_result, ex_ante_roe, share_rr_act_rev_expost, share_rr_act_rev_exante)
}

# prep data ----

## separate in two tables for pivoting 
data_for_chart_value <- data_for_chart_wide %>% 
  select(-c(share_rr_act_rev_expost, share_rr_act_rev_exante)) %>% 
  pivot_longer(-year_text, names_to = "xlabel", values_to = 'mymetric') %>% 
  mutate(xlabel = case_when(
    xlabel == 'regulatory_result'  ~ 'Ex-post',
    xlabel == 'ex_ante_roe'  ~ 'Ex-ante')
    , mymetric = if_else(xlabel == "Ex-post" & as.numeric(str_sub(year_text, 1,4)) > max (2021, year_report), NA, mymetric/1000)
  )

data_for_chart_share <- data_for_chart_wide %>% 
  select(-c(regulatory_result, ex_ante_roe)) %>% 
  pivot_longer(-year_text, names_to = "xlabel", values_to = 'share') %>% 
  mutate(xlabel = case_when(
    xlabel == 'share_rr_act_rev_expost'  ~ 'Ex-post',
    xlabel == 'share_rr_act_rev_exante'  ~ 'Ex-ante'),
    share = if_else(xlabel == "Ex-post" & as.numeric(str_sub(year_text, 1,4)) > max (2021, year_report), NA, share)
  )

data_prep <- data_for_chart_value %>% 
  left_join(data_for_chart_share, by = c('year_text', 'xlabel')) %>% 
  mutate(type = xlabel,
         xlabel = year_text
  )
  
# chart parameters ----
mysuffix <- ""
mydecimals <- 1

### trace parameters
mycolors = c( '#CEE0EA', '#4B8DB1')
###set up order of traces
myfactor <- c("Ex-ante",
              "Ex-post")
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- "middle"
mytextfont_color <- 'transparent'

mytrace_showlegend <- F

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- if_else(country == rp_full, 
                        "RR - Main ANSPs",
                        paste0("RR - ", main_ansp)
                        )

mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "RR"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".1f"

#### legend
mylegend_y <- -0.24

#### margin
mylocalmargin = list(t = 60, b = 80, l = 40, r = 60)

# setup ranges to ensure zero line at same height
# https://stackoverflow.com/questions/76289470/plotly-barplot-with-two-y-axis-aligned-at-zero
# 1. Ensure y1 includes zero
y1_max <- max(max(data_prep$mymetric, na.rm = TRUE), 0)
y1_min <- min(min(data_prep$mymetric, na.rm = TRUE), 0)

# 2. Ensure y2 includes zero
y2_max <- max(max(data_prep$share, na.rm = TRUE), 0)
y2_min <- min(min(data_prep$share, na.rm = TRUE), 0)

# 3. Compute y1 range with standard plotly padding
y1_padding <- (y1_max - y1_min) / 16
y1_range <- c(y1_min - y1_padding, y1_max + y1_padding)

# 4. Find the relative position of 0 in y1 range
y1_relative_zero <- (0 - y1_range[1]) / (y1_range[2] - y1_range[1])

# 5. Calculate the total range for y2 to mirror zero position
y2_total_range <- (y2_max - y2_min) / (1 - 1.5 * y1_relative_zero)

# 6. Calculate padding needed on both ends
y2_padding_lower <- y1_relative_zero * y2_total_range
y2_padding_upper <- (1 - y1_relative_zero) * y2_total_range

# 7. Final y2 range
y2_range <- c(0 - y2_padding_lower, 0 + y2_padding_upper)

# plot chart  ----
myplot<- mybarchart(data_prep, mywidth, myheight + 30, myfont, mylocalmargin, mydecimals) %>%  
add_trace(
  inherit = FALSE,
  data = data_prep,
  x = ~xlabel ,
  y = ~share,
  name = "RR in percent of en-route revenues",
  yaxis = "y2",
  # mode = "markers",
  type = 'box',
  color = ~ factor(type, levels = myfactor),
  line = list(color = '#E46C0A', width = mylinewidth), fillcolor = '#E46C0A',
  # marker = list(size = mylinewidth * 3, color = '#E46C0A'),
  hoverinfo = 'none',
  showlegend = F
) %>% 
  add_trace(
    data = filter(data_prep, type == "Ex-ante"),
    x = ~ xlabel,
    y = ~ paste0(share, '%'),
    yaxis = "y2",
    mode = "markers", 
    type = 'scatter',
    name = "RR (ex-ante) in % of revenues",
    text = "" ,
    marker = list(size = 1, 
                  color = "transparent"),
    hovertemplate <- paste0('%{y:,.2f}%'),
    showlegend = F
  )  %>% 
  add_trace(
    data = filter(data_prep, type == "Ex-post"),
    x = ~ xlabel,
    y = ~ share,
    yaxis = "y2",
    mode = "markers", 
    type = 'scatter',
    name = "RR (ex-post) in % of revenues",
    text = "" ,
    marker = list(size = 1, 
                  color = "transparent"),
    hovertemplate <- paste0('%{x:,.2f}%'),
    showlegend = F
  )  %>%     
  layout(
    yaxis = list(
      # rangemode = "tozero",
      range = y1_range,
      secondary_y= TRUE
    ),
    yaxis2 = list(
      title = 'RR as % of revenues',
      overlaying = "y",
      zerolinecolor = '#E8E8E8',
      # rangemode = "tozero",
      range = y2_range,
      secondary_y= FALSE,
      ticksuffix = '%',
      tickformat = if_else(max(data_prep$share, na.rm = TRUE) >0.1, 
                            ".1f",
                            ".0f"),
      side = 'right',
      showgrid = FALSE
    ),
  boxmode = "group", bargroupgap = 0.1, boxgroupgap = 0.4, 
  boxgap = mybargap,
  # fake legend
  annotations = list(
      list (
      xanchor = "left",
      x = 0.0,
      y = mylegend_y,
      text = '■',
      font = list(size = myfont * 1.2, color = mycolors[[1]]),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      ax = 0,
      ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.07,
        y = mylegend_y,
        text = 'Ex-ante RR (in value)',
        font = list(size = myfont*0.9, color = "black"),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.6,
        y = mylegend_y,
        text = '■',
        font = list(size = myfont * 1.2, color = mycolors[[2]]),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.67,
        y = mylegend_y,
        text = 'Ex-post RR (in value)',
        font = list(size = myfont*0.9, color = "black"),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.0,
        y = mylegend_y-0.1,
        text = '<b>―</b>',
        font = list(size = myfont * 1.2, color = '#E46C0A'),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ),
      list (
        xanchor = "left",
        x = 0.07,
        y = mylegend_y-0.1,
        text = 'RR in percent of en route revenues',
        font = list(size = myfont*0.9, color = "black"),
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        ax = 0,
        ay = 0
      ) 
      )
  ) %>% 
  add_empty_trace(data_prep)
  

myplot

