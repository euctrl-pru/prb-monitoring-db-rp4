if (!exists("country") | is.na(country)) {country <- rp_full
source("R/parameters.R")
}

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
  ## import data & prep ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES CEFF.xlsx"),
    sheet = if_else(cztype == "terminal", "SES_TRM_all", "SES_ERT_all"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_pre_prep <- data_raw |> 
    filter(status == "A") |> 
    select(
      year_text = year,
      cost_sharing_ansp1,
      inflation_adjustment_ansp1,
      cost_exempt_ansp1,
      trs = trs_ansp1,
      financial_incentive = incentives_ansp1,
      ex_post_roe = ro_e_ansp1
    ) |> 
    mutate(
      year_text = case_when(
        year_text == 2021 | year_text == 2020 ~ "2020-2021",
        .default = as.character(year_text)
        )
    ) |> 
    group_by(year_text) |> 
    summarise(
      atsp_gain_loss_cost_sharing = sum(cost_sharing_ansp1, cost_exempt_ansp1, inflation_adjustment_ansp1, na.rm = TRUE)/1000,
      trs = sum(trs, na.rm = TRUE)/1000,
      financial_incentive = sum(financial_incentive, na.rm = TRUE)/1000,
      ex_post_roe = sum(ex_post_roe, na.rm = TRUE)/1000
    ) |> 
    filter(
      year_text == if_else(year_report == 2021 | year_report == 2020, "2020-2021", as.character(year_report))
    )
  
} else {
  # State ----
  ## import data  ----
  data_raw  <-  regulatory_result(cztype, mycz)
  
  ## pre-prepare data ----
  data_pre_prep <- data_raw %>% 
    filter(type == "Main ANSP",
           year_text == if_else(year_report == 2021 | year_report == 2020, "2020-2021", as.character(year_report))
           )  %>% 
    select(year_text, atsp_gain_loss_cost_sharing, trs, financial_incentive, ex_post_roe) 
}

# pre-prepare data ----
data_prep <- data_pre_prep |> 
  pivot_longer(-year_text, names_to = "status", values_to = "mymetric") %>% 
  mutate (
    mymetric = mymetric/1000,
    ylabel = case_when (
      status == 'atsp_gain_loss_cost_sharing' ~ "Cost sharing", 
      status == 'trs' ~ "Traffic risk sharing",
      status == 'financial_incentive' ~ "Incentives",
      status == 'ex_post_roe' ~ "Actual RoE in value"),
    mylabel = format(round(mymetric,1), big.mark = ",", nsmall =1)
  )


# chart parameters ----
if (knitr::is_latex_output()) {
  mylocalheight <- myheight
  
}else{
  mylocalheight <- myheight+30
}

mychart_title <- paste0("Net result from ", 
                        if_else(cztype == 'terminal', 'terminal', 'en route'),
                        " activity - ", 
                        if_else(country == rp_full, "Main ANSPs ", paste0(main_ansp," ")),
                        if_else(year_report == 2021 | year_report == 2020, "2020-2021", as.character(year_report))
                        )
mytitle_y <- 0.99
myaxis_title <- ""
mybarcolor_pos <- '#9ECF8D'
mybarcolor_neg <- '#F87474'
mytextcolor <- 'black'
myhovertemplate <- paste0('%{x:,.1f}<extra></extra>')
myxaxis_tickformat <- "0,.1f"

###set up order of traces
myfactor <- c("Actual RoE in value",
              "Incentives",
              "Traffic risk sharing",
              "Cost sharing")

mylocalmargin <- list (t = 30, b = 70)
mydecimals <- 1

# plot chart  ----

### calculate x range, and annotation and image position
myroundup <- max(floor((log10(abs(max(data_prep$mymetric, na.rm = TRUE))))), floor((log10(abs(min(data_prep$mymetric, na.rm = TRUE))))))
range_min <- floor(min(data_prep$mymetric, na.rm = TRUE)/10^myroundup) * 10^myroundup - 10^myroundup/1.2
range_min <- if_else(range_min >0, 0, range_min)
range_max <- ceiling(max(data_prep$mymetric, na.rm = TRUE)/10^myroundup) * 10^myroundup + 10^myroundup/2

### plot chart and add annotations
myplot <- myhbarc(mywidth, mylocalheight, myfont, mylocalmargin) %>% 
  layout(
    uniformtext=list(minsize = 14, mode='show'),
    xaxis = list(
      title = "",
      range = c(range_min, range_max)
      ),
    images = list(
      list(
        # Add images
        source =  base64enc::dataURI(file = here("images","arrow_right.png")),  
        xref = "paper",  
        yref = "paper",  
        x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)),  
        y = -0.20,  
        sizex = 0.18,  
        sizey = 0.18,  
        layer = "above",
        xanchor="left",  
        yanchor="bottom" 
      ),
      list(
        # Add images
        source =  base64enc::dataURI(file = here("images","arrow_left.png")),  
        xref = "paper",  
        yref = "paper",  
        x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)),  
        y = -0.20,  
        sizex = 0.18,  
        sizey = 0.18,  
        layer = "above",
        xanchor="right",  
        yanchor="bottom" 
      )
      )
    ,
    annotations = list(
      list(
      xanchor = "left",
      x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)) + 0.05,
      y = -0.28,
      text = '<b>ANSP gain</b>',
      font = list(size = myfont*0.85),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      # arrowhead = 7,
      ax = 0,
      ay = 0
    ),
    list(
      xanchor = "right",
      x = if_else(range_min >=0,0, abs(range_min)/(range_max-range_min)) -0.05,
      y = -0.28,
      text = '<b>ANSP loss</b>',
      font = list(size = myfont*0.85),
      xref = "paper",
      yref = "paper",
      showarrow = FALSE,
      # arrowhead = 7,
      ax = 0,
      ay = 0
    ),
    list(
      xanchor = "center",
      x = 0.5,
      y = -0.17,
      text = 'M€',
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

myplot

