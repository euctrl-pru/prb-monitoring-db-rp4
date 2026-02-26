
# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "enroute")}
# ez=1

# define cz ----
if (country != "MUAC") {
  ez <- as.numeric(cz[[1]])
  cztype <- cz[[2]]
  # cztype <- "terminal"
  mycz <- if_else(cztype == "terminal",
                  tcz_list$tcz_id[ez],
                  ecz_list$ecz_id[ez])
  mycz_name <- if_else(cztype == "terminal",
                       tcz_list$tcz_name[ez],
                       ecz_list$ecz_name[ez])
}

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
    mutate(x5_3_cost_nc2017 = costs_eur2017_cz,
           xrate2017 = 1)
  
} else {

  data_raw  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = if_else(cztype == "terminal", "Terminal_T1", "Enroute_T1"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
}

# prepare data ----
if(country == "MUAC") {
  data_pre_prep <- data_raw |> 
    filter(grepl("MUAC", entity_code)) |> 
    mutate(entity_code = NA) |> 
    group_by(year, status, entity_code) |> 
    summarise(x5_3_cost_nc2017 = sum(x5_3_cost_nc2017, na.rm = TRUE)) |> 
    ungroup() |> 
    mutate(xrate2017 = 1)
  
} else {
  
  data_pre_prep <- data_raw %>% 
    filter(entity_code == mycz) 
}

data_prep_split <- data_pre_prep %>% 
  filter(year != 20202021) %>% 
  mutate(
    mymetric = case_when (
      status == 'A' & year > max(.env$year_report, 2021) ~ NA,
      .default = x5_3_cost_nc2017/xrate2017
    ),
    xlabel = as.character(year)
  ) %>%  
  select(
    year,
    status,
    mymetric,
    xlabel
  ) 

data_prep2020_2021 <- data_prep_split %>% 
  filter(
    year < 2022) %>% 
  group_by(status) |> 
  summarise(mymetric = sum(mymetric, na.rm = TRUE)) |> 
  mutate(xlabel = "2020-2021")

data_prep <- data_prep_split |> 
  filter(year > 2021) |> 
  select(-year) |>
  rbind(data_prep2020_2021) |> 
  mutate(mymetric = round(mymetric/10^6, 2),
         status = str_replace(status, "A", "Actual costs"),
         status = str_replace(status, "D", "Determined costs")
  ) %>% 
  arrange(xlabel) %>% 
  rename(type = status)


### replace 0 by NAs so they are not plotted
data_prep[data_prep == 0] <- NA

# chart parameters ----
mysuffix <- ""
mydecimals <- if_else(country == rp_full | country == "MUAC", 0, 1)

### trace parameters
mycolors = c('#5B9BD5', '#FFC000')
###set up order of traces
myfactor <- data_prep %>% select(type) %>% unique() 
invisible(as.list(myfactor$type))
myfactor <- sort(myfactor$type, decreasing = TRUE)
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- "middle"
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("Total costs")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- paste0(if_else(cztype == "terminal", "Terminal", "En route"),
                        " costs (M€<sub>2017</sub>)")
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ",.0f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin = mymargin

# plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals)
