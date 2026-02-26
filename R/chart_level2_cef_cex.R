
# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "terminal")}
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

  ## prepare data ----
  data_prep_split <- data_raw %>% 
    filter(status == "A") |> 
    mutate(
      mymetric = case_when (
        year > .env$year_report ~ NA,
        .default = cecs_total_eur_cz
      ),
      xlabel = as.character(year)
    ) %>%  
    select(
      year,
      mymetric,
      xlabel
    ) 
  
  data_prep2020_2021 <- data_prep_split %>% 
    filter(
      year < 2022) %>% 
    summarise(mymetric = sum(mymetric, na.rm = TRUE)) |> 
    mutate(xlabel = "2020-2021")
  
  data_prep <- data_prep_split |> 
    filter(year > 2021) |> 
    select(-year) |>
    rbind(data_prep2020_2021) |> 
    mutate(mymetric = round(mymetric/1000, 2),
           type = 'Cost exempt') |>  
    arrange(xlabel)
  
} else {
  # State  ----
  ## import data  ----
  data_raw  <-  read_xlsx(
  paste0(data_folder, "CEFF dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = if_else(cztype == "terminal", "Terminal_T2", "Enroute_T2"),
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 
  
  ## prepare data ----
  data_prep_t2 <- data_raw %>% 
    filter(
      entity_code == mycz,
    ) %>% 
    select(year, x3_8_diff_det_cost_actual_cost) %>% 
    mutate(
      actual = case_when(
        year > year_report & year != 20202021 ~ NA,
        .default = round(x3_8_diff_det_cost_actual_cost/1000,2)
      ),
      year = as.character(year),
      year = str_replace(year, "20202021", "2020-2021")
    )
  
  # t exchange rates
  yearly_xrates <- get_xrates(cztype, mycz)
  
  data_prep_xrates <- yearly_xrates %>% 
    filter(
      entity_code == mycz
    ) %>% 
    select(-entity_code) %>% 
    filter(year > 2020) %>% 
    mutate(year = as.character(year),
           year = if_else(year == '2021', '2020-2021', year)
    ) 
  
  data_prep <- data_prep_t2 %>% 
    left_join(data_prep_xrates, by = 'year') %>% 
    mutate(mymetric = actual/pp_exchangerate,
           xlabel = year,
           type = 'Cost exempt') %>% 
    arrange(xlabel)
  
}


# chart parameters ----
mysuffix <- ""
mydecimals <- if_else(country == rp_full, 0, 1)

### trace parameters
mycolors = c( '#8497B0')
###set up order of traces
myfactor <- data_prep %>% select(type) %>% unique() 
invisible(as.list(myfactor$type))
myfactor <- sort(myfactor$type, decreasing = TRUE)
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- 0
mytextposition <- "auto"
myinsidetextanchor <- NULL
mytextfont_color <- 'black'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("Cost exempt from cost sharing")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "Cost exempt from cost sharing\n(€'000)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ",.0f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin = mymargin

# plot chart  ----
mybarchart(data_prep, mywidth, myheight+20, myfont, mylocalmargin, mydecimals)  %>% 
  add_empty_trace(., data_prep) 
