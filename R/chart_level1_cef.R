
# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "terminal", "level1")}
# ez=1

# define cz & level----
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]
# cztype <- "terminal"
mycz <- if_else(cztype == "terminal",
                tcz_list$tcz_id[ez],
                ecz_list$ecz_id[ez])
mycz_name <- if_else(cztype == "terminal",
                     tcz_list$tcz_name[ez],
                     ecz_list$ecz_name[ez])

doclevel <- cz[[3]]

# import data  ----
if (country == rp_full) {
  ## SES  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES CEFF.xlsx"),
    sheet = if_else(cztype == "terminal", "SES_TRM_all", "SES_ERT_all"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() |>
    # so it has the same structure as the state table
    mutate(
      x5_5_unit_cost_nc2017 = duc_2017eur_combined,
      xrate2017 = 1,
      year = if_else(year == 2021, 20202021, year)
    )
  
} else {
  ## State  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = if_else(cztype == "terminal", "Terminal_T1", "Enroute_T1"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 


}

# prepare data ----
data_prep <- data_raw %>% 
  filter(
    entity_code == mycz) %>% 
  mutate(
    mymetric = case_when(
      year > year_report & status == 'A' & year != 20202021 ~ NA,
      .default = round(x5_5_unit_cost_nc2017/xrate2017, 2)
    )
  ) %>%  
  select(
    year,
    status,
    mymetric
  ) %>%  
  filter(year > 2021) %>% 
  mutate(xlabel = as.character(year),
         xlabel = str_replace(xlabel, "20202021", "2020-2021"),
         status = str_replace(status, "A", "Actual unit cost"),
         status = str_replace(status, "D", "Determined unit cost")
  ) %>% 
  arrange(xlabel) %>% 
  rename(type = status)
  
### replace 0 by NAs so they are not plotted
data_prep[data_prep == 0] <- NA

# chart parameters ----
mysuffix <- ""
mydecimals <- 2

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
myminsize <- myfont*0.95

#### title
if (knitr::is_latex_output()) {
  if ((cztype == "terminal" & no_tcz > 1) | (cztype == "enroute" & no_ecz > 1))  {
    mylevel1_title <- paste0(" determined/actual unit\ncosts (DUC/AUC) - ",
                             mycz_name)
  } else{
    mylevel1_title <- paste0(" determined/actual unit\ncosts (DUC/AUC)")

  }
  mytitle_y <- 0.95
  mylocalmargin <- list(t = 50)
  
} else {
  if ((cztype == "terminal" & no_tcz > 1) | (cztype == "enroute" & no_ecz > 1))  {
    mylevel1_title <- paste0(" determined/actual unit costs (DUC/AUC)\n",
                             mycz_name)
    mytitle_y <- 0.95
  } else{
    mylevel1_title <- paste0(" determined/actual unit costs (DUC/AUC)")
    mytitle_y <- 0.99
    
  }
  mylocalmargin <- mymargin
  
}

mytitle_text <- if_else(doclevel == "level1",
                        paste0(if_else(cztype == "terminal", "Terminal", "En route"),
                               mylevel1_title),
                        "DUC/AUC")


#### xaxis

#### yaxis
myyaxis_title <- paste0(if_else(cztype == "terminal", "Terminal ", "En route "), 
                        " unit costs (€<sub>2017</sub>)")
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".0f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin


# plot chart  ----
mybarchart(data_prep, mywidth, myheight, myfont, mylocalmargin, mydecimals)


