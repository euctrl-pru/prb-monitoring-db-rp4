
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
      year,
      ro_e_ansp1,
      ro_e_ansp_other,
      ro_e_ansp_met,
      
      net_result_ansp1,
      net_result_other_ansp,
      net_result_met,
    ) |> 
    rowwise() |> 
    mutate(
      regulatory_result_ansp1 = sum(ro_e_ansp1, net_result_ansp1, na.rm = TRUE)/10^6,
      regulatory_result_ansp_other = sum(ro_e_ansp_other, net_result_other_ansp, na.rm = TRUE)/10^6,
      regulatory_result_met = sum(ro_e_ansp_met, net_result_met, na.rm = TRUE)/10^6,
    ) 
    
  data_prep <- data_pre_prep |> 
    select(year, regulatory_result_ansp1, regulatory_result_ansp_other,  regulatory_result_met) |> 
    mutate(
      regulatory_result_ansp1 = case_when(
        year == 2021 ~ regulatory_result_ansp1 + pull(select(filter(data_pre_prep, year == 2020), regulatory_result_ansp1)),
        .default = regulatory_result_ansp1),
      
      regulatory_result_ansp_other = case_when(
        year == 2021 ~ regulatory_result_ansp_other + pull(select(filter(data_pre_prep, year == 2020), regulatory_result_ansp_other)),
        .default = regulatory_result_ansp_other),
      
      regulatory_result_met = case_when(
        year == 2021 ~ regulatory_result_met + pull(select(filter(data_pre_prep, year == 2020), regulatory_result_met)),
        .default = regulatory_result_met),
      
      xlabel = if_else(year == 2021, "2020-2021", as.character(year))
    ) |> 
    filter(year > 2020) |> 
    select(-year) |> 
    pivot_longer(-xlabel, names_to = "type", values_to = "mymetric") |> 
    mutate(
      type = case_when(
        type == "regulatory_result_ansp1" ~ "Main ANSP",
        type == "regulatory_result_ansp_other" ~ "Other ANSP",
        type == "regulatory_result_met" ~ "MET"),
      
      mymetric = case_when(
            as.numeric(str_replace(xlabel, "-", "")) > year_report & xlabel != '2020-2021' ~ NA,
            .default = mymetric
            )
      )
  
} else {
  # State ----
  ## import data & prep ----
  data_prep <- regulatory_result(cztype, mycz)
  data_prep <- data_prep %>% 
    mutate(mymetric = regulatory_result / 1000) %>% 
    mutate_if(is.numeric, 
              ~ ifelse(as.numeric(str_replace(year_text,"-", "")) > year_report & year_text != "2020-2021", 
                       NA,
                       .)
    ) |> 
    rename(xlabel = year_text) 

}
    
# chart parameters ----
mysuffix <- ""
mydecimals <- 1

### trace parameters
mycolors = c( '#5B9BD5', '#FFC000', '#BFBFBF')
###set up order of traces
myfactor <- c("Main ANSP",
              "Other ANSP",
              "MET")
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- "middle"
mytextfont_color <- 'transparent'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("RR by entity group")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "RR (M€)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ",.1f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin = mymargin

# plot chart  ----
mybarchart(data_prep, mywidth, myheight+10, myfont, mylocalmargin, mydecimals) %>% 
  add_empty_trace(., data_prep)  
