
if (exists("cztype") == FALSE) {cztype = "enroute"}

# import data  ----
## dimension table ----
cz_table <- read_mytable("Lists.xlsx", "Lists", 
                          if_else(cztype == "terminal", "Table_TCZ", "Table_ECZ")) %>% 
  clean_names() |> 
  select(State = state, 
         cz_id = if_else(cztype == "terminal", "tcz_id", "ecz_id"))

state_table <- state_list |> as_tibble() |> 
  filter(value != "MUAC", 
         value != "Luxembourg", 
         value != rp_full, 
         value != "Network Manager") |> 
  select(State = value) |> 
  left_join(cz_table, by = "State")

## ses ----
data_raw_ses  <-  read_xlsx(
  paste0(data_folder, "SES CEFF.xlsx"),
  sheet = if_else(cztype == "terminal", "SES_TRM_all", "SES_ERT_all"),
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 

## CZ  ----
data_raw_cz  <-  read_xlsx(
  paste0(data_folder, "CEFF dataset master.xlsx"),
  sheet = if_else(cztype == "terminal", "Terminal_T1", "Enroute_T1"),
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names()


# prepare data ----
## SES ----
data_prep_ses_split <- data_raw_ses %>% 
  mutate(entity_name = "Union-wide") |> 
  select(entity_name, 
         year, 
         status, 
         su = su_cz,
         costs = costs_eur2017nominal_cz,
         costs_eur2017 = costs_eur2017_cz,
         duc = duc_2017eur_combined)

data_prep_ses_20202021 <- data_prep_ses_split |> 
  filter(year < 2022) |> 
  group_by(entity_name, status) |> 
  summarise(su = sum(su, na.rm = TRUE),
            costs = sum(costs, na.rm = TRUE),
            costs_eur2017 = sum(costs_eur2017, na.rm = TRUE),
            duc = sum(duc, na.rm = TRUE)
            ) |> 
  mutate(year = 20202021) |> 
  select(entity_name, year, status, su, costs, costs_eur2017, duc)

data_prep_ses <- data_prep_ses_split |> 
  filter(year > 2021) |> 
  rbind(data_prep_ses_20202021)

## state ----
data_prep_cz <- data_raw_cz |> 
  filter(year > 2021,
         entity_type == if_else(cztype == "terminal", "TCZ", "ECZ")) |> 
  select(
    entity_name, 
    year,
    status,
    su = x5_4_total_su,
    costs = x4_2_cost_excl_vfr,
    costs_eur2017 = x5_3_cost_nc2017,
    duc = x5_5_unit_cost_nc2017
  )

data_prep <- data_prep_cz |> 
  rbind(data_prep_ses) |> 
  filter(year == if_else(year_report == 2020 | year_report == 2021, 20202021, year_report)) |> 
  select(-year)

data_prep_su <- data_prep |> 
  pivot_wider(id_cols = -c(costs, costs_eur2017, duc), values_from = "su", names_from = "status") |> 
  mutate(
    mymetric = (A/D-1) * 100,
    entity_name = factor(entity_name, levels = entity_name[order(mymetric)]),
    mycolor = if_else(entity_name == "Union-wide", "#FFC000", "#0070C0")) |> 
  arrange(entity_name)

data_prep_costs <- data_prep |> 
  pivot_wider(id_cols = -c(su, costs_eur2017, duc), values_from = "costs", names_from = "status") |> 
  mutate(mymetric = (A/D-1) * 100,
         mycolor = if_else(entity_name == "Union-wide", "#FFC000", "#0070C0")) 

data_prep_costs2017 <- data_prep |> 
  pivot_wider(id_cols = -c(su, costs, duc), values_from = "costs_eur2017", names_from = "status") |> 
  mutate(mymetric = (A/D-1) * 100,
         mycolor = if_else(entity_name == "Union-wide", "#FFC000", "#0070C0")) 

data_prep_duc <- data_prep |> 
  pivot_wider(id_cols = -c(su, costs, costs_eur2017), values_from = "duc", names_from = "status") |> 
  mutate(mymetric = (A/D-1) * 100,
         mycolor = if_else(entity_name == "Union-wide", "#FFC000", "#0070C0")) 

# plot function  ----
cef_plot <- function(df, xtitle) {
  plot_ly(
  df,
  x = ~mymetric, 
  y = ~entity_name,
  text = ~paste0(round(mymetric,0), "%"),
  textposition = "outside",
  textfont = list(size = myminsize),
  type = 'bar',
  cliponaxis = FALSE,
  hoverinfo = "none",
  orientation = 'h', 
  marker = list(color = ~mycolor)
  ) %>%
  layout(
    showlegend = FALSE,
    uniformtext=list(minsize = myminsize, mode='show'),
    font = list(family = myfont_family),
    dragmode = FALSE,
    xaxis = list(
      title =  list(text = xtitle,
                    font = list(
                      size = myxtitlefontsize
                      ),
                    standoff = mystandoff # Adjust this value to control spacing
                    ),
      showticklabels = FALSE,
      showgrid = FALSE,
      range = c(floor((min(df$mymetric)-5)/10)*10-5, ceiling((max(df$mymetric)+5)/10)*10+5)
      ), 
    yaxis = list(
      title = '', 
      tickfont = list(size=mytickfontsize),
      showgrid = FALSE
      )
    ) |> 
  config( responsive = TRUE,
          displaylogo = FALSE,
          displayModeBar = F
          # modeBarButtons = list(list("toImage")),
  )
}

if (knitr::is_latex_output()) {
  myminsize <- 7
  mytickfontsize <- myminsize
  myxtitlefontsize <- myminsize +1
  mystandoff <- 0
  mytitle1 <- '% difference\nservice units'
  mytitle2 <- '% difference total\ncosts nominal €'
  mytitle3 <- '% difference total\ncosts €<sub>2017</sub>'
  mytitle4 <- '% difference\nAUC/DUC'
}else{
  mytickfontsize <- myfont*0.93
  myxtitlefontsize <- myfont
  mystandoff <- NULL
  mytitle1<- '% difference service units'
  mytitle2 <- '% difference total costs nominal €'
  mytitle3 <- '% difference total costs €<sub>2017</sub>'
  mytitle4 <- '% difference AUC/DUC'
}


p1 <- cef_plot(data_prep_su, mytitle1) |> 
  layout(xaxis=list(range = c(floor((min(data_prep_su$mymetric)-5)/10)*10-20, 
                              ceiling((max(data_prep_su$mymetric)+5)/10)*10+10)
))
p2 <- cef_plot(data_prep_costs, mytitle2)
p3 <- cef_plot(data_prep_costs2017, mytitle3)
p4 <- cef_plot(data_prep_duc, mytitle4)


subplot(p1, p2, p3, p4, nrows = 1, shareY = TRUE, titleX = TRUE) |> 
  layout(
    yaxis = list(tickmode = "linear", dtick = 1)
    )
  
