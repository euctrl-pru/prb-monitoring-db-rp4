
# fix ez if script not executed from qmd file ----
if (exists("cztype") == FALSE) {cz = c("1", "enroute")}
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

if (country != rp_full) {
  # import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = if_else(cztype == "terminal", "Terminal_T1", "Enroute_T1"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  # prepare data ----
  data_prep1 <- data_raw %>% 
    filter(
      entity_code == mycz) %>% 
    mutate(
      mymetric = paste0(format(round(x5_1_inflation_rate*100, 1), nsmall =1), '%'),
      mymetric = case_when( 
        year > year_report & year != 20202021 & status == "A" ~ NA,
        .default = mymetric)
    ) %>%  
    select(
      year,
      status,
      mymetric
    ) %>%  
    filter(year > 2021) %>% 
    mutate(year = as.character(year),
           year = str_replace(year, "20202021", "2020-2021"),
           status = str_replace(status, "A", "Actual inflation rate"),
           status = str_replace(status, "D", "Determined inflation rate")
    ) %>% 
    arrange(year) %>% 
    pivot_wider(values_from = 'mymetric', names_from = 'year') %>% 
    rename(ia = status)
  
  data_prep2 <- data_raw %>% 
    filter(
      entity_code == mycz) %>% 
    mutate(
      mymetric = x5_2_inflation_index_nc2017,
      mymetric = case_when( 
        year > year_report & status == "A" ~ NA,
        .default = mymetric)
    ) %>%  
    select(
      year,
      status,
      mymetric
    ) %>%  
    filter(year > 2021) %>% 
    mutate(year = as.character(year),
           year = str_replace(year, "20202021", "2020-2021"),
           status = str_replace(status, "A", "Actual inflation index"),
           status = str_replace(status, "D", "Determined inflation index")
    ) %>% 
    arrange(year) %>% 
    pivot_wider(values_from = 'mymetric', names_from = 'status') %>% 
    mutate('Difference inflation index (p.p.)' = case_when(
      year<= .env$year_report ~ .[[2]] - .[[3]],
      .default = NA)
    ) %>% 
    mutate_at(c(2:4), ~as.character(round(.,1))) %>% 
    mutate_at(c(4), ~ if_else(as.numeric(.)>=0, paste0('+', .), .)) %>% 
    pivot_longer(-year, names_to = "ia", values_to = 'mymetric') %>% 
    pivot_wider(values_from = 'mymetric', names_from = 'year')
  
  data_prep <- data_prep1 %>% rbind(data_prep2) %>% 
    mutate('2020-2021' = 'NA')  %>% 
    mutate_at(c(1), ~ factor(.,
                                            levels = c('Determined inflation rate',
                                              'Determined inflation index',
                                              'Actual inflation rate',
                                              'Actual inflation index',
                                              'Difference inflation index (p.p.)'
                                              ))) %>% 
    arrange(ia) %>% 
    rename('Inflation assumptions' = ia)
    
  

  table1 <- mygtable(data_prep, myfont) %>% 
    tab_options(column_labels.background.color = "#F2F2F2",
                column_labels.font.weight = 'bold',
                container.padding.y = 0) %>% 
    cols_align(columns = 1, align = "left") %>%
    tab_style(
      style = list(
        # cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = 1
      ))
}    


if (!knitr::is_latex_output()) {
  table1
}
