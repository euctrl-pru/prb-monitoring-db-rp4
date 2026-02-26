## import data  ----
data_raw  <-  read_xlsx(
  paste0(data_folder, "ENV dataset master.xlsx"),
  # here("data","hlsr2021_data.xlsx"),
  sheet = "FUA_PIs",
  range = cell_limits(c(1, 1), c(NA, NA))) %>%
  as_tibble() %>% 
  clean_names() 


## prepare data ----
rp3_years <- 2020:2024 %>% as_tibble %>% rename(year = value)

data_prep <- data_raw %>% 
  filter(
    ses_state == .env$country,
    year <= .env$year_report) %>%
  right_join(rp3_years, by="year") %>% 
  mutate(ses_state = .env$country) %>% 
  mutate_at(c(4:9), ~ ./100)

# initialise lists to store tables ----
mytable = list()
data_for_table <- list()

# loop through PIs
for (i in 1:3) {
 
  data_for_table[[i]] <- data_prep %>% 
    select(ses_state, year, i+2) %>%   
    pivot_wider(names_from = "year", values_from = 3
                # , names_glue = "{year}_{.value}" #suffix to prefix
                ) 
  mycolnames <- colnames(data_for_table[[i]])
  mycolnames[[1]] <- paste0('Ratio PI#',i+5)
  colnames(data_for_table[[i]]) <- mycolnames
  


## plot table ----
mytable[[i]] <- mygtable(data_for_table[[i]], myfont) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold') %>% 
    fmt_percent(
      columns = c(2:6),
      decimals = 0,
    )
}

# create html plotlist ----
htmltools::tagList(mytable)

# https://stackoverflow.com/questions/35193612/multiple-r-plotly-figures-generated-in-a-rmarkdown-knitr-chunk-document  


