if (!exists("country") | is.na(country)) {country = rp_full
source("R/parameters.R")
}

# fix ez if script not executed from qmd file ----
if (exists("cztype") == FALSE) {cz = c("1", "enroute")}
ez <- as.numeric(cz[[1]])
cztype <- cz[[2]]

# define cz ----
if (country != "MUAC") {
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
    mutate(x4_2_cost_excl_vfr = costs_eur2017nominal_cz,
           xrate2017 = 1)
    
  
} else {
  ## State  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "CEFF dataset master.xlsx"),
    sheet = if_else(cztype == "terminal", "Terminal_T1", "Enroute_T1"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
}

# pre-prepare data ----
if (country == "MUAC") {
  data_pre_prep <- data_raw |> 
    filter(grepl("MUAC", entity_code)) |> 
    mutate(entity_code = NA) |> 
    group_by(year, status, entity_code) |> 
    summarise(x4_2_cost_excl_vfr = sum(x4_2_cost_excl_vfr, na.rm = TRUE)) |> 
    ungroup() |> 
    mutate(xrate2017 = 1)
  
} else {
  data_pre_prep <- data_raw |> 
    filter(
      entity_code == mycz)
}

# prepare data ----
data_prep_split <- data_pre_prep %>% 
  filter(
    year != 20202021) %>% 
  mutate(
    mymetric = case_when (
      status == 'A' & year > max(.env$year_report, 2021) ~ NA,
      .default = x4_2_cost_excl_vfr/xrate2017
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
  pivot_wider(values_from = 'mymetric', names_from = 'status') %>% 
  mutate('Difference costs' = .[[2]] - .[[3]]) %>% 
  mutate_at(c(2:4), ~round(.,0)) %>% 
  pivot_longer(-xlabel, names_to = "name", values_to = 'mymetric') %>% 
  pivot_wider(values_from = 'mymetric', names_from = 'xlabel')

# this is not working at the moment
if (knitr::is_latex_output()) {
  mytablefontsize <- "8pt"
  mytitletablefontsize <- "9pt"
}else{
  mytablefontsize <- NULL
  mytitletablefontsize <- NULL
}

# pdf table
data_prep_pdf <- data_prep %>% 
  mutate(
    across(c(2:5), ~format(round(.x,0), nsmall =0, big.mark = ","))
  )
  

# plot table ----
table1 <- mygtable(data_prep_pdf, myfont) %>% 
  cols_label(name = html("Total costs - nominal (M€)")) %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  cols_align(columns = 1, align = "left") %>%
  tab_style(
    style = cell_text(size = mytablefontsize),  # Set font size
    locations = list(
      cells_body(),                    # Apply to the table content
      cells_column_labels()         # Apply to column labels
    )
  )|> 

  tab_header(
    title = md(paste0("**Actual and determined data**"))
  )|> 
  tab_style(
    style = cell_text(size = mytitletablefontsize),  # Set font size to 8pt
    locations = list(
      cells_title(groups = "title")   # Apply to the title
    )
  ) 


if (!knitr::is_latex_output()) {
  table1
}
