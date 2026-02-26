if (!data_loaded) {
  source("R/get_data.R")
}

# import data  ----
state_table <- state_list |> as_tibble() |> 
  filter(value != "MUAC", 
         value != rp_full, 
         value != "Network Manager") |> 
  select(State = value) |> 
  arrange(State)

data_raw_target  <- cap_trm_target
  # 
  # read_xlsx(
  # paste0(data_folder, "cap_actuals.xlsx"),
  # # here("data","hlsr2021_data.xlsx"),
  # sheet = "terminal delay targets",
  # range = cell_limits(c(1, 1), c(NA, NA))) %>%
  # as_tibble() %>% 
  # clean_names() 

data_raw_actual  <-  cap_trm_atfm_actual

# prepare data ----
data_prep_target <- data_raw_target %>% 
  filter(year == .env$year_report) |> 
  select(
    State = state,
    Target = delay_target
  ) %>% 
  filter(!is.na(Target))
  
  
data_prep_actual <- data_raw_actual %>% 
  filter(year == .env$year_report) %>% 
  mutate(average_delay = if_else  (is.na(average_delay_per_flight) == TRUE, 0, 
                                   round(average_delay_per_flight, 2))) |> 
  select(
    State = state,
    Actual = average_delay
  ) 

data_prep <- data_prep_target %>% 
  # left_join(data_prep_target, by = "State") |> 
  left_join(data_prep_actual, by = "State") |> 
  mutate(Actual = case_when(
    is.na(Target) == TRUE ~ "",
    .default = paste0("<span style='font-size:0.8rem;'>",
                        format(round(Actual,2), nsmall = 2),
                        "</span>",
                        if_else(Actual <= Target,
                                "<span style='color:green; font-weight:bold; font-size:0.8rem;'>&nbsp;&nbsp;&#10003;</span>",
                                "<span style='color:red; font-size:0.8rem;'>&nbsp;&nbsp;&#10008;</span>")
                        )
         
         ),
    Target = format(round(Target, 2), nsmall = 2)
    
    )

data_prep_pdf <- data_prep_target |> 
  # left_join(data_prep_target, by = "State") |> 
  left_join(data_prep_actual, by = "State") |> 
  mutate(
    Actual = format(round(Actual, 2), nsmall = 2),
    "_" = if_else(Actual <= Target,1,0)
  )


# plot table ----

table1 <- mygtable(data_prep, myfont)|> 
  tab_header(
    title = md("Arrival delay (min/flight)")
  ) |> 
  fmt_markdown(columns = Actual) 


if (!knitr::is_latex_output()) {
  table1
}

