if (!data_loaded) {
  source("R/get_data.R")
}

# import data  ----
state_table <- state_list |> as_tibble() |> 
  filter(value != "MUAC", 
         value != "Luxembourg", 
         value != rp_full, 
         value != "Network Manager") |> 
  select(State = value)

data_raw_target  <-  cap_ert_target

data_raw_actual  <-  cap_ert_atfm_actual

# prepare data ----
data_prep_target <- data_raw_target %>% 
  filter(year == .env$year_report) |> 
  select(
    State = state,
    Target = delay_target
  ) 
  
data_prep_actual <- data_raw_actual %>% 
  filter(year == .env$year_report) %>% 
  mutate(average_delay = if_else  (is.na(average_delay_per_flight) == TRUE, 0, average_delay_per_flight)) |> 
  select(
    State = state,
    Actual = average_delay
  ) 

data_prep <- state_table |> 
  left_join(data_prep_target, by = "State") |> 
  left_join(data_prep_actual, by = "State") |> 
  mutate(Actual = paste0("<span style='font-size:0.8rem;'>",
                        format(round(Actual,2), nsmall = 2),
                        "</span>",
                        if_else(Actual <= Target,
                                "<span style='color:green; font-weight:bold; font-size:0.8rem;'>&nbsp;&nbsp;&#10003;</span>",
                                "<span style='color:red; font-size:0.8rem;'>&nbsp;&nbsp;&#10008;</span>")
                        )
         
         )

data_prep_pdf <- state_table |> 
  left_join(data_prep_target, by = "State") |> 
  left_join(data_prep_actual, by = "State") |> 
  mutate(
    Actual = format(round(Actual, 2), nsmall = 2),
    Target = format(round(Target, 2), nsmall = 2),
         "_" = if_else(Actual <= Target,1,0)
  )

 
# plot table ----

table1 <- mygtable(data_prep, myfont)|> 
  tab_header(
    title = md("En route delay (min/flight)")
  ) |> 
  fmt_markdown(columns = Actual) 



if (!knitr::is_latex_output()) {
  table1
}