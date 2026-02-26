# import data  ----
state_table <- state_list |> as_tibble() |> 
  filter(value != "MUAC", 
         value != "Luxembourg", 
         value != rp_full, 
         value != "Network Manager") |> 
  select(State = value)

data_raw_target  <-  kea_target

data_raw_actual  <-  kea_actual
  
# prepare data ----
data_prep_target <- data_raw_target %>% 
  filter(year == year_report) |> 
  mutate(
    # type = 'Target',
    target = round(kea_target, 2)
  ) %>% 
  select(
    State = state,
    Target = target
  ) 
  
data_prep_actual <- data_raw_actual %>% 
  filter(year == year_report) %>% 
  mutate (Actual = round(value,2)) %>%
  select(
    State = state,
    Actual
  ) 

data_prep <- state_table |> 
  left_join(data_prep_target, by = "State") |> 
  left_join(data_prep_actual, by = "State") |> 
  mutate(Actual = paste0("<span style='font-size:0.8rem;'>",
                        format(Actual, nsmall = 2),
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
    Assessment = if_else(Actual <= Target,0,1)
  )
  


# plot table ----

table1 <- mygtable(data_prep, myfont)|> 
  tab_header(
    title = md("KEA (%)")
  ) |> 
  fmt_markdown(columns = Actual) 



if (!knitr::is_latex_output()) {
  table1
}
