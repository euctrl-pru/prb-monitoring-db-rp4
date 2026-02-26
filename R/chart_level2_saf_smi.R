############### state level adapted to RP4, not SES

# import data  ----
if (country == rp_full){
  ## SES case ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES file.xlsx"),
    # here("data","hlsr2021_data.xlsx"),
    sheet = "SMI - occurrences",
    range = cell_limits(c(1, 1), c(NA, 5))) %>%
    as_tibble() %>% 
    clean_names() |> 
    mutate(state = rp_full)
    
} else  {
  ## State case ----
  if (!data_loaded) {
    source("R/get_data.R")
  }
  
  data_raw  <-  saf_smi_actual
}

# prepare data ----

data_prep <- data_raw %>%
  filter(
    state == country,
    year <= year_report) |> 
  select(-state, -type, -reference_period) |> 
  pivot_longer(-c(year), names_to = "type", values_to = "mymetric") |> 
  mutate(xlabel = year,
         type = if_else(type == "rate_per_100_000",
                        "Rate of SMI",
                        "EU Wide Average"),
         textposition = "top center",
         linedash = if_else(type == "Rate of SMI",
                            "solid",
                            if_else(country == rp_full,
                                    "solid",
                                    "dot")
         )
  ) |> 
  select(xlabel, type, mymetric, textposition, linedash) |> 
  #otherwise the lindash column does not work
  arrange(desc(linedash))


# chart parameters ----
c_suffix <- ""
c_decimals <- 2

### trace parameters
local_color <- PRBActualColor
c_colors = c(local_color, local_color)

###set up order of traces
c_factor <- c("Rate of RI", "EU Wide Average")

c_hovertemplate <- paste0('%{y:,.', c_decimals, 'f}', c_suffix)

#### title
c_title_text <-  paste0("SMIs per 100,000 flight hours")

#### yaxis
c_yaxis_title <- "SMIs per 100,000 flight hours"
c_yaxis_tickformat <- ".1f"

# plot chart ----
##I had to do it this way because when there are NA values the dash doesn't work
data_prep_s1 <- data_prep |> filter(type == "EU Wide Average")
data_prep_s2 <- data_prep |> filter(type == "Rate of SMI") |> 
  mutate(myothermetric = round(mymetric,2))

p1 <- mylinechart2(data_prep_s1, 
                   height = myheight,
                   colors = c_colors,
                   local_factor = c_factor,
                   suffix = c_suffix,
                   decimals = c_decimals,
                   
                   hovertemplate = c_hovertemplate,
                   
                   title_text = c_title_text,
                   
                   yaxis_title = c_yaxis_title,
                   yaxis_ticksuffix = c_suffix,
                   yaxis_tickformat = c_yaxis_tickformat
) %>% 
  layout(yaxis = list(rangemode = "tozero"),
         xaxis = list(range = c(rp_min_year-0.5, rp_max_year+0.5)))

p1 %>%  add_line_trace2(., data_prep_s2,
                        name = "Rate of SMI",
                        textfontcolor = "black",
                        markercolor = local_color,
                        linecolor = local_color
                        
) |> 
  layout(legend=list(
    traceorder= "reversed"))

