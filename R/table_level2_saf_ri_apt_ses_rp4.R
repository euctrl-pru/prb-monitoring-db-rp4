if (!exists("country") | is.na(country)) {country <- rp_full
source("R/parameters.R")
}
if (!data_loaded) {
  source("R/get_data.R")
}

# import data  ----
data_raw  <-  saf_ri_actual_apt %>% 
  select(-rate) %>% 
  rename(
    name = name_apt,
    mvt = ifr_movements,
    rate = rate_per_100_000
  )

# process data ----
data_prep_mvt <- data_raw %>% 
  filter(year == year_report) %>% 
  arrange(desc(mvt)) %>% 
  mutate(rank = row_number()) %>% 
  select(
    rank,
    name,
    mvt,
    ri,
    rate
  ) %>% 
  slice(1:10)

mytype <- "TOP 10 APTs in terms of movements_"
mycolnames <- paste0(mytype, colnames(data_prep_mvt))

colnames(data_prep_mvt) <- mycolnames

data_prep_ri <- data_raw %>% 
  filter(year == year_report) %>% 
  arrange(desc(ri)) %>% 
  mutate(rank = row_number()) %>% 
  select(
    rank,
    name,
    mvt,
    ri,
    rate
  ) %>% 
  slice(1:10) 

mytype <- "TOP 10 APTs in terms of number of RIs_"
mycolnames <- paste0(mytype, colnames(data_prep_ri))

colnames(data_prep_ri) <- mycolnames

data_prep_rate <- data_raw %>% 
  filter(year == year_report) %>% 
  arrange(desc(rate)) %>% 
  mutate(rank = row_number()) %>% 
  select(
    rank,
    name,
    mvt,
    ri,
    rate
  ) %>% 
  slice(1:10) 

mytype <- "TOP 10 APTs in terms of rate of RIs_"
mycolnames <- paste0(mytype, colnames(data_prep_rate))

colnames(data_prep_rate) <- mycolnames



data_prep <- data_prep_mvt %>% 
  cbind(data_prep_ri) %>% 
  cbind(data_prep_rate) 


# Get column names of the final table
colnames_final <- colnames(data_prep)

# Define dynamic labels based on suffix
label_map <- c(
  rank = "#",
  name = "Airport name",
  mvt  = "APT mvts.",
  ri   = "Number of RI",
  rate = "Rate RI per 100,000 mvts."
)

# Extract the suffix from each column name
suffixes <- str_match(colnames_final, ".*_(rank|name|mvt|ri|rate)$")[,2]

# Map each suffix to its label
labels <- label_map[suffixes]

# Create the named vector for cols_label()
label_list <- set_names(labels, colnames_final)

label_list_pdf <- c(
  "TOP 10 APTs in terms of movements_#",
  "TOP 10 APTs in terms of movements_Airport",
  "TOP 10 APTs in terms of movements_Mvts.",
  "TOP 10 APTs in terms of movements_Total RI",
  "TOP 10 APTs in terms of movements_RI per 100,000 mvts.",
  
  "TOP 10 APTs in terms of total RIs_#",
  "TOP 10 APTs in terms of total RIs_Airport",
  "TOP 10 APTs in terms of total RIs_Mvts.",
  "TOP 10 APTs in terms of total RIs_Total RI",
  "TOP 10 APTs in terms of total RIs_RI per 100,000 mvts.",
  
  "TOP 10 APTs in terms of rate of RIs_#",
  "TOP 10 APTs in terms of rate of RIs_Airport",
  "TOP 10 APTs in terms of rate of RIs_Mvts.",
  "TOP 10 APTs in terms of rate of RIs_Total RI",
  "TOP 10 APTs in terms of rate of RIs_RI per 100,000 mvts."
  )


nowrap_cols <- grep("_mvt$|_rank$", colnames(data_prep), value = TRUE)

rank_cols <- grep("_rank$", colnames(data_prep), value = TRUE)
mvt_cols <- grep("_mvts$", colnames(data_prep), value = TRUE)
name_cols <- grep("_name$", colnames(data_prep), value = TRUE)
ri_cols <- grep("ri$", colnames(data_prep), value = TRUE)
rate_cols <- grep("rate$", colnames(data_prep), value = TRUE)

# Add nowrap spans
data_prep_web <- data_prep %>%
  mutate(across(all_of(nowrap_cols), ~ paste0(
    "<span style='white-space:nowrap;'>", 
    format(as.numeric(.), big.mark = ",", scientific = FALSE),
    "</span>"
  )))

data_prep_pdf <- data_prep %>% 
  mutate(
    across(c(3,4,8,9,13,14), ~format(round(.x,0), nsmall =0, big.mark = ","))
  ) %>% 
  mutate(
    across(c(5,10,15), ~format(round(.x,2), nsmall =2, big.mark = ","))
  )

colnames(data_prep_pdf) <- label_list_pdf

# web table ----
table1 <-mygtable(data_prep_web,
                  myfont*0.9) %>% 
  tab_spanner_delim(
    delim = "_"
  ) %>% 
  tab_header(
    title = md("**Rate of RI per 100,000 airport movements**")
  ) %>% 
  cols_align(
    align = "left",
    columns = c(2,7,12)
  ) %>% 
  fmt_number(
    columns = c(3:4, 8:9, 13:14),  # Specify the columns to format
    decimals = 0,  # Number of decimal places
    use_seps = TRUE  # Use thousands separator
  ) %>% 
  fmt_number(
    columns = c(5, 10, 15),  # Specify the columns to format
    decimals = 2
  )  %>% 
  cols_label(.list = label_list)  %>%
  fmt_markdown(columns = all_of(nowrap_cols)) 


if (!knitr::is_latex_output()) {
  table1 
  
}

