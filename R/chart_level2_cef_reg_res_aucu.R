
# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "terminal")}
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
  ## import data  ----
  data_raw  <-  read_xlsx(
    paste0(data_folder, "SES CEFF.xlsx"),
    sheet = if_else(cztype == "terminal", "SES_TRM_all", "SES_ERT_all"),
    range = cell_limits(c(1, 1), c(NA, NA))) %>%
    as_tibble() %>% 
    clean_names() 
  
  data_prep <- data_raw |> 
    filter(status == "A") |> 
    mutate(
      su_cz_combined = case_when(
        year == 2021 ~ su_cz + pull(select(filter(data_raw, year == 2020 & status =="A"), su_cz)),
        .default = su_cz),
      aucu_excluding_or = aucu_combined - other_revenues_eur_su_combined,
      
      ro_e_combined = case_when(
        year == 2021 ~ ro_e_value_eur + pull(select(filter(data_raw, year == 2020 & status =="A"), ro_e_value_eur)),
        .default = ro_e_value_eur),
      
      net_result_ansp_combined = case_when(
        year == 2021 ~ net_result_ansp + pull(select(filter(data_raw, year == 2020 & status =="A"), net_result_ansp)),
        .default = net_result_ansp),
      
      reg_res = ro_e_combined + net_result_ansp_combined,
      reg_res_per_su = reg_res/su_cz_combined,
      rr_as_perc_aucu = round(reg_res_per_su/aucu_combined * 100, 2)
    ) |> 
    filter(year>=2021) |> 
    mutate_at(c(-1), ~if_else(year > year_report, NA, .))  |> 
    mutate(xlabel = if_else(year == 2021, "2020-2021", as.character(year))) |> 
    select(
      xlabel,
      myothermetric = rr_as_perc_aucu,
      reg_res_per_su,
      aucu_excluding_or
    ) |> 
    pivot_longer(-c(xlabel, myothermetric), names_to = 'type', values_to = 'mymetric') |> 
    mutate(type = case_when(
      type == 'reg_res_per_su'  ~ 'Regulatory result per SU',
      type == 'aucu_excluding_or'  ~ 'AUCU (before other revenues)'
      ))
    
  
  
} else {
  # State  ----
  ## get data ----
  # regulatory result
  data_prep_reg_all <- regulatory_result(cztype, mycz)
  data_prep_reg <- data_prep_reg_all %>% 
    group_by(year_text, x5_4_total_su) %>% 
    summarise(reg_res = sum(regulatory_result)) %>% 
    mutate(reg_res_per_su = reg_res * 1000 / x5_4_total_su) %>% 
    select(year_text, reg_res, reg_res_per_su) %>% 
    mutate(
      reg_res = case_when(
        as.numeric(str_replace(year_text, "-", "")) > year_report & year_text != '2020-2021' ~ NA,
        .default = reg_res
      ),
      reg_res_per_su = case_when(
        as.numeric(str_replace(year_text, "-", "")) > year_report & year_text != '2020-2021' ~ NA,
        .default = reg_res_per_su
      )
      
    )
  
  # aucu
  data_prep_aucu_all <- aucu(cztype, mycz) 
  data_prep_aucu <- data_prep_aucu_all %>% 
    select(year_text, aucu_excluding_or) %>% 
    mutate (aucu_excluding_or = case_when(
      as.numeric(str_replace(year_text, "-", "")) > year_report & year_text != '2020-2021' ~ NA,
      .default = aucu_excluding_or)
    )
  
  # join tables
  data_prep <- data_prep_reg %>% 
    left_join(data_prep_aucu, by = 'year_text') %>% 
    select(-reg_res) %>% 
    mutate(rr_as_perc_aucu = reg_res_per_su / aucu_excluding_or * 100) %>% 
    pivot_longer(-c(year_text, rr_as_perc_aucu), names_to = 'type', values_to = 'mymetric') %>% 
    mutate(type = case_when(
      type == 'reg_res_per_su'  ~ 'Regulatory result per SU',
      type == 'aucu_excluding_or'  ~ 'AUCU (before other revenues)'
      ),
      xlabel = year_text,
      myothermetric = round(rr_as_perc_aucu, 2)
    ) 
}

# chart parameters ----
mysuffix <- ""
mydecimals <- 1

### trace parameters
mycolors = c( '#9DC3E6', '#FFC000')
###set up order of traces
myfactor <- c("AUCU (before other revenues)",
              "Regulatory result per SU")
myhovertemplate <- paste0('%{y:,.', mydecimals, 'f}', mysuffix)

mytextangle <- -90
mytextposition <- "inside"
myinsidetextanchor <- "middle"
mytextfont_color <- 'transparent'

### layout parameters
mybargap <- 0.25
mybarmode <- 'group'

#### title
mytitle_text <- paste0("Share of RR in AUCU")
mytitle_y <- 0.99

#### xaxis

#### yaxis
myyaxis_title <- "AUCU & RR (€/SU)"
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".0f"

#### legend
mylegend_x <- 0.5
mylegend_xanchor <- 'center'

#### margin
mylocalmargin = list(t = 60, b = 0, l = 60, r = 60)

#____additional trace parameters
myat_name <- "Share of RR in AUCU (%)"
myat_mode <- "markers"
myat_yaxis <- "y2"
myat_symbol <- NA
myat_marker_color <- '#E46C0A'
myat_line_color <- 'transparent'
myat_line_width <- mylinewidth
myat_showlegend <- T

myat_textbold <- FALSE
myat_textangle <- 0
myat_textposition <- 'top'
myat_textfont_color <- 'transparent'
myat_textfont_size <- myfont


# setup ranges to ensure zero line at same height
# https://stackoverflow.com/questions/76289470/plotly-barplot-with-two-y-axis-aligned-at-zero
# === Step 1: y1 setup ===
y1_max <- max(max(data_prep$mymetric, na.rm = TRUE), 0)
y1_min <- min(min(data_prep$mymetric, na.rm = TRUE), 0)
y1_padding <- (y1_max - y1_min) / 16
y1_range <- c(y1_min - y1_padding, y1_max + y1_padding)

# === Step 2: relative zero ===
y1_relative_zero <- (0 - y1_range[1]) / (y1_range[2] - y1_range[1])

# === Step 3: y2 data range ===
y2_data_min <- min(data_prep$myothermetric, na.rm = TRUE)
y2_data_max <- max(data_prep$myothermetric, na.rm = TRUE)
y2_data_range <- y2_data_max - y2_data_min

# === Step 4: Compute y2 total range to match y1 zero alignment ===
y2_total_range <- y2_data_range / (1 - 2 * y1_relative_zero)

# === Step 5: Apply padding symmetrically around 0 ===
y2_padding_lower <- y1_relative_zero * y2_total_range
y2_padding_upper <- (1 - y1_relative_zero) * y2_total_range
y2_range_candidate <- c(0 - y2_padding_lower, 0 + y2_padding_upper)

# === Step 6: Ensure all y2 data is visible ===
# (Adjust range only outward to include all points)
y2_range <- c(
  min(y2_range_candidate[1], y2_data_min),
  max(y2_range_candidate[2], y2_data_max)
)

# === Step 7: RECOMPUTE y2 range to force aligned zero ===
# In case we had to expand for visibility
y2_total_range <- y2_range[2] - y2_range[1]

y2_range <- c(
  floor(y2_range[1] / 5) * 5,
  ceiling(y2_range[2] / 5) * 5
)

# Recompute new y2 total range and zero position
y2_total_range <- y2_range[2] - y2_range[1]
y2_relative_zero <- (0 - y2_range[1]) / y2_total_range

# Realign y1 range to this updated relative zero
y1_total_range <- y1_range[2] - y1_range[1]
y1_range <- c(
  0 - y2_relative_zero * y1_total_range,
  0 + (1 - y2_relative_zero) * y1_total_range
)

# plot chart  ----
mybarchart(data_prep, mywidth, myheight+30, myfont, mylocalmargin, mydecimals) %>% 
  add_line_trace(., filter(data_prep, type == 'Regulatory result per SU'))  %>% 
  add_empty_trace(., data_prep) %>% 
  layout(
    yaxis = list(
      range = y1_range
    ),   # to force the zeros to coincide
    yaxis2 = list(
      title = 'RR as % of AUCU',
      zerolinecolor = '#E8E8E8',
      range = y2_range,
      # rangemode = "nonnegative",
      overlaying = "y",
      ticksuffix = '%',
      side = 'right',
      showgrid = FALSE
  ))
  
