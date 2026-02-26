if (!exists("country") | is.na(country)) {country <- rp_full
source("R/parameters.R")
}

# fix ez if script not executed from qmd file ----
if (exists("cz") == FALSE) {cz = c("1", "enroute")}
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
  
    data_prep_split <- data_raw %>% 
    filter(status == "A") |> 
    mutate(year = as.character(year)) |> 
    select(
      year,
      # duc_initially_charged_eur_combined,
      # retroactive_application_of_unit_rate_combined,
      new_duc_eur_combined,
      inflation_adjustment_eur_su,
      cecs_total_eur_su,
      trs_eur_su,
      traffic_adjustment_eur_su,
      incentives_eur_su,
      modulation_eur_su,
      cross_financing_eur_su,
      other_revenues_eur_su_combined,
      unit_rate_reduction_su,
      total_adjustments_su_combined,
      aucu_combined
      ) |> 
      mutate('AUCU vs. DUC' = aucu_combined/new_duc_eur_combined-1 ) |> 
      pivot_longer(-c(year), names_to = "type", values_to = "value")
  
  data_prep2020_2021 <- data_prep_split %>% 
    filter(
      year < 2022) %>% 
    group_by(type) |> 
    summarise(value = sum(value, na.rm = TRUE)) |> 
    mutate(year = "2020-2021") 
  
  data_prep <- data_prep_split |> 
    rbind(data_prep2020_2021) |> 
    filter(year == if_else(year_report == 2020 | year_report == 2021, "2020-2021", as.character(year_report))) |> 
    select(-year) |> 
    mutate(value = round(value, 2),
           type = factor(type, levels = c(
                                    # "duc_initially_charged_eur_combined",
                                    # "retroactive_application_of_unit_rate_combined",
                                    "new_duc_eur_combined",
                                    "inflation_adjustment_eur_su",
                                    "cecs_total_eur_su",
                                    "trs_eur_su",
                                    "traffic_adjustment_eur_su",
                                    "incentives_eur_su",
                                    "modulation_eur_su",
                                    "cross_financing_eur_su",
                                    "other_revenues_eur_su_combined",
                                    "unit_rate_reduction_su",
                                    "total_adjustments_su_combined",
                                    "aucu_combined",
                              "AUCU vs. DUC"))) |> 
  arrange(type) |> 
    mutate(
           type = c( 
             # 'Initial DUC charged',
                      # 'DUC to be charged retroactively',
                      'DUC',
                      'Inflation adjustment',
                      'Cost exempt from cost-sharing',
                      'Traffic risk sharing adjustment',
                      'Traffic adj. (costs not TRS)',
                      'Finantial incentives',
                      'Modulation of charges',
                      'Cross-financing',
                      'Other revenues',
                      'Application of lower unit rate',
                      'Total adjustments',
                      'AUCU',
                      'AUCU vs. DUC')
    ) 
  
  
} else {
  # State  ----
  ## import data  ----
  data_calc <- aucu(cztype, mycz)
  
  ## prep data ----
  data_prep <- data_calc %>% 
    filter(year_text == if_else(year_report == 2021 | year_report == 2020, "2020-2021", as.character(year_report))) %>% 
    select(
      # initial_duc,
      # retro_ur,
      new_duc,
      infl_adj,
      dif_a_d_costs,
      trs_adj,
      dc_notrs,
      fin_inc,
      rev_c_mod,
      cross_fin,
      other_rev,
      loss_rev,
      total_adjustments_aucu,
      aucu
    ) %>% 
    rename(
      # 'Initial DUC charged' = initial_duc,
      # 'DUC to be charged retroactively' = retro_ur,
      DUC = new_duc,
      'Inflation adjustment' = infl_adj,
      'Cost exempt from cost-sharing' = dif_a_d_costs,
      'Traffic risk sharing adjustment' = trs_adj,
      'Traffic adj. (costs not TRS)' = dc_notrs,
      'Finantial incentives' = fin_inc,
      'Modulation of charges' = rev_c_mod,
      'Cross-financing' = cross_fin,
      'Other revenues' = other_rev,
      'Application of lower unit rate' = loss_rev,
      'Total adjustments' = total_adjustments_aucu,
      AUCU = aucu
    ) %>% 
    mutate('AUCU vs. DUC' = AUCU/DUC-1 ) %>% 
    pivot_longer(cols = everything() ,names_to = "type", values_to = "value")
}
  

# pdf table ----
data_prep_pdf <- data_prep %>% 
  mutate(
    across(2, ~if_else(type == 'AUCU vs. DUC',
              paste0(if_else(.x >=0, "+", ""),
                format(round(.x*100,1), nsmall =1, big.mark = ","),
                "%"
                ),
             format(round(.x,2), nsmall =2, big.mark = ","))
    )
  )

# plot table  ----
table1 <- mygtable(data_prep_pdf, myfont*0.95) %>% 
  cols_label(type = paste0("Components of the AUCU in ", 
                           if_else(year_report == 2021 | year_report == 2020, "2020-2021", as.character(year_report))), 
             value = "€/SU") %>% 
  tab_options(column_labels.background.color = "#F2F2F2",
              column_labels.font.weight = 'bold',
              container.padding.y = 0) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = c(1,12, 13)
    )
  ) %>% 
  tab_header(
    title = md(paste0("**AUCU components (€/SU) – ",
                      if_else(year_report == 2021 | year_report == 2020, "2020-2021", as.character(year_report)), "**"))
  )
  
if (!knitr::is_latex_output()) {
 table1
}
