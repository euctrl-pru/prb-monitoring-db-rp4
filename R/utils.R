# libraries ----
# library(webshot)
# library(data.table)
# library(here)
# library(fs)
# library(purrr)
# library(plotly)
# library(gt)

# functions ----
## right x characters function ----
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

## read range function ----
read_range <- function(file, sheet, range){
    read_excel(
      file,
      sheet = sheet,
      range = range) %>%
      mutate_all(~ str_replace_all(., "\r\n\r\n", "\r\n")) %>%
      mutate_all(~ str_replace_all(., "<br><br><br><br>", "<br><br>"))
  }

## read table range function ----
read_mytable <- function(file, sheet, table){
  wb <- loadWorkbook(paste0(data_folder, file))
  tables <- getTables(wb, sheet = sheet)
  # get the range
  table_range <- names(tables[tables == table])
  # read range
  read_range(paste0(data_folder, file), sheet, table_range) %>% clean_names()
  }

## add columns to df if they don't exist ----
#   add_cols <- function(df, cols) {
#     add <- cols[!cols %in% names(df)]
#     if (length(add) != 0) {
#       df[add] <- NA
#     }
#     return(df)
#   }
# 
# 
## aucu calculations ----
aucu <- function(cztype, mycz) {
## import data  ----
  if(cztype == "enroute") {
    data_raw_t1  <- ceff_t1_ert %>% filter(entity_type == "ECZ")
    data_raw_t2  <- ceff_t2_ert %>% filter(entity_type == "ECZ")
    data_raw_t3  <- ceff_t3_ert %>% filter(entity_type == "ECZ")
  } else {
    data_raw_t1  <- ceff_t1_trm %>% filter(entity_type == "TCZ")
    data_raw_t2  <- ceff_t2_trm %>% filter(entity_type == "TCZ")
    data_raw_t3  <- ceff_t3_trm %>% filter(entity_type == "TCZ")
  }

  ## prepare data ----
  # filter raw tables 
  ## t1
  data_prep_t1_a <- data_raw_t1 %>%
    filter(
      # entity_code == mycz,
      status == 'A'
    )

  colnames(data_prep_t1_a) <- paste('a', colnames(data_prep_t1_a), sep = '_')

  data_prep_t1_d <- data_raw_t1 %>%
      filter(
        status == 'D'
      )
  colnames(data_prep_t1_d) <- paste('d', colnames(data_prep_t1_d), sep = '_')

  # data_prep_t1 <- cbind(data_prep_t1_a, data_prep_t1_d) |> rename(year = a_year)
  
  data_prep_t1 <- data_prep_t1_d %>% 
    left_join(data_prep_t1_a, by = c("d_year" = "a_year", "d_entity_code"= "a_entity_code")) %>%
    rename(entity_code = d_entity_code, year = d_year)


  ## t2
  data_prep_t2 <- data_raw_t2 
  # %>%
  #   filter(
  #     entity_code == mycz
  #   )

  if (cztype == "terminal") {
    data_prep_t2 <- data_prep_t2 |>
      mutate(x8_1_temp_unit_rate = x8_1_temp_unit_rate/1000)

  }
  
#   NOTE -----
  #temp table with values for 2020 and 2021 separated that I'll need later for aucu calculations
  #first I need the 10.5 and 4.7 value separated also for the calculations

  # other_rev_20202021 <- data_prep_t2_ini %>% filter(year == 20202021) %>% select(x10_5_other_revenue) %>% pull()
  # total_su_t2_20202021<- data_prep_t2_ini %>% filter(year == 20202021) %>% select(x4_7_total_su) %>% pull()
  # 
  # data_temp_t2 <- data_prep_t2_ini %>%
  #   filter(year == 20202021) %>%
  #   mutate_all(~ if_else(is.numeric(.x),NA,.x)) %>%
  #   mutate(year = 2020,
  #          x10_5_other_revenue = other_rev_20202021,
  #          x4_7_total_su= total_su_t2_20202021
  #   ) %>%
  #   rbind(filter(data_prep_t2_ini, year == 20202021)) %>%
  #   mutate(year = if_else(year == 20202021, 2021, year))
  # 
  # data_prep_t2 <- data_prep_t2_ini %>% rbind(data_temp_t2)

  ## t2 terminal temp ur / forecast su
  data_raw_temp_ur_prep_t2 <- cef_temp_su_t2 
  # %>%
  #   filter(
  #     entity_code == mycz
  #   )

  # complete t2 terminal table with temporary values that are not yet in the DB
  data_prep_t2 <- data_prep_t2 |>
    left_join(data_raw_temp_ur_prep_t2, by = c("entity_code", "year"))

    ## t3
  data_prep_t3 <- data_raw_t3 %>%
    filter(
      # entity_code == mycz,
      year != 'After RP' & year != 'Amounts'
    ) %>%
    mutate(year = as.numeric(year))

  ## t exchange rates
  data_prep_xrates <- xrate_year %>%
    # filter(
    #   cz_code == mycz
    # ) %>% 
    select(year, xrate, entity_code = cz_code)

  #join all tables
  data_prep_all <- data_prep_t1 %>%
    left_join(data_prep_t2, by = c('year', 'entity_code'), suffix = c(".t1", ".t2")) %>%
    left_join(data_prep_t3, by = c('year', 'entity_code'), suffix = c("", ".t3")) %>%
    left_join(data_prep_xrates, by = c('year', 'entity_code')) %>% as_tibble()


  # get some parameters for 2020 and 2021. Needed later for calcs
  # initial_duc_2020 <- data_prep_t2 %>%
  #   filter(year == 20202021) %>%
  #   select(x15_unit_rate_temp_2020) %>% pull()
  # 
  # initial_duc_2021 <- data_prep_t2 %>%
  #   filter(year == 20202021) %>%
  #   select(x15_unit_rate_temp_2021) %>% pull()
  # 
  # tsu_2020 <- data_prep_t1 %>%
  #   filter(year == 2020) %>%
  #   select(a_x5_4_total_su) %>% pull()

  # tsu_2021 <- data_prep_t1 %>%
  #   filter(year == 2021) %>%
  #   select(a_x5_4_total_su) %>% pull()
  # 
  # tsu_20202021_a <- data_prep_t1 %>%
  #   filter(year == 20202021) %>%
  #   select(a_x5_4_total_su) %>% pull()
  # 
  # tsu_20202021_d <- data_prep_t1 %>%
  #   filter(year == 20202021) %>%
  #   select(d_x5_4_total_su) %>% pull()

  # create table with forecast sus and format it so it can be used in calcs
  # data_prep_forecast_su <- data_prep_all %>%
  #   add_cols(., c('x15_forecast_su_temp_2022',       # add missing columns
  #                 'x15_forecast_su_temp_2023',
  #                 'x15_forecast_su_temp_2024')
  #   ) %>%
  #   select(contains('x15_forecast_su_temp_' )) %>%
  #   pivot_longer(cols = starts_with("x15_forecast_su_temp_"),
  #                names_to = "year",
  #                values_to = 'x15_forecast_su_temp') %>%
  #   mutate(year = str_replace_all(year, 'x15_forecast_su_temp_', ''),
  #          year = as.numeric(year),
  #          x15_forecast_su_temp = if_else(x15_forecast_su_temp == 0, NA, x15_forecast_su_temp),
  #   ) %>%
  #   group_by(year) %>%
  #   summarise(x15_forecast_su_temp = min(x15_forecast_su_temp, na.rm = TRUE)) %>%
  #   mutate(x15_forecast_su_temp = if_else(x15_forecast_su_temp == Inf, NA, x15_forecast_su_temp),
  #   )

  # add new forecast sus to full table
  # data_prep_all <- data_prep_all 
  # %>%
  #   left_join(data_prep_forecast_su, by = 'year')
  
  # select relevant columns to prepare for SES calcs
  data_prep_selected <- data_prep_all %>% 
    select(
      entity_code,
      year,
      xrate,
      x8_1_temp_unit_rate,
      total_adjustment,
      x15_forecast_su_temp,
      d_x4_2_cost_excl_vfr,
      d_x5_4_total_su,
      
      x4_7_total_su,
      x2_5_adjust_inflation,
      x3_8_diff_det_cost_actual_cost,
      x4_9_adjust_traffic_risk_art_27_2,
      x5_1_det_cost_no_traffic_risk,
      x6_4_financial_incentive,
      x7_1_adj_revenue_charge_modulation,
      x9_1_cross_financing_other,
      x10_5_other_revenue,
      x11_1_loss_revenue_lower_unit_rate,
      
      x12_total_adjust,
      x8_2_diff_revenue_temp_unit_rate,
      x5_2_unit_rate_no_traffic_risk
    )
  
  # transform to euros so we can sum for SES
  data_prep_euro_ses <- data_prep_selected %>% 
    mutate(
      total_adjustment = total_adjustment / xrate,
      d_x4_2_cost_excl_vfr = d_x4_2_cost_excl_vfr / xrate,
      
      x2_5_adjust_inflation = x2_5_adjust_inflation / xrate,
      x3_8_diff_det_cost_actual_cost = x3_8_diff_det_cost_actual_cost / xrate,
      x4_9_adjust_traffic_risk_art_27_2 = x4_9_adjust_traffic_risk_art_27_2 / xrate,
      x5_1_det_cost_no_traffic_risk = x5_1_det_cost_no_traffic_risk / xrate,
      x6_4_financial_incentive = x6_4_financial_incentive / xrate,
      x7_1_adj_revenue_charge_modulation = x7_1_adj_revenue_charge_modulation / xrate,
      x9_1_cross_financing_other = x9_1_cross_financing_other / xrate,
      x10_5_other_revenue = x10_5_other_revenue / xrate,
      x11_1_loss_revenue_lower_unit_rate = x11_1_loss_revenue_lower_unit_rate / xrate,
      
      x12_total_adjust = x12_total_adjust / xrate,
      x8_2_diff_revenue_temp_unit_rate = x8_2_diff_revenue_temp_unit_rate / xrate,
      x5_2_unit_rate_no_traffic_risk = x5_2_unit_rate_no_traffic_risk / xrate
    ) %>% 
    group_by(year) %>% 
    summarise(
      # x8_1_temp_unit_rate,
      total_adjustment = sum(total_adjustment, na.rm = TRUE),
      x15_forecast_su_temp = sum(x15_forecast_su_temp, na.rm = TRUE),
      d_x4_2_cost_excl_vfr = sum(d_x4_2_cost_excl_vfr, na.rm = TRUE),
      d_x5_4_total_su = sum(d_x5_4_total_su, na.rm = TRUE),
      
      x4_7_total_su = sum(x4_7_total_su, na.rm = TRUE),
      x2_5_adjust_inflation = sum(x2_5_adjust_inflation, na.rm = TRUE),
      x3_8_diff_det_cost_actual_cost = sum(x3_8_diff_det_cost_actual_cost, na.rm = TRUE),
      x4_9_adjust_traffic_risk_art_27_2 = sum(x4_9_adjust_traffic_risk_art_27_2, na.rm = TRUE),
      x5_1_det_cost_no_traffic_risk = sum(x5_1_det_cost_no_traffic_risk, na.rm = TRUE),
      x6_4_financial_incentive = sum(x6_4_financial_incentive, na.rm = TRUE),
      x7_1_adj_revenue_charge_modulation = sum(x7_1_adj_revenue_charge_modulation, na.rm = TRUE),
      x9_1_cross_financing_other = sum(x9_1_cross_financing_other, na.rm = TRUE),
      x10_5_other_revenue = sum(x10_5_other_revenue, na.rm = TRUE),
      x11_1_loss_revenue_lower_unit_rate = sum(x11_1_loss_revenue_lower_unit_rate, na.rm = TRUE),
      
      x12_total_adjust = sum(x12_total_adjust, na.rm = TRUE),
      x8_2_diff_revenue_temp_unit_rate = sum(x8_2_diff_revenue_temp_unit_rate, na.rm = TRUE),
      x5_2_unit_rate_no_traffic_risk = sum(x5_2_unit_rate_no_traffic_risk, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(
      entity_code = "SES",
      year,
      xrate = 1,
      x8_1_temp_unit_rate = 0, 
    ) %>% 
    select(
      entity_code,
      year,
      xrate,
      x8_1_temp_unit_rate,
      total_adjustment,
      x15_forecast_su_temp,
      d_x4_2_cost_excl_vfr,
      d_x5_4_total_su,
      
      x4_7_total_su,
      x2_5_adjust_inflation,
      x3_8_diff_det_cost_actual_cost,
      x4_9_adjust_traffic_risk_art_27_2,
      x5_1_det_cost_no_traffic_risk,
      x6_4_financial_incentive,
      x7_1_adj_revenue_charge_modulation,
      x9_1_cross_financing_other,
      x10_5_other_revenue,
      x11_1_loss_revenue_lower_unit_rate,
      
      x12_total_adjust,
      x8_2_diff_revenue_temp_unit_rate,
      x5_2_unit_rate_no_traffic_risk
    )
    
  # join SES with the rest of the table
  data_prep_selected_ses <- data_prep_selected %>% rbind(data_prep_euro_ses)
  
  # calcs
  ## calculate all values for individual years following the indications in the CEFF computations file
  data_prep <- data_prep_selected %>%
    mutate_all(~ ifelse(is.na(.), 0, .)) %>%
    mutate(
      initial_duc = if_else(x8_1_temp_unit_rate >0,
                           x8_1_temp_unit_rate - (total_adjustment/x15_forecast_su_temp),
                           d_x4_2_cost_excl_vfr/d_x5_4_total_su
                           ),
      initial_duc = initial_duc / xrate,
      new_duc = d_x4_2_cost_excl_vfr / d_x5_4_total_su / xrate,
      retro_ur = new_duc - initial_duc,

      infl_adj = x2_5_adjust_inflation / x4_7_total_su / xrate,
      dif_a_d_costs = x3_8_diff_det_cost_actual_cost / x4_7_total_su / xrate,
      trs_adj = x4_9_adjust_traffic_risk_art_27_2 / x4_7_total_su / xrate,
      dc_notrs = x5_1_det_cost_no_traffic_risk / x4_7_total_su / xrate,
      fin_inc = x6_4_financial_incentive / x4_7_total_su / xrate,
      rev_c_mod = x7_1_adj_revenue_charge_modulation / x4_7_total_su / xrate,
      cross_fin = x9_1_cross_financing_other / x4_7_total_su / xrate,
      other_rev = x10_5_other_revenue / x4_7_total_su / xrate,
      loss_rev = x11_1_loss_revenue_lower_unit_rate / x4_7_total_su / xrate,

      total_adjustments_aucu = infl_adj + dif_a_d_costs + trs_adj + dc_notrs + fin_inc + rev_c_mod + cross_fin + other_rev + loss_rev,
      aucu = new_duc + total_adjustments_aucu,

      aucu_excluding_or = aucu - other_rev,

      check_adj = (x12_total_adjust - x8_2_diff_revenue_temp_unit_rate - x5_2_unit_rate_no_traffic_risk) / x4_7_total_su / xrate

    ) %>%
    select(
      entity_code,
      year,
      x4_7_total_su,
      d_x5_4_total_su,

      x8_1_temp_unit_rate,
      x15_forecast_su_temp,
      total_adjustment,
      initial_duc,
      retro_ur,
      new_duc,
      retro_ur,
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
      check_adj,
      aucu,
      aucu_excluding_or
    ) %>%
    arrange(year)

  ## calculate values 2020-2021 as a sum of the individual years
  # data_prep_20202021 <- data_prep_years_split %>%
  #   filter(year_text == '2020' | year_text == '2021') %>%
  #   bind_rows(summarise(., across(where(is.numeric), sum),
  #                       across(where(is.character), ~'2020-2021'))) %>%
  #   filter(year_text == '2020-2021') %>%
  #   mutate(x4_7_total_su = x4_7_total_su/2)  # I didn't want to sum this one

  ## join prep tables with the relevant years
  aucu_data <- data_prep %>% 
    filter(
      entity_code == mycz,
    )

  return(aucu_data)
  }

## regulatory result calculations ----
regulatory_result <- function(cztype, mycz) {
  ## import data  ----
  if(cztype == "enroute") {
    data_raw_t1  <- ceff_t1_ert
    data_raw_t2  <- ceff_t2_ert
    
    tsus <- data_raw_t1 %>%
      filter(entity_type == "ECZ",
             # entity_code == if_else(cztype == "enroute", ecz_list$ecz_id[ez], tcz_list$tcz_id[ez]),
             status == 'A') %>%
      select(year, charging_zone_code, x5_4_total_su)
    
  } else {
    data_raw_t1  <- ceff_t1_trm
    data_raw_t2  <- ceff_t2_trm
    
    tsus <- data_raw_t1 %>%
      filter(entity_type == "TCZ",
        # entity_code == if_else(cztype == "enroute", ecz_list$ecz_id[ez], tcz_list$tcz_id[ez]),
             status == 'A') %>%
      select(year, charging_zone_code, x5_4_total_su)
  }
  
  # get exchange rates
  data_prep_xrates <- xrate_year %>%
    filter(
      # cz_code == mycz
    ) %>% 
    select(year, xrate, charging_zone_code = cz_code)
  
  ## prepare data ----
  data_filtered_t1 <- data_raw_t1 %>%
    filter(
      #we filter on the cz_code instead of entity_code to get all entities
      # charging_zone_code == mycz,
      #we keep only ANSPs
      entity_type %in% c('ANSP', 'MET', 'MUAC')
    )

  #subtable for the ex post roe calc
  data_prep_t1_selected <- data_filtered_t1 %>%
    select(year,
           status,
           entity_type,
           charging_zone_code,
           entity_type_id,
           entity_name,
           entity_code,
           x3_4_total_assets,
           x3_8_share_of_equity_perc,
           x3_6_return_on_equity_perc,
           x4_2_cost_excl_vfr)
  
  data_prep_t1_ses <- data_prep_t1_selected %>% 
    left_join(data_prep_xrates, by = c("charging_zone_code", "year")) %>% 
    group_by(year, status, xrate) %>% 
    summarise(
      x3_4_total_assets = sum(x3_4_total_assets, na.rm = TRUE),
      x3_8_share_of_equity_perc = sum(x3_8_share_of_equity_perc, na.rm = TRUE),
      x3_6_return_on_equity_perc = sum(x3_6_return_on_equity_perc, na.rm = TRUE),
      x4_2_cost_excl_vfr = sum(x4_2_cost_excl_vfr, na.rm = TRUE),
      .groups = "drop"
      ) %>% 
    mutate(
      entity_type = if_else(cztype == "enroute", "ECZ", "TCZ"),
      charging_zone_code = "SES",
      entity_type_id = "SES",
      entity_name = rp_full,
      entity_code = "SES",
      x3_4_total_assets = x3_4_total_assets/xrate,
      x3_8_share_of_equity_perc = x3_8_share_of_equity_perc/xrate,
      x3_6_return_on_equity_perc = x3_6_return_on_equity_perc/xrate,
      x4_2_cost_excl_vfr = x4_2_cost_excl_vfr / xrate
    ) %>% 
    select(
      year,
      status,
      entity_type,
      charging_zone_code,
      entity_type_id,
      entity_name,
      entity_code,
      x3_4_total_assets,
      x3_8_share_of_equity_perc,
      x3_6_return_on_equity_perc,
      x4_2_cost_excl_vfr
    )
  
  data_prep_t1_selected_ses <- data_prep_t1_selected %>% rbind(data_prep_t1_ses)
  
  data_prep_t1_1  <- data_prep_t1_selected_ses %>% 
    mutate(
      roe = x3_4_total_assets * x3_8_share_of_equity_perc * x3_6_return_on_equity_perc,
    ) %>%
    select(-c(x3_4_total_assets, x3_8_share_of_equity_perc, x3_6_return_on_equity_perc)) %>%
    pivot_wider(names_from = 'status',
                values_from = 'roe') %>%
    rename(ex_ante_roe_nc = D,
           ex_post_roe_nc = A)

  #subtable for the calc of Difference in costs: gain (+)/Loss (-) retained/borne by the ATSP
  data_prep_t1_2 <- data_prep_t1_selected_ses %>%
    select(year,
           entity_code,
           status,
           x4_2_cost_excl_vfr
    ) %>%
    mutate(
      dif_cost_gain_loss = case_when(
        # we are calculating D-A costs for years<= year_report
        status == 'A' ~ (-1)*x4_2_cost_excl_vfr,
        .default = x4_2_cost_excl_vfr),

      x4_2_cost_excl_vfr_d = case_when(
        # we are calculating D-A costs for years<= year_report
        status == 'D' ~ x4_2_cost_excl_vfr,
        .default = 0)
    ) %>%
    group_by(year, entity_code) %>%
    summarise(
      dif_cost_gain_loss = sum(dif_cost_gain_loss),
      x4_2_cost_excl_vfr_d = sum(x4_2_cost_excl_vfr_d),
      .groups = "drop"
    )


  #subtable for the calc actual revenues
  data_prep_t1_3 <- data_prep_t1_selected_ses %>%
    filter(status == 'A') %>%
    select(year,
           entity_code,
           status,
           x4_2_cost_excl_vfr
           )

  # join the t1 subtables
  data_prep_t1 <- data_prep_t1_1 %>%
    left_join(data_prep_t1_2, by = c("year", "entity_code")) %>%
    left_join(data_prep_t1_3, by = c("year", "entity_code"))

  # extract data from t2
  data_prep_t2_all <- data_raw_t2 %>%
    filter(
      #we filter on the cz_code instead of entity_code to get all entities
      # charging_zone_code == mycz,
      #we keep only ANSPs
      entity_type %in% c('ANSP', 'MET', 'MUAC'),
      #we need actuals
      status == 'A',
    ) %>% 
    select(
      year, 
      charging_zone_code,
      entity_code,
      x4_1_det_cost_traffic_risk,
      x4_6_total_su_forecast,
      x4_7_total_su,
      x4_9_adjust_traffic_risk_art_27_2,
      
      x2_5_adjust_inflation,
      x3_8_diff_det_cost_actual_cost,
      x6_4_financial_incentive
    )
  
  data_prep_t2_ses <- data_prep_t2_all %>% 
    left_join(data_prep_xrates, by = c("charging_zone_code", "year")) %>% 
    group_by(year, xrate) %>% 
    summarise(
      x4_1_det_cost_traffic_risk = sum(x4_1_det_cost_traffic_risk, na.rm = TRUE),
      x4_6_total_su_forecast = sum(x4_6_total_su_forecast, na.rm = TRUE),
      x4_7_total_su = sum(x4_7_total_su, na.rm = TRUE),
      x4_9_adjust_traffic_risk_art_27_2 =  sum(x4_9_adjust_traffic_risk_art_27_2, na.rm = TRUE),
      
      x2_5_adjust_inflation = sum(x2_5_adjust_inflation, na.rm = TRUE),
      x3_8_diff_det_cost_actual_cost = sum(x3_8_diff_det_cost_actual_cost, na.rm = TRUE),
      x6_4_financial_incentive = sum(x6_4_financial_incentive, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(
      charging_zone_code = "SES",
      entity_code = "SES",
      x4_1_det_cost_traffic_risk = x4_1_det_cost_traffic_risk / xrate,
      x4_9_adjust_traffic_risk_art_27_2 = x4_9_adjust_traffic_risk_art_27_2 / xrate,
      x2_5_adjust_inflation = x2_5_adjust_inflation / xrate,
      x3_8_diff_det_cost_actual_cost = x3_8_diff_det_cost_actual_cost / xrate,
      x6_4_financial_incentive = x6_4_financial_incentive / xrate,
    ) %>% 
    select(
      year, 
      charging_zone_code,
      entity_code,
      x4_1_det_cost_traffic_risk,
      x4_6_total_su_forecast,
      x4_7_total_su,
      x4_9_adjust_traffic_risk_art_27_2,
      
      x2_5_adjust_inflation,
      x3_8_diff_det_cost_actual_cost,
      x6_4_financial_incentive
    )
      
  data_prep_t2_all_ses <- data_prep_t2_all %>% rbind(data_prep_t2_ses)
  
  data_prep_t2 <- data_prep_t2_all_ses %>% 
    mutate(
      trs = (x4_7_total_su / x4_6_total_su_forecast -1) * x4_1_det_cost_traffic_risk + x4_9_adjust_traffic_risk_art_27_2
      ) %>%
    select(year,
           entity_code,
           x2_5_adjust_inflation,
           x3_8_diff_det_cost_actual_cost,
           trs,
           x6_4_financial_incentive)

  # join t1 and t2 for joint calculations
  data_prep_years_split <- data_prep_t1 %>%
    left_join(data_prep_t2, by = c("year", "entity_code")) %>%
    rowwise() %>%
    mutate(
      atsp_gain_loss_cost_sharing = sum(dif_cost_gain_loss, x2_5_adjust_inflation, x3_8_diff_det_cost_actual_cost, na.rm = TRUE),
      total_net_gain_loss = sum(atsp_gain_loss_cost_sharing, trs, x6_4_financial_incentive, na.rm = TRUE),
      regulatory_result_nc = sum(total_net_gain_loss, ex_post_roe_nc, na.rm = TRUE),
      actual_revenues_nc = sum(x4_2_cost_excl_vfr, total_net_gain_loss, na.rm = TRUE)
    ) %>%
    select(-entity_type, -charging_zone_code, -entity_name, -entity_code) %>%
    mutate(type = case_when(
      entity_type_id == 'ANSP1'  ~ 'Main ANSP',
      stringr::str_detect(entity_type_id, 'MET')  ~ 'MET',
      .default = 'Other ANSP'
    )) %>%
    group_by(year, type) %>%
    # the plot function already divides by 1000
    summarise(
      atsp_gain_loss_cost_sharing_nc = sum(atsp_gain_loss_cost_sharing)/10^3,
      trs_nc = sum(trs) /10^3,
      financial_incentive_nc = sum(x6_4_financial_incentive)/10^3,
      regulatory_result_nc = sum(regulatory_result_nc)/10^3,
      ex_ante_roe_nc = sum(ex_ante_roe_nc)/10^3,
      ex_post_roe_nc = sum(ex_post_roe_nc)/10^3,
      actual_revenues_nc = sum(actual_revenues_nc)/10^3,
      x4_2_cost_excl_vfr_d_nc = sum(x4_2_cost_excl_vfr_d)/10^3,
      .groups = "drop"
              ) %>%
    select(year,
           type,
           regulatory_result_nc,
           ex_ante_roe_nc,
           ex_post_roe_nc,
           actual_revenues_nc,
           atsp_gain_loss_cost_sharing_nc,
           trs_nc,
           financial_incentive_nc,
           x4_2_cost_excl_vfr_d_nc
           )


  regulatory_result_euro_split <- data_prep_years_split %>%
    left_join(data_prep_xrates, by = "year") %>%
    mutate(regulatory_result = regulatory_result_nc / xrate,
           ex_ante_roe = ex_ante_roe_nc / xrate,
           ex_post_roe = ex_post_roe_nc / xrate,
           actual_revenues = actual_revenues_nc / xrate,

           atsp_gain_loss_cost_sharing = atsp_gain_loss_cost_sharing_nc / xrate,
           trs = trs_nc / xrate,
           financial_incentive = financial_incentive_nc / xrate,
           x4_2_cost_excl_vfr_d = x4_2_cost_excl_vfr_d_nc / xrate

    ) %>%
    select(-xrate,
           -regulatory_result_nc,
           -ex_ante_roe_nc,
           -ex_post_roe_nc,
           -actual_revenues_nc,
           -atsp_gain_loss_cost_sharing_nc,
           -trs_nc,
           -financial_incentive_nc,
           -x4_2_cost_excl_vfr_d_nc)

  regulatory_result <- regulatory_result_euro_split %>%
    left_join(tsus, by = "year")


  return(regulatory_result)
}


# ## export figure function ----
#   # the export function needs webshot and PhantomJS. Install PhantomJS with 'webshot::install_phantomjs()' and then cut the folder from wherever is installed and paste it in C:\Users\[username]\dev\r\win-library\4.2\webshot\PhantomJS
# 
#   export_fig <- function (fig, fig_name, width, height) {
#     fig_dir <- paste0('images/', year_report, '/', country,'/')
#     invisible(export(fig, paste0(fig_dir, fig_name)))
#     invisible(figure <- image_read(paste0(fig_dir,fig_name)))
#     invisible(cropped <- image_crop(figure, paste0(width, "x", height)))
#     invisible(image_write(cropped, paste0(fig_dir, fig_name)))
#   }
#   
# universal donut chart  ----
mydonutchart <-  function(df,
                          width = mywidth,
                          height = myheight,
                          font = myfont,
                          margin = mymargin,
                          decimals = mydecimals,
                          hole = 0.6,
                          textinfo = "percent",
                          insidetextorientation = "horizontal",
                          colors = mycolors,
                          shape = c(''),
                          hovertemplate = myhovertemplate,
                          sort = TRUE,
                          direction = "clockwise",
                          rotation = 0,

                          minsize = myminsize,
                          font_family = myfont_family,
                          title_text = "Chart title",
                          title_x = mytitle_x,
                          title_xanchor = mytitle_xanchor,
                          title_y = mytitle_y,
                          title_yanchor = mytitle_yanchor,
                          title_font_size = mytitle_font_size,
                          textfont_size = mytextfont_size,
                          hovermode = myhovermode,
                          hoverlabel_bgcolor = myhoverlabel_bgcolor,
                          legend_traceorder = mylegend_traceorder,
                          legend_orientation = mylegend_orientation,
                          legend_x = mylegend_x,
                          legend_xanchor = mylegend_xanchor,
                          legend_y = mylegend_y,
                          legend_yanchor = mylegend_xanchor,
                          legend_font_size = mylegend_font_size
                          ) {
    plot_ly(
      data = df,
      width = width,
      height = height,
      labels = ~ type,
      values = ~ mymetric,
      texttemplate = "%{customdata}",
      customdata = ~textlabel,
      insidetextorientation = insidetextorientation,
      type = "pie",
      hole = hole,
      hovertemplate = hovertemplate,
      sort = sort,
      rotation = rotation,

      textinfo = textinfo,
      textfont = list(size =  textfont_size),
      textposition = ~ textposition,
      marker = list(colors = colors, pattern = list(shape = shape))
      ) %>%
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>%
    layout(
      uniformtext=list(minsize = minsize, mode='show'),
      font = list(family = font_family),
      title = list(text = title_text,
                   x = title_x,
                   y = title_y,
                   xanchor = title_xanchor,
                   yanchor = title_yanchor,
                   font = list(size = title_font_size)
      ),
      dragmode = FALSE,
      hovermode = hovermode,
      hoverlabel = list(bgcolor = hoverlabel_bgcolor),

      legend = list(
        traceorder= legend_traceorder,
        orientation = legend_orientation,
        xanchor = legend_xanchor,
        yanchor = legend_yanchor,
        x = legend_x,
        y = legend_y,
        font = list(size = legend_font_size),
        orientation = legend_orientation
      ),
      margin = margin
    )

}
 
 
# ## universal linechart  ----
mylinechart2 <-  function(df,
                          width = mywidth, 
                          height = myheight, 
                          colors = mycolors,
                          local_factor = c(""),
                          
                          fontsize = myfont, 
                          decimals = mydecimals,
                          suffix = mysuffix,
                          
                          textangle = mytextangle,
                          textfontcolor = mytextfont_color,
                          textfontsize = mytextfont_size,
                          
                          linewidth = mylinewidth,
                          
                          hovertemplate = myhovertemplate,
                          
                          showlegend = mytrace_showlegend,
                          
                          minsize = myminsize,
                          
                          title_text = "Title",
                          title_x = mytitle_x,
                          title_y = mytitle_y,
                          title_xanchor = mytitle_xanchor,
                          title_yanchor = mytitle_yanchor,
                          title_font_size = mytitle_font_size,
                          
                          hovermode = myhovermode,
                          hoverlabel_bgcolor = myhoverlabel_bgcolor,

                          xaxis_title = myxaxis_title,
                          xaxis_gridcolor = myxaxis_gridcolor,
                          xaxis_showgrid = myxaxis_showgrid,
                          xaxis_showline = myxaxis_showline,
                          xaxis_showticklabels = myxaxis_showticklabels,
                          xaxis_dtick = myxaxis_dtick,
                          xaxis_tickformat = myxaxis_tickformat,
                          xaxis_zeroline = myxaxis_zeroline,
                          xaxis_tickfontsize = myxaxis_tickfont_size,
                          
                          yaxis_title = myyaxis_title,
                          yaxis_gridcolor = myyaxis_gridcolor,
                          yaxis_showgrid = myyaxis_showgrid,
                          yaxis_showline = myyaxis_showline,
                          yaxis_tickprefix = myyaxis_tickprefix,
                          yaxis_ticksuffix = myyaxis_ticksuffix,
                          yaxis_tickformat = myyaxis_tickformat,
                          yaxis_zeroline = myyaxis_zeroline,
                          yaxis_zerolinecolor = myyaxis_zerolinecolor,
                          yaxis_titlefontsize = myyaxis_titlefont_size,
                          yaxis_tickfontsize = myyaxis_tickfont_size,
                          
                          legend_traceorder= mylegend_traceorder,
                          legend_orientation = mylegend_orientation,
                          legend_xanchor = mylegend_xanchor,
                          legend_yanchor = mylegend_yanchor,
                          legend_x = mylegend_x,
                          legend_y = mylegend_y,
                          legend_fontsize = mylegend_font_size,
                          
                          margin = mymargin
                          
                          ) 
  {
  df %>%
    plot_ly(
      width = width,
      height = height,
      x = ~ xlabel,
      y = ~ mymetric,
      yaxis = "y1",
      colors = colors,
      color = ~ factor(type, levels = local_factor),
      text = ~ paste0(format(round(mymetric, decimals),  big.mark  = ",", nsmall = decimals), suffix),
      textangle = textangle,
      textposition = ~ textposition,
      textfont = list(color = textfontcolor, size = textfontsize),
      cliponaxis = FALSE,
      type = 'scatter',  mode = 'lines+markers+text',
      line = list(width = linewidth, dash = ~linedash),
      hovertemplate = hovertemplate,
      showlegend = showlegend
    ) %>%
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
    ) %>%
    layout(
      uniformtext=list(minsize = minsize, mode='show'),
      font = list(family = myfont_family),
      title = list(text = title_text,
                   x = title_x,
                   y = title_y,
                   xanchor = title_xanchor,
                   yanchor = title_yanchor,
                   font = list(size = title_font_size)
      ),
      dragmode = FALSE,
      hovermode = hovermode,
      hoverlabel = list(bgcolor = hoverlabel_bgcolor),
      xaxis = list(title = xaxis_title,
                   gridcolor = xaxis_gridcolor,
                   showgrid = xaxis_showgrid,
                   showline = xaxis_showline,
                   showticklabels = xaxis_showticklabels,
                   dtick = xaxis_dtick,
                   tickformat = xaxis_tickformat,
                   zeroline = xaxis_zeroline,
                   tickfont = list(size = xaxis_tickfontsize)
      ),
      yaxis = list(title = yaxis_title,
                   gridcolor = yaxis_gridcolor,
                   showgrid = yaxis_showgrid,
                   showline = yaxis_showline,
                   tickprefix = yaxis_tickprefix,
                   ticksuffix = yaxis_ticksuffix,
                   tickformat = yaxis_tickformat,
                   zeroline = yaxis_zeroline,
                   zerolinecolor = yaxis_zerolinecolor,
                   titlefont = list(size = yaxis_titlefontsize),
                   tickfont = list(size = yaxis_tickfontsize)
      ),
      legend = list(
        traceorder= legend_traceorder,
        orientation = legend_orientation,
        xanchor = legend_xanchor,
        yanchor = legend_yanchor,
        x = legend_x,
        y = legend_y,
        font = list(size = legend_fontsize)
      ),
      margin = margin
    )
}
   
   
# better universal barchart  ----
mybarchart2 <-  function(df,
                        width = mywidth,
                        height = myheight,
                        colors = mycolors,
                        font = myfont,
                        decimals = mydecimals,
                        suffix = mysuffix,
                        local_factor = c(""),
                        shape = c(""),

                        text = ~ paste0(format(round(mymetric, decimals),  big.mark  = ",", nsmall = decimals), suffix),
                        meta = ~type,
                        textangle = mytextangle,
                        textposition = mytextposition,
                        insidetextanchor = myinsidetextanchor,
                        textfont_color = mytextfont_color,
                        textfont_size = mytextfont_size,

                        hovertemplate = myhovertemplate,
                        showlegend = mytrace_showlegend,

                        minsize = myminsize,
                        family = myfont_family,

                        title_text = "Chart title",
                        title_x = mytitle_x,
                        title_y = mytitle_y,
                        title_xanchor = mytitle_xanchor,
                        title_yanchor = mytitle_yanchor,
                        title_font_size = mytitle_font_size,

                        bargap = mybargap,
                        barmode = mybarmode,
                        hovermode = myhovermode,
                        hoverlabel_bgcolor = myhoverlabel_bgcolor,
                        trace_showlegend = TRUE,

                        xaxis_title = "",
                        xaxis_gridcolor = myxaxis_gridcolor,
                        xaxis_showgrid = myxaxis_showgrid,
                        xaxis_showline = myxaxis_showline,
                        xaxis_showticklabels = myxaxis_showticklabels,
                        xaxis_dtick = myxaxis_dtick,
                        xaxis_tickformat = myxaxis_tickformat,
                        xaxis_zeroline = myxaxis_zeroline,
                        xaxis_tickfont_size = myxaxis_tickfont_size,
                        xaxis_tickangle = NULL,

                        yaxis_title = "",
                        yaxis_gridcolor = myyaxis_gridcolor,
                        yaxis_showgrid = myyaxis_showgrid,
                        yaxis_showline = myyaxis_showline,
                        yaxis_tickprefix = myyaxis_tickprefix,
                        yaxis_ticksuffix = myyaxis_ticksuffix,
                        yaxis_tickformat = myyaxis_tickformat,
                        yaxis_zeroline = myyaxis_zeroline,
                        yaxis_zerolinecolor = myyaxis_zerolinecolor,
                        yaxis_titlefont_size = myyaxis_titlefont_size,
                        yaxis_tickfont_size = myyaxis_tickfont_size,
                        yaxis_standoff = NA,

                        legend_traceorder = mylegend_traceorder,
                        legend_orientation = mylegend_orientation,
                        legend_xanchor = mylegend_xanchor,
                        legend_yanchor = mylegend_yanchor,
                        legend_x = mylegend_x,
                        legend_y = mylegend_y,
                        legend_fontsize = myfont,

                        margin = mymargin) {
  df %>%
    plot_ly(
      width = width,
      height = height,
      x = ~ xlabel,
      y = ~ mymetric,
      yaxis = "y1",
      marker = list(pattern = list(shape = shape)),
      colors = colors,
      color = ~ factor(type, levels = local_factor),
      text = text,
      meta = meta,
      # text = ~ mymetric,
      textangle = textangle,
      textposition = textposition,
      insidetextanchor = insidetextanchor,
      textfont = list(color = textfont_color, size = textfont_size),
      cliponaxis = FALSE,
      type = "bar",
      hovertemplate = hovertemplate,
      showlegend = trace_showlegend
    ) %>%
    config( responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>%
    layout(
      uniformtext=list(minsize = minsize, mode='show'),
      font = list(family = family),
      title = list(text = title_text,
                   x = title_x,
                   y = title_y,
                   xanchor = title_xanchor,
                   yanchor = title_yanchor,
                   font = list(size = title_font_size)
      ),
      dragmode = FALSE,
      bargap = bargap,
      barmode = barmode,
      hovermode = hovermode,
      hoverlabel = list(bgcolor = hoverlabel_bgcolor),
      xaxis = list(title = xaxis_title,
                   gridcolor = xaxis_gridcolor,
                   showgrid = xaxis_showgrid,
                   showline = xaxis_showline,
                   showticklabels = xaxis_showticklabels,
                   dtick = xaxis_dtick,
                   tickformat = xaxis_tickformat,
                   zeroline = xaxis_zeroline,
                   tickfont = list(size = xaxis_tickfont_size),
                   tickangle = xaxis_tickangle
      ),
      yaxis = list(title = list(text = yaxis_title,
                                standoff = yaxis_standoff),
                   gridcolor = yaxis_gridcolor,
                   showgrid = yaxis_showgrid,
                   showline = yaxis_showline,
                   tickprefix = yaxis_tickprefix,
                   ticksuffix = yaxis_ticksuffix,
                   tickformat = yaxis_tickformat,
                   zeroline = yaxis_zeroline,
                   zerolinecolor = yaxis_zerolinecolor,
                   titlefont = list(size = yaxis_titlefont_size),
                   tickfont = list(size = yaxis_tickfont_size)

      ),
      legend = list(
        traceorder= legend_traceorder,
        orientation = legend_orientation,
        xanchor = legend_xanchor,
        yanchor = legend_yanchor,
        x = legend_x,
        y = legend_y,  # this is on purpose
        font = list(size = legend_fontsize)
      ),
      margin = margin
    )
}

# 
# ## add empty trace to force year series  ----
add_empty_trace <- function(myplot, df){
  myplot %>%
  add_trace(
    data = df,
    x = ~ xlabel,
    y = ~ '',
    name = "Fake series to force all years in x axis",
    yaxis = "y1",
    mode = "markers",
    type = 'scatter',
    marker = list(size = mylinewidth, color = 'transparent'),
    showlegend = F,
    # hovertemplate = '',
    hoverinfo = 'none'
    )
}

# add linetrace  ----
add_line_trace2 <- function(myplot, 
                            df,
                            name = "Target",
                            yaxis = 1,
                            mode = "line+markers",
                            
                            textweight = FALSE,
                            textdecimals = 0,
                            textsuffix = '',
                            textangle = 0,
                            textposition = 'top',
                            textfontcolor = PRBTargetColor,
                            textfontsize = myfont,
                            
                            linecolor = PRBTargetColor,
                            linewidth = mylinewidth,
                            
                            markersize = mylinewidth * 3,
                            markercolor = PRBTargetColor,
                            markersymbol = NA,
                            
                            showlegend = TRUE
                            ){
  myplot %>%
    add_trace(
      data = df,
      x = ~ xlabel,
      y = ~ myothermetric,
      yaxis = yaxis,
      mode = mode,
      type = 'scatter',
      name = name,
      text = ~ paste0(if_else(textweight == TRUE, "<b>",""),
                      format(myothermetric,  big.mark  = ",", nsmall = textdecimals), textsuffix,
                      if_else(textweight == TRUE, "</b>","")),
      textangle = textangle,
      textposition = textposition,
      textfont = list(color = textfontcolor, size = textfontsize),
      line = list(color = linecolor, width = linewidth),
      marker = list(size = markersize,
                    color = markercolor,
                    symbol = markersymbol),
      showlegend = showlegend
    )
}


# better  horizontal barchart  ----
myhbarc2 <-  function(df,
                      width = mywidth,
                      height = myheight,

                      font = myfont,
                      decimals = mydecimals,
                      suffix = mysuffix,

                      local_factor = NA,

                      mybarcolor_pos = 'green',
                      mybarcolor_neg = 'red',

                      textangle = 0,
                      textposition = "auto",
                      textfont_color = mytextfont_color,
                      textfont_size = mytextfont_size,

                      hovertemplate = myhovertemplate,
                      showlegend = mytrace_showlegend,

                      minsize = myminsize,
                      family = myfont_family,

                      title_text = "Chart title",
                      title_x = mytitle_x,
                      title_y = mytitle_y,
                      title_xanchor = mytitle_xanchor,
                      title_yanchor = mytitle_yanchor,
                      title_font_size = mytitle_font_size,

                      bargap = 0.25,
                      barmode = 'stack',
                      hovermode = "y",

                      xaxis_title = "",
                      xaxis_titlefont_size = myfont,
                      xaxis_showgrid = FALSE,
                      xaxis_showline = FALSE,
                      xaxis_fixedrange = TRUE,
                      xaxis_showticklabels = myxaxis_showticklabels,
                      xaxis_ticksuffix = "",
                      xaxis_tickformat = myxaxis_tickformat,
                      xaxis_zeroline = TRUE,
                      xaxis_tickfont_size = myxaxis_tickfont_size,
                      xaxis_tickangle = NULL,

                      yaxis_title = "",
                      yaxis_gridcolor = myyaxis_gridcolor,
                      yaxis_showgrid = FALSE,
                      yaxis_showline = FALSE,
                      yaxis_showticklabels = TRUE,
                      yaxis_tickformat = myyaxis_tickformat,
                      yaxis_zeroline = myyaxis_zeroline,
                      yaxis_zerolinecolor = myyaxis_zerolinecolor,
                      yaxis_titlefont_size = myyaxis_titlefont_size,
                      yaxis_tickfont_size = myyaxis_tickfont_size,
                      yaxis_standoff = NA,

                      margin = mymargin) {
  df %>%
    plot_ly(
      width = width,
      height = height,
      x = ~ round(mymetric, decimals),
      y = ~ factor(ylabel, levels = local_factor),
      yaxis = "y1",
      marker = list(color = ~ifelse(mymetric>=0, mybarcolor_pos, mybarcolor_neg)),
      text = ~ mylabel,
      textangle = textangle,
      textposition = textposition,
      cliponaxis = FALSE,
      orientation = 'h',
      textfont = list(color = textfont_color, size = textfont_size),
      type = "bar",
      hovertemplate = hovertemplate,
      # hoverinfo = "none",
      showlegend = showlegend
    ) %>%
    config(responsive = TRUE,
            displaylogo = FALSE,
            displayModeBar = F
            # modeBarButtons = list(list("toImage")),
    ) %>%
    layout(
      font = list(family = "Roboto"),
      title = list(text = title_text,
                   y = title_x ,
                   x = title_y,
                   xanchor = title_xanchor,
                   yanchor =  title_yanchor,
                   font = list(size = title_font_size)
      ),
      dragmode = FALSE,
      bargap = bargap,
      barmode = barmode,
      hovermode = hovermode,
      hoverlabel=list(bgcolor="rgba(255,255,255,0.88)"),
      yaxis = list(title = yaxis_title,
                   titlefont = list(size = yaxis_titlefont_size),
                   gridcolor = yaxis_gridcolor,
                   showgrid = yaxis_showgrid,
                   showline = yaxis_showline,
                   showticklabels = yaxis_showticklabels,
                   zeroline = yaxis_zeroline,
                   tickfont = list(size = yaxis_tickfont_size)
      ),
      xaxis = list(title = xaxis_title,
                   # automargin = T,
                   # gridcolor = 'rgb(255,255,255)',
                   showgrid = xaxis_showgrid,
                   showline = xaxis_showline,
                   # tickprefix = if_else(" ",
                   ticksuffix = xaxis_ticksuffix,
                   fixedrange = xaxis_fixedrange,
                   tickformat = xaxis_tickformat,
                   tickangle = xaxis_tickangle,
                   showticklabels = xaxis_showticklabels,
                   # tickcolor = 'rgb(127,127,127)',
                   # ticks = 'outside',
                   zeroline = xaxis_zeroline,
                   zerolinecolor = 'rgb(225,225,225)',
                   titlefont = list(size = xaxis_titlefont_size),
                   tickfont = list(size = xaxis_tickfont_size)
      ),
      margin = margin

    )
}

# gt table  ----
mygtable <-  function(df, myfont) {
  gt(df) %>%
  tab_options(
    table.width = '100%',
    table.font.size = myfont,
    # column_labels.vlines.color = "black", # don't know why this doesn't work
    column_labels.vlines.width = '1px',
    # column_labels.border.top.color = "#EAEAEA",
    column_labels.border.top.width = '1px',
    table.border.top.width = '1px',
    table.border.bottom.width = '1px',
    data_row.padding = '3px',
    # to disable the striped bootstrap issue
    quarto.disable_processing = TRUE)  %>%
    cols_align(
      align = "right",
      columns = c(-1)
    ) %>%
    opt_css(
      css = "
      td {
        white-space: normal !important;
        word-break: break-word;
      }
    "
    )
}

# replace links in html page ----
replace_links <- function(filename) {

  tmp_text <- readLines(paste0(site_dir, "/", filename))

  # strings I do not want to modify
  not_modify <- c(paste0(home_address, "/", year_report,"/ses-rp3/"),
                  paste0(home_address, "/", year_report,"/network-manager/"),
                  paste0(home_address, "/", year_report,"/muac/")
  )

  tmp_text <- str_replace_all(tmp_text, not_modify[1], "temporary text1")
  tmp_text <- str_replace_all(tmp_text, not_modify[2], "temporary text2")
  tmp_text <- str_replace_all(tmp_text, not_modify[3], "temporary text3")

  # Define the pattern to match
  ## The regular expression pattern now includes - to match dashes in country names. [A-Za-z-]+ matches one or more alphabetic characters or dashes. the double \\ escapes the .

  pattern1 <- paste0("(", str_replace_all(home_address, ".", "\\."), "/", year_folder, "/[A-Za-z-]+/)")

  # Replace the pattern with the same string plus "#section"
  ## \\1 refers to the entire matched string,

  replacement_link <- case_when(
    filename %like% "cost-efficiency" ~ "cost-efficiency-er1-1.html", #for no terminal cases
    .default = filename
  )

  tmp_text <- gsub(pattern1,
                   paste0("\\1", replacement_link),
                   tmp_text)

  # simplify links to rp3 summary page

  # Escape home address for regex
  escaped_home <- gsub("\\.", "\\\\.", home_address)

  # Normalize country to URL slug
  country_slug <- tolower(gsub("\\s+", "-", country))  # "Czech Republic" -> "czech-republic"

  # ✅ Capture entire match, NOT just the prefix — group only the part you want to keep
  pattern2 <- paste0("(", escaped_home, "/rp3/", country_slug, "/)", "[^\\s\"'>]*")

  # ✅ Use correct backreference
  replacement_link2 <- "\\1"

  # ✅ Apply gsub
  tmp_text <- gsub(pattern2, replacement_link2, tmp_text, perl = TRUE, ignore.case = TRUE)


  #replace year report links & fix side effect from previous replacement
  tmp_text <- str_replace_all(tmp_text, paste0(replacement_link,"index.html#main"), replacement_link)
  tmp_text <- str_replace_all(tmp_text, "index.html#main", replacement_link)
  tmp_text <- str_replace_all(tmp_text,
                              fixed("-prb-findings--"),
                              fixed("#main-prb-findings--"))

  # last minute fix to avoid complicating the issue
  if (country == rp_full) {
   tmp_text <- str_replace_all(tmp_text,
                              fixed("#effectiveness-of-safety-management-eosm-kpi1"),
                              fixed("#actual-versus-planned-number-of-ansps-achieving-the-level-of-the-eosm-targets-for-rp3-ahead-of-2024"))
  }

  #restore strings I didn't want modified
  tmp_text <- str_replace_all(tmp_text, "temporary text1", not_modify[1])
  tmp_text <- str_replace_all(tmp_text, "temporary text2", not_modify[2])
  tmp_text <- str_replace_all(tmp_text, "temporary text3", not_modify[3])

  # rewrite file
  writeLines(tmp_text, paste0(site_dir, "/", filename))

}


# ## get PRB conclusions  ----
# as_markdown_bullets <- function(x) {
#   # Split on 2+ newlines (blank-line separated paragraphs)
#   items <- strsplit(x, "\\n{2,}", perl = TRUE)[[1]]
#   items <- trimws(items)
#   items <- items[nzchar(items)]
#   
#   # Remove leading unicode bullet if present
#   items <- sub("^\\s*[▪•·]\\s*", "", items)
#   
#   # Emit Markdown bullets with a blank line between items
#   paste0("- ", items, collapse = "\n\n")
# }
# 
# get_prb_conclusions <- function(filename, kpi, table) {
#   
#   if (country == 'Network Manager') {
#     conc <- read_mytable(filename, kpi, table) %>%
#       filter(tolower(as.character(Year_Report)) == as.character(year_folder)) %>%
#       select(Conclusion)
#   } else {
#     conc <- read_mytable(filename, kpi, table) %>%
#       filter(tolower(as.character(Year_Report)) == as.character(year_folder), State == country) %>%
#       select(Conclusion)
#   }
#   
#   conc_string <- conc %>% 
#     toString() %>% paste0(., if_else(nrow(conc) == 1, "", "@"))
#   
#   prb_conc <- conc_string %>% str_replace_all(c('", ' = '\n\n', '\"' = '', '\n\n\n' = '\n\n')) %>% str_replace(fixed('c('), '') %>%  str_replace(fixed(')@'), '\n')
#   
#   if (knitr::is_latex_output()) {
#     prb_conc <- prb_conc %>%
#       # str_replace_all(c('▪' = '\\\\textbullet\\\\quad ', '%' = '\\\\%'))  # Escape `%` for LaTeX
#       as_markdown_bullets
#   }
#   
#   return(prb_conc)
# }
# 
# ## latex tables for pdf  ----
# mylatex <- function(gttable, firstcolumn = 2.7) {
#   # latex_string <- table_level2_cef_aucu
#   latex_string <- gttable|> as_latex() %>%
#     stringr::str_replace_all(fixed("{20}{25}"), 
#                              fixed("{11}{11}")) %>% 
#     stringr::str_replace_all(fixed("{10.5pt}{12.6pt}"), 
#                              fixed("{8pt}{9.6pt}")) %>% 
#     ## the open bracket here for the shaded caption is closed in the second replacement
#     stringr::str_replace_all(fixed("caption*"), 
#                              fixed("caption*{\\captionbar")) %>% 
#     stringr::str_replace_all(fixed("\\fontsize{10}{13}\\selectfont }"), 
#                              fixed("}}\\vspace{-6pt}")) %>% 
#     #shading for column title row
#     stringr::str_replace_all(fixed("\\toprule\n"), 
#                              fixed("\\toprule\n\\rowcolor{HeaderShade}")) %>% 
#     # Replace with 9pt font size
#     stringr::str_replace(
#       "\\\\large",  # Match \large
#       "\\\\fontsize{9pt}{10.8pt}\\\\selectfont"  
#     ) %>% 
#     stringr::str_replace_all(
#       c("✓" = "\\\\tick", "✘" = "\\\\cross")
#     )
#   
#   # ensure the table fontsize is 8 pt
#   if (!str_detect(latex_string, fixed("{8pt}{9.6pt}"))) {
#     latex_string <- latex_string %>% 
#       str_replace(
#         "(\\\\begin\\{tabular\\*\\}[\\s\\S]*?\\\\end\\{tabular\\*\\})",  # Match the entire tabular environment
#         "\\\\fontsize{8pt}{9.6pt}\\\\selectfont\n\\1"  # Wrap with font size commands
#       )
#     
#   #change border colours
#   latex_string <- latex_string %>% 
#     str_replace(
#       "\\\\begin\\{tabular\\*",
#       "\\\\arrayrulecolor{CHAPBLUE}\\\\begin{tabular*"
#     )
#   }
# 
#   if (!is.na(firstcolumn)) {
#     latex_string <- latex_string %>% 
#       str_replace(
#         "\\\\begin\\{tabular\\*\\}\\{1\\\\linewidth\\}\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}lrrrr\\}",  # Match the original tabular definition
#        paste0("\\\\begin{tabular*}{1\\\\linewidth}{@{\\\\extracolsep{\\\\fill\\}}P{",
#               firstcolumn,
#               "cm}rrrr}")   # Use P column type for the first column
#     ) %>% 
#     str_replace(
#       "\\{@\\{\\\\extracolsep\\{\\\\fill\\}\\}lrr\\}",  # Match the column definition
#       paste0("{@{\\\\extracolsep{\\\\fill}}P{",
#         firstcolumn,
#         "cm}rr}")        # Replace `l` with `P{2.5cm}`
#       )
#   }
#   
#   return(latex_string)
# }
# 
# ## latex layout 2 figures side by side  ----
# layout_2fig <- function(chart1, chart2, width1 = 0.48, width2 = 0.48) {
#   
#   layout_string <- paste0('
# ```{=tex}
# \\begin{figure}[H]
# \\centering
# \\begin{minipage}{', width1, '\\linewidth}
#     \\centering
#     \\includegraphics[width=1\\linewidth,height=\\textheight,keepaspectratio]{index_files/figure-pdf/', chart1, '-1.pdf}
# \\end{minipage}%
# \\hspace{0.015\\linewidth} % Adds a small empty space (2% of linewidth)
# \\begin{minipage}{', width2, '\\linewidth}
#     \\centering
#     \\includegraphics[width=1\\linewidth,height=\\textheight,keepaspectratio]{index_files/figure-pdf/', chart2, '-1.pdf}
# \\end{minipage}%
# \\end{figure}
# ```
# ')
#   return(layout_string)
# }
# 
# ## setup gt latex output  ----
# setup_latex_table <- function(table1) {
#   # table1 <- table_level2_cef_cex
#   # remove the table env incompatible with the minipage we need
#   tablestring <- table1 %>% 
#     stringr::str_replace_all("\\\\begin\\{table\\}\\[!t\\]\\n",
#                              "") %>% 
#     stringr::str_replace_all("\\\\ \n",
#                              "\\\\\n") %>%
#     stringr::str_replace_all("\\\\end\\{table\\}\n",
#                              "") 
#   
#   # remove this line that quarto adds to gt latex output
#   layout_string <- fixed(tablestring)%>% 
#     str_replace_all(fixed("\\begin{table}\n"),"")
# 
#   return(layout_string)
# }
# 
# ## latex layout figure table side by side----
# layout_fig_table <- function(chart1, table1, vspace, table2 = NULL) {
#   
#   layout_string <-paste0(
#     "```{=tex}\n\\begin{figure}[H]
# \\begin{minipage}{0.50\\linewidth}
# %
# \\includegraphics[width=1\\linewidth,height=\\textheight,keepaspectratio]{index_files/figure-pdf/",chart1,"-1.pdf}
# %
# \\end{minipage}%
# %
# \\begin{minipage}{0.02\\linewidth}
# 
# \\hspace*{0.2cm}
# 
# \\end{minipage}%
# %
# \\begin{minipage}{0.48\\linewidth}\n",
# "\\vspace*{", vspace, "cm}\n",
# table1,
# if_else(is.null(table2), "",paste0("
# 
# ", table2)),
# "%
# \\end{minipage}%\n
# \\end{figure}%\n```\n"
#     )
#   
#   return(layout_string)
# }
# 
# ## latex layout summary table----
# layout_summary_table <- function(table1, hspace = 0.1, left_margin = 0, width = 1) {
#   
#   layout_string <-paste0(
#     "```{=tex}\n\\begin{figure}[H]
# \\hspace*{", left_margin, "cm}
# \\begin{minipage}{", hspace, "\\linewidth}
# 
# \\end{minipage}%
# %
# \\begin{minipage}{", (1-2*hspace)*width , "\\linewidth}\n",
# table1,
#     "%
# \\end{minipage}%
# %
# \\begin{minipage}{", hspace, "\\linewidth}
# 
# \\end{minipage}%
# %
# \\end{figure}%\n```\n"
#   )
#   
#   return(layout_string)
# }
# 
# ## latex wrap text around figure ----
# layout_wrap_figure <- function(chart1, chart2 = NULL, text, vspace, chart3 = NULL, boxsize = 7) {
#   
#   layout_string <-paste0(
#     "```{=tex}\n\\sbox{0}{\\parbox{1\\textwidth}{
# 
# \\begin{wrapfigure}[", vspace ,"]{l}{0.5\\linewidth}
# \\vspace{-15pt}
# 
# \\includegraphics[width=1\\linewidth,height=\\textheight,keepaspectratio]{index_files/figure-pdf/",chart1,"-1.pdf}
# 
# ", if_else(is.null(chart2), "", paste0("\\vspace{10pt} % vertical space between charts,
# 
# \\includegraphics[width=1\\linewidth,height=\\textheight,keepaspectratio]{index_files/figure-pdf/",chart2,"-1.pdf}
# 
# ")),
#     if_else(is.null(chart3), "", paste0("\\vspace{10pt} % vertical space between charts,
# 
# \\includegraphics[width=1\\linewidth,height=\\textheight,keepaspectratio]{index_files/figure-pdf/",chart3,"-1.pdf}
# 
# ")),
# "\\end{wrapfigure}
# 
# \\setlength\\parskip{1em plus 0.8em} % Space between paragraphs
# 
# ", text,"
# 
# }}
# \\ifdim\\dimexpr\\ht0+\\dp0<",boxsize,"cm 
# \\dp0\\dimexpr",boxsize,"cm-\\ht0\\fi
# 
# \\fbox{\\usebox{0}}
# 
# ```\n"
#   )
#   
#   return(layout_string)
# }
# 
# ## setup quarto line breaks ----
# # R/quarto_paragraph_breaks.R
# quarto_paragraph_breaks <- function(x) {
#   fix_italics_across_paragraphs <- function(s) {
#     s <- gsub("\r\n", "\n", s, fixed = TRUE)
#     
#     n <- nchar(s)
#     if (n == 0) return("")
#     
#     out <- character(0)
#     italics_open <- FALSE
#     i <- 1L
#     
#     char_at <- function(idx) {
#       if (idx < 1L || idx > n) return("")
#       substr(s, idx, idx)
#     }
#     
#     is_escaped_asterisk <- function(idx) {
#       idx > 1L && char_at(idx - 1L) == "\\"
#     }
#     
#     is_double_asterisk <- function(idx) {
#       char_at(idx) == "*" && char_at(idx + 1L) == "*"
#     }
#     
#     is_list_marker <- function(idx) {
#       if (char_at(idx) != "*") return(FALSE)
#       if (char_at(idx + 1L) != " ") return(FALSE)
#       idx == 1L || char_at(idx - 1L) == "\n"
#     }
#     
#     is_toggle_asterisk <- function(idx) {
#       char_at(idx) == "*" &&
#         !is_escaped_asterisk(idx) &&
#         !is_double_asterisk(idx) &&
#         !is_list_marker(idx)
#     }
#     
#     while (i <= n) {
#       # Paragraph break handling
#       if (char_at(i) == "\n" && char_at(i + 1L) == "\n") {
#         if (italics_open) {
#           # close before + reopen after paragraph break
#           out <- c(out, "*", "\n\n", "*")
#         } else {
#           out <- c(out, "\n\n")
#         }
#         i <- i + 2L
#         next
#       }
#       
#       # Italics toggle on valid single '*'
#       if (is_toggle_asterisk(i)) {
#         italics_open <- !italics_open
#         out <- c(out, "*")
#         i <- i + 1L
#         next
#       }
#       
#       # default: copy char
#       out <- c(out, char_at(i))
#       i <- i + 1L
#     }
#     
#     paste0(out, collapse = "")
#   }
#   
#   tryCatch({
#     if (is.null(x) || length(x) == 0) return("")
#     if (length(x) > 1) x <- paste(x, collapse = "")
#     if (is.na(x)) return("")
#     
#     s <- trimws(as.character(x))
#     
#     # Remove leading whitespace before common list markers (-, *, +)
#     s <- gsub("(?m)^\\s+([-*+]\\s+)", "\\1", s, perl = TRUE)
#     
#     # Split on <br>, <br/>, <br />
#     parts <- stringr::str_split(s, "<br\\s*/?>")[[1]]
#     parts <- parts[trimws(parts) != ""]
#     if (length(parts) == 0) return("")
#     
#     # Join into Quarto paragraphs
#     s_out <- paste(parts, collapse = "\n\n")
#     
#     # Fix italics spans that would cross paragraph breaks
#     fix_italics_across_paragraphs(s_out)
#   }, error = function(e) "")
# }
# 
# 
# 
# ## setup latex line breaks ----
# latex_linebreaks <- function(mystring) {
# 
#   tryCatch({
#     mystring_list <- str_split(mystring, "<br/>")[[1]] %>%
#       discard(~ .x == "")
#     
#     latex_string <- reduce(mystring_list, ~ paste(.x, .y, sep = fixed('`\\\\`{=tex}')))
#     
#     return(latex_string)
#   }, error = function(e) {
#     return("")  # Return empty string on error
#   }
# )
#     
# }
# 
# ## break level 1 text  ----
# break_l1_text <- function(mystring) {
#   
#   mylength <- nchar(mystring)
#   no_bullets <- str_count(mystring, fixed("\n\n")) 
#   pos_bullets <- str_locate_all(mystring, fixed("\n\n"))[[1]]
#   mytotal_length <- mylength + no_bullets * 100
# 
#   if (mytotal_length < 900) {
#     level1_text1 <- mystring
#     level1_text2 <- ''
#   } else {
#     
#     break_point_check <- numeric(no_bullets)
#     
#     for (i in 1:no_bullets) {
#       if (i == 1) {
#         # Handle first iteration separately (to avoid pos_bullets[i-1] issue)
#         break_point_check[i] <- pos_bullets[i]
#       } else {
#         break_point_check[i] <- pos_bullets[i] + (i)*100
#       }
#     }
#     
#     break_point <- which(break_point_check > 900)[1] 
#     
#     if (is.na(break_point)) {
#       level1_text1 <- mystring
#       level1_text2 <- ''} 
#     else {
#       level1_text1 <- substr(mystring, 1, pos_bullets[break_point]+1)
#       level1_text2 <- substr(mystring, pos_bullets[break_point]+2, mylength)        
#       }
#     
#   }
#   
# 
#   return((list(text1 = level1_text1, text2 =level1_text2)))
#   
# }
# 
# ## Custom formatting function ----
# format_parens <- function(x) {
#   ifelse(x < 0,
#          paste0("(", format(abs(round(x, 2)), nsmall = 2), ")"),
#          format(round(x, 2), nsmall = 2))
# }
# 
# ## wrap long labels -----
# wrap_label <- function(label, width = 30) {
#   if (is.na(label)) return(NA_character_)
#   
#   words <- strsplit(label, " ")[[1]]
#   lines <- c()
#   current_line <- ""
#   
#   for (word in words) {
#     if (nchar(current_line) + nchar(word) + 1 > width) {
#       lines <- c(lines, current_line)
#       current_line <- word
#     } else {
#       current_line <- paste(current_line, word)
#       current_line <- trimws(current_line)
#     }
#   }
#   
#   lines <- c(lines, current_line)
#   paste(lines, collapse = "<br>")
# }
# 
## latex tables -----
make_quarto_latex_table <- function(
    title,
    df,
    col_labels = names(df),
    placement = "H",
    chapblue_name = "CHAPBLUE",
    header_shade_name = "HeaderShade",
    data_shade_name = "DataShade",
    shade_data_rows = TRUE,
    caption_fontsize = c(11, 12),
    body_fontsize = c(10, 12),
    arrayrulewidth_pt = 0.75,        # ~1px
    tabcolsep_pt = 0,
    caption_skip_pt = 0,
    caption_vspace_pt = 0,
    caption_after_vspace_pt = -1,
    escape_latex = TRUE,
    wrap_raw_block = TRUE,
    checkmarks = FALSE,              # FALSE/TRUE or integer column indices (e.g. c(2,3))
    col_align = NA,                  # REQUIRED, e.g. "cc" or "clrrr"
    cell_pad_em = 0.6,               # per-side padding
    col_widths = NULL,               # NULL => equal widths; else numeric pct vector summing to 100 (integers after rounding)
    row_extrarowheight_pt = 2,       # adds vertical padding to header+body rows
    bold_data_rows = integer(0),     # 1-based indices of data rows to make bold, e.g. c(1,2)
    sloppy_table = TRUE,             # table-local \sloppy
    emergencystretch_em = 2,         # table-local \emergencystretch=<N>em
    paginate = FALSE                 # TRUE => longtable with page breaks
) {
  stopifnot(is.data.frame(df))
  if (ncol(df) < 1) stop("df must have at least 1 column.")
  if (length(col_labels) != ncol(df)) stop("col_labels must have length ncol(df).")

  has_caption <- !(is.null(title) || (length(title) == 1 && isTRUE(is.na(title))))
  if (has_caption && (!is.character(title) || length(title) != 1)) {
    stop("title must be a single character string, or NA to omit the caption.")
  }

  if (!is.numeric(caption_after_vspace_pt) || length(caption_after_vspace_pt) != 1 || is.na(caption_after_vspace_pt)) {
    stop("caption_after_vspace_pt must be a single numeric value (e.g., -4, 0, 2).")
  }

  if (!is.numeric(row_extrarowheight_pt) || length(row_extrarowheight_pt) != 1 ||
      is.na(row_extrarowheight_pt) || row_extrarowheight_pt < 0) {
    stop("row_extrarowheight_pt must be a single non-negative number (e.g., 2).")
  }

  if (!is.character(data_shade_name) || length(data_shade_name) != 1 || !nzchar(data_shade_name)) {
    stop("data_shade_name must be a non-empty single character string (e.g., 'DataShade').")
  }
  if (!is.logical(shade_data_rows) || length(shade_data_rows) != 1 || is.na(shade_data_rows)) {
    stop("shade_data_rows must be TRUE/FALSE.")
  }

  if (!is.logical(sloppy_table) || length(sloppy_table) != 1 || is.na(sloppy_table)) {
    stop("sloppy_table must be TRUE/FALSE.")
  }
  if (!is.numeric(emergencystretch_em) || length(emergencystretch_em) != 1 ||
      is.na(emergencystretch_em) || emergencystretch_em < 0) {
    stop("emergencystretch_em must be a single non-negative number (e.g., 2).")
  }
  if (!is.logical(paginate) || length(paginate) != 1 || is.na(paginate)) {
    stop("paginate must be TRUE/FALSE.")
  }

  n <- ncol(df)

  if (length(col_align) != 1 || is.na(col_align)) {
    stop("col_align is required. Example: 'cc' for 2 centered cols, or 'clrrr' for 5 cols.")
  }
  if (!is.character(col_align)) stop("col_align must be a single character string like 'clrrr'.")
  if (nchar(col_align) != n) stop(sprintf("col_align must have length %d (ncol(df)).", n))
  if (grepl("[^clr]", col_align)) stop("col_align may only contain: c, l, r")

  if (!is.numeric(cell_pad_em) || length(cell_pad_em) != 1 || is.na(cell_pad_em) || cell_pad_em < 0) {
    stop("cell_pad_em must be a single non-negative number (e.g., 0.6).")
  }

  if (!is.null(col_widths)) {
    if (!is.numeric(col_widths)) stop("col_widths must be numeric (percentages).")
    if (length(col_widths) != n) stop(sprintf("col_widths must have length %d (ncol(df)).", n))
    if (any(!is.finite(col_widths)) || any(col_widths <= 0)) stop("col_widths must be finite and > 0.")
    if (abs(sum(col_widths) - 100) > 1e-6) stop("col_widths must sum to 100 (e.g. c(50,25,25)).")
    col_widths <- as.integer(round(col_widths))
    if (sum(col_widths) != 100) {
      col_widths[length(col_widths)] <- col_widths[length(col_widths)] + (100 - sum(col_widths))
    }
  }

  if (is.null(bold_data_rows)) bold_data_rows <- integer(0)
  if (!is.numeric(bold_data_rows)) stop("bold_data_rows must be numeric/integer indices like c(1,2).")
  bold_data_rows <- unique(as.integer(bold_data_rows[is.finite(bold_data_rows)]))
  if (any(bold_data_rows <= 0)) stop("bold_data_rows must contain positive (1-based) indices only.")
  if (length(bold_data_rows) > 0 && nrow(df) > 0 && any(bold_data_rows > nrow(df))) {
    stop(sprintf("bold_data_rows contains indices > nrow(df) (%d).", nrow(df)))
  }

  # checkmarks columns selection (supports legacy TRUE/FALSE)
  check_cols <- rep(FALSE, n)
  if (is.null(checkmarks) || (is.logical(checkmarks) && length(checkmarks) == 1 && !isTRUE(checkmarks))) {
    check_cols <- rep(FALSE, n)
  } else if (is.logical(checkmarks) && length(checkmarks) == 1 && isTRUE(checkmarks)) {
    check_cols <- rep(TRUE, n)
  } else if (is.numeric(checkmarks)) {
    idx <- unique(as.integer(checkmarks[is.finite(checkmarks)]))
    if (any(idx <= 0)) stop("checkmarks column indices must be positive (1-based).")
    if (any(idx > n)) stop(sprintf("checkmarks contains indices > ncol(df) (%d).", n))
    check_cols[idx] <- TRUE
  } else {
    stop("checkmarks must be FALSE/TRUE or a numeric vector of column indices (e.g., c(2,3)).")
  }

  escape_tex <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    x <- gsub("([%#$&_{}])", "\\\\\\1", x, perl = TRUE)
    x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
    x
  }
  fmt <- function(x) if (escape_latex) escape_tex(x) else as.character(x)

  softbreak_plain <- function(s) {
    gsub("/", paste0("/", intToUtf8(92), "hspace{0pt}"), s, fixed = TRUE)
  }


  is_zero_one <- function(x) {
    if (is.na(x) || x == "") return(FALSE)
    suppressWarnings({
      nx <- as.numeric(x)
      !is.na(nx) && (nx == 0 || nx == 1)
    })
  }

  fmt_cell_scalar_for_col <- function(x, col_i) {
    x_chr <- as.character(x)
    if (is.na(x_chr)) x_chr <- ""
    if (check_cols[col_i] && is_zero_one(x_chr)) {
      nx <- suppressWarnings(as.numeric(x_chr))
      return(if (nx == 1) "\\tick" else "\\cross")
    }
    return(softbreak_plain(fmt(x_chr)))
  }

  excel_col_name <- function(i) {
    name <- ""
    while (i > 0) {
      r <- (i - 1) %% 26
      name <- paste0(intToUtf8(65 + r), name)
      i <- (i - 1) %/% 26
    }
    name
  }
  col_len_names <- vapply(seq_len(n), function(i) paste0("colw", excel_col_name(i)), character(1))

  width_defs <- if (is.null(col_widths)) {
    paste0(
      "\\makeatletter\n",
      "\\@ifundefined{colw}{\\newlength{\\colw}}{}\n",
      "\\makeatother\n",
      "\\setlength{\\colw}{\\dimexpr(\\linewidth-", (n + 1), "\\arrayrulewidth)/", n, "\\relax}\n"
    )
  } else {
    newlength_lines <- paste0(
      vapply(col_len_names, function(nm) paste0("\\@ifundefined{", nm, "}{\\newlength{\\", nm, "}}{}\n"),
             character(1)),
      collapse = ""
    )
    setlength_lines <- paste0(
      vapply(seq_len(n), function(i) {
        nm <- col_len_names[i]
        pct <- col_widths[i]
        paste0(
          "\\setlength{\\", nm, "}{\\tableWunit}\n",
          "\\multiply\\", nm, " by ", pct, "\n"
        )
      }, character(1)),
      collapse = ""
    )
    paste0(
      "\\makeatletter\n",
      "\\@ifundefined{tableW}{\\newlength{\\tableW}}{}\n",
      "\\@ifundefined{tableWunit}{\\newlength{\\tableWunit}}{}\n",
      newlength_lines,
      "\\makeatother\n",
      "\\setlength{\\tableW}{\\dimexpr\\linewidth-", (n + 1), "\\arrayrulewidth\\relax}\n",
      "\\setlength{\\tableWunit}{\\dimexpr\\tableW/100\\relax}\n",
      setlength_lines
    )
  }

  width_token <- function(i) if (is.null(col_widths)) "\\colw" else paste0("\\", col_len_names[i])

  pad_dim <- paste0(format(cell_pad_em, trim = TRUE, scientific = FALSE), "em")

  align_preamble <- function(ch) {
    base <- "\\setlength{\\parindent}{0pt}\\setlength{\\parfillskip}{0pt}"
    if (ch == "l") {
      return(paste0(
        base,
        "\\setlength{\\leftskip}{", pad_dim, "}",
        "\\setlength{\\rightskip}{", pad_dim, " plus 1fil}",
        "\\arraybackslash"
      ))
    }
    if (ch == "r") {
      return(paste0(
        base,
        "\\setlength{\\leftskip}{", pad_dim, " plus 1fil}",
        "\\setlength{\\rightskip}{", pad_dim, "}",
        "\\arraybackslash"
      ))
    }
    paste0(
      base,
      "\\setlength{\\leftskip}{", pad_dim, " plus 1fil}",
      "\\setlength{\\rightskip}{", pad_dim, " plus 1fil}",
      "\\arraybackslash"
    )
  }

  align_chars <- strsplit(col_align, "", fixed = TRUE)[[1]]
  spec_for <- function(ch, wtok) paste0(">{", align_preamble(ch), "}p{", wtok, "}")
  colspec_parts <- mapply(spec_for, align_chars, vapply(seq_len(n), width_token, character(1)))
  colspec <- paste(colspec_parts, collapse = "|")

  header_cells <- paste0("\\bfseries ", fmt(col_labels))
  header_row <- paste0("\\rowcolor{", header_shade_name, "}", paste(header_cells, collapse = " & "), " \\\\\n")

  df_chr <- as.data.frame(
    lapply(seq_len(n), function(j) vapply(df[[j]], fmt_cell_scalar_for_col, character(1), col_i = j)),
    stringsAsFactors = FALSE
  )
  names(df_chr) <- names(df)

  body_rows <- if (nrow(df_chr) == 0) {
    ""
  } else {
    row_prefix <- if (shade_data_rows) paste0("\\rowcolor{", data_shade_name, "}") else ""
    paste(
      vapply(seq_len(nrow(df_chr)), function(i) {
        row <- df_chr[i, , drop = TRUE]
        if (i %in% bold_data_rows) {
          row <- vapply(row, function(v) paste0("{\\bfseries ", v, "}"), character(1))
        }
        paste0(row_prefix, paste(row, collapse = " & "))
      }, character(1)),
      collapse = " \\\\\n\\hline\n"
    )
  }
  if (nzchar(body_rows)) body_rows <- paste0(body_rows, " \\\\\n")

  caption_block <- if (has_caption) {
    paste0(
      "\\captionsetup{skip=", caption_skip_pt, "pt}\n",
      "\\caption*{\\captionbar{{\\fontsize{", caption_fontsize[1], "}{", caption_fontsize[2],
      "}\\selectfont\\textbf{", fmt(title), "}}}",
      if (caption_vspace_pt != 0) paste0("\\vspace{", caption_vspace_pt, "pt}") else "",
      "}\n"
    )
  } else ""

  after_caption_fix <- if (has_caption && caption_after_vspace_pt != 0) {
    paste0("\\vspace*{", format(caption_after_vspace_pt, trim = TRUE, scientific = FALSE), "pt}\n")
  } else ""

  wrap_cmds <- paste0(
    if (sloppy_table) "\\sloppy\n" else "",
    "\\emergencystretch=", format(emergencystretch_em, trim = TRUE, scientific = FALSE), "em\n"
  )

  if (!paginate) {
    tex <- paste0(
      "\\begin{table}[", placement, "]\n",
      caption_block,
      after_caption_fix,
      "\\fontsize{", body_fontsize[1], "pt}{", body_fontsize[2], "pt}\\selectfont\n",
      "\\arrayrulecolor{", chapblue_name, "}\n",
      "\\setlength{\\arrayrulewidth}{", arrayrulewidth_pt, "pt}\n",
      "\\setlength{\\tabcolsep}{", tabcolsep_pt, "pt}\n",
      "\\setlength{\\extrarowheight}{", row_extrarowheight_pt, "pt}\n",
      wrap_cmds, "\n",
      width_defs,
      "\\begin{tabular}{@{}|", colspec, "|@{}}\n",
      "\\hline\n",
      header_row,
      "\\hline\n",
      body_rows,
      "\\hline\n",
      "\\end{tabular}\n",
      "\\end{table}\n"
    )
  } else {
    caption_line <- if (has_caption) paste0(
      "\\captionsetup{skip=", caption_skip_pt, "pt}\n",
      "\\caption*{\\captionbar{{\\fontsize{", caption_fontsize[1], "}{", caption_fontsize[2],
      "}\\selectfont\\textbf{", fmt(title), "}}}",
      if (caption_vspace_pt != 0) paste0("\\vspace{", caption_vspace_pt, "pt}") else "",
      "}"
    ) else ""

    caption_break <- if (has_caption) {
      if (caption_after_vspace_pt != 0) {
        paste0("\\\\[", format(caption_after_vspace_pt, trim = TRUE, scientific = FALSE), "pt]\n")
      } else {
        "\\\\\n"
      }
    } else ""

    tex <- paste0(
      "\\begingroup\n",
      "\\fontsize{", body_fontsize[1], "pt}{", body_fontsize[2], "pt}\\selectfont\n",
      "\\arrayrulecolor{", chapblue_name, "}\n",
      "\\setlength{\\arrayrulewidth}{", arrayrulewidth_pt, "pt}\n",
      "\\setlength{\\tabcolsep}{", tabcolsep_pt, "pt}\n",
      "\\setlength{\\extrarowheight}{", row_extrarowheight_pt, "pt}\n",
      wrap_cmds, "\n",
      width_defs,
      "\\begin{longtable}{@{}|", colspec, "|@{}}\n",
      if (has_caption) paste0(caption_line, caption_break) else "",
      "\\hline\n",
      header_row,
      "\\hline\n",
      "\\endfirsthead\n",
      "\\hline\n",
      header_row,
      "\\hline\n",
      "\\endhead\n",
      body_rows,
      "\\hline\n",
      "\\end{longtable}\n",
      "\\endgroup\n"
    )
  }

  if (!wrap_raw_block) return(tex)
  paste0("```{=latex}\n", tex, "```\n")
}


# R/make_quarto_latex_table_2level.R
make_quarto_latex_table_2level <- function(
    title,
    df,
    col_labels = names(df),
    placement = "H",
    chapblue_name = "CHAPBLUE",
    header_shade_name = "HeaderShade",
    data_shade_name = "DataShade",
    shade_data_rows = TRUE,
    caption_fontsize = c(11, 11),
    body_fontsize = c(10, 12),
    arrayrulewidth_pt = 0.75,  # ~1px
    tabcolsep_pt = 0,
    caption_skip_pt = 0,
    caption_vspace_pt = 0,
    caption_after_vspace_pt = -1,
    escape_latex = TRUE,
    wrap_raw_block = TRUE,
    checkmarks = FALSE,
    col_align = NA,             # REQUIRED: e.g. "cc" or "clrrr"
    col_widths_pct = NULL,      # OPTIONAL: numeric vector, sums to 100
    cell_pad_em = 0.6,
    header_row_height_ex = 3.2,
    row_extrarowheight_pt = 2,
    sloppy_table = TRUE,
    emergencystretch_em = 2,
    paginate = FALSE
) {
  stopifnot(is.data.frame(df))
  if (ncol(df) < 1) stop("df must have at least 1 column.")
  if (length(col_labels) != ncol(df)) stop("col_labels must have length ncol(df).")

  has_caption <- !(is.null(title) || (length(title) == 1 && isTRUE(is.na(title))))
  if (has_caption && (!is.character(title) || length(title) != 1)) {
    stop("title must be a single character string, or NA to omit the caption.")
  }

  if (!is.numeric(caption_after_vspace_pt) || length(caption_after_vspace_pt) != 1 ||
      is.na(caption_after_vspace_pt)) {
    stop("caption_after_vspace_pt must be a single numeric value (e.g., -4, 0, 2).")
  }

  if (!is.numeric(row_extrarowheight_pt) || length(row_extrarowheight_pt) != 1 ||
      is.na(row_extrarowheight_pt) || row_extrarowheight_pt < 0) {
    stop("row_extrarowheight_pt must be a single non-negative number (e.g., 0.5).")
  }

  if (!is.character(data_shade_name) || length(data_shade_name) != 1 || !nzchar(data_shade_name)) {
    stop("data_shade_name must be a non-empty single character string (e.g., 'DataShade').")
  }
  if (!is.logical(shade_data_rows) || length(shade_data_rows) != 1 || is.na(shade_data_rows)) {
    stop("shade_data_rows must be TRUE/FALSE.")
  }

  if (!is.numeric(cell_pad_em) || length(cell_pad_em) != 1 || is.na(cell_pad_em) || cell_pad_em < 0) {
    stop("cell_pad_em must be a single non-negative number (e.g., 0.6).")
  }

  if (!is.logical(sloppy_table) || length(sloppy_table) != 1 || is.na(sloppy_table)) {
    stop("sloppy_table must be TRUE/FALSE.")
  }
  if (!is.numeric(emergencystretch_em) || length(emergencystretch_em) != 1 ||
      is.na(emergencystretch_em) || emergencystretch_em < 0) {
    stop("emergencystretch_em must be a single non-negative number (e.g., 2).")
  }
  if (!is.logical(paginate) || length(paginate) != 1 || is.na(paginate)) {
    stop("paginate must be TRUE/FALSE.")
  }

  n <- ncol(df)
  rules_count <- n + 1

  if (is.na(col_align) || !nzchar(col_align)) {
    stop("col_align is required (e.g. 'cc' or 'clrrr') and must have length ncol(df).")
  }
  if (nchar(col_align) != n) stop(sprintf("col_align must have %d characters (one per column).", n))
  if (!grepl(sprintf("^[lcr]{%d}$", n), col_align)) stop("col_align must contain only 'l', 'c', 'r'.")

  fmt_num <- function(x) format(x, trim = TRUE, scientific = FALSE, digits = 15)

  if (!is.null(col_widths_pct)) {
    if (length(col_widths_pct) != n) stop("col_widths_pct must have length ncol(df).")
    if (any(!is.finite(col_widths_pct)) || any(col_widths_pct <= 0)) stop("col_widths_pct must be positive numbers.")
    if (abs(sum(col_widths_pct) - 100) > 1e-6) stop("col_widths_pct must sum to 100.")
    col_widths_pct <- as.numeric(col_widths_pct)
  }

  escape_tex <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- gsub("\\\\", "\\\\textbackslash{}", x)
    x <- gsub("([%#$&_{}])", "\\\\\\1", x, perl = TRUE)
    x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("\\^", "\\\\textasciicircum{}", x, perl = TRUE)
    x
  }
  fmt <- function(x) if (escape_latex) escape_tex(x) else as.character(x)

  softbreak_plain <- function(s) {
    gsub("/", paste0("/", intToUtf8(92), "hspace{0pt}"), s, fixed = TRUE)
  }

  is_zero_one <- function(x) {
    if (is.na(x) || x == "") return(FALSE)
    suppressWarnings({
      nx <- as.numeric(x)
      !is.na(nx) && (nx == 0 || nx == 1)
    })
  }

  fmt_cell <- function(x) {
    x_chr <- as.character(x)
    x_chr[is.na(x_chr)] <- ""
    if (checkmarks && is_zero_one(x_chr)) {
      nx <- suppressWarnings(as.numeric(x_chr))
      return(if (nx == 1) "\\tick" else "\\cross")
    }
    softbreak_plain(fmt(x_chr))
  }

  split_two_levels <- function(lbl) {
    lbl <- as.character(lbl)
    m <- regexec("^(.*)_(.*)$", lbl)
    r <- regmatches(lbl, m)[[1]]
    if (length(r) == 3) list(group = r[2], sub = r[3]) else list(group = "", sub = lbl)
  }

  parts <- lapply(col_labels, split_two_levels)
  lvl1 <- vapply(parts, `[[`, character(1), "group")
  lvl2 <- vapply(parts, `[[`, character(1), "sub")

  width_exprs <- if (is.null(col_widths_pct)) {
    rep(
      paste0("\\dimexpr\\linewidth/", n, "-", rules_count, "\\arrayrulewidth/", n, "\\relax"),
      n
    )
  } else {
    vapply(col_widths_pct, function(p) {
      pk <- p * rules_count
      paste0("\\dimexpr", fmt_num(p), "\\linewidth/100-", fmt_num(pk), "\\arrayrulewidth/100\\relax")
    }, character(1))
  }

  pad_dim <- paste0(format(cell_pad_em, trim = TRUE, scientific = FALSE), "em")

  align_prefix <- function(ch) {
    base <- "\\setlength{\\parindent}{0pt}\\setlength{\\parfillskip}{0pt}"
    if (ch == "l") {
      return(paste0(
        ">{", base,
        "\\setlength{\\leftskip}{", pad_dim, "}",
        "\\setlength{\\rightskip}{", pad_dim, " plus 1fil}",
        "\\arraybackslash}"
      ))
    }
    if (ch == "r") {
      return(paste0(
        ">{", base,
        "\\setlength{\\leftskip}{", pad_dim, " plus 1fil}",
        "\\setlength{\\rightskip}{", pad_dim, "}",
        "\\arraybackslash}"
      ))
    }
    paste0(
      ">{", base,
      "\\setlength{\\leftskip}{", pad_dim, " plus 1fil}",
      "\\setlength{\\rightskip}{", pad_dim, " plus 1fil}",
      "\\arraybackslash}"
    )
  }

  colspec_parts <- vapply(seq_len(n), function(i) {
    paste0(align_prefix(substr(col_align, i, i)), "p{", width_exprs[i], "}")
  }, character(1))
  colspec <- paste(colspec_parts, collapse = "|")

  rr <- rle(lvl1)
  spans <- rr$lengths
  groups <- rr$values

  header1_cells <- vapply(seq_along(spans), function(i) {
    g <- fmt(groups[i])
    span <- spans[i]
    mc_spec <- if (i == 1) "|c|" else "c|"
    content <- if (nzchar(g)) paste0("\\bfseries ", g) else paste0("\\rule{0pt}{", header_row_height_ex, "ex}")
    paste0(
      "\\multicolumn{", span, "}{", mc_spec, "}{",
      "\\cellcolor{", header_shade_name, "}",
      content,
      "}"
    )
  }, character(1))
  header_row_1 <- paste0(paste(header1_cells, collapse = " & "), " \\\\\n")

  header2_cells <- vapply(seq_len(n), function(i) {
    paste0(
      "\\cellcolor{", header_shade_name, "}",
      "\\rule{0pt}{", header_row_height_ex, "ex}",
      "\\bfseries ", fmt(lvl2[i])
    )
  }, character(1))
  header_row_2 <- paste0(paste(header2_cells, collapse = " & "), " \\\\\n")

  df_chr <- as.data.frame(lapply(df, fmt_cell), stringsAsFactors = FALSE)
  row_prefix <- if (shade_data_rows) paste0("\\rowcolor{", data_shade_name, "}") else ""

  body_rows <- if (nrow(df_chr) == 0) {
    ""
  } else {
    paste(
      apply(df_chr, 1, function(row) paste0(row_prefix, paste(row, collapse = " & "))),
      collapse = " \\\\\n\\hline\n"
    )
  }
  if (nzchar(body_rows)) body_rows <- paste0(body_rows, " \\\\\n")

  extra_rowheight_cmd <- if (row_extrarowheight_pt > 0) {
    paste0(
      "\\setlength{\\extrarowheight}{",
      format(row_extrarowheight_pt, trim = TRUE, scientific = FALSE),
      "pt}\n"
    )
  } else ""

  caption_block <- if (has_caption) {
    paste0(
      "\\captionsetup{skip=", caption_skip_pt, "pt}\n",
      "\\caption*{\\captionbar{{\\fontsize{", caption_fontsize[1], "}{", caption_fontsize[2],
      "}\\selectfont\\textbf{", fmt(title), "}}}",
      if (caption_vspace_pt != 0) paste0("\\vspace{", caption_vspace_pt, "pt}") else "",
      "}\n"
    )
  } else ""

  after_caption_fix <- if (has_caption && caption_after_vspace_pt != 0) {
    paste0("\\vspace*{", format(caption_after_vspace_pt, trim = TRUE, scientific = FALSE), "pt}\n")
  } else ""

  wrap_cmds <- paste0(
    if (sloppy_table) "\\sloppy\n" else "",
    "\\emergencystretch=", format(emergencystretch_em, trim = TRUE, scientific = FALSE), "em\n"
  )

  if (!paginate) {
    tex <- paste0(
      "\\begin{table}[", placement, "]\n",
      caption_block,
      after_caption_fix,
      "\\fontsize{", body_fontsize[1], "pt}{", body_fontsize[2], "pt}\\selectfont\n",
      "\\arrayrulecolor{", chapblue_name, "}\n",
      "\\setlength{\\arrayrulewidth}{", arrayrulewidth_pt, "pt}\n",
      "\\setlength{\\tabcolsep}{", tabcolsep_pt, "pt}\n",
      extra_rowheight_cmd,
      wrap_cmds, "\n",
      "\\begin{tabular}{@{}|", colspec, "|@{}}\n",
      "\\hline\n",
      header_row_1,
      "\\hline\n",
      header_row_2,
      "\\hline\n",
      body_rows,
      "\\hline\n",
      "\\end{tabular}\n",
      "\\end{table}\n"
    )
  } else {
    caption_line <- if (has_caption) paste0(
      "\\captionsetup{skip=", caption_skip_pt, "pt}\n",
      "\\caption*{\\captionbar{{\\fontsize{", caption_fontsize[1], "}{", caption_fontsize[2],
      "}\\selectfont\\textbf{", fmt(title), "}}}",
      if (caption_vspace_pt != 0) paste0("\\vspace{", caption_vspace_pt, "pt}") else "",
      "}"
    ) else ""

    caption_break <- if (has_caption) {
      if (caption_after_vspace_pt != 0) {
        paste0("\\\\[", format(caption_after_vspace_pt, trim = TRUE, scientific = FALSE), "pt]\n")
      } else {
        "\\\\\n"
      }
    } else ""

    tex <- paste0(
      "\\begingroup\n",
      "\\fontsize{", body_fontsize[1], "pt}{", body_fontsize[2], "pt}\\selectfont\n",
      "\\arrayrulecolor{", chapblue_name, "}\n",
      "\\setlength{\\arrayrulewidth}{", arrayrulewidth_pt, "pt}\n",
      "\\setlength{\\tabcolsep}{", tabcolsep_pt, "pt}\n",
      extra_rowheight_cmd,
      wrap_cmds, "\n",
      "\\begin{longtable}{@{}|", colspec, "|@{}}\n",
      if (has_caption) paste0(caption_line, caption_break) else "",
      "\\hline\n",
      header_row_1,
      "\\hline\n",
      header_row_2,
      "\\hline\n",
      "\\endfirsthead\n",
      "\\hline\n",
      header_row_1,
      "\\hline\n",
      header_row_2,
      "\\hline\n",
      "\\endhead\n",
      body_rows,
      "\\hline\n",
      "\\end{longtable}\n",
      "\\endgroup\n"
    )
  }

  if (!wrap_raw_block) return(tex)
  paste0("```{=latex}\n", tex, "```\n")
}
