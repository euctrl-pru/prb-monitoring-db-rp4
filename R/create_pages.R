
# replace index, quarto yaml and css files ----
## quarto yaml
if (out_format == 'pdf') {
  file.copy('_original_files/full_quarto_pdf.yml', '_quarto.yml', overwrite = TRUE, copy.mode = TRUE)
} else {
  file.copy('_original_files/full_quarto.yml', '_quarto.yml', overwrite = TRUE, copy.mode = TRUE)
}

## styles css
file.copy('_original_files/full_styles.css', 'styles.css', overwrite = TRUE, copy.mode = TRUE)
  
# remove .qmd files ----
## get file list 
root_files <- list.files()
  
## remove all non '_' qmd files 
for (i in 1:length(root_files)) {
  if (grepl('.qmd', root_files[i], fixed = TRUE) == TRUE & substr(root_files[i], 1, 1) != '_') {
    file.remove(root_files[i])
  }
}
    
# create index pages ----
if (investments) {
  tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
  
  if (out_format == 'pdf') {
    # tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/nm_index_pdf.qmd")
  } else {
    tmp_text <- str_replace(tmp_text, "file-placeholder", "_investments.qmd") 
  }
  
  writeLines(tmp_text, 'index.qmd')

} else if (country == "Home") {
  file.copy('_original_files/home_index.qmd', 'index.qmd', overwrite = TRUE, copy.mode = TRUE)
  ###for the home page we add as well the other qmds here
  file.copy('_original_files/home_about.qmd', 'about.qmd', overwrite = TRUE, copy.mode = TRUE)
  file.copy('_original_files/home_disclaimer.qmd', 'disclaimer.qmd', overwrite = TRUE, copy.mode = TRUE)
  file.copy('_original_files/home_download.qmd', 'download.qmd', overwrite = TRUE, copy.mode = TRUE)
  
  ### we also remove one line from the css file
  tmp_text <- readLines("_original_files/full_styles.css")
  tmp_text <- str_replace(tmp_text, fixed("  font-size: 0.9rem;"), "") 
  writeLines(tmp_text, 'styles.css')

} else if (country == "Network Manager") {
  tmp_text <- readLines("_original_files/common_qmd_setup.qmd")

  if (out_format == 'pdf') {
    tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/nm_index_pdf.qmd")
  } else {
    tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/nm_index.qmd") 
  }
  
  writeLines(tmp_text, 'index.qmd')

} else if (country == "MUAC") {
  tmp_text <- readLines("_original_files/common_qmd_setup.qmd")

  if (out_format == 'pdf') {
    tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/muac_index_pdf.qmd")
  } else {
    tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/muac_index.qmd")
  }

  writeLines(tmp_text, 'index.qmd')

} else if (country == rp_full) {
    tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
    if (out_format == 'pdf') {
      tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/ses_index_pdf.qmd")
    } else {
      tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/ses_index.qmd")
    }
    writeLines(tmp_text, 'index.qmd')
    
} else {
    tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
    if (out_format == 'pdf') {
      tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/state_index_pdf.qmd")
    } else {
      tmp_text <- str_replace(tmp_text, "file-placeholder", "_original_files/state_index.qmd") 
    }
    writeLines(tmp_text, 'index.qmd')
}
  
# generate level 2 .qmd master files ----
## set list of level 2 files depending on case
if (investments | country == "Home" | country == "Network Manager" | year_folder == "rp3") {
  level2_files <- ""
  
} else if(country == "MUAC") {
  level2_files <- c("capacity.qmd",
                    "cost-efficiency-muac.qmd",
                    "safety.qmd")

} else {
  level2_files <- c("capacity.qmd",
                    "environment.qmd",
                    "safety.qmd")
  
  for (i in 1:no_ecz) {
    # add level2 cef er .qmd file name to list
    level2_files <- append(level2_files, paste0("cost-efficiency-er",i,"-1.qmd"))
    
    # generate level2 _cef enroute files from generic file
    tmp_text <- readLines("_cost-efficiency-generic.qmd")
    tmp_text <- str_replace(tmp_text, "@@cz_index@@", as.character(i)) 
    tmp_text <- str_replace(tmp_text, "@@cz_type@@", "enroute") 
    tmp_text <- str_replace(tmp_text, "@@cz_type_proper@@", "En route") 
    tmp_text <- str_replace(tmp_text, "@@cz_short@@", "ecz") 
    writeLines(tmp_text, paste0('_cost-efficiency-er', i,'-1.qmd'))
    
  }
  
  if (no_tcz > 0) {
    for (i in 1:no_tcz) {
      # add level2 cef trm .qmd file name to list
      level2_files <- append(level2_files, paste0("cost-efficiency-tz",i,"-1.qmd"))
      
      # generate level2 _cef enroute files from generic file
      tmp_text <- readLines("_cost-efficiency-generic.qmd")
      tmp_text <- str_replace(tmp_text, "@@cz_index@@", as.character(i)) 
      tmp_text <- str_replace(tmp_text, "@@cz_type@@", "terminal") 
      tmp_text <- str_replace(tmp_text, "@@cz_type_proper@@", "Terminal") 
      tmp_text <- str_replace(tmp_text, "@@cz_short@@", "tcz") 
      
      writeLines(tmp_text, paste0('_cost-efficiency-tz', i,'-1.qmd'))
    }
  }
}
  
level2_files <- sort(level2_files)
  
# generate actual .qmd files linked to respective _.qmd files
for (i in 1:length(level2_files)) {
  tmp_text <- readLines("_original_files/common_qmd_setup.qmd")
  tmp_text <- str_replace(tmp_text, "file-placeholder", paste0("_", level2_files[i])) 
  writeLines(tmp_text, level2_files[i])
}
  
# remove and regenerate variables ----
  file.remove('_variables.yml')
  newvariables <- paste0("doc:
    year_report: ", year_report, "
    year_folder: ", toupper(as.character(year_folder)), "
    country: '", country, "'
    country_lower: '", country_lower, "'
    ecz1: '", ecz_list$ecz_name[1], "'
    ecz2: '", ecz_list$ecz_name[2], "'
    tcz1: '", tcz_list$tcz_name[1], "'
    tcz2: '", tcz_list$tcz_name[2], "'
  "
  )
  
  cat(newvariables, file = "_variables.yml")

# modify _quarto.yml ----
if (out_format == 'web') {
  tx  <- readLines("_quarto.yml")
  ## replace string by country name as {{< var doc.country >}} gives problems in small screens
  tx <- str_replace(tx, 'country placeholder', country)
  ## set destination directory
  tx <- str_replace(tx, 'destination-dir/', external_address)
  ## set home address for other year reports
  tx <- str_replace(tx, 'home_address', home_address)
  tx <- str_replace(tx, 'country_lower', country_lower)  
  ## add check box to year report
  tx <- str_replace(tx, paste0('text: "', toupper(year_folder), '"'),
                        paste0('text: "<span style= \'color: #2151bf\'>', toupper(year_folder), ' &#10003</span>"'))
    
  if (investments | country == "Home") {
    ## Home page case ----
    ### find beginning and end of blocks to remove
    for (i in 1:length(tx)) {
      if (tx[i] %like% 'sidebar:') {block_beg = i}
      if (tx[i] %like% 'block level2 end') {block_end = i}
      }
    ### this removes the unwanted lines
    tx <- tx[-c(block_beg:block_end)]
    
    if (investments) {
      ### add investments sidebar
      sidebar_text <- readLines("_original_files/side_bar_investments.yml")
      ### modify tags
      sidebar_text <- str_replace(sidebar_text, 
                                  'country placeholder', 
                                  country)
      sidebar_text <- str_replace(sidebar_text, 
                                  '_countryansp', 
                                  paste0(
                                    "-",
                                    country_lower,
                                    "-",
                                    str_replace_all(tolower(main_ansp),
                                                    " ",
                                                    "-"))
                                  )  
      
      ## remove terminal entry
      if(no_tcz == 0) {
        sidebar_text <- str_replace(sidebar_text,
                                    '- text: "Terminal"',
                                    '# - text: "Terminal"')
        sidebar_text <- str_replace(sidebar_text,
                                    'href: index.html#costs-of-investments-by-nature---terminal', 
                                    '# href: index.html#costs-of-investments-by-nature---terminal' 
                                  )  
        
      }
      
      ## SES case
      if(country == rp_full) {
        sidebar_text <- str_replace(sidebar_text,
                                    '- text: "Compared to Union-wide"',
                                    '# - text: "Compared to Union-wide"')
        sidebar_text <- str_replace(sidebar_text,
                                    'href: index.html#asset-value-for-new-investments-ansp-compared-to-union-wide', 
                                    '# href: index.html#asset-value-for-new-investments-ansp-compared-to-union-wide' 
        )  

        sidebar_text <- str_replace(sidebar_text,
                                    '- text: "New major investments"',
                                    '# - text: "New major investments"')
        sidebar_text <- str_replace(sidebar_text,
                                    'href: index.html#costs-by-new-major-investments', 
                                    '# href: index.html#costs-by-new-major-investments' 
        )  
        
      }
      
      
      tx <- append(tx, sidebar_text, after = block_beg - 1)
    }
    
    ### write new file
    # writeLines(tx, con="_quarto.yml")
      
  } 
  else if (country == "Network Manager") {
    ## NM ----
    ### level 1 ----
    ### find beginning and end of level 1 state-SES block
    for (i in 1:length(tx)) {
      if (tx[i] %like% 'block level1 state beginning') {block_l1_sta_beg = i}
      if (tx[i] %like% 'block level1 ses end') {block_l1_ses_end = i}
      # if (tx[i] %like% 'block level1 state end') {block_l1_sta_end = i}
    }
    
    tx <- tx[-c(block_l1_sta_beg:block_l1_ses_end)]
    
    ### level 2 ----
    ### find beginning and end of level 2 block to remove
    for (i in 1:length(tx)) {
      if (tx[i] %like% 'block level2 beginning') {block_l2_beg = i}
      if (tx[i] %like% 'block level2 end') {block_l2_end = i}
    }  
  
    tx <- tx[-c(block_l2_beg:block_l2_end)]

  } 
  else {
    ## level 1 ----
    if (country == rp_full) {
      ### SES case ----
      ### remove nm level 1 block
      for (i in 1:length(tx)) {
        if (tx[i] %like% 'block level1 nm beginning') {block_l1_nm_beg = i}
        if (tx[i] %like% 'block level1 nm end') {block_l1_nm_end = i}
        }
       
      tx <- tx[-c(block_l1_nm_beg:block_l1_nm_end)]
       
      ### remove state level1 block
      for (i in 1:length(tx)) {
        if (tx[i] %like% 'block level1 state beginning') {block_l1_sta_beg = i}
        if (tx[i] %like% 'block level1 state end') {block_l1_sta_end = i}
        }

      tx <- tx[-c(block_l1_sta_beg:block_l1_sta_end)]
     
    } 
    else {
    ### state case ----
      ### find beginning and end of level 1 blocks to remove
      for (i in 1:length(tx)) {
         if (tx[i] %like% 'block level1 ses beginning') {block_l1_ses_beg = i}
         if (tx[i] %like% 'block level1 nm end') {block_l1_nm_end = i}
       }
      
       tx <- tx[-c(block_l1_ses_beg:block_l1_nm_end)]
       
       ### remove env for luxembourg, MUAC ----
       if (country == "Luxembourg" | country == "MUAC") {
         tx <- str_replace(tx, '- text: "Environment"', '# - text: "Environment""')
         tx <- str_replace(tx, 'href: index.html#environment-member-state', '# href: index.html#environment-member-state')
       }
       
       ### remove traffic for MUAC ----
       if (country == "MUAC") {
         tx <- str_replace(tx, '- text: "Traffic"', '# - text: "Traffic""')
         tx <- str_replace(tx, 'href: index.html#traffic-en-route-traffic-zone', 
                           '# href: index.html#traffic-en-route-traffic-zone')
       }
    }
     
    ## level 2 ----
    ### MUAC, remove elements ----
    if (country == "MUAC") {
      
      ### replace some links 
      lines_to_replace <- c(
        '-main-ansp #tag',
        '-member-state #tag',
        '-en-routeterminal-charging-zones #tag'
      )

      for (i in 1:length(lines_to_replace)) {
        tx <- str_replace(tx, lines_to_replace[i], '')
      }
      
      ### find beginning and end of safety occurrences to remove
      for (i in 1:length(tx)) {
        if (tx[i] %like% 'block muac beginning') {block_muac_beg = i}
        if (tx[i] %like% 'block muac end') {block_muac_end = i}
      }
      
      tx <- tx[-c(block_muac_beg:block_muac_end)]
      
      ### find beginning and end of level2 environment to remove
      for (i in 1:length(tx)) {
        if (tx[i] %like%  'environment level2 beginning') {block_l2_env_beg = i}
        if (tx[i] %like%  'environment level2 end') {block_l2_env_end = i}
      }
      
      tx <- tx[-c(block_l2_env_beg:block_l2_env_end)]
      
      ### find beginning and end of level2 ceff to remove
      for (i in 1:length(tx)) {
        if (tx[i] %like% 'cost-efficiency level2 beginning') {block_l2_cef_beg = i}
        if (tx[i] %like% 'block level2 end') {block_l2_cef_end = i}
      }
      
      tx <- tx[-c(block_l2_cef_beg:block_l2_cef_end)]
      
      ### add specific muac ceff except for year rp3
      if (year_folder != 'rp3') {
        tx_cef_muac <- readLines("_original_files/level2_cef_muac.yml")
        tx <- append(tx, tx_cef_muac, block_l2_cef_beg)
      }
    }
    
    if (state_type != 0) {
      ### SES/luxembourg, remove env-mil ----
      if (country == rp_full | country == "Luxembourg") {
        tx <- str_replace(tx, '- text: "<b>CIV-MIL</b>"', '# - text: "<b>CIV-MIL</b>"')
        tx <- str_replace(tx, 'href: environment.html#civil-military-dimension', '# href: environment.html#civil-military-dimension')
      }

      ### luxembourg, remove other elements ----
      if (country == "Luxembourg") {
        lines_to_comment <- c(
          '- section: "<b>En route performance</b>" #environment',
          'href: environment.html#en-route-performance',
          'contents: #environment',
          '- text: "Horizontal flight efficiency"',
          'href: environment.html#horizontal-flight-efficiency-of-the-actual-trajectory-kea-kpi1-of-the-last-filed-flight-plan-kep-pi1-shortest-constrained-route-scr-pi2',
          '- section: "<b>En route performance</b>" #capacity',
          'href: capacity.html#en-route-performance',
          'contents: #capacity',
          '- text: "En route ATFM delay"',
          'href: capacity.html#en-route-atfm-delay-kpi1',
          '- text: "Other indicators"',
          'href: capacity.html#other-indicators'
        )
        
        for (i in 1:length(lines_to_comment)) {
          tx <- str_replace(tx, lines_to_comment[i],
                            paste0("# ", lines_to_comment[i]))
        }
      }
      
      ### with terminal zone(s) ----
      
      # add text for the additional tczs and env/cap terminal
      if (country != rp_full) {tx_env <- readLines("_original_files/level2_env_terminal.yml")} else {tx_env = ''}
      tx_cap <- readLines("_original_files/level2_cap_terminal.yml")
      tx_tcz_initial <- readLines("_original_files/level2_cef_tcz_xy.yml")
      tx_tcz <- ''
      
      for (i in 1:no_tcz) {
        tx_tcz <- append(tx_tcz, str_replace(tx_tcz_initial, "@@cz_index@@", as.character(i)))
        tx_tcz <- str_replace(tx_tcz, "@@cz_name@@", if_else(no_tcz == 1, "", paste0(" - ", tcz_list$tcz_name[[i]]))) 
      }
      
      # find position to insert terminal blocks
      for (i in 1:length(tx)) {
        if (tx[i] %like% '# include env_terminal block') {env_block_beg = i}
        if (tx[i] %like% '# include cap_terminal block') {cap_block_beg = i}
        if (tx[i] %like% '# include TCZ block') {cef_block_beg = i}
      }  
      
      tx <- append(tx, tx_env, env_block_beg) 
      tx <- append(tx, tx_cap, cap_block_beg + length(tx_env)) 
      tx <- append(tx, tx_tcz, cef_block_beg + length(tx_cap) + + length(tx_env)) 
      
      ### 2 en route cz (spain) ----
      if (state_type == 3) {
        tx <- str_replace(tx, "<b>En route CZ</b>", paste0("<b>En route CZ - ", ecz_list$ecz_name[[1]], "</b>")) 
        
        # add text for the additional eczs
        tx_ecz_initial <- readLines("_original_files/level2_cef_ecz_xy.yml")
        tx_ecz <- ''
        for (i in 2:no_ecz) {
          tx_ecz <- append(tx_ecz, str_replace(tx_ecz_initial, "@@cz_index@@", as.character(i)))
          tx_ecz <- str_replace(tx_ecz, "@@cz_name@@", paste0(" - ", ecz_list$ecz_name[[i]])) 
        }
        
        # find position to insert
        for (i in 1:length(tx)) {
          if (tx[i] %like% '# include ECZ2 block') {block_beg = i}
        }  
        
        tx <- append(tx, tx_ecz, block_beg) 
        
        }

      }
  

  }
  
  ## year = rp3 case ----
  if (year_folder == "rp3") {
    ### level 2
    ### find beginning and end of level 2 block to remove
      for (i in 1:length(tx)) {
        if (tx[i] %like% 'rp3 marker beg') {block_l2_beg = i}
        if (tx[i] %like% 'rp3 marker end') {block_l2_end = i}
      }  
    
    tx <- tx[-c(block_l2_beg:block_l2_end)]
  }
    
  ## write new file ----
  writeLines(tx, con="_quarto.yml")
}
    
# render site ----
  quarto::quarto_render(as_job = FALSE,
                        execute_params = list(home_address = home_address,
                                              state_list = state_list, 
                                              country = country, 
                                              year_report = year_report,
                                              year_folder = year_folder,
                                              data_folder = data_folder,
                                              forecast = forecast,
                                              forecast_id = forecast_id,
                                              statfor_zone = statfor_zone,
                                              ecz_list = ecz_list,
                                              no_ecz = no_ecz,
                                              tcz_list = tcz_list,
                                              no_tcz = no_tcz,
                                              nat_curr = nat_curr,
                                              xrate2017 = xrate2017,
                                              pp_version = pp_version,
                                              acc_no = acc_no,
                                              acc_list = acc_list,
                                              acc1 = acc1,
                                              acc2 = acc2,
                                              acc3 = acc3,
                                              acc4 = acc4,
                                              acc5 = acc5,
                                              no_apt_big = no_apt_big,
                                              no_apt_small = no_apt_small,
                                              tsu_share = tsu_share,
                                              ert_cost_share = ert_cost_share,
                                              ert_trm_share = ert_trm_share,
                                              main_ansp = main_ansp,
                                              main_ansp_aua = main_ansp_aua,
                                              other_ansps_str = other_ansps_str,
                                              other_met_str = other_met_str,
                                              
                                              no_saf_ansps = no_saf_ansps,
                                              main_safety_ansp = main_safety_ansp,
                                              saf_ansps = saf_ansps,
                                              
                                              # yearly_xrates = yearly_xrates,
                                              
                                              # chart_layout = chart_layout, 
                                              mywidth = mywidth,
                                              myheight = myheight,
                                              myfont = myfont,
                                              mymargin = mymargin,
                                              mylinewidth = mylinewidth,
                                              
                                              mysuffix =mysuffix,
                                              mydecimals =mydecimals,
                                              
                                              ### trace parameters
                                              mycolors = mycolors,
                                              mytextangle =mytextangle,
                                              mytextposition =mytextposition,
                                              myinsidetextanchor =myinsidetextanchor,
                                              mytextfont_color =mytextfont_color,
                                              mytextfont_size =mytextfont_size,
                                              
                                              # myhovertemplate =myhovertemplate,
                                              mytrace_showlegend =mytrace_showlegend,
                                              
                                              ### layout parameters
                                              myfont_family =myfont_family,
                                              mybargap =mybargap,
                                              mybarmode =mybarmode,
                                              myhovermode =myhovermode,
                                              myhoverlabel_bgcolor =myhoverlabel_bgcolor,
                                              myminsize =myminsize,
                                              
                                              #### title
                                              mytitle_text =mytitle_text,
                                              mytitle_x =mytitle_x,
                                              mytitle_y =mytitle_y,
                                              mytitle_xanchor =mytitle_xanchor,
                                              mytitle_yanchor =mytitle_yanchor,
                                              mytitle_font_size =mytitle_font_size,
                                              
                                              #### xaxis
                                              myxaxis_title =myxaxis_title,
                                              myxaxis_gridcolor =myxaxis_gridcolor,
                                              myxaxis_showgrid =myxaxis_showgrid,
                                              myxaxis_showline =myxaxis_showline,
                                              myxaxis_showticklabels =myxaxis_showticklabels,
                                              myxaxis_tickformat =myxaxis_tickformat,
                                              myxaxis_dtick =myxaxis_dtick,
                                              myxaxis_zeroline =myxaxis_zeroline,
                                              myxaxis_tickfont_size =myxaxis_tickfont_size,
                                              
                                              #### yaxis
                                              myyaxis_title =myyaxis_title,
                                              myyaxis_gridcolor =myyaxis_gridcolor,
                                              myyaxis_showgrid =myyaxis_showgrid,
                                              myyaxis_showline =myyaxis_showline,
                                              myyaxis_tickprefix =myyaxis_tickprefix,
                                              myyaxis_ticksuffix =myyaxis_ticksuffix,
                                              myyaxis_tickformat =myyaxis_tickformat,
                                              
                                              myyaxis_zeroline =myyaxis_zeroline,
                                              myyaxis_zerolinecolor =myyaxis_zerolinecolor,
                                              myyaxis_titlefont_size =myyaxis_titlefont_size,
                                              myyaxis_tickfont_size =myyaxis_tickfont_size,
                                              
                                              #### legend
                                              mylegend_traceorder =mylegend_traceorder,
                                              mylegend_orientation =mylegend_orientation,
                                              mylegend_xanchor =mylegend_xanchor,
                                              mylegend_yanchor =mylegend_yanchor,
                                              mylegend_x =mylegend_x,
                                              mylegend_y =mylegend_y,
                                              mylegend_font_size =mylegend_font_size,
                                              
                                              #### margin
                                              mylocalmargin = mylocalmargin,
                                              
                                              ## pdf
                                              mywidth_pdf = mywidth_pdf,
                                              myheight_pdf = myheight_pdf,
                                              myfont_pdf = myfont_pdf,
                                              mytitle_font_size_pdf = mytitle_font_size_pdf,
                                              mymargin_pdf = mymargin_pdf,
                                              mylinewidth_pdf = mylinewidth_pdf,

                                              ## monitoring files
                                              cap_file = cap_file,
                                              cap_trm_file = cap_trm_file,
                                              ceff_file = ceff_file,
                                              ceff_file_canarias = ceff_file_canarias,
                                              env_kea_file = env_kea_file,
                                              env_apt_file = env_apt_file,
                                              env_mil_file = env_mil_file,
                                              saf_eosm_file = saf_eosm_file
                                              )
                        )

