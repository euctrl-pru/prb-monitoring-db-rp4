if (!exists("country") | is.na(country)) {country = "Austria"}
if (exists("cztype") == FALSE) {cztype = "terminal"}

# safety ----
# define ranges and import data
if (country == rp_full) {
  saf_text_ses <- read_xlsx(
    path = here::here(data_folder, "SAF_input_text.xlsx"),
    sheet = "SAF_text",
    range = cell_limits(c(1, 1), c(NA, NA))
  ) %>%
    as_tibble() %>%
    janitor::clean_names()
  
  ## eosm
  saf_eosm_text_df <- saf_text_ses %>% 
    filter(topic == "EoSM" & year == year_report) %>% 
    arrange(numbering) %>% 
    select(title, text)
  
  saf_eosm_text <- '' 
  
  for (i in 1:nrow(saf_eosm_text_df)) {
    saf_eosm_text <- paste0(saf_eosm_text,
                            if_else(i!=1, "\n\n", ""),
                            saf_eosm_text_df$title[i],
                            "\n\n",
                            saf_eosm_text_df$text[i],
                            "\n\n"
    )
  }
  
  ## ri
  saf_ri_text_df <- saf_text_ses %>% 
    filter(topic == "RI" & year == year_report) %>% 
    arrange(numbering) %>% 
    select(title, text)
  
  saf_ri_text <- '' 
  
  for (i in 1:nrow(saf_ri_text_df)) {
    saf_ri_text <- paste0(saf_ri_text,
                          if_else(i!=1, "\n\n", ""),
                          saf_ri_text_df$title[i],
                          "\n\n",
                          saf_ri_text_df$text[i],
                          "\n\n"
    )
  }
  
  # exception for 2024 ses report
  if(country == rp_full & year_folder == 2024){
    saf_ri_text <- saf_ri_text %>% 
      str_replace_all(fixed('**RI with Safety Impact by Airport**'),
                      fixed('\\newpage\\\
                            **RI with Safety Impact by Airport**'))
      
  }
  
  ## smi
  saf_smi_text_df <- saf_text_ses %>% 
    filter(topic == "SMI" & year == year_report) %>% 
    arrange(numbering) %>% 
    select(title, text)
  
  saf_smi_text <- '' 
  
  for (i in 1:nrow(saf_smi_text_df)) {
    saf_smi_text <- paste0(saf_smi_text,
                           if_else(i!=1, "\n\n", ""),
                           saf_smi_text_df$title[i],
                           "\n\n",
                           saf_smi_text_df$text[i],
                           "\n\n"
    )
  }
  
  if (year_report == 2024) {
    pagebreak <- "\n\n```{=tex}\n\\newpage\n```\n\n"
    needle <- "**SMI 2023-2025**"
    
    saf_smi_text <- gsub(
      needle,
      paste0(pagebreak, needle),
      saf_smi_text,
      fixed = TRUE
    )
  }
  
  ## Quality reporting
  saf_qr_text_df <- saf_text_ses %>% 
    filter(topic == "Quality reporting" & year == year_report) %>% 
    arrange(numbering) %>% 
    select(title, text)
  
  saf_qr_text <- '' 
  
  for (i in 1:nrow(saf_qr_text_df)) {
    saf_qr_text <- paste0(saf_qr_text,
                          if_else(i!=1, "\n\n", ""),
                          saf_qr_text_df$title[i],
                          "\n\n",
                          saf_qr_text_df$text[i],
                          "\n\n"
    )
  }
  
  saf_asdr_text <- ''
  
  
} else {
  sheet <- country
  
  range <- "A15:G30"
  saf_all <- read_range(saf_eosm_file, sheet, range) 
  saf_all <- saf_all |> 
    rename(a = 1) 
  
  # replace NAs by empty strings
  saf_all[is.na(saf_all)] <- ""
  
  #define titles to lookup
  saf_titles <- c("Observations",
                  "**Observations**"
  )
  
  saf_heading_positions <- which(saf_all$a %in% saf_titles)
  
  saf_eosm_text <- saf_all[(saf_heading_positions[1]+1):(saf_heading_positions[1]+1), 1]
  
  #additional saf text fro excel file
  saf_text_country <- read_xlsx(
    path = here::here(data_folder, "SAF_input_text.xlsx"),
    sheet = "SAF_STATE_text",
    range = cell_limits(c(1, 1), c(NA, NA))
  ) %>%
    as_tibble() %>%
    janitor::clean_names() %>% 
    filter(state == country)
  
  ## eosm
  saf_eosm_text_df <- saf_text_country %>% 
    filter(tolower(topic) == "eosm" & year == year_report) %>% 
    arrange(numbering) %>% 
    select(text)
  
  
  for (i in 1:nrow(saf_eosm_text_df)) {
    saf_eosm_text <- paste0(saf_eosm_text,
                            if_else(i== 1, "\n\n",""),
                            saf_eosm_text_df$text[i],
                            if_else(i== nrow(saf_eosm_text_df), "","\n\n")
    )
  }
  
  ## ri
  saf_ri_text_df <- saf_text_country %>% 
    filter(tolower(topic) == "ri" & year == year_report) %>% 
    arrange(numbering) %>% 
    select(text)
  
  saf_ri_text <- '' 
  
  for (i in 1:nrow(saf_ri_text_df)) {
    saf_ri_text <- paste0(saf_ri_text,
                          saf_ri_text_df$text[i],
                          if_else(i== nrow(saf_ri_text_df), "","\n\n")
    )
  }
  
  ## smi
  saf_smi_text_df <- saf_text_country %>% 
    filter(tolower(topic) == "smis" & year == year_report) %>% 
    arrange(numbering) %>% 
    select(text)
  
  saf_smi_text <- '' 
  
  for (i in 1:nrow(saf_smi_text_df)) {
    saf_smi_text <- paste0(saf_smi_text,
                           saf_smi_text_df$text[i],
                           if_else(i== nrow(saf_smi_text_df), "","\n\n")
    )
  }
  
  ## Quality reporting
  saf_qr_text_df <- saf_text_country %>% 
    filter(tolower(topic) == "quality reporting" & year == year_report) %>% 
    arrange(numbering) %>% 
    select(text)
  
  saf_qr_text <- '' 
  
  for (i in 1:nrow(saf_qr_text_df)) {
    saf_qr_text <- paste0(saf_qr_text,
                          saf_qr_text_df$text[i],
                          if_else(i== nrow(saf_qr_text_df), "","\n\n")
    )
  }
  
  ## asdr
  saf_asdr_text_df <- saf_text_country %>% 
    filter(tolower(topic) == "asdr" & year == year_report) %>% 
    arrange(numbering) %>% 
    select(text)
  
  saf_asdr_text <- '' 
  
  for (i in 1:nrow(saf_asdr_text_df)) {
    saf_asdr_text <- paste0(saf_asdr_text,
                            saf_asdr_text_df$text[i],
                            if_else(i== nrow(saf_asdr_text_df), "","\n\n")
    )
  }
  
  
}



# environment ----

if (country == rp_full) {
  #we need to define this variables to avoid errors
  env_apt_2 <- ''
  env_apt_3 <- ''
  env_apt_4 <- ''
  env_mil_1 <- ''
  env_mil_2 <- ''
  env_mil_4 <- ''
  env_mil_6 <- ''
  env_mil_8 <- ''
  
} else {
  
  # define ranges and import data
  sheet <- country
  
  if (no_tcz >0) {
    range <- "A2:P80"
    env_apt_all <- read_range(env_apt_file, sheet, range) 
    env_apt_all <- env_apt_all |> 
      rename(a = 1) 
    
    # replace NAs by empty strings
    env_apt_all[is.na(env_apt_all)] <- ""
    
    env_apt_titles <- c("1. Overview",
                        "**1. Overview**",
                        "2. Additional Taxi-Out Time", 
                        "**2. Additional Taxi-Out Time**", 
                        "3. Additional ASMA Time",
                        "**3. Additional ASMA Time**",
                        "4. Share of arrivals applying CDO",
                        "**4. Share of arrivals applying CDO**",
                        "5. Appendix",
                        "**5. Appendix**"
    )
    
    env_apt_heading_positions <- which(env_apt_all$a %in% env_apt_titles)
    
    for (i in 1:length(env_apt_heading_positions)) {
      if (i<length(env_apt_heading_positions)) {
        mytext <- apply(env_apt_all[(env_apt_heading_positions[i]+1):(env_apt_heading_positions[i+1]-1), ], 1, function(col) paste(col, collapse = "<br>"))
      } else{
        mytext <- apply(env_apt_all[(env_apt_heading_positions[i]+1):(env_apt_heading_positions[i]+1), ], 1, function(col) paste(col, collapse = "<br>"))
        
      }
      # replace repeated breaks by single  
      mytext <- gsub("(<br>)+", "<br>", mytext)
      
      #remove empty elements of vector
      mytext <- mytext[mytext != "<br>"]
      mytext <- mytext[mytext != ""]
      
      #replace double breaks by single breaks
      pattern <- "(<br>){2,}"
      mytext <- str_replace_all(mytext, pattern, "<br>")
      
      ### remove leading and trailing line breaks
      mytext <- sub("^<br>|<br>$", "", mytext)
      
      ### assign text to variable
      assign(paste0("env_apt_", i, ""), paste0(mytext,collapse = ""))
    }
    
  
  }
  
  # env mil
  range <- "A2:P80"
  env_mil_all <- read_range(env_mil_file, sheet, range) 
  env_mil_all <- env_mil_all |> 
    rename(a = 1) 
  
  # replace NAs by empty strings
  env_mil_all[is.na(env_mil_all)] <- ""
  
  env_mil_titles <- c("Update on Military dimension of the plan",
                      "**Update on Military dimension of the plan**",
                      "Military - related measures implemented or planned to improve environment and capacity",														
                      "**Military - related measures implemented or planned to improve environment and capacity**",											
                      "Military - related measures implemented or planned to improve capacity", 
                      "**Military - related measures implemented or planned to improve capacity**",
                      "PI#6 Effective use of reserved or segregated airspace - national level",
                      "**PI#6 Effective use of reserved or segregated airspace - national level**",
                      "Initiatives implemented or planned to improve PI#6",
                      "**Initiatives implemented or planned to improve PI#6**",
                      "**PI#7 Rate of planning via available airspace structures - national level**",
                      "PI#7 Rate of planning via available airspace structures - national level",
                      "Initiatives implemented or planned to improve PI#7",
                      "**Initiatives implemented or planned to improve PI#7**",
                      "PI#8 Rate of using available airspace structures - national level",
                      "**PI#8 Rate of using available airspace structures - national level**",
                      "Initiatives implemented or planned to improve PI#8",
                      "**Initiatives implemented or planned to improve PI#8**"
  )
  
  env_mil_heading_positions <- which(env_mil_all$a %in% env_mil_titles)
  
  for (i in 1:length(env_mil_heading_positions)) {
    if (i<length(env_mil_heading_positions)) {
      mytext <- apply(env_mil_all[(env_mil_heading_positions[i]+1):(env_mil_heading_positions[i+1]-1), ], 1, function(col) paste(col, collapse = "<br>"))
    } else{
      mytext <- apply(env_mil_all[(env_mil_heading_positions[i]+1):(env_mil_heading_positions[i]+4), ], 1, function(col) paste(col, collapse = "<br>"))
      
    }
    # replace repeated breaks by single  
    mytext <- gsub("(<br>)+", "<br>", mytext)
    
    #remove empty elements of vector
    mytext <- mytext[mytext != "<br>"]
    mytext <- mytext[mytext != ""]
    
    #replace double breaks by single breaks
    pattern <- "(<br>){2,}"
    mytext <- str_replace_all(mytext, pattern, "<br>")
    
    ### remove leading and trailing line breaks
    mytext <- sub("^<br>|<br>$", "", mytext)
    
    ### assign text to variable
    assign(paste0("env_mil_", i, ""), paste0(mytext,collapse = ""))
  }
  
  # 
  # range <- "A3:O7"
  # env_mil_3 <- read_range(env_mil_file, sheet, range) 
  # 
  # range <- "A8:O12"
  # env_mil_2 <- read_range(env_mil_file, sheet, range) 
  # 
  # range <- "A30:O34"
  # env_mil_5 <- read_range(env_mil_file, sheet, range) 
  # 
  # range <- "A52:O56"
  # env_mil_8 <- read_range(env_mil_file, sheet, range) 
  # 
  # range <- "A74:O78"
  # env_mil_11 <- read_range(env_mil_file, sheet, range) 
}

# capacity ----

if (country == rp_full | country == "MUAC") {
  #we need to define this variables to avoid errors
  cap_er_nsa_2 <- ''
  cap_er_nsa_3 <- ''
  cap_er_nsa_4 <- ''
  cap_er_nsa_5 <- ''
  cap_er_nsa_6 <- ''
  cap_er_nsa_7 <- ''
  cap_er_nsa_8 <- ''
  cap_er_nsa_incentive  <- '' 
  
  cap_trm_nsa_1 <- ''
  cap_trm_nsa_2 <- ''
  cap_trm_nsa_3 <- ''
  cap_trm_nsa_4 <- ''
  cap_trm_nsa_5 <- ''
  cap_trm_nsa_6 <- ''
  
  cap_heading_ukraine <- 0
} else {
  
  # define ranges and import data
  sheet <- country
  # sheet <- "Czech Republic"
  
  # find headings to define ranges
  range <- "A3:O81"
  cap_er_all <- read_range(cap_file, sheet, range) 
  cap_er_all <- cap_er_all |> 
    # rename the columns where the text I need is
    rename(a = 1,
           b = 10,
           c = 11) |> 
    select(a, b, c)
  
  # replace NAs by empty strings
  cap_er_all[is.na(cap_er_all)] <- ""
  
  cap_titles <- c("Minutes of ATFM en-route delay",
                  "**Minutes of ATFM en-route delay**",
                  "NSA's assessment of capacity performance", 
                  "**NSA's assessment of capacity performance**", 
                  "Monitoring process for capacity performance",
                  "**Monitoring process for capacity performance**",
                  "Capacity Planning",
                  "**Capacity Planning**",
                  "ATCO in OPS (FTE)",
                  "**ATCO in OPS (FTE)**",
                  "Additional information relating to Russia's war of agression against Ukraine",
                  "**Additional information relating to Russia's war of agression against Ukraine**",
                  "Additional Information Related to Russia's War of Aggression Against Ukraine",
                  "**Additional Information Related to Russia's War of Aggression Against Ukraine**",
                  "Application of Corrective Measures for Capacity (if applicable)",
                  "**Application of Corrective Measures for Capacity (if applicable)**",
                  "Summary of capacity performance",
                  "**Summary of capacity performance**",
                  "En route Capacity Incentive Scheme",
                  "**En route Capacity Incentive Scheme**"
  )
  
  cap_heading_positions <- which(cap_er_all$a %in% cap_titles)
  cap_heading_ukraine <- which(cap_er_all$a %in% c("Additional information relating to Russia's war of agression against Ukraine", 
                                                   "**Additional information relating to Russia's war of agression against Ukraine**", 
                                                   "Additional Information Related to Russia's War of Aggression Against Ukraine",
                                                   "**Additional Information Related to Russia's War of Aggression Against Ukraine**"
  ))
  
  for (i in 2:length(cap_heading_positions)) {
    mytext <- apply(cap_er_all[(cap_heading_positions[i-1]+1):(cap_heading_positions[i]-1), ], 1, function(col) paste(col, collapse = "<br>"))
    
    # replace repeated breaks by single  
    mytext <- gsub("(<br>)+", "<br>", mytext)
    
    #remove empty elements of vector
    mytext <- mytext[mytext != "<br>"]
    
    #replace double breaks by single breaks
    pattern <- "(<br>){2,}"
    mytext <- str_replace_all(mytext, pattern, "<br>")
    
    ### remove leading and trailing line breaks
    mytext <- sub("^<br>|<br>$", "", mytext)
    
    ### assign text to variable
    assign(paste0("cap_er_nsa_", i), paste0(mytext,collapse = ""))
    
  }
  
  if (length(cap_heading_positions) == 7) {cap_er_nsa_8 = ""}
  
  # cap_er_nsa_9
  
  ## incentive scheme text --------------
  ### find observations positions
  observations_positions <- which(cap_er_all$b %in% c("Observations", "**Observations**"))
  observations_positions <- observations_positions[-1] # ignore first one
  
  ### build observations text
  mytext <- ""
  for (i in observations_positions) {
    # check if there is text for the observation
    check_observation <- if_else(cap_er_all$b[i+1] == "", FALSE, TRUE)
    
    if (check_observation) {
      mytext <- paste0(mytext,
                       if_else(cap_er_all$a[i] == "****", 
                               "",
                               paste0(cap_er_all$a[i], ": ")),
                       cap_er_all$b[i+1],
                       '<br>')
    }
  }
  
  ### add overall text at the end
  # find the last title Actual performance
  actual_perf_positions <- which(cap_er_all$a %in% c("Actual performance", "**Actual performance**"))
  actual_perf_positions <- actual_perf_positions[length(actual_perf_positions)] # we only need the last
  
  mytext_inc <- apply(cap_er_all[(actual_perf_positions+1):nrow(cap_er_all), ], 2, function(col) paste(col, collapse = "<br>"))
  
  # replace repeated breaks by single  
  mytext_inc <- gsub("(<br>)+", "<br>", mytext_inc)
  
  #remove empty elements of vector
  mytext_inc <- mytext_inc[mytext_inc != "<br>"]
  
  ### remove leading and trailing line breaks
  mytext_inc <- sub("^<br>|<br>$", "", mytext_inc)
  
  if(length(mytext_inc) >1) {
    cap_er_nsa_incentive <- paste0(mytext, mytext_inc[[1]])
  } else {
    cap_er_nsa_incentive <- paste0(mytext, mytext_inc)
  }
  
  ## ACC text --------------
  ### find observations positions
  observations_positions <- which(cap_er_all$c %in% c("Observations", "**Observations**"))
  
  ### build observations text
  mytext <- ""
  for (i in observations_positions) {
    # check if there is text for the observation
    check_observation <- if_else(cap_er_all$c[i+1] == "", FALSE, TRUE)
    # check if there is text for the acc
    check_acc <- if_else(cap_er_all$a[i+3] == "" | cap_er_all$a[i+3] == "****", 
                         FALSE, TRUE)
    
    if (check_observation) {
      mytext <- paste0(mytext,
                       if_else(cap_er_all$a[i] == "****", 
                               "",
                               paste0(cap_er_all$a[i], ": ")),
                       cap_er_all$c[i+1],
                       '<br>'
      )
    }
    
    if (check_acc) {
      mytext <- paste0(mytext, cap_er_all$a[i+3],'<br>')
    }    
    
  }
  
  mytext_acc <- mytext
  
  # replace repeated breaks by single  
  mytext_acc <- gsub("(<br>)+", "<br>", mytext_acc)
  
  #remove empty elements of vector
  mytext_acc <- mytext_acc[mytext_acc != "<br>"]
  
  ### remove leading and trailing line breaks
  mytext_acc <- sub("^<br>|<br>$", "", mytext_acc)
  
  cap_er_nsa_5 <- paste0(mytext_acc)
  
  ## terminal ----------------------------------------------------------- 
  # find headings to define ranges
  if (no_tcz > 0) {
    range <- "A2:U81"
    cap_trm_all <- read_range(cap_trm_file, sheet, range) 
    cap_trm_all <- cap_trm_all |> 
      rename(a = 1) 
    
    # replace NAs by empty strings
    cap_trm_all[is.na(cap_trm_all)] <- ""
    
    cap_trm_titles <- c("1. Overview",
                        "**1. Overview**",
                        "2. Arrival ATFM Delay", 
                        "**2. Arrival ATFM Delay**", 
                        "3. Arrival ATFM Delay – National Target and Incentive Scheme",
                        "**3. Arrival ATFM Delay – National Target and Incentive Scheme**",
                        "4. ATFM Slot Adherence",
                        "**4. ATFM Slot Adherence**",
                        "5. ATC Pre-departure Delay",
                        "**5. ATC Pre-departure Delay**",
                        "6. All Causes Pre-departure Delay",
                        "**6. All Causes Pre-departure Delay**",
                        "7. Appendix",
                        "**7. Appendix**"
    )
    
    cap_trm_heading_positions <- which(cap_trm_all$a %in% cap_trm_titles)
    
    for (i in 1:length(cap_trm_heading_positions)) {
      if (i<length(cap_trm_heading_positions)) {
        mytext <- apply(cap_trm_all[(cap_trm_heading_positions[i]+1):(cap_trm_heading_positions[i+1]-1), ], 1, function(col) paste(col, collapse = "<br>"))
      } else{
        mytext <- apply(cap_trm_all[(cap_trm_heading_positions[i]+1):(cap_trm_heading_positions[i]+1), ], 1, function(col) paste(col, collapse = "<br>"))
      }
      
      # replace repeated breaks by single  
      mytext <- gsub("(<br>)+", "<br>", mytext)
      
      #remove empty elements of vector
      mytext <- mytext[mytext != "<br>"]
      mytext <- mytext[mytext != ""]
      
      #replace double breaks by single breaks
      pattern <- "(<br>){2,}"
      mytext <- str_replace_all(mytext, pattern, "<br>")
      
      ### remove leading and trailing line breaks
      mytext <- sub("^<br>|<br>$", "", mytext)
      
      ### assign text to variable
      assign(paste0("cap_trm_nsa_", i), paste0(mytext,collapse = ""))
    }
  }
  
}

# cost-efficiency -----

get_cef_level2_text <- function(cz_index, cz_type) {
  
  ez <- cz_index
  
  # cz=c("@@cz_index@@", "@@cz_type@@")
  # 
  # # define cz
  # ez <- as.numeric(cz[[1]])
  # cztype <- cz[[2]]
  
  if (country == rp_full | country == "MUAC") {
    # to avoid errors when processing the file
    cef_txt_1_4_duc <- ''
    cef_txt_1_4_su <- '' 
    cef_txt_1_4_cost_all <- ''
    cef_txt_1_4_cost_main <- ''
    cef_txt_1_4 <- data.frame(a=c(NA,NA), b=c(NA,NA)) |> as_tibble()
    
    my_index_su <- 1
    my_index_cost_by_ent <- 1
    my_index_cost_main <- 1
    
    cef_txt_1_9 <- data.frame(a=c(NA,NA), b=c(NA,NA)) |> as_tibble()
    cef_txt_1_10_txt <- ''
    cef_txt_1_13 <- data.frame(a=c(NA,NA,NA,NA,NA), b=c(NA,NA,NA,NA,NA)) |> as_tibble()
    
  } else {
    # define ranges and import data
    ## 1st sheet
    sheet <- if_else(cztype == "terminal",
                     if_else(ez >1, paste0("5_TRM (", ez, ")"), "5_TRM"),
                     "1_ERT")
    ceff_file <- if_else(country == "Spain" & ez == 2, ceff_file_canarias, ceff_file)
    
    ## range 1.4
    range <- "C50:M71"
    cef_txt_1_4  <- read_range(ceff_file, sheet, range) %>% rename(a = 1)
    
    ### Check if the whole range is merged and apply different logic
    if (is.na(cef_txt_1_4[2,1]) == FALSE) {
      ### find specific headings
      my_index_su <- which(cef_txt_1_4$a %like% paste0(if_else(cztype == "terminal",
                                                               "Terminal",
                                                               "En route"), 
                                                       ' service units')) + 1
      
      if(identical(my_index_su, numeric(0)) == TRUE) {my_index_su <- which(cef_txt_1_4$a %like% paste0("Terminal charging zone ", ez,' service units')) + 1} 
      
      my_index_cost_by_ent <- which(cef_txt_1_4$a %like% paste0(if_else(cztype == "terminal",
                                                                        "Terminal",
                                                                        "En route"), 
                                                                ' costs by entity')) + 1
      
      if(identical(my_index_cost_by_ent, numeric(0)) == TRUE) {my_index_cost_by_ent <- which(cef_txt_1_4$a %like% paste0("Terminal charging zone ", ez,' costs by entity')) + 1} 
      
      my_index_cost_main <- which(cef_txt_1_4$a %like% paste0(if_else(cztype == "terminal",
                                                                      "Terminal",
                                                                      "En route"), 
                                                              " costs for the main ANSP")) + 1
      
      if(identical(my_index_cost_main, numeric(0)) == TRUE) {my_index_cost_main <- which(cef_txt_1_4$a %like% paste0("Terminal charging zone ", ez,' costs for the main ANSP')) + 1}
      if(identical(my_index_cost_main, numeric(0)) == TRUE) {my_index_cost_main <- which(cef_txt_1_4$a %like% paste0("Terminal charging zone costs for the main ANSP")) + 1}
      
      ### to avoid errors if else is executed
      cef_txt_1_4_duc <- ""
      cef_txt_1_4_su <- ""
      cef_txt_1_4_cost_all <- ""
      cef_txt_1_4_cost_main <- ""
      
    } else {
      ### find beginning and end of headings
      tx <- cef_txt_1_4[1,1] |> pull()
      heading1 <- str_locate(tx, fixed('AUC vs DUC')) + 7 
      if(is.na(heading1[1]) == TRUE) {heading1 <- str_locate(tx, fixed('AUC vs. DUC')) + 7}
      heading2 <- str_locate(str_replace_all(tx, "-", " "), fixed(paste0(if_else(cztype == "terminal",
                                                                                 "**Terminal",
                                                                                 "**En route"), 
                                                                         " service units"))) 
      
      heading3_1 <- str_locate(str_replace_all(tx, "-", " "), fixed(paste0(if_else(cztype == "terminal",
                                                                                   "**Terminal",
                                                                                   "**En route"),
                                                                           ' costs by entity at charging zone level**<br/>')))
      heading3_2 <- str_locate(str_replace_all(tx, "-", " "), fixed(paste0(if_else(cztype == "terminal",
                                                                                   "**Terminal",
                                                                                   "**En route"),
                                                                           ' costs by entity**<br/>')))
      
      heading4_beg <- str_locate(str_replace_all(tx, "-", " "), fixed(paste0(if_else(cztype == "terminal",
                                                                                     "**Terminal",
                                                                                     "**En route"),
                                                                             ' costs for the main ANSP')))
      all_bold_locations <- str_locate_all(tx, fixed('**'))
      heading_4 <- tail(all_bold_locations[[1]], n = 1)
      
      cef_txt_1_4_duc <- str_replace_all(substr(tx, max(heading1)+1, min(heading2)-1), fixed("<br/><br/>"), fixed("<br/>"))
      cef_txt_1_4_su <- str_replace_all(substr(tx, max(heading2)+1+7, min(heading3_1, heading3_2, na.rm = TRUE)-1) , fixed("<br/><br/>"), fixed("<br/>"))
      cef_txt_1_4_cost_all <- str_replace_all(substr(tx, max(heading3_1, heading3_2, na.rm = TRUE)+1, min(heading4_beg)-1) , fixed("<br/><br/>"), fixed("<br/>"))
      cef_txt_1_4_cost_main <- str_replace_all(substr(tx, heading_4 + 2, nchar(tx)) , fixed("<br/><br/>"), fixed("<br/>"))
      
      ### remove leading and trailing line breaks
      cef_txt_1_4_duc <- sub("^<br/>|<br/>$", "", cef_txt_1_4_duc)
      cef_txt_1_4_su <- sub("^<br/>|<br/>$", "", cef_txt_1_4_su)
      cef_txt_1_4_cost_all <- sub("^<br/>|<br/>$", "", cef_txt_1_4_cost_all)
      cef_txt_1_4_cost_main <- sub("^<br/>|<br/>$", "", cef_txt_1_4_cost_main)
      
      ### just to avoid errors
      my_index_su <- 2
      my_index_cost_by_ent <-2
      my_index_cost_main <- 2
      
    }
    
    ## 2nd sheet
    sheet <- if_else(cztype == "terminal",
                     if_else(ez >1, paste0("6_TRM (", ez, ")"), "6_TRM"),
                     "2_ERT")
    range <- "C11:M14"
    cef_txt_1_5  <- read_range(ceff_file, sheet, range)
    
    range <- "J16:M33"
    cef_txt_1_6  <- read_range(ceff_file, sheet, range)
    
    ## range 1.9
    range <- "C62:M71"
    cef_txt_1_9  <- read_range(ceff_file, sheet, range)
    
    ### check if title in the right spot
    mycolnames <- colnames(cef_txt_1_9)
    if (is.na(max(str_locate(mycolnames[1], "9. Focus"))) == TRUE) {
      range <- "C63:M71"
      cef_txt_1_9  <- read_range(ceff_file, sheet, range)
    }
    
    # range <- if_else(year_report == 2020 | year_report == 2023, "C62:M71", "C63:M71")
    
    ## 3rd sheet
    sheet <- if_else(cztype == "terminal",
                     if_else(ez >1, paste0("7_TRM_ATSP (", ez, ")"), "7_TRM_ATSP"),
                     "3_ATSP")
    
    range <- "C11:M22"
    cef_txt_1_10  <- read_range(ceff_file, sheet, range)
    
    ## find the notes in box 10
    cef_txt_1_10_txt <- cef_txt_1_10[1,1] 
    note_position <- str_locate(cef_txt_1_10_txt, fixed("**Note"))
    note_position <- if_else(is.na(note_position) == TRUE, 
                             str_locate(cef_txt_1_10_txt, fixed("Note")),
                             note_position)
    
    cef_txt_1_10_txt <- ifelse(is.na(note_position[1,1]) == TRUE, "",
                               substring(cef_txt_1_10_txt, note_position[1,1]))
    
    range <- if_else(year_report >= 2022, "C66:M71", "C64:M71")
    cef_txt_1_13  <- read_range(ceff_file, sheet, range)
    ## the titles are already bold in the .qmd below
    if (year_report >= 2023) {
      cef_txt_1_13 <- cef_txt_1_13 %>% 
        mutate_all(., ~ str_replace_all(., fixed("**"),""))
    }
    
    if (year_report == 2024) {
      cef_txt_1_13 <- cef_txt_1_13 %>%
        mutate_all(., ~ str_replace_all(., fixed("RP3 summary"), fixed("**RP3 summary**")))
      
    }
  }
  
  return(list(cef_txt_1_4 = cef_txt_1_4,
              cef_txt_1_4_duc = cef_txt_1_4_duc,
              cef_txt_1_4_su = cef_txt_1_4_su,
              cef_txt_1_4_cost_all =  cef_txt_1_4_cost_all,
              cef_txt_1_4_cost_main = cef_txt_1_4_cost_main,
              
              my_index_su = my_index_su,
              my_index_cost_by_ent = my_index_cost_by_ent,
              my_index_cost_main = my_index_cost_main,
              
              cef_txt_1_5 = cef_txt_1_5,
              cef_txt_1_6 = cef_txt_1_6,
              cef_txt_1_9 = cef_txt_1_9,
              cef_txt_1_10_txt = cef_txt_1_10_txt,
              cef_txt_1_13 = cef_txt_1_13
              )
         )
  
}
