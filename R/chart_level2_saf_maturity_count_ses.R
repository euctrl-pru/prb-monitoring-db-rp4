if (exists("country") == FALSE) {country <- rp_full}
if (!data_loaded) {
  source("R/get_data.R")
}

# import data  ----
data_raw  <-  saf_eosm_interdep_ses

# process data  ----
data_prep <- data_raw %>% 
  filter(year == year_report) %>% 
  mutate(
    xlabel = "ANSPs maturity",
    mymetric = if_else(is.na(number_of_ans_ps), 0, number_of_ans_ps),
    type = factor(level, levels = c("A", "B", "C", "D")),
    textposition = "auto",
    textlabel = paste0(level, ": ", mymetric)
    ) %>% 
  arrange(desc(type)) %>% 
  select(
    xlabel,
    type,
    mymetric,
    textposition,
    textlabel
  )


# chart ----
## legend
if (knitr::is_latex_output()) {
  local_height <- myheight
  local_title_text <- "Interdependencies –\nNumber of ANSPs per maturity level"
  local_title_font_size <- mytitle_font_size*1.1
  local_title_y <- 0.94
  local_legend_x <- 0.5
  local_legend_y <- -0.05
  local_legend_xanchor <- 'center'
  local_legend_font_size <- myfont*1.1
  local_margin <- list(t = 80)
} else {
  local_height <- myheight
  local_title_text <- "Interdependencies – Number of ANSPs per maturity level"
  local_title_font_size <- mytitle_font_size
  local_title_y <- mytitle_y
  local_legend_x <- 0.5
  local_legend_y <- -0.05
  local_legend_xanchor <- 'center'
  local_legend_font_size <- myfont
  local_margin <- list(t = 60)
}

# find starting angle
# Get total of the metric
total <- sum(data_prep$mymetric)
valueofd <- data_prep %>% filter(type == "D") %>% select(mymetric) %>% pull()

# Now convert to angle
rotation_angle <- valueofd/total * 360


# plot chart ----
mydonutchart(data_prep, 
             height = local_height,
             colors = c('#196AB4', '#00B0F0', '#FFC000', '#585858' ),
             hovertemplate = "%{label}: %{value}",
             title_text = local_title_text,
             title_font_size = local_title_font_size,
             title_y = local_title_y,
             minsize = 14,
             sort = FALSE,
             rotation = -rotation_angle,
             legend_traceorder = "reversed",
             legend_x = local_legend_x,
             legend_y = local_legend_y,
             legend_xanchor = local_legend_xanchor,
             legend_orientation = "h",
             legend_font_size = local_legend_font_size,
             margin = local_margin)


