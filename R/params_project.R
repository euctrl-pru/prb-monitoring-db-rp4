# rp parameters ----
rp <- 4
rp_full <- paste0("SES RP", rp)
rp_summary_year <- paste0('rp', rp)
rp_years <- c(2025, 2026, 2027, 2028, 2029)
rp_min_year <- min(rp_years)
rp_max_year <- max(rp_years)

# source data folders ----
data_folder <- 'G:/HQ/dgof-pru/Data/SES Monitoring Dashboard/RP4/data_for_web/'
data_folder_a2 <- paste0(data_folder, "monitoring_files/", year_report, "/")


# set site parameters ----
site_dir <- here("_site")
root_dir <- if_else(test_check == TRUE, 
                    '//ihx-vdm05/LIVE_var_www_performance$/prb-monitoring/rp4/test/',
                    '//ihx-vdm05/LIVE_var_www_performance$/prb-monitoring/rp4/prod/'
                    )
destination_dir <- paste0(root_dir, year_folder, '/')
home_address <- if_else(test_check == TRUE, 
                        'https://www.eurocontrol.int/performance/prb-monitoring/rp4/test',
                        'https://www.sesperformance.eu'
                        )
external_address <- if_else(test_check == TRUE,
                            str_replace(destination_dir,
                                        fixed('//ihx-vdm05/LIVE_var_www_performance$'),
                                        'https://www.eurocontrol.int/performance'),
                            str_replace(destination_dir,
                                        fixed(root_dir),
                                        paste0(home_address, '/'))
                                )

destination_dir_investments <- paste0(root_dir, "investments/", "rp4/" )
  
# set data files ----
statfor_mvt_file <- "statfor_forecast_en_route_mvt.xlsx"
statfor_tsu_file <- "statfor_forecast_en_route_tsu.xlsx"

# set graphs parameters ----
## web
mywidth = NULL
myheight = 300
myfont = 14
mymargin = list (t = 40)
mylinewidth = 3

mysuffix <- ""
mydecimals <- 2

PRBTargetColor <- '#FF0000'
PRBActualColor <- '#FFC000'
PRBPlannedColor <- '#5B9BD5'

PRBSecondBlue <- '#0070C0'

### trace parameters
mycolors <- PRBActualColor

mytextangle <- 0
mytextposition <- "outside"
myinsidetextanchor <- NULL
mytextfont_color <- 'black'
mytextfont_size <- myfont

mytrace_showlegend <- T

### layout parameters
myfont_family <- "Roboto"
mybargap <- 0.25
mybarmode <- 'group'
myhovermode <- "x unified"
myhoverlabel_bgcolor <- 'rgba(255,255,255,0.88)'
myminsize <- myfont*0.8

#### title
mytitle_text <- "Chart title"
mytitle_x <- 0.5
mytitle_y <- 0.99
mytitle_xanchor <- 'center'
mytitle_yanchor <- 'top'
mytitle_font_size <- myfont * 16/15

#### xaxis
myxaxis_title <- ''
myxaxis_gridcolor <- 'rgb(255,255,255)'
myxaxis_showgrid <- TRUE
myxaxis_showline <- FALSE
myxaxis_showticklabels <- TRUE
myxaxis_tickformat <- "0"
myxaxis_dtick <- 1
myxaxis_zeroline <- TRUE
myxaxis_tickfont_size <- myfont

#### yaxis
myyaxis_title <- "Y axis title"
myyaxis_gridcolor <- 'rgb(240,240,240)'
myyaxis_showgrid <- TRUE
myyaxis_showline <- FALSE
myyaxis_tickprefix <- ""
myyaxis_ticksuffix <- ""
myyaxis_tickformat <- ".1f"

myyaxis_zeroline <- TRUE
myyaxis_zerolinecolor <- 'rgb(240,240,240)'
myyaxis_titlefont_size <- myfont
myyaxis_tickfont_size <- myfont

#### legend
mylegend_traceorder <- 'normal'
mylegend_orientation <- 'h'
mylegend_xanchor <- "center"
mylegend_yanchor <- "center"
mylegend_x <- 0.5
mylegend_y <- -0.1
mylegend_font_size <- myfont

## pdf
mywidth_pdf = 300 *4
myheight_pdf = 220 *4
myfont_pdf = 64
mytitle_font_size_pdf <- 10 *4
mymargin_pdf = list (t = 30)
mylinewidth_pdf = 3
  
